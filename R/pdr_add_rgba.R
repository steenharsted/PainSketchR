#' Convert a single-row pain drawing to an RGBA raster array
#'
#' Renders the strokes and spray points from a single-row pain drawing tibble
#' into an RGBA numeric array. This is the low-level workhorse called by
#' [pdr_add_rgba()].
#'
#' @param .data A single-row pain drawing tibble as produced by
#'   [pdr_import_json()]. Must contain columns `id`, `s`, `p`, `w`, and `h`
#'   (or the names supplied via `col_*` arguments).
#'   Passing more than one row is an error.
#' @param method Character. Controls the rasterization back-end. Either:
#'   * `"memory"` (default) — renders in-memory via [ragg::agg_capture()],
#'     no temp file is written to disk.
#'   * `"file"` — saves to a temporary PNG via [ggplot2::ggsave()] and reads
#'     it back with [png::readPNG()]. Use this if the `"memory"` path produces
#'     unexpected rendering artefacts.
#' @param clean_up Logical. If `TRUE` (default), the temporary PNG file is
#'   deleted on exit. Only relevant when `method = "file"`. Set to `FALSE` to
#'   retain the file for debugging.
#' @param dpi Resolution used during rasterization. Defaults to `96`, matching
#'   the CSS pixel density of the web canvas where drawings are collected —
#'   this ensures a 1:1 pixel correspondence between the original drawing and
#'   the output. Increase for print-quality output (e.g. `300`), noting this
#'   does not affect the canvas dimensions, only output pixel density.
#' @param col_id,col_s,col_p,col_w,col_h Column name strings for the drawing
#'   ID, strokes list-column, points list-column, canvas width, and canvas
#'   height respectively. Defaults match the output of [pdr_import_json()]:
#'   `"id"`, `"s"`, `"p"`, `"w"`, `"h"`.
#'
#' @return A numeric array of dimensions `height × width × 4` (RGBA channels,
#'   values in \[0, 1\]). Transparent pixels are set to black
#'   (`RGB = 0`) rather than transparent white.
#'
#' @seealso [pdr_add_rgba()] for the user-facing vectorised wrapper.
#'
#' @examples
#' \dontrun{
#' pd <- pdr_import_json("data-raw/two_geoms.json")
#' raster <- pdr_add_rgba_single(pd[1, ])
#' grid::grid.newpage()
#' grid::grid.raster(raster)
#' }
#'
#' @importFrom dplyr select mutate filter full_join n
#' @importFrom tidyr unnest uncount
#' @importFrom png readPNG
#' @importFrom ragg agg_capture
#' @importFrom ggplot2 ggplot aes coord_fixed theme_void scale_color_identity scale_size_identity scale_alpha_identity scale_linewidth_identity geom_path geom_point ggsave
#' @importFrom cli cli_abort
#'
#' @export
pdr_add_rgba_single <- function(
  paindrawr_data = pdr_data,
  method = "memory",
  clean_up = TRUE,
  dpi = 96
) {

  # Check data
  pdr_check_data(paindrawr_data, verbose = FALSE)

  #
  method <- match.arg(method, choices = c("memory", "file"))

  # Extract height and width from the single row
  image_width <- purrr::map_dbl(.x = paindrawr_data, .f = \(list) list$.width )
  image_height <- purrr::map_dbl(.x = paindrawr_data, .f = \(list) list$.heigth )

  return(list(image_width, image_height))

 

  # Unnest and join the s (strokes) and p (points) list-columns
  pdr_s <- .data |>
    dplyr::select(dplyr::all_of(c(col_id, col_s))) |>
    tidyr::unnest(cols = dplyr::all_of(col_s))

  pdr_p <- .data |>
    dplyr::select(dplyr::all_of(c(col_id, col_p))) |>
    tidyr::unnest(cols = dplyr::all_of(col_p))

  pd <- dplyr::full_join(pdr_s, pdr_p, by = c(col_id, "i")) # "i" is the stroke-index-column that comes from unnesting s and p

  # Scale brush width (pixels) to mm for ggplot rendering
  pd <- pd |>
    dplyr::mutate(size_mm = pdr_scale_bw(bw))

  # Split into pen and spray subsets
  pdr_pen <- dplyr::filter(pd, t == "pen")
  pdr_spray <- dplyr::filter(pd, t == "spray")

  # Recreate spray jitter — only when spray strokes exist
  if (nrow(pdr_spray) > 0) {
    pdr_spray <- pdr_spray |>
      tidyr::uncount(weights = .data$pd) |>

      # Uniform distribution within a circle of radius pr
      dplyr::mutate(
        angle = runif(dplyr::n(), 0, 2 * pi),
        radius = sqrt(runif(dplyr::n(), 0, 1)) * pr
      ) |>
      dplyr::mutate(
        x = x + radius * cos(angle),
        y = y + radius * sin(angle)
      )
  }

  # Base plot (pen strokes)
  pdr_base <- pdr_pen |>
    ggplot2::ggplot(ggplot2::aes(
      x = x,
      y = y,
      color = c,
      alpha = a / 255
    )) +
    ggplot2::coord_fixed(
      xlim = c(0, image_width),
      ylim = c(0, image_height),
      expand = FALSE
    ) +
    ggplot2::theme_void(base_size = 12) +
    ggplot2::scale_color_identity() +
    ggplot2::scale_size_identity() +
    ggplot2::scale_alpha_identity() +
    ggplot2::scale_linewidth_identity() +
    ggplot2::geom_path(
      ggplot2::aes(
        group = paste0(.data[[col_id]], "_", i),
        linewidth = size_mm + 1
      ),
      linetype = 1
    )

  # Add spray points on top
  plot_out <- pdr_base +
    ggplot2::geom_point(
      data = pdr_spray,
      ggplot2::aes(size = size_mm),
      shape = 15
    )

  # Translate canvas pixels to mm for the rendering device
  mm_per_inch <- 25.4
  pixel_to_mm <- mm_per_inch / 96 # physical canvas size is fixed at 96 dpi
  width_mm <- image_width * pixel_to_mm
  height_mm <- image_height * pixel_to_mm

  if (method == "memory") {
    arr <- ragg::agg_capture(
      width = width_mm,
      height = height_mm,
      units = "mm",
      res = dpi,
      bg = "transparent"
    )
    print(plot_out) # must explicitly print to render to the device
    cap <- arr()
    dev.off()

    # Convert colour names → integer RGBA → numeric [0,1] array
    rgba_int <- col2rgb(cap, alpha = TRUE) # 4 × (w*h) matrix, values 0-255
    rgba_arr <- array(
      t(rgba_int) / 255, # normalise to [0,1]
      dim = c(image_height, image_width, 4) # height × width × RGBA
    )
  } else {
    tmp <- tempfile(fileext = ".png")
    if (clean_up) {
      on.exit(unlink(tmp), add = TRUE)
    }

    ggplot2::ggsave(
      plot = plot_out,
      file = tmp,
      width = width_mm,
      height = height_mm,
      units = "mm",
      dpi = dpi
    )

    rgba_arr <- png::readPNG(tmp)
  }

  # Make transparent pixels black (rather than transparent white)
  rgba_arr[,, 1:3][rgba_arr[,, 4] == 0] <- 0

  rgba_arr
}


#' Add RGBA raster arrays to a pain drawing tibble
#'
#' Renders each row of a pain drawing tibble into an RGBA numeric array and
#' returns a list suitable for use as a new column via [dplyr::mutate()].
#'
#' @param .data A pain drawing tibble as produced by [pdr_import_json()]. Must
#'   contain columns `id`, `s`, `p`, `w`, and `h` (or the names supplied via
#'   `col_*` arguments).
#' @param method Character. Controls the rasterization back-end. Either
#'   `"memory"` (default, via [ragg::agg_capture()]) or `"file"` (via
#'   [ggplot2::ggsave()] and [png::readPNG()]). Passed through to
#'   [pdr_add_rgba_single()].
#' @param clean_up Logical. If `TRUE` (default), temporary PNG files are
#'   deleted after each raster is read into memory. Only relevant when
#'   `method = "file"`. Passed through to [pdr_add_rgba_single()].
#' @param dpi Resolution used during rasterization. Defaults to `96`, matching
#'   the CSS pixel density of the web canvas where drawings are collected.
#'   All rows are rendered at the same `dpi`.
#' @param col_id,col_s,col_p,col_w,col_h Column name strings for the drawing
#'   ID, strokes list-column, points list-column, canvas width, and canvas
#'   height respectively. Defaults match the output of [pdr_import_json()]:
#'   `"id"`, `"s"`, `"p"`, `"w"`, `"h"`. Passed through to
#'   [pdr_add_rgba_single()].
#'
#' @return A list of numeric arrays, one per row, each of dimensions
#'   `height × width × 4` (RGBA channels, values in \[0, 1\]).
#'
#' @seealso [pdr_add_rgba_single()] for the single-row primitive,
#'   [pdr_import_json()] for reading pain drawing JSON files.
#'
#' @examples
#' \dontrun{
#' pd <- pdr_import_json(c("data-raw/two_geoms.json", "data-raw/four_geoms.json"))
#'
#' # Add RGBA arrays as a new column
#' pd <- pd |> dplyr::mutate(rgba = pdr_add_rgba(pd))
#'
#' # Display the first drawing
#' grid::grid.newpage()
#' grid::grid.raster(pd$rgba[[1]])
#' }
#'
#' @importFrom purrr map
#' @importFrom cli cli_abort
#'
#' @export
pdr_add_rgba <- function(
  .data,
  method = "memory",
  clean_up = TRUE,
  dpi = 96,
  col_id = "id",
  col_s = "s",
  col_p = "p",
  col_w = "w",
  col_h = "h"
) {
  purrr::map(
    seq_len(nrow(.data)),
    \(i) {
      pdr_add_rgba_single(
        .data[i, ],
        method = method,
        clean_up = clean_up,
        dpi = dpi,
        col_id = col_id,
        col_s = col_s,
        col_p = col_p,
        col_w = col_w,
        col_h = col_h
      )
    }
  )
}
