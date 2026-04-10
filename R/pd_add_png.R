#' Convert a single-row pain drawing to a PNG raster array
#'
#' Renders the strokes and spray points from a single-row pain drawing tibble
#' into a PNG raster array. This is the low-level workhorse function; for
#' processing multiple rows use [pd_add_png()].
#'
#' @param .data A single-row pain drawing tibble as produced by
#'   [pd_json2pd()]. Must contain columns `id`, `s`, `p`, `w`, and `h`.
#'   Passing more than one row is an error.
#' @param clean_up Logical. If `TRUE` (default), the temporary `.png` file
#'   written by [ggplot2::ggsave()] is deleted after the raster is read into
#'   memory. Set to `FALSE` to retain the file for debugging.
#' @param dpi Resolution passed to [ggplot2::ggsave()]. Defaults to `96`,
#'   matching the CSS pixel density of the web canvas where drawings are
#'   collected — this ensures a 1:1 pixel correspondence between the original
#'   drawing and the output. Increase for print-quality output (e.g. `300`),
#'   noting this does not affect the canvas dimensions, only output pixel
#'   density. Must match the `dpi` used in [pd_add_png()] to ensure consistent
#'   array dimensions across the `.png` column.
#'
#' @return A numeric array of dimensions `height × width × 4` (RGBA channels,
#'   values in \[0, 1\]) as returned by [png::readPNG()]. Array dimensions
#'   scale with `dpi`.
#'
#' @seealso [pd_add_png()] for the user-facing multi-row wrapper.
#'
#' @examples
#' \dontrun{
#' pd <- pd_json2pd("data-raw/two_geoms.json")
#' raster <- pd_to_png_single(pd[1, ])
#' grid::grid.newpage()
#' grid::grid.raster(raster)
#' }
#'
#' @export
pd_to_png_single <- function(.data, clean_up = TRUE, dpi = 96) {
  if (nrow(.data) != 1L) {
    cli::cli_abort(
      "{.fn pd_to_png_single} expects a single-row tibble, but received {nrow(.data)} rows.
      Use {.fn pd_add_png} to process multiple rows."
    )
  }

  # Extract height and width from the single row
  image_width <- .data$w[[1]]
  image_height <- .data$h[[1]]

  # Unnest and join the s (strokes) and p (points) list-columns
  pd_s <- .data |>
    dplyr::select(id, s) |>
    tidyr::unnest(cols = s)

  pd_p <- .data |>
    dplyr::select(id, p) |>
    tidyr::unnest(cols = p)

  pd <- dplyr::full_join(pd_s, pd_p, by = dplyr::join_by(id, i))

  # Scale brush width (pixels) to mm for ggplot rendering
  pd <- pd |>
    dplyr::mutate(size_mm = pd_scale_bw(bw))

  # Split into pen and spray subsets
  pd_pen <- dplyr::filter(pd, t == "pen")
  pd_spray <- dplyr::filter(pd, t == "spray")

  # Recreate spray jitter — only when spray strokes exist
  if (nrow(pd_spray) > 0) {
    pd_spray <- pd_spray |>

      # Replicate points by spray density; rename to avoid shadowing outer 'pd'
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
  pd_base <- pd_pen |>
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
        group = paste0(id, "_", i),
        linewidth = size_mm + 1
      ),
      linetype = 1
    )

  # Add spray points on top
  plot_out <- pd_base +
    ggplot2::geom_point(
      data = pd_spray,
      ggplot2::aes(size = size_mm),
      shape = 15
    )

  ### Translate size
  mm_per_inch <- 25.4
  pixel_to_mm <- mm_per_inch / dpi
  width_mm <- image_width * pixel_to_mm
  height_mm <- image_height * pixel_to_mm

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

  png::readPNG(tmp)
}


#' Add a PNG raster column to a pain drawing tibble
#'
#' Renders each row of a pain drawing tibble into a PNG raster array and stores
#' the results in a new `.png` list-column. This is the user-facing wrapper
#' around [pd_to_png_single()].
#'
#' @param .data A pain drawing tibble as produced by [pd_json2pd()], with one
#'   row per drawing. Input is validated with [pd_check_data()] before
#'   processing.
#' @param clean_up Logical. If `TRUE` (default), temporary `.png` files are
#'   deleted after each raster is read into memory. Passed through to
#'   [pd_to_png_single()].
#' @param dpi Resolution passed to [ggplot2::ggsave()] for each drawing.
#'   Defaults to `96`, matching the CSS pixel density of the web canvas where
#'   drawings are collected — this ensures a 1:1 pixel correspondence between
#'   the original drawing and the output. Increase for print-quality output
#'   (e.g. `300`), noting this does not affect canvas dimensions, only output
#'   pixel density. All rows are rendered at the same `dpi`, ensuring consistent
#'   array dimensions across the `.png` column.
#'
#' @return The input tibble with an additional `.png` list-column (placed after
#'   `s`). Each element is a numeric array of dimensions
#'   `height × width × 4` (RGBA channels, values in \[0, 1\]). Array dimensions
#'   scale with `dpi`.
#'
#' @seealso [pd_to_png_single()] for the single-row primitive,
#'   [pd_json2pd()] for reading pain drawing JSON files.
#'
#' @examples
#' \dontrun{
#' pd <- pd_json2pd(c("data-raw/two_geoms.json", "data-raw/four_geoms.json"))
#' pd <- pd |> pd_add_png()
#'
#' # Display the first drawing
#' grid::grid.newpage()
#' grid::grid.raster(pd$.png[[1]])
#' }
#'
#' @export
pd_add_png <- function(.data, clean_up = TRUE, dpi = 96) {
  pd_check_data(.data)

  rasters <- purrr::map(
    seq_len(nrow(.data)),
    \(i) pd_to_png_single(.data[i, ], clean_up = clean_up, dpi = dpi)
  )

  dplyr::mutate(.data, .png = rasters, .after = s)
}
