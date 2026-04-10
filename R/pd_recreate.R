#' Recreate a pain drawing as a ggplot
#'
#' Renders stroke and spray data from a pain drawing tibble into a ggplot,
#' optionally composited with a background image. When `rasterize = TRUE`
#' (default), the plot is first saved to a temp file and returned as a
#' pixel-accurate raster layer — preserving exact canvas proportions. When
#' `rasterize = FALSE`, the raw ggplot with vector strokes is returned instead.
#'
#' Multiple drawings are arranged with [ggplot2::facet_wrap()]. A warning is
#' issued when more than 9 drawings are passed, as rendering may be slow.
#'
#' @param .data A pain drawing tibble as produced by [pd_json2pd()]. Must
#'   contain columns `id`, `s`, `p`, `w`, and `h`. All drawings must share the
#'   same canvas dimensions (`w` and `h`).
#' @param background_image Optional background image displayed behind the
#'   strokes. Accepts either:
#'   * A file path to a PNG file (character string), or
#'   * A numeric array as returned by [png::readPNG()].
#'
#'   Defaults to `NULL` (no background).
#' @param include_id Logical. If `TRUE`, the `id` value is shown as a facet
#'   strip label above each panel. Default is `FALSE`.
#' @param rasterize Logical. If `TRUE` (default), the plot is rendered to a
#'   temp PNG via [ggplot2::ggsave()], read back with [png::readPNG()], and
#'   returned as a new ggplot with [ggplot2::annotation_raster()]. The output
#'   is pixel-accurate with a locked aspect ratio. If `FALSE`, the raw ggplot
#'   with vector strokes is returned.
#' @param clean_up Logical. If `TRUE` (default), the temporary PNG file created
#'   during rasterization is deleted on exit. Set to `FALSE` to retain the file
#'   for debugging.
#'
#' @return A [ggplot2::ggplot()] object. When `rasterize = TRUE`, the plot
#'   contains a single [ggplot2::annotation_raster()] layer with a locked
#'   aspect ratio. When `rasterize = FALSE`, the plot contains vector stroke
#'   layers and supports further ggplot2 additions.
#'
#' @seealso [pd_json2pd()] to read pain drawing JSON files,
#'   [pd_add_png()] to store raster arrays as a column in the tibble,
#'   [pd_to_png_single()] for the single-row raster primitive.
#'
#' @examples
#' \dontrun{
#' pd <- pd_json2pd(c("data-raw/two_geoms.json", "data-raw/four_geoms.json"))
#'
#' # Default: rasterized output with background
#' pd |> pd_recreate_drawing(background_image = "inst/background.png")
#'
#' # Vector output — can add further ggplot layers
#' pd |> pd_recreate_drawing(rasterize = FALSE)
#'
#' # Show id labels in facet strips
#' pd |> pd_recreate_drawing(include_id = TRUE)
#' }
#'
#' @importFrom dplyr select mutate filter full_join join_by n
#' @importFrom tidyr unnest uncount
#' @importFrom png readPNG
#' @importFrom ggplot2 ggplot aes coord_fixed theme_void theme element_blank
#'   scale_color_identity scale_size_identity scale_alpha_identity
#'   scale_linewidth_identity geom_path geom_point facet_wrap
#'   annotation_raster ggsave
#' @importFrom cli cli_warn cli_abort
#'
#' @export
pd_recreate_drawing <- function(
  .data,
  background_image = NULL,
  include_id = FALSE,
  rasterize = TRUE,
  clean_up = TRUE
) {
  # Making sure data has required columns
  pd_check_data(.data)

  # Test that we only 1 height and width value, respectively
  if (
    (.data$w |> unique() |> length() > 1) ||
      (.data$h |> unique() |> length() > 1)
  ) {
    cli::cli_abort(
      "All drawings must share the same canvas dimensions, but multiple
      values of {.field w} or {.field h} were found. Process one canvas
      size at a time."
    )
  }

  # Validate and process background_image
  if (!is.null(background_image)) {
    if (is.character(background_image)) {
      # It's a file path
      if (!file.exists(background_image)) {
        cli::cli_abort(
          "Background image file does not exist: {.path {background_image}}"
        )
      }
      if (!grepl("\\.png$", background_image, ignore.case = TRUE)) {
        cli::cli_abort(
          "Background image must be a PNG file (ending in {.val .png})."
        )
      }
      background_image <- png::readPNG(background_image)
    } else if (!is.numeric(background_image) || !is.array(background_image)) {
      # Not a numeric array (expected output from png::readPNG)
      cli::cli_abort(
        "{.arg background_image} must be either a file path to a PNG image
        (character string) or a raster array from {.fn png::readPNG}."
      )
    }
  }

  # Extract number of ids
  id_n <- nrow(.data)

  # Warn user if numbers of ids > 9
  if (id_n > 9) {
    cli::cli_warn(
      "Recreating {id_n} drawings. This may be slow for large numbers of panels."
    )
  }

  # Extract height and width
  image_width <- .data$w |> unique()
  image_height <- .data$h |> unique()

  # Unnest and Join the s and p columns
  pd_s <- .data |>
    dplyr::select(id, s) |>
    tidyr::unnest(cols = s)

  pd_p <- .data |>
    dplyr::select(id, p) |>
    tidyr::unnest(cols = p)

  .data <- dplyr::full_join(pd_s, pd_p, by = dplyr::join_by(id, i))

  # map bw to size in mm using scale_bw funtion
  .data <- .data |>
    dplyr::mutate(
      size_mm = pd_scale_bw(bw)
    )

  ## Spray and Pen needs to be plotted differently
  ## We achieve this by making a coordinate set for Spray and Pen, respectively

  pd_pen <- .data |>
    dplyr::filter(t == "pen")

  pd_spray <- .data |>
    dplyr::filter(t == "spray")

  ### Recreate jitter in spray Data
  ### But only if spray data exists

  if (nrow(pd_spray) > 0) {
    pd_spray <- pd_spray |>

      # Create extra rows according to spray density (pd)
      tidyr::uncount(weights = .data$pd) |>

      # Generate random angles and radii for uniform distribution within a circle
      # pr holds information about the spray radius
      dplyr::mutate(
        angle = runif(dplyr::n(), 0, 2 * pi),
        radius = sqrt(runif(dplyr::n(), 0, 1)) * pr
      ) |>

      # Calculate offsets and new x, y positions
      dplyr::mutate(
        offsetX = radius * cos(angle),
        offsetY = radius * sin(angle),
        x = x + offsetX,
        y = y + offsetY
      )
  }

  ## Lets plot!

  ### Base plot
  pd_base <- pd_pen |>
    ggplot2::ggplot(ggplot2::aes(
      x = x,
      y = y,
      color = c,
      alpha = a / 255
    ))

  ### If background image is provided, we plot it now
  if (!is.null(background_image)) {
    pd_base <- pd_base +
      ggplot2::annotation_raster(
        as.raster(background_image),
        -Inf,
        Inf,
        -Inf,
        Inf
      )
  }

  ### Build plot(s) with pen strokes
  pd_base <- pd_base +
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

  plot_out <- pd_base +
    ggplot2::geom_point(
      data = pd_spray,
      ggplot2::aes(size = size_mm),
      shape = 15
    )

  plot_out <- plot_out +
    ggplot2::facet_wrap(facets = ~id)

  if (!include_id) {
    plot_out <- plot_out +
      ggplot2::theme(strip.text = ggplot2::element_blank())
  }

  if (!rasterize) {
    return(plot_out)
  }

  ### Translate size to mm, accounting for facet grid dimensions
  pixels_per_inch <- 96
  mm_per_inch <- 25.4

  # ncol defaults to what facet_wrap uses: ceiling(sqrt(n))
  n_col <- ceiling(sqrt(id_n))
  n_row <- ceiling(id_n / n_col)

  pixel_to_mm <- mm_per_inch / pixels_per_inch # ≈ 0.2646 mm per pixel
  width_mm <- image_width * pixel_to_mm * n_col # ≈ 119.07 mm for single image
  height_mm <- image_height * pixel_to_mm * n_row # ≈ 132.29 mm for single image

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
    dpi = 96
  )

  raster_plot <- ggplot2::ggplot() +
    ggplot2::theme_void(base_size = 12) +
    ggplot2::coord_fixed(ratio = height_mm / width_mm) +
    ggplot2::annotation_raster(
      as.raster(png::readPNG(tmp)),
      -Inf,
      Inf,
      -Inf,
      Inf,
      interpolate = TRUE
    )

  return(raster_plot)
}
