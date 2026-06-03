# Creates faceted density heatmaps showing how pain location patterns vary
#' across one or two stratification variables.
#'
#' @param .data A tibble with a single list-column of pain drawing data, as
#'   produced by [pdr_import_json()]. Each row holds a named list containing
#'   the drawing data for one recording. The name of that list-column is
#'   specified via `paindrawr_data`. All drawings must share the same canvas
#'   dimensions (`.width` and `.height`). Any additional columns (e.g.
#'   grouping variables) are preserved and available via `variables`.
#' @param paindrawr_data The name of the list-column in `.data` that contains
#'   the pain drawing data. Defaults to `pdr_data`. The column is unpacked with
#'   [tidyr::unnest_wider()] before processing; the resulting columns are
#'   expected to include `.id`, `.width`, `.height`, `.strokes`, `.points`,
#'   `.tool`, `.tool_width`, `.color`, `.alpha`, `.spray_radius`, and
#'   `.point_density`.
#' @param variables Character vector of length 1 or 2 specifying the grouping
#'   variable names present in `.data`. Numeric variables are binned into
#'   quantile groups; categorical variables use their existing levels.
#' @param n_groups Integer vector matching the length of `variables`. Specifies
#'   the number of groups to create for each variable. Default is `c(2, 2)`.
#'   For categorical variables, this must match the number of unique levels;
#'   if it does not, the actual number of levels is used with a message.
#' @param equal_n Logical. If `TRUE`, balances sample sizes across groups by
#'   sampling. Default is `TRUE`.
#' @param max_n Integer. Maximum number of observations per group when
#'   `equal_n = TRUE`. Default is `1000`.
#' @param background_image Optional background image displayed behind the
#'   heatmap. Accepts either:
#'   * A file path to a PNG file (character string), or
#'   * A numeric array as returned by [png::readPNG()].
#'
#'   Defaults to `NULL` (no background).
#' @param grid_size Integer. Coordinate binning resolution in pixels. Smaller
#'   values create finer grids but increase computation time. Default is `10`.
#'   Recommended range is 5–50 pixels; a warning is issued outside this range.
#' @param point_size Numeric. Size of points in the heatmap. Default is `0.25`.
#' @param min_alpha Numeric. Minimum alpha value for points. Default is `0.1`.
#' @param color_scale Character or numeric. How to scale colours across facets:
#'   * `"max"` (default) — scales all facets to the global maximum density.
#'   * `"facet"` — each facet is scaled independently.
#'   * A numeric value — sets a specific upper limit for the colour scale.
#' @param label_format Character. Format for quantile group labels:
#'   * `"pct"` (default) — percentage ranges.
#'   * `"num"` — numeric ranges.
#'   * `"both"` — combined markdown labels (requires `strips_in_markdown = TRUE`).
#' @param show_n Logical. If `TRUE`, displays sample sizes as text in facet
#'   strips. Default is `TRUE`.
#' @param strips_in_markdown Logical. If `TRUE`, renders strip text using
#'   [ggtext::element_markdown()], enabling markdown and HTML formatting in
#'   facet labels. Required when `label_format = "both"`. Default is `TRUE`.
#'
#' @return A named list with two elements:
#'   \item{plot}{A [ggplot2::ggplot()] object with the faceted heatmap.}
#'   \item{data}{A tibble with the density calculations underlying the plot,
#'     containing columns `.id`, `.index`, `.q`, `.tool`, `.tool_width`,
#'     `.color`, `.alpha`, `.spray_radius`, `.point_density`, grouping columns
#'     (`.VAR1_grp`, optionally `.VAR2_grp`), `.x`, `.y`, `x_bin`, `y_bin`,
#'     `n_id_at_location`, `n_group`, and `pct`.}
#'
#' @details
#' The function processes data in the following steps:
#' \enumerate{
#'   \item Validates inputs and unpacks the `paindrawr_data` list-column with
#'     [tidyr::unnest_wider()].
#'   \item Extracts canvas dimensions from `.width` and `.height`; all drawings
#'     must share identical dimensions.
#'   \item Creates stratification groups (quantile binning for numeric
#'     variables, existing levels for categorical variables).
#'   \item Optionally balances sample sizes across groups.
#'   \item Unnests `.strokes` and `.points` and joins them on `.id` and
#'     `.index`.
#'   \item Recreates spray effects for any rows where `.tool == "spray"`.
#'   \item Bins coordinates into grid cells and calculates pain density as the
#'     percentage of participants with a mark at each location.
#'   \item Renders a faceted heatmap with a viridis colour scale.
#' }
#'
#' @section Warnings:
#' The function will warn when:
#' \itemize{
#'   \item More than 16 facets are created (may be slow).
#'   \item `grid_size` is outside the recommended range of 5–50 pixels.
#'   \item NA values are removed from grouping variables.
#'   \item Both pen and spray tools are detected in the same dataset.
#' }
#'
#' @examples
#' \dontrun{
#' # Two-way stratification
#' pd |>
#'   pdr_plot_heatmap(
#'     variables = c("age_group", "pain_duration"),
#'     n_groups = c(3, 2)
#'   )
#'
#' # Single variable with custom settings
#' result <- pd |>
#'   pdr_plot_heatmap(
#'     variables = "pain_intensity",
#'     n_groups = 4,
#'     grid_size = 5,
#'     background_image = "inst/background.png"
#'   )
#'
#' # Use a custom list-column name
#' pd |>
#'   pdr_plot_heatmap(
#'     paindrawr_data = my_col,
#'     variables = "group"
#'   )
#' }
#'
#' @importFrom dplyr select mutate filter full_join join_by n_distinct slice_sample summarize all_of count left_join distinct pull n count left_join distinct pull
#' @importFrom tidyr unnest_wider
#' @importFrom tidyr unnest uncount
#' @importFrom png readPNG
#' @importFrom scales percent
#' @importFrom ggplot2 ggplot aes coord_fixed theme_minimal scale_colour_viridis_c geom_jitter facet_wrap facet_grid annotation_raster labs theme element_rect element_blank element_text geom_text guide_colorbar guides vars
#' @importFrom ggtext element_markdown
#' @importFrom vctrs vec_ptype_abbr
#'
#' @export
pdr_plot_heatmap <- function(
  .data,
  paindrawr_data = pdr_data,
  variables = NULL,
  n_groups = c(2, 2),
  equal_n = TRUE,
  max_n = 1000,
  background_image = NULL,
  grid_size = 10,
  point_size = 0.25,
  min_alpha = 0.1,
  color_scale = "max",
  label_format = "pct",
  show_n = TRUE,
  strips_in_markdown = TRUE
) {
  # ============================================================================
  # STEP 1: Validate inputs
  # ============================================================================
  message("Validating inputs...")

  # Making sure data has required columns
  pdr_check_data(.data$pdr_data, verbose = FALSE)

  # Unnest list
  .data <- .data |> tidyr::unnest_wider({{ paindrawr_data }})

  # Validating input specific to pdr_plot_heatmap
  validate_heatmap_inputs(
    .data = .data,
    variables = variables,
    n_groups = n_groups,
    grid_size = grid_size,
    label_format = label_format,
    color_scale = color_scale
  )

  # Adjust n_groups if only one variable provided
  if (length(variables) == 1) {
    n_groups <- n_groups[1]
  }

  # Warn about large facet counts
  if (prod(n_groups) > 16) {
    warning(
      "Creating ",
      prod(n_groups),
      " facets may be slow and produce large plots.",
      call. = FALSE
    )
  }

  # ============================================================================
  # STEP 2: Extract canvas dimensions
  # ============================================================================
  message("\nExtracting canvas dimensions...")

  image_width <- .data$.width |> unique()
  image_height <- .data$.height |> unique()

  if (length(image_width) != 1 || length(image_height) != 1) {
    stop(
      "Data must have single unique width and height values.\n",
      "You can only create heatmaps from one type of canvas at a time.",
      call. = FALSE
    )
  }

  # Validate and process background_image (same logic as pdr_recreate_drawing)
  if (!is.null(background_image)) {
    if (is.character(background_image)) {
      if (!file.exists(background_image)) {
        stop(
          "background_image file does not exist: ",
          background_image,
          call. = FALSE
        )
      }
      if (!grepl("\\.png$", background_image, ignore.case = TRUE)) {
        stop(
          "background_image must be a PNG file (ending in .png)",
          call. = FALSE
        )
      }
      background_image <- png::readPNG(background_image)
    } else if (!is.numeric(background_image) || !is.array(background_image)) {
      stop(
        "background_image must be either:\n",
        "  - A file path to a PNG image (character string)\n",
        "  - A raster array from png::readPNG() (numeric array)",
        call. = FALSE
      )
    }
  }

  # ============================================================================
  # STEP 3: Create stratification groups BEFORE unnesting
  # ============================================================================
  message("\nCreating stratification groups...")

  data_grouped <- stratify_data(
    .data = .data,
    variables = variables,
    n_groups = n_groups,
    label_format = label_format
  )

  # ============================================================================
  # STEP 4: Balance groups (if requested)
  # ============================================================================
  if (equal_n) {
    message("\nBalancing group sizes...")

    group_vars <- paste0(".VAR", seq_along(variables), "_grp")
    data_grouped <- balance_groups(
      .data = data_grouped,
      group_vars = group_vars,
      max_n = max_n
    )
  }

  # ============================================================================
  # STEP 5: Unnest coordinates
  # ============================================================================
  message("\nUnnesting coordinate data...")

  group_vars <- paste0(".VAR", seq_along(variables), "_grp")

  pdr_s <- data_grouped |>
    dplyr::select(dplyr::all_of(c(".id", ".strokes", group_vars))) |>
    tidyr::unnest(cols = .strokes)

  pdr_p <- data_grouped |>
    dplyr::select(dplyr::all_of(c(".id", ".points"))) |>
    tidyr::unnest(cols = .points)

  coords_with_groups <- dplyr::full_join(
    pdr_s,
    pdr_p,
    by = dplyr::join_by(.id, .index)
  )

  # Check for multiple tools
  tools_present <- coords_with_groups$.tool |> unique()

  if (length(tools_present) > 1) {
    warning(
      "More than one tool detected: ",
      paste(tools_present, collapse = ", "),
      ".\n",
      "These are combined into one heatmap.\n",
      "We recommend only combining drawings from one tool.",
      call. = FALSE
    )
  }

  # ============================================================================
  # STEP 6: Recreate spray if needed
  # ============================================================================
  if ("spray" %in% tools_present) {
    message("\nRecreating spray effect...")
    coords_with_groups_spray <- coords_with_groups |>
      dplyr::filter(.tool == "spray")
    coords_with_groups_spray <- recreate_spray(coords_with_groups_spray)

    # Recombine expanded spray data
    coords_with_groups <- coords_with_groups |>
      dplyr::filter(.tool != "spray") |>
      dplyr::full_join(coords_with_groups_spray)

    rm(coords_with_groups_spray)
  }

  # ============================================================================
  # STEP 7: Calculate density
  # ============================================================================
  message("\nCalculating point density...")

  density_data <- calculate_pain_density(
    .data = coords_with_groups,
    group_vars = group_vars,
    grid_size = grid_size
  )

  # ============================================================================
  # STEP 8: Create plot
  # ============================================================================
  message("\nCreating heatmap plot...")

  plot_out <- create_heatmap_plot(
    density_data = density_data,
    variables = variables,
    background_image = background_image,
    image_width = image_width,
    image_height = image_height,
    n_vars = length(variables),
    point_size = point_size,
    min_alpha = min_alpha,
    color_scale = color_scale,
    show_n = show_n,
    strips_in_markdown = strips_in_markdown,
    label_format = label_format
  )

  # ============================================================================
  # STEP 9: Return
  # ============================================================================

  message("\nDone!")
  return(list(
    plot = plot_out,
    data = density_data
  ))
}
