#' Create Heatmap Visualization of Pain Drawing Density by Groups
#'
#' This function creates faceted density heatmaps showing how pain location
#' patterns vary across one or two stratification variables. It supports both
#' pen and spray tool types (but not simultaneously) and can display sample
#' sizes per group.
#'
#' @param .data A data frame containing drawing data with required columns:
#'   `id` (unique identifier),
#'   `p` (list column with coordinates),
#'   `s` (list column with stroke information),
#'   `w` (canvas width),
#'   `h` (canvas height),
#'   and the grouping variables specified in `variables`.
#' @param id_col Character. Name of the ID column. Default is "id".
#' @param variables Character vector of length 1 or 2 specifying the grouping
#'   variable names. For numeric variables, they will be binned into quantile
#'   groups. For categorical variables, existing levels will be used.
#' @param n_groups Integer vector matching the length of `variables`. Specifies
#'   the number of groups to create for each variable. Default is c(2, 2).
#' @param equal_n Logical. If TRUE, balances sample sizes across groups by
#'   sampling. Default is TRUE.
#' @param max_n Integer. Maximum number of observations per group when
#'   `equal_n = TRUE`. Default is 1000.
#' @param tool Character. Which drawing tool to visualize: "pen" or "spray".
#'   Default is "pen".
#' @param background_image Optional. Background image for the plot. Can be:
#'   (1) a file path to a PNG image, or (2) a numeric array from png::readPNG().
#'   Default is NULL.
#' @param grid_size Integer. Coordinate binning resolution in pixels. Smaller
#'   values create finer grids but slower computation. Default is 10.
#' @param point_size Numeric. Size of points in the heatmap. Default is 0.25.
#' @param min_alpha Numeric. Minimum alpha value for points. Default is 0.1.
#' @param color_scale Character or numeric. How to scale colors: "max" scales
#'   across all facets using the maximum density value, "facet" scales within
#'   each facet independently, or a numeric value to set a specific maximum.
#'   Default is "max".
#' @param label_format Character. Format for quantile labels: "pct" for
#'   percentages, "num" for numeric ranges, or "both" for combined markdown
#'   labels. Default is "pct".
#' @param show_n Logical. If TRUE, displays sample sizes as text in facet
#'   strips. Default is TRUE.
#' @param strips_in_markdown Logical. If TRUE, renders strip text using
#'   \code{ggtext::element_markdown()}, enabling markdown and HTML formatting
#'   in facet labels. Required when \code{label_format = "both"}. Default is TRUE.
#'
#' @return Returns a list with:
#'   \item{plot}{ggplot2 object}
#'   \item{data}{data frame with density calculations}
#'   If save_plot is TRUE, saves PNG file(s) and returns a message.
#'
#' @details
#' The function processes data by:
#' \itemize{
#'   \item Creating stratification groups (quantile binning for numeric variables)
#'   \item Optionally balancing sample sizes across groups
#'   \item Unnesting coordinate data from nested columns
#'   \item Binning coordinates into grid cells
#'   \item Calculating pain density (percentage of participants per location)
#'   \item Creating faceted heatmap with viridis color scale
#' }
#'
#' For spray tool data, the function recreates spray effects similar to
#' pd_recreate_drawing(). Grid size determines visualization resolution;
#' smaller values (e.g., 5) produce finer detail but are slower.
#'
#' @section Warnings:
#' The function will warn when:
#' \itemize{
#'   \item Creating more than 16 facets (may be slow)
#'   \item Grid size is outside recommended range (5-50 pixels)
#'   \item Removing NA values from grouping variables
#' }
#'
#' @examples
#' \dontrun{
#' # Two-way stratification
#' pd_create_heatmap(
#'   drawing_data,
#'   variables = c("age_group", "pain_duration"),
#'   n_groups = c(3, 2)
#' )
#'
#' # Single variable with custom settings
#' result <- pd_create_heatmap(
#'   drawing_data,
#'   variables = "pain_intensity",
#'   n_groups = 4,
#'   tool = "spray",
#'   grid_size = 5,
#'   save_plot = FALSE
#' )
#' }
#'
#' @importFrom dplyr select mutate filter full_join join_by n_distinct
#'   slice_sample summarize all_of count left_join distinct pull n
#' @importFrom tidyr unnest uncount
#' @importFrom rlang sym
#' @importFrom png readPNG
#' @importFrom scales percent
#' @importFrom ggplot2 ggplot aes coord_fixed theme_minimal scale_colour_viridis_c
#'   geom_jitter facet_wrap facet_grid annotation_raster labs theme element_rect
#'   element_blank element_text geom_text guide_colorbar guides vars
#' @importFrom ggtext element_markdown
#' @importFrom vctrs vec_ptype_abbr
#'
#' @export
pd_create_heatmap <- function(
  .data,
  id_col = "id",
  variables = NULL,
  n_groups = c(2, 2),
  equal_n = TRUE,
  max_n = 1000,
  tool = "pen",
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
  pdr_check_data(.data)

  # Validating input specific to pd_create_heatmap
  validate_heatmap_inputs(
    .data = .data,
    id_col = id_col,
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

  image_width <- .data$w |> unique()
  image_height <- .data$h |> unique()

  if (length(image_width) != 1 || length(image_height) != 1) {
    stop(
      "Data must have single unique width and height values.\n",
      "You can only create heatmaps from one type of canvas at a time.",
      call. = FALSE
    )
  }

  # Validate and process background_image (same logic as pd_recreate_drawing)
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
    id_col = id_col,
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

  pd_s <- data_grouped |>
    dplyr::select(dplyr::all_of(c(id_col, "s", group_vars))) |>
    tidyr::unnest(cols = s)

  pd_p <- data_grouped |>
    dplyr::select(dplyr::all_of(c(id_col, "p"))) |>
    tidyr::unnest(cols = p)

  coords_with_groups <- dplyr::full_join(
    pd_s,
    pd_p,
    by = dplyr::join_by(!!rlang::sym(id_col), i)
  )

  # Check for multiple tools
  tools_present <- coords_with_groups$t |> unique()

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
      dplyr::filter(t == "spray")
    coords_with_groups_spray <- recreate_spray(coords_with_groups_spray)

    # Recombine expanded spray data
    coords_with_groups <- coords_with_groups |>
      dplyr::filter(t != "spray") |>
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
    grid_size = grid_size,
    id_col = id_col
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
