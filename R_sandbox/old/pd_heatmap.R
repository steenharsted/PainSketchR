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
#' @param save_plot Logical. If TRUE, saves plot as PNG. If FALSE, returns
#'   ggplot2 object. Default is FALSE.
#' @param filename Character. Output filename pattern for saved plots.
#'   Default is "heatmap_%03d.png".
#' @param scale Numeric. Scaling factor for saved plot size. Default is 1.5.
#' @param width Numeric. Plot width in mm. Default is NA (auto-calculated).
#' @param height Numeric. Plot height in mm. Default is NA (auto-calculated).
#'
#' @return If save_plot is FALSE, returns a list with:
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
#' @importFrom dplyr select mutate filter full_join join_by n_distinct slice_sample
#'   summarize across all_of count left_join
#' @importFrom tidyr unnest uncount
#' @importFrom png readPNG
#' @importFrom ggplot2 ggplot aes coord_fixed theme_void scale_colour_viridis_c
#'   geom_jitter facet_wrap facet_grid annotation_raster ggsave labs theme
#'   element_text geom_text
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
  strips_in_markdown = TRUE,
  save_plot = FALSE,
  filename = "heatmap_%03d.png",
  scale = 1.5,
  width = NA,
  height = NA
) {
  # ============================================================================
  # STEP 1: Validate inputs
  # ============================================================================
  message("Validating inputs...")

  validate_heatmap_inputs(
    .data = .data,
    id_col = id_col,
    variables = variables,
    n_groups = n_groups,
    tool = tool,
    grid_size = grid_size,
    label_format = label_format,
    color_scale = color_scale,
    save_plot = save_plot,
    filename = filename
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
  message("Extracting canvas dimensions...")

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
  message("Creating stratification groups...")

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
    message("Balancing group sizes...")

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
  message("Unnesting coordinate data...")

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
    message("Recreating spray effect...")
    coords_with_groups_spray <- coords_with_groups |> filter(t == "spray")
    coords_with_groups_spray <- recreate_spray(coords_with_groups_spray)

    # Recombine expanded spray data
    coords_with_groups <- coords_with_groups |>
      filter(t != "spray") |>
      full_join(coords_with_groups_spray)

    rm(coords_with_groups_spray)
  }

  # ============================================================================
  # STEP 7: Calculate density
  # ============================================================================
  message("Calculating pain density...")

  density_data <- calculate_pain_density(
    .data = coords_with_groups,
    group_vars = group_vars,
    grid_size = grid_size,
    id_col = id_col
  )

  # ============================================================================
  # STEP 8: Create plot
  # ============================================================================
  message("Creating heatmap plot...")

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
  # STEP 9: Save or return
  # ============================================================================
  if (save_plot) {
    message("Saving plot...")

    # Calculate dimensions
    pixels_per_inch <- 96
    mm_per_inch <- 25.4
    pixel_to_mm <- mm_per_inch / pixels_per_inch

    if (is.na(width)) {
      width <- image_width * pixel_to_mm * sqrt(prod(n_groups))
    }
    if (is.na(height)) {
      height <- image_height * pixel_to_mm * sqrt(prod(n_groups))
    }

    ggplot2::ggsave(
      plot = plot_out,
      filename = filename,
      width = width,
      height = height,
      units = "mm",
      dpi = 96,
      scale = scale
    )

    message("Heatmap saved to ", filename)
  } else {
    message("Done!")
    return(list(
      plot = plot_out,
      data = density_data
    ))
  }
}


# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

#' Validate Heatmap Inputs
#' @noRd
validate_heatmap_inputs <- function(
  .data,
  id_col,
  variables,
  n_groups,
  tool,
  grid_size,
  label_format,
  color_scale,
  save_plot,
  filename
) {
  # Required columns
  required_cols <- c(id_col, "s", "p", "w", "h")
  missing_cols <- setdiff(required_cols, names(.data))
  if (length(missing_cols) > 0) {
    stop(
      "Missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  # Variables exist
  if (is.null(variables) || length(variables) == 0) {
    stop(
      "'variables' must be a character vector of length 1 or 2",
      call. = FALSE
    )
  }
  if (length(variables) > 2) {
    stop("'variables' can have at most 2 elements", call. = FALSE)
  }
  missing_vars <- setdiff(variables, names(.data))
  if (length(missing_vars) > 0) {
    stop(
      "Variables not found in data: ",
      paste(missing_vars, collapse = ", "),
      call. = FALSE
    )
  }

  # n_groups matches variables length
  if (length(n_groups) != length(variables)) {
    stop(
      "'n_groups' must have the same length as 'variables' (",
      length(variables),
      ")",
      call. = FALSE
    )
  }

  # Tool is valid
  if (!tool %in% c("pen", "spray")) {
    stop("'tool' must be either 'pen' or 'spray'", call. = FALSE)
  }

  # Grid size warning
  if (grid_size < 5 || grid_size > 50) {
    warning(
      "grid_size = ",
      grid_size,
      " is outside the recommended range (5-50).\n",
      "Very small values may be slow; very large values may produce coarse grids.",
      call. = FALSE
    )
  }

  # Label format is valid
  if (!label_format %in% c("pct", "num", "both")) {
    stop("'label_format' must be one of: 'pct', 'num', 'both'", call. = FALSE)
  }

  # Color scale is valid
  if (is.character(color_scale)) {
    if (!color_scale %in% c("max", "facet")) {
      stop(
        "'color_scale' must be 'max', 'facet', or a numeric value",
        call. = FALSE
      )
    }
  } else if (!is.numeric(color_scale) || length(color_scale) != 1) {
    stop(
      "'color_scale' must be 'max', 'facet', or a single numeric value",
      call. = FALSE
    )
  }

  # Filename validation
  if (save_plot && !grepl("\\.png$", filename, ignore.case = TRUE)) {
    warning("'filename' should end with .png for PNG output", call. = FALSE)
  }

  invisible(TRUE)
}


#' Stratify Data by Creating Group Variables
#' @noRd
stratify_data <- function(.data, id_col, variables, n_groups, label_format) {
  result <- .data

  for (i in seq_along(variables)) {
    var_name <- variables[i]
    n_grp <- n_groups[i]
    grp_col_name <- paste0(".VAR", i, "_grp")

    # Extract column and check for NAs
    var_data <- result[[var_name]]
    na_count <- sum(is.na(var_data))

    if (na_count > 0) {
      message("  ", na_count, " NA values removed from '", var_name, "'")
      result <- result |> dplyr::filter(!is.na(!!rlang::sym(var_name)))
      var_data <- result[[var_name]]
    }

    # Detect variable type
    var_type <- vctrs::vec_ptype_abbr(var_data)

    if (var_type %in% c("fct", "chr", "ord")) {
      # Categorical variable
      levels_present <- length(unique(var_data))

      if (n_grp != levels_present) {
        stop(
          "Variable '",
          var_name,
          "' has ",
          levels_present,
          " unique levels but n_groups[",
          i,
          "] = ",
          n_grp,
          ".\n",
          "For categorical variables, n_groups must match the number of levels.",
          call. = FALSE
        )
      }

      # Use as factor
      result[[grp_col_name]] <- factor(var_data)
      message(
        "  '",
        var_name,
        "' treated as categorical (",
        levels_present,
        " levels)"
      )
    } else {
      # Numeric variable - use my_quantile for binning
      result[[grp_col_name]] <- my_quantile(var_data, n_grp, txt = label_format)
      result[[grp_col_name]] <- factor(result[[grp_col_name]])
      message("  '", var_name, "' binned into ", n_grp, " quantile groups")
    }
  }

  return(result)
}


#' Balance Group Sizes by Sampling
#' @noRd
balance_groups <- function(.data, group_vars, max_n) {
  # Calculate minimum group size across all combinations
  min_group <- .data |>
    dplyr::summarize(n = dplyr::n(), .by = dplyr::all_of(group_vars)) |>
    dplyr::pull(n) |>
    min()

  # Target n is minimum of smallest group and max_n
  target_n <- min(min_group, max_n)

  message(
    "  Smallest group has n = ",
    min_group,
    "\n",
    "  Sampling n = ",
    target_n,
    " per group (max_n = ",
    max_n,
    ")"
  )

  # Warn if reducing groups
  if (target_n < min_group) {
    warning(
      "Rows removed to balance groups (n = ",
      target_n,
      " per group).\n",
      "Strip labels show ranges from full dataset, not reduced sample.",
      call. = FALSE
    )
  }

  # Sample equal n from each group combination
  result <- .data |>
    dplyr::slice_sample(n = target_n, by = dplyr::all_of(group_vars))

  return(result)
}


#' Recreate Spray Effect by Expanding Spray Points
#' @noRd
recreate_spray <- function(.data) {
  .data |>
    # Create spray points
    dplyr::mutate(
      spray_id = dplyr::row_number(),
      count = pd
    ) |>
    tidyr::uncount(count) |>
    dplyr::mutate(
      # Add random offset within spray radius
      angle = stats::runif(dplyr::n(), 0, 2 * pi),
      distance = stats::runif(dplyr::n(), 0, pr),
      x = x + distance * cos(angle),
      y = y + distance * sin(angle)
    ) |>
    dplyr::select(-spray_id, -angle, -distance)
}


#' Calculate Pain Density for Each Grid Cell and Group
#' @noRd
calculate_pain_density <- function(.data, group_vars, grid_size, id_col) {
  # Bin coordinates into grid
  data_binned <- .data |>
    dplyr::mutate(
      x_bin = round(x / grid_size) * grid_size,
      y_bin = round(y / grid_size) * grid_size
    )

  # Calculate total IDs per group
  group_totals <- data_binned |>
    dplyr::summarize(
      n_group = dplyr::n_distinct(!!rlang::sym(id_col)),
      .by = dplyr::all_of(group_vars)
    )

  # Calculate density per location per group
  density <- data_binned |>
    dplyr::mutate(
      n_id_at_location = dplyr::n_distinct(!!rlang::sym(id_col)),
      .by = c(dplyr::all_of(group_vars), x_bin, y_bin)
    ) |>
    dplyr::left_join(group_totals, by = group_vars) |>
    dplyr::mutate(
      pct = n_id_at_location / n_group
    )

  return(density)
}


#' Create Heatmap Plot
#' @noRd
create_heatmap_plot <- function(
  density_data,
  variables,
  background_image,
  image_width,
  image_height,
  n_vars,
  point_size,
  min_alpha,
  color_scale,
  show_n,
  label_format,
  strips_in_markdown
) {
  # Determine color scale limits
  if (is.character(color_scale)) {
    if (color_scale == "max") {
      scale_limits <- c(0, max(density_data$pct, na.rm = TRUE))
    } else {
      # "facet" - will use default (per-facet scaling)
      scale_limits <- NULL
    }
  } else {
    # Numeric value provided
    scale_limits <- c(0, color_scale)
  }

  # Build base plot
  p <- ggplot2::ggplot(density_data, ggplot2::aes(x = x, y = y))

  # Add background image if provided
  if (!is.null(background_image)) {
    p <- p +
      ggplot2::annotation_raster(
        background_image,
        xmin = 0,
        xmax = image_width,
        ymin = 0,
        ymax = image_height,
        interpolate = TRUE
      )
  }

  # Add density points
  p <- p +
    ggplot2::geom_jitter(
      ggplot2::aes(
        colour = pct,
        alpha = pct^2 / 2
      ),
      width = point_size,
      height = point_size,
      size = point_size
    ) +

    # Scale Alpha

    #ggplot2::scale_alpha_continuous(range = c(min_alpha, 1), guide = "none")
    ggplot2::scale_alpha_identity()

  # Add color scale
  if (is.null(scale_limits)) {
    # Per-facet scaling
    p <- p +
      ggplot2::scale_colour_viridis_c(
        option = "plasma",
        direction = -1,
        begin = 0,
        end = 1,
        labels = scales::percent
      )
  } else {
    # Fixed scale across facets
    p <- p +
      ggplot2::scale_colour_viridis_c(
        option = "plasma",
        limits = scale_limits,
        direction = -1,
        begin = 0,
        end = 1,
        labels = scales::percent
      )
  }

  # Style color bar
  p <- p +
    ggplot2::guides(
      color = ggplot2::guide_colorbar(
        title = NULL,
        barheight = 0.5,
        barwidth = 20,
        nbin = 50
      )
    )

  # Add faceting (1 or 2 variables)
  if (n_vars == 1) {
    p <- p + ggplot2::facet_wrap(~.VAR1_grp)
  } else {
    p <- p +
      ggplot2::facet_grid(
        rows = ggplot2::vars(.VAR1_grp),
        cols = ggplot2::vars(.VAR2_grp),
        switch = "y",
        labeller = ggplot2::label_wrap_gen(width = 15, multi_line = TRUE)
      )
  }

  # Add sample sizes if requested
  if (show_n) {
    # Calculate n per group
    if (n_vars == 1) {
      n_data <- density_data |>
        dplyr::summarize(
          n = dplyr::first(n_group),
          .by = .VAR1_grp
        ) |>
        dplyr::mutate(
          label = paste0("n = ", n),
          x = image_width * 0.05,
          y = image_height * 0.95
        )

      p <- p +
        ggplot2::geom_text(
          data = n_data,
          ggplot2::aes(x = x, y = y, label = label),
          inherit.aes = FALSE,
          hjust = 0,
          vjust = 1,
          size = 3,
          colour = "white",
          fontface = "bold"
        )
    } else {
      n_data <- density_data |>
        dplyr::summarize(
          n = dplyr::first(n_group),
          .by = c(.VAR1_grp, .VAR2_grp)
        ) |>
        dplyr::mutate(
          label = paste0("n = ", n),
          x = image_width * 0.05,
          y = image_height * 0.95
        )

      p <- p +
        ggplot2::geom_text(
          data = n_data,
          ggplot2::aes(x = x, y = y, label = label),
          inherit.aes = FALSE,
          hjust = 0,
          vjust = 1,
          size = 3,
          colour = "white",
          fontface = "bold"
        )
    }
  }

  # Apply theme
  p <- p +
    ggplot2::labs(
      subtitle = paste0(variables[2]),
      y = paste0(variables[1])
    ) +
    ggplot2::coord_fixed(xlim = c(0, image_width), ylim = c(0, image_height)) +
    ggplot2::theme_minimal(base_size = 16) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", color = "white"),
      panel.border = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "white", color = "white"),
      panel.grid = ggplot2::element_blank(),
      legend.position = "bottom",
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title.x.top = ggplot2::element_text(hjust = 0.5), # x axis title on top
      axis.title.x.bottom = ggplot2::element_blank(), # remove x axis title on bottom
      plot.subtitle = ggtext::element_markdown(hjust = 0.5, vjust = 0.5)
    )

  # Add markdown formatting for strip text if using "both" label format
  if (label_format == "both") {
    if (requireNamespace("ggtext", quietly = TRUE)) {
      p <- p +
        ggplot2::theme(
          strip.text = ggtext::element_markdown()
        )
    } else {
      message(
        "  Install 'ggtext' package for better label formatting with label_format = 'both'"
      )
    }
  }

  if (strips_in_markdown) {
    p <- p +
      ggplot2::theme(
        strip.text.y.left = ggtext::element_markdown(angle = 45, vjust = 0.5),
        strip.text.x.top = ggtext::element_markdown(hjust = 0.5)
      )
  } else {
    p <- p +
      ggplot2::theme(
        strip.text.y.left = element_text(angle = 0, vjust = 0.5),
        strip.text.x.top = element_text(hjust = 0.5)
      )
  }

  return(p)
}


#' Quantile Binning Helper Function
#'
#' Creates quantile-based bins for numeric variables with customizable labels.
#' Adapted from the original my_quantile function.
#'
#' @param x Numeric vector to bin
#' @param n_groups Number of quantile groups to create
#' @param txt Label format: "pct" (percentages), "num" (numeric ranges),
#'   or "both" (markdown combined)
#' @return Factor with labeled bins
#' @noRd
my_quantile <- function(x, n_groups, txt = "pct") {
  # Calculate quantile breaks
  probs <- seq(0, 1, length.out = n_groups + 1)
  breaks <- quantile(x, probs = probs, na.rm = TRUE)

  # Handle duplicate breaks (if all values are the same)
  if (any(duplicated(breaks))) {
    warning(
      "Cannot create ",
      n_groups,
      " distinct quantile groups.\n",
      "Variable has insufficient variation.",
      call. = FALSE
    )
    return(factor(rep("All values", length(x))))
  }

  # Create bins
  bins <- cut(x, breaks = breaks, include.lowest = TRUE)

  # Create labels based on format
  if (txt == "pct") {
    # Percentage labels
    pct_lower <- probs[-length(probs)] * 100
    pct_upper <- probs[-1] * 100

    labels <- character(n_groups)
    for (i in seq_len(n_groups)) {
      if (i == 1) {
        labels[i] <- paste0("\u003c ", pct_upper[i], "%") # ≤ (less-than-or-equal)
      } else if (i == n_groups) {
        labels[i] <- paste0("\u2265 ", pct_lower[i], "%")
      } else {
        labels[i] <- paste0(pct_lower[i], "-", pct_upper[i], "%")
      }
    }
  } else if (txt == "num") {
    # Numeric range labels
    labels <- character(n_groups)
    for (i in seq_len(n_groups)) {
      labels[i] <- paste0(
        "[",
        round(breaks[i], 1),
        ", ",
        round(breaks[i + 1], 1),
        ")"
      )
    }
    # Fix last bracket to be inclusive
    labels[n_groups] <- sub("\\)$", "]", labels[n_groups])
  } else if (txt == "both") {
    # Combined markdown labels
    pct_lower <- probs[-length(probs)] * 100
    pct_upper <- probs[-1] * 100

    labels <- character(n_groups)
    for (i in seq_len(n_groups)) {
      pct_part <- if (i == 1) {
        paste0("\u2264 ", pct_upper[i], "%")
      } else if (i == n_groups) {
        paste0("\u2265 ", pct_lower[i], "%")
      } else {
        paste0(pct_lower[i], "-", pct_upper[i], "%")
      }

      num_part <- paste0(
        "(",
        round(breaks[i], 1),
        "-",
        round(breaks[i + 1], 1),
        ")"
      )

      labels[i] <- paste0("**", pct_part, "**<br>", num_part)
    }
  } else {
    stop("txt must be one of: 'pct', 'num', 'both'", call. = FALSE)
  }

  # Apply labels
  levels(bins) <- labels

  return(bins)
}
