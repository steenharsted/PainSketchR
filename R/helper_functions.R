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
  grid_size,
  label_format,
  color_scale
) {
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
        message(
          "Variable '",
          var_name,
          "' has ",
          levels_present,
          " unique levels but n_groups[",
          i,
          "] = ",
          n_grp,
          ".\n",
          "For categorical variables, n_groups must match the number of levels.\n",
          "Continuing with ",
          levels_present,
          " groups."
        )
        n_grp = levels_present
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
      # Numeric variable - use pd_quantile for binning
      result[[grp_col_name]] <- pd_quantile(var_data, n_grp, txt = label_format)
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
    if (n_vars == 1) {
      n_data <- density_data |>
        dplyr::distinct(.VAR1_grp, n_group) |>
        dplyr::mutate(
          label = paste0("n = ", n_group),
          x = image_width * 0.45,
          y = image_height * 0.1 # bottom-left, away from body
        )
    } else {
      n_data <- density_data |>
        dplyr::distinct(.VAR1_grp, .VAR2_grp, n_group) |>
        dplyr::mutate(
          label = paste0("n = ", n_group),
          x = image_width * 0.45,
          y = image_height * 0.1 # bottom-left, away from body
        )
    }

    p <- p +
      ggplot2::geom_text(
        data = n_data,
        ggplot2::aes(x = x, y = y, label = label),
        inherit.aes = FALSE,
        hjust = 0,
        vjust = 0,
        size = 3,
        colour = "grey30",
        fontface = "bold"
      )
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
pd_quantile <- function(x, n_groups, txt = "pct") {
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
        labels[i] <- paste0("\u003c ", pct_upper[i] |> round(1), "%") # < (less-than)
      } else if (i == n_groups) {
        labels[i] <- paste0("\u2265 ", pct_lower[i] |> round(1), "%")
      } else {
        labels[i] <- paste0(
          pct_lower[i] |> round(1),
          "-",
          pct_upper[i] |> round(1),
          "%"
        )
      }
    }
  } else if (txt == "num") {
    # Numeric range labels
    labels <- character(n_groups)
    for (i in seq_len(n_groups)) {
      labels[i] <- paste0(
        "(",
        round(breaks[i], 1),
        "-",
        round(breaks[i + 1], 1),
        ")"
      )
    }
    # Fix last bracket to be inclusive
    labels[n_groups] <- sub("\\)$", ")", labels[n_groups])
  } else if (txt == "both") {
    # Combined markdown labels
    pct_lower <- probs[-length(probs)] * 100
    pct_upper <- probs[-1] * 100

    labels <- character(n_groups)
    for (i in seq_len(n_groups)) {
      pct_part <- if (i == 1) {
        paste0("\u003c ", pct_upper[i], "%")
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

      labels[i] <- paste0("", pct_part, "<br>", num_part) # Markdown code here? e.g "**", pct_part, "**<br>"
    }
  } else {
    stop("txt must be one of: 'pct', 'num', 'both'", call. = FALSE)
  }

  # Apply labels
  levels(bins) <- labels

  return(bins)
}
