# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

#' Stratify Data by Creating Group Variables
#' @noRd
stratify_data <- function(.data, variables, n_groups, label_format) {
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
        n_grp <- levels_present
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
      # Numeric variable - use pdr_quantile for binning
      result[[grp_col_name]] <- pdr_quantile(
        var_data,
        n_grp,
        txt = label_format
      )
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
      count = .point_density
    ) |>
    tidyr::uncount(count) |>
    dplyr::mutate(
      # Add random offset within spray radius
      angle = stats::runif(dplyr::n(), 0, 2 * pi),
      distance = stats::runif(dplyr::n(), 0, .spray_radius),
      .x = .x + distance * cos(angle),
      .y = .y + distance * sin(angle)
    ) |>
    dplyr::select(-spray_id, -angle, -distance)
}


#' Calculate Pain Density for Each Grid Cell and Group
#' @noRd
calculate_pain_density <- function(.data, group_vars, grid_size) {
  # Bin coordinates into grid
  data_binned <- .data |>
    dplyr::mutate(
      x_bin = round(.x / grid_size) * grid_size,
      y_bin = round(.y / grid_size) * grid_size
    )

  # Calculate total IDs per group
  group_totals <- data_binned |>
    dplyr::summarize(
      n_group = dplyr::n_distinct(.id),
      .by = dplyr::all_of(group_vars)
    )

  # Calculate density per location per group
  density <- data_binned |>
    dplyr::mutate(
      n_id_at_location = dplyr::n_distinct(.id),
      .by = c(dplyr::all_of(group_vars), x_bin, y_bin)
    ) |>
    dplyr::left_join(group_totals, by = group_vars) |>
    dplyr::mutate(
      pct = n_id_at_location / n_group
    )

  return(density)
}


#### Scaling function
# maps bw to size in mm using scale_bw funtion
pdr_scale_bw <- function(bw) {
  min_bw <- 1
  max_bw <- 20
  min_target <- 0.1
  max_target <- 5
  ((bw - min_bw) / (max_bw - min_bw)) * (max_target - min_target) + min_target
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
  p <- ggplot2::ggplot(density_data, ggplot2::aes(x = .x, y = .y))

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
        strip.text.y.left = ggplot2::element_text(angle = 0, vjust = 0.5),
        strip.text.x.top = ggplot2::element_text(hjust = 0.5)
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
pdr_quantile <- function(x, n_groups, txt = "pct") {
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

check_p_col <- function(p) {
  if (pdr_check_data(p)) {
    warning(
      "`p` is a valid pain drawing data structure -- not a `p` column from such."
    )
  }
  if (!is.list(p)) {
    warning("`p` is not a valid list-column")
    return(FALSE)
  }
  if (
    !all(
      p |>
        purrr::map_lgl(\(tib) {
          identical(tib, NA) || tibble::is_tibble(tib)
        })
    )
  ) {
    warning(
      "Every element of `p` should be a tibble -- perhaps you should use `pdr_sanitize()` in mutate calls?"
    )
    return(FALSE)
  }
  if (
    !all(
      p |>
        purrr::map_lgl(\(tib) {
          identical(tib, NA) || all(c("i", "x", "y") %in% names(tib))
        })
    )
  ) {
    warning("Every tibble element of `p` must include columns i, x and y")
    return(FALSE)
  }
  if (
    !all(
      p |>
        purrr::map_lgl(\(tib) {
          identical(tib, NA) || all(is.integer(c(tib$i, tib$x, tib$y)))
        })
    )
  ) {
    warning(
      "One or more tibble element of `p` includes columns i,x and/or y which are not integers"
    )
    return(FALSE)
  }
  return(TRUE)
}

psr_example <- function(path = NULL) {
  # This function provides users with easy access to example
  # data stored in the inst/extdata folder
  if (is.null(path)) {
    dir(system.file("extdata", package = "paindrawr"))
  } else {
    system.file("extdata", path, package = "paindrawr", mustWork = TRUE)
  }
}

is_list_list <- function(x) {
  if (
    is.list(x) &&
      purrr::every(x, \(e) {
        is.list(e)
      })
  ) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

is_list_xy <- function(x) {
  if (
    is.list(x) &&
      length(x) == 2 &&
      {
        purrr::every(x, \(e) {
          is.integer(e) || is.numeric(e)
        })
      }
  ) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

wrap_pc_union <- function(A, B, op = "union") {
  polyclip::polyclip(A, B, op) |>
    purrr::map_depth(.depth = 2, \(e) {
      as.integer(round(e))
    })
}

wrap_pc_simplify <- function(A) {
  A |>
    polyclip::polysimplify(A) |>
    purrr::map_depth(.depth = 2, \(e) {
      as.integer(round(e))
    })
}

wrap_pc_offset <- function(A, d = 5, e = "round") {
  A |>
    polyclip::polylineoffset(A, delta = d, endtype = e) |>
    purrr::map_depth(.depth = 2, \(e) {
      as.integer(round(e))
    })
}

wrap_geo_polyarea <- function(x, y) {
  # Sanity checks?

  # Close polygon, if not unclosed
  if (
    !all(
      identical(x[1], x[nrow(x)]) &&
        identical(y[1], y[nrow(y)])
    )
  ) {
    x <- c(x, x[1])
    y <- c(y, y[1])
  }

  return(as.integer(round(
    geometry::polyarea(x, y)
  )))
}

is_polygon <- function(A) {
  return(length(wrap_pc_simplify(A)) > 0)
}

attempt_union <- function(A, B, edges) {
  # This function expects A and B to be in the format
  # required by polyclip::polyclip()

  # The polyclip function might (?) change the coordinates data
  # by moving the start vertex, by changing the direction
  # (clockwise/counterclockwise) and by un-closing the
  # polygon. Also it may return a list of 1 or more
  # polygons -- we need to handle this complexity and
  # return a result of a list of length 2 $x and $y

  # If parameter edges is TRUE, polygons should be merged
  # even if only connected by an edge without overlap

  if (edges) {
    they_intersect <- length(wrap_pc_union(A, B, op = "union")) == 1
  } else {
    they_intersect <- length(wrap_pc_union(A, B, op = "intersection")) > 0
  }

  if (!they_intersect) {
    # As they do not intersect, we just need an indicator of
    # 'no union' -- we do not need to return any polygons
    return("FAIL")
  } else {
    # As they do intersect, we need to return the resulting
    # merged polygon -- however we want simplified polygons
    # and not complex polygons with holes, etc.
    result <- wrap_pc_union(A, B, op = "union")
    if (length(result) == 1) {
      # This must be a simple polygon, so return it
      return(result[[1]])
    } else {
      # This must be a complex polygon, so handle it by just
      # picking the polygon with the largest bounding box
      tmp_indx <- result |>
        purrr::map_int(\(e) {
          bb_area <- (max(e$x) - min(e$x)) * (max(e$y) - min(e$y))
        }) |>
        purrr::as_vector() |>
        which.max()
      result[[tmp_indx]]
    }
  }
}

pdr_help_merge_overlapping_polygons <- function(.points, edges = FALSE) {
  # .points is a tibble with columns .index, .x, and .y
  # As we are going to merge polygons, the .index values will no
  # longer be important -- should probably be recoded as 1:n
  # We therefor recast .points as a list of lists of x and y
  # with one element per .index

  ##########################################
  ## -- if less than 2 points quit now -- ##
  ##########################################
  if (identical(NA, .points)) {
    return(NA)
  }
  if (length(unique(.points$.index)) < 2) {
    return(.points)
  }

  ## As this function is called from pdr_modify, we can assume
  ## that all polygon data has been sanitized

  # Reshape points data to fit the required format for polyclip
  pointsxy <- .points |>
    tidyr::nest(.by = .index) |>
    dplyr::pull(data) |>
    purrr::map(\(e) {
      list(x = e$.x, y = e$.y)
    })

  # Number of strokes -- NOTE:This will change in the while loop if there are overlaps
  n_strokes <- length(pointsxy)
  # Hold the actual indexes of each strokes
  i_strokes <- 1:n_strokes

  p1 <- 1 # pointer 1
  p2 <- 2 # pointer 2

  # Helper functions
  # The loop-in-loop will iterate each combination of pairs of strokes for overlap - merging as we go
  # The order of the strokes is not important, thus the runtime will be O(½n²-½n) which
  # is half of the nxn matrix. We can represent these stroke combinations with a single
  # vector of n elements if we use two pointers to iterate the vector - we will simply id
  # the strokes by their index in the list-of-strokes list

  #     index
  #     -----
  # p1 -> 1
  #       2 <- p2
  #       3
  #       4
  #       5
  #       6

  # If the two polygons at indexes p1 and p2 overlap, they are merged to replace the p1
  # polygon, the p2 polygon is deleted and the p2 pointer is reset to p1+1.
  # When p2 reaches the end of indexes, p1 is advanced and p2  .. until it reaches reach the end.

  # Note that we rely on 'intersection' to identify overlaps (as opposed to 'union') -- this
  # is necessary for merging anatomy polygons in background templates.

  # In the following, I use aan (anatomy area names) to help debug
  #aan <- pdr_example_anatomy |> purrr::map_chr(\(x) {x$.id}) # anatomy area names
  #aan <- c("Back_left_buttock","Back_left_calf","Back_left_foot","Back_right_buttock","Back_left_thigh") # 13 10
  #aan <- c("Back_left_buttock","Back_left_calf","Back_left_foot","Back_left_thigh","Back_right_buttock") # 10 13
  while (p1 < n_strokes) {
    while (p2 <= n_strokes) {
      union_result <- attempt_union(pointsxy[[p1]], pointsxy[[p2]], edges)
      if (is.character(union_result) && union_result == "FAIL") {
        # These two strokes do not overlap -- so just move on
        p2 <- p2 + 1
      } else {
        # Replace p1 data with union_result
        pointsxy[[p1]] <- union_result
        # Delete p2 data
        pointsxy[p2] <- NULL
        # Reset p2 pointer (p1+1)
        p2 <- p1 + 1
        # Reset number of strokes (-1)
        n_strokes <- n_strokes - 1
      } # endif
    } # endwhile
    p1 <- p1 + 1
    p2 <- p1 + 1
  } # endwhile

  # Now re-cast .points in the usual format
  pointsxy <- pointsxy |>
    purrr::imap(\(e, i) {
      tibble::tibble(
        .index = as.integer(round(i)),
        .x = as.integer(round(e$x)),
        .y = as.integer(round(e$y))
      )
    }) |>
    purrr::list_rbind()

  return(pointsxy)
}

pdr_help_reduce_stroke_data <- function(.strokes, .index) {
  # When reducing a set of pain drawings strokes, e.g. when
  # merging overlapping strokes, we need to reduce the
  # strokes meta data also -- not just point data .

  # Principle: For each stroke variable (e.g. 'color'),
  # we will retain the data _if_ there is only one unique
  # value. For instance, if all strokes have tool "pen", we
  # will retain that value. However, if some strokes have
  # tool "pen" and some have "spray", we will set tool as
  # NA

  # We will need to retain a specific number of observations
  # however -- supplied as .index

  # The input parameter .strokes is a tibble of strokes data.
  # The .index is a list of the unique .index values left in
  # the .points tibble _after_ reduction by e.g merging.

  .index <- unique(.index) # just in case...

  # So throw away original index values, summarize all other
  # variables to either a) the one unique value observed in
  # all rows og b) NA..
  .strokes <- .strokes |>
    dplyr::select(-.index) |>
    dplyr::summarise_all(
      ~ ifelse(length(unique(.x)) == 1, .x, NA)
    )

  # ..the merge this with input parameter .index (.strokes
  # will be repeated as necessary)
  return(
    dplyr::bind_cols(tibble::tibble(.index), .strokes)
  )
}
