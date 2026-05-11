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

pdr_poly_clip <- function(A, B, operation = "intersection") {
  # This function is a simple wrapper for polyclip::polyclip
  # A and B should be dataframes which contain two columns x and y
  result <- polyclip::polyclip(
    A = list(x = A$x, y = A$y),
    B = list(x = B$x, y = B$y),
    op = operation
  )

  if (purrr::is_empty(result)) {
    result <- tibble::tibble(
      i = as.integer(),
      x = as.integer(),
      y = as.integer()
    )
  } else {
    result <- result |>
      purrr::map_dfr(
        \(q) {
          q
        },
        .id = "i"
      ) |>
      dplyr::mutate(
        i = as.integer(i),
        x = as.integer(round(x)),
        y = as.integer(round(y))
      )
  }
  result
}

## pdr_poly_manage_overlaps and loop_pairwise work together to
## merge overlapping strokes 
pdr_poly_manage_overlaps <- function(p, method="union") {
  # This function takes the column p from a pain drawing data structure and for each pain drawing (row)
  # it handles any overlapping polygons

  if (!is.list(p)) {stop("The data 'p' is not a list")}  
  if (!purrr::every(p, ~ !identical(.x, NA) || tibble::is_tibble(.x))) {stop("The data 'p' is not a list of tibbles")}  

  p <- p |> # p is a list of tibbles (of x,y,i)
    purrr::map(\(df, i_df) {
      if(!tibble::is_tibble(df)) { # Should we accept data frames as well?
        NA
      } else {
        # We have coordinates in the data frame, but how many strokes        
        if (length(unique(df$i))<2) {
          # There is only a single stroke in data frame - thus no overlaps
          df # ..so just stick with current data frame
        } else {
          # There are multiple strokes in data frame - check for overlaps in external function
          loop_pairwise(df, method) # Handle the overlaps pairwise
        }
      }
    })
  
  p
}

loop_pairwise <- function(df, method="intersection") {
  i_strokes <- as.integer(unique(df$i)) # Hold the actual identifiers of the discrete strokes
  n_strokes <- length(i_strokes) # How many of them there are (this may change in the while loop)
  if (n_strokes < 2) {return(df)} # Only one stroke - no overlaps!
  p1 <- 1 # pointer 1
  p2 <- 2 # pointer 2

  # These loops will iterate each combination of pairs of strokes for overlap - merging as we go
  # The order of the strokes is not important, thus the runtime will be O(½n²-½n) which
  # is half of the nxn matrix. We can represent these stroke combinations with a single
  # vector of n elements if we use two pointers to iterate the vector - we will simply id
  # the strokes by their index in the list-of-strokes (los) list
  
  while (p1 < n_strokes) { # Loop all polygons as the first polygon in pairwise comparison
    while (p2 <= n_strokes) { # ..for each, loop remaining polygons as the second polygon in pairwise comparison
      
      a <- df |> dplyr::filter(i==i_strokes[p1]) # coordinates of first polygon
      b <- df |> dplyr::filter(i==i_strokes[p2]) # coordinates of second polygon

      # The following will return a df of x and y polygon coordinates (one for each intersection)
      if (method=="intersection") {
        intersection <- pd_poly_clip(a, b, op = "intersection") 
        they_overlap <- nrow(intersection)>1 # TRUE or FALSE -- intersection is false if no area to overlap (e.g. overlapping vertices)
      } else if (method=="union") {
        intersection <- pd_poly_clip(a, b, op = "union") 
        they_overlap <- length(unique(intersection$i))==1 # TRUE or FALSE -- If no overlap: length=2 
      }
      
      if (they_overlap) {
        if (method=="intersection" | method=="union") {
          # Replace p1 by the union of p1 and p2, delete p2, reset length of strokes and reset p2=p1+1
          # and keep only the first polygon (the rest are holes)
          merge_result <- pd_poly_clip(a, b, op = "union") |> dplyr::filter(i==1) 
          df <- df |>
            dplyr::group_by(i) |>
            dplyr::group_modify(\(grp, indx) {
              if (grp$i != i_strokes[p1] & grp$i != i_strokes[p2]) { grp }
              if (grp$i == i_strokes[p1]) { tibble::tibble(i=i_strokes[p1], x=merge_result$x, y=merge_result$y) } 
              if (grp$i == i_strokes[p2]) { tibble::tibble(x=as.integer(), x=as.integer(), y=as.integer()) } 
            }) |>
            dplyr::ungroup()
            #dplyr::filter(i != i_strokes[p1] & i != i_strokes[p2]) |> # Remove p1 and p2 from pd1 points data frame
            #dplyr::bind_rows(tibble::tibble(i=i_strokes[p1], x=merge_result$x, y=merge_result$y)) # Add the merge result as p1 in the pd1 points data frame
          print(paste0("Merged stroke ", i_strokes[p1] ," and ", i_strokes[p2]," with ",nrow(a), " and ",nrow(b), " points respectively into a new strokes with ",df|>dplyr::filter(i==i_strokes[p1])|>nrow()," points"))
          i_strokes <- i_strokes[-p2] # Remove p2 from vector of stroke indices we are iterating          
          n_strokes <- n_strokes-1
          p2 <- p1+1
        } else {
          # The method is not 'merge', but something else (e.g. split) -- not yet implemented
        }
      } else { 
        # if the do not overlap just move to the next stroke polygon
        p2 <- p2+1
      }
    } # end of inner loop (p2)
    p1 <- p1+1 # Advance p1
    p2 <- p1+1 # Reset p2
  } # end of out loop (p1)    
  return(df)
}
