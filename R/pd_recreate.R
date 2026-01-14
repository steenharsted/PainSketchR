pd_recreate_drawing <- function(
  .data,
  background_image = NULL,
  save_plot = TRUE,
  filename = "drawing%03d.png"
) {
  pd <- .data

  # TEST that pd has the columns we need
  if (!("id" %in% names(pd))) {
    stop("The column 'id' is needed")
  }
  if (!("p" %in% names(pd))) {
    stop("The column 'p' containing coordinates is needed")
  }
  if (!("s" %in% names(pd))) {
    stop("The column 's' containing stroke information is needed")
  }

  # Test that height and width are the same
  if (pd$w |> unique() |> length() != 1 | pd$h |> unique() |> length() != 1) {
    stop(
      "More than one width or height value of image found.\n 
    You can only recreate drawings from one type of canvas at a time"
    )
  }

  # Test that filename ends with .png if save_plot = TRUE
  if (save_plot & stringr::str_extract(filename, "...$") != "png") {
    stop(
      "Filename must end with png"
    )
  }

  # Test that ids are unique
  if (pd$id |> length() != pd$id |> unique() |> length()) {
    stop("all values in the 'id' column must be unique")
  }

  # Extract number of ids
  id_n <- pd$id |> length()

  # Warn user if numbers of ids > 9
  if (id_n > 9) {
    warning(
      "You are trying to draw more than 9 drawings. This may cause performance issues. Do you want to continue? (yes/no)"
    )
    user_input <- readline("Enter your choice (yes/no): ")

    if (tolower(user_input) != "yes") {
      stop("Operation aborted by the user.")
    }
  }

  # Extract height and width
  image_width <- pd$w |> unique()
  image_height <- pd$h |> unique()

  # Unnest and Join the s and p columns
  pd_s <- pd |>
    dplyr::select(id, s) |>
    tidyr::unnest(cols = s)

  pd_p <- pd |>
    dplyr::select(id, p) |>
    tidyr::unnest(cols = p)

  pd <- dplyr::full_join(pd_s, pd_p, by = dplyr::join_by(id, i))

  # Revert y coords
  pd <- pd |> dplyr::mutate(y = image_height - y)

  # map bw to size in mm using scale_bw funtion

  #### SHOULD THIS BE SOMEWHERE ELSE ???? #####
  #### Scaling function
  scale_bw <- function(bw) {
    min_bw <- 1
    max_bw <- 20
    min_target <- 0.1
    max_target <- 5
    ((bw - min_bw) / (max_bw - min_bw)) * (max_target - min_target) + min_target
  }
  #######################

  pd <- pd |>
    dplyr::mutate(
      size_mm = scale_bw(bw)
    )

  ## Spray and Pen needs to be plotted differently
  ## We achieve this by making a coordinate set for Spray and Pen, respectively

  pd_pen <- pd |>
    dplyr::filter(t == "pen")

  pd_spray <- pd |>
    dplyr::filter(t == "spray")

  ### Recreate jitter in spray Data
  ### But only if spray data exists

  if (nrow(pd_spray > 0)) {
    pd_spray <- pd_spray |>

      # Create extra rows according to spray density (pd)
      uncount(weights = pd) |>

      # Generate random angles and radii for uniform distribution within a circle
      # pr holds information about the spray radius
      mutate(
        angle = runif(n(), 0, 2 * pi),
        radius = sqrt(runif(n(), 0, 1)) * pr
      ) |>

      # Calculate offsets and new x, y positions
      mutate(
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
      linewidth = bw,
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

  if (save_plot) {
    ### Translate size
    pixels_per_inch <- 96
    mm_per_inch <- 25.4
    pixel_to_mm <- mm_per_inch / pixels_per_inch # ≈ 0.2646 mm per pixel
    width_mm <- image_width * pixel_to_mm # ≈ 119.07 mm
    height_mm <- image_height * pixel_to_mm # ≈ 132.29 mm

    ggplot2::ggsave(
      plot = plot_out,
      file = filename,
      width = width_mm,
      height = height_mm,
      units = "mm",
      dpi = 96,
      scale = id_n
    )

    message(
      paste0("Drawing saved in ", filename)
    )
  } else {
    return(plot_out)
  }
}
