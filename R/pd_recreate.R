#' Recreate Drawings from Coordinate Data
#'
#' This function recreates drawings from a data frame containing coordinate and
#' stroke information. It supports both pen and spray tool types and can save
#' the output as PNG files or return a ggplot2 object.
#'
#' @param .data A data frame containing drawing data with required columns:
#'   `id` (unique identifier for each drawing),
#'   `p` (list column containing coordinate information),
#'   `s` (list column containing stroke information),
#'   `w` (canvas width), and
#'   `h` (canvas height).
#' @param background_image Optional. Background image for the plot. Can be either:
#'   (1) a character string giving the file path to a PNG image (will be read
#'   with png::readPNG()), or (2) a numeric array (pre-processed raster array
#'   from png::readPNG()). Default is NULL (no background).
#' @param save_plot Logical. If TRUE, saves the plot as a PNG file. If FALSE,
#'   returns a ggplot2 object. Default is TRUE.
#' @param filename Character string specifying the output filename pattern.
#'   Must end with ".png" if save_plot is TRUE. Use sprintf-style formatting.
#'
#' @return If save_plot is TRUE, saves PNG file(s) and returns a message.
#'   If save_plot is FALSE, returns a ggplot2 object.
#'
#' @details
#' The function processes drawing data by:
#' \itemize{
#'   \item Validating required columns and data structure
#'   \item Unnesting nested coordinate and stroke data
#'   \item Reverting y-coordinates to match plotting conventions
#'   \item Scaling brush widths to appropriate sizes
#'   \item Handling pen and spray tools differently (spray adds jitter)
#'   \item Creating faceted plots for multiple drawings
#' }
#'
#' For spray tool data, the function recreates the spray effect by adding
#' random jitter within the spray radius. Brush widths are scaled from
#' the range 1-20 to 0.1-5 mm for plotting.
#'
#' @section Warnings:
#' The function will warn and prompt for confirmation when attempting to
#' recreate more than 9 drawings due to potential performance issues.
#'
#' @examples
#' \dontrun{
#' # Recreate and save drawings
#' pd_recreate_drawing(drawing_data)
#'
#' # Return plot object without saving
#' plot <- pd_recreate_drawing(drawing_data, save_plot = FALSE)
#'
#' # Use custom filename and background
#' pd_recreate_drawing(
#'   drawing_data,
#'   background_image = bg_img,
#'   filename = "output.png"
#' )
#' }
#'
#' @importFrom dplyr select mutate filter full_join join_by n
#' @importFrom tidyr unnest uncount
#' @importFrom png readPNG
#' @importFrom ggplot2 ggplot aes coord_fixed theme_void scale_color_identity
#'   scale_size_identity scale_alpha_identity scale_linewidth_identity
#'   geom_path geom_point facet_wrap annotation_raster ggsave
#' @importFrom stringr str_extract
#'
#' @export
pd_recreate_drawing <- function(
  .data,
  background_image = NULL,
  save_plot = TRUE,
  filename = "drawing%03d.png" # SON: I would suggest the user supplies a filename column (which could be `id` for instance) and then use {{filename}} in ggsave
) {
  pd <- .data
  # SON: rm(.data) to avoid copying in memory?

  # Making sure data has required columns
  pd_check_data(pd)

  # Test that we only 1 height and width value, respectively
  if ((pd$w |> unique() |> length() > 1) || (pd$h |> unique() |> length() > 1)) {
    stop(
      "More than one width or height value of image found.\n 
    You can only recreate drawings from one type of canvas at a time"
    )
  }

  # Test that filename ends with .png if save_plot = TRUE
  # SON: If we go with a filename column in data, this should be changed to just add '.png' if missing (e.g. if `id` is used as filename)
  if (save_plot & stringr::str_extract(filename, "...$") != "png") {
    stop(
      "Filename must end with png"
    )
  }

  # Validate and process background_image
  if (!is.null(background_image)) {
    if (is.character(background_image)) {
      # It's a file path
      if (!file.exists(background_image)) {
        stop("background_image file does not exist: ", background_image)
      }
      if (!grepl("\\.png$", background_image, ignore.case = TRUE)) {
        stop("background_image must be a PNG file (ending in .png)")
      }
      background_image <- png::readPNG(background_image)
    } else if (!is.numeric(background_image) || !is.array(background_image)) {
      # Not a numeric array (expected output from png::readPNG)
      stop(
        "background_image must be either:\n",
        "  - A file path to a PNG image (character string)\n",
        "  - A raster array from png::readPNG() (numeric array)"
      )
    }
  }

  # Test that ids are unique 
  if(any(duplicated(pd$id))) { # Good point! I have added this to the pd_check_data function, so we could drop it from here (SON)
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
      tidyr::uncount(weights = pd) |>

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
  ### Add a check for nrow(pd_pen) != 0 ..? (SON)
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
  }

  return(pd)
  # Alternatively:
  # return(list(data=pd, plot=plot_out))
}
