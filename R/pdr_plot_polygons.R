#' Plot polygons
#' 
#' This function is a simple wrapper for the 
#' `pdr_plot_drawing()` function. 
#' 
#' It substitutes the polygon coordinates for the raw data
#' coordinates in the pain drawing data list-col, before
#' plotting. 
#'
#' For a description of parameters and other details, 
#' see `pdr_plot_drawing()` documentation.
#' 
#' @examples
#' # Plot the raw data from geometry examples
#' pdr_example_geometry |> pdr_plot_drawing()
#' 
#' # Plot as polygons -- note closure and buffering
#' pdr_example_geometry |> 
#'   mutate(pdr_data = pdr_polygonize(pdr_data, buffer=5)) |> 
#'   pdr_plot_polygons()
#' 
#' # Merge overlapping strokes before plotting polygons
#' pdr_example_geometry |> 
#'   mutate(pdr_data = pdr_polygonize(pdr_data, buffer=5)) |> 
#'   mutate(pdr_data = pdr_modify_polygons(pdr_data, "merge_overlaps")) |> 
#'   pdr_plot_polygons()
pdr_plot_polygons <- function(
  .data,
  paindrawr_data = pdr_data,
  background_image = NULL,
  include_id = FALSE,
  rasterize = TRUE,
  method = "memory",
  clean_up = TRUE,
  dpi = 96,
  type = "path"
) {
  # This function is a simple wrapper for the 
  # dpr_plot_drawing() function

  # Make a copy of the pdr list-col, then
  # extract coordinates from the polygons and substitute 
  # them for .points
  .data <- .data |> mutate(pdr = {{ paindrawr_data }})
  .data <- .data |>
    mutate(pdr = pdr |> 
      purrr::map(\(e) {
        e$.points <- e$.polygons |> sf::st_coordinates() |>
          tibble::as_tibble() |>
          dplyr::select(.x=X, .y=Y, .index=L2)
        e # return
      })
    )
  

  pdr_plot_drawing(
    .data,
    pdr,
    background_image,
    include_id,
    rasterize,
    method,
    clean_up,
    dpi,
    type
  )
}