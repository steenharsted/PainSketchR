
#' Title
#'
#' @param pdr
#'
#' @returns
#'
#' @export
#' @examples
pdr_polygonize <- function(pdr, buffer=5) {
  # pdr is assumed to be valid pain drawing list-col data
  buffer = as.integer(buffer)

  # Creates new element .polygons in each pain drawing in pdr
  # Each .polygons is an sf object - st_multipolygon
  
  # Sanity checks
  # Buffer can not be less than 1
  if(buffer < 0) {
    warning("Buffer must be a positive integer, i.e. grater than 0")
    return(NA)
  }
  # We need to ensure than any buffering can be clipped to 
  # canvas if it exceeds it, i.e. must be < ½ canvas dimensions
  min_width <- pdr |> purrr::map_int(\(df) df$.width) |> min() 
  min_height <- pdr |> purrr::map_int(\(df) df$.height) |> min()
  if(!(buffer < 0.5*min_width & buffer < 0.5*min_height)) {
    warning("Buffer should not exceed ½ of any canvas width or height.")
    return(NA)
  }

  # Helper functions
  remove_consecutive_duplicates <- function(stroke_points) {
    if(identical(NA, stroke_points)) {
      return(NA)
    } else {
      stroke_points |>
        dplyr::filter_out(dplyr::lag(.x)==.x & dplyr::lag(.y)==.y)
    }
  }

  clip2canvas <- function(stroke_points, xmax, ymax) {
    stroke_points |> 
      dplyr::mutate(.x = ifelse(.x<0, 0, .x)) |>
      dplyr::mutate(.y = ifelse(.y<0, 0, .y)) |>
      dplyr::mutate(.x = ifelse(.x>xmax, xmax, .x)) |>
      dplyr::mutate(.y = ifelse(.y>ymax, ymax, .y)) 
  }

  buff_if_point <- function(stroke_points, buffer=5, xmax, ymax) {
    # This function assumes no consecutive duplicate rows

    if(identical(NA, stroke_points)) {
      return(NA)
    } else if(nrow(stroke_points) > 1) {
      stroke_points # ..as it's not a point )
    } else {
      x0 <- stroke_points[[1,'.x']] # pull value
      y0 <- stroke_points[[1,'.y']] # and create new tibble
      tibble::tibble(
        .x = c(x0-buffer, x0-buffer, x0+buffer, x0+buffer),
        .y = c(y0-buffer, y0+buffer, y0+buffer, y0-buffer)
      ) |> 
        clip2canvas(xmax, ymax)
    }
  }

  close_polygon <- function(stroke_points) {
    if(identical(stroke_points, NA)) {
      NA
    } else {
      if(identical(stroke_points[1,], stroke_points[nrow(stroke_points),])) {
        stroke_points
      } else {
        dplyr::bind_rows(stroke_points, stroke_points[1,])
      }   
    }
  }

  # mirai::daemons(n)
  # map(in_parallel(...))
  # ??

  pdr <- pdr |> purrr::map(\(a_drawing) {
    # Deal with stuff in the tibble before converting
    a_drawing$.polygons <- a_drawing$.points |> 
      dplyr::group_by(.index) |>
      dplyr::group_modify(~ {
        .x |>
          remove_consecutive_duplicates() |>
          buff_if_point(buffer, a_drawing$.width, a_drawing$.height) |>
          close_polygon() 
      }) |> 
      # Now to list - each element a tibble of stroke coordinates
      dplyr::group_split(.keep=FALSE) |> 
      purrr::map(\(e_stroke) {
        as.matrix(e_stroke, ncol=2, byrow=TRUE) |>
          list() |>
          sf::st_polygon()
      })
    
    a_drawing$.polygons <- a_drawing$.polygons |>
      purrr::map(\(a_polygon) {
        if(sf::st_area(a_polygon) == 0) {
          a_polygon |>
            sf::st_buffer(dist=buffer, nQuadSegs = 1) |>
            sf::st_convex_hull() 
        } else {
          a_polygon
        }
      })
    
    a_drawing # return at endmap
  })

  return(pdr)
}
