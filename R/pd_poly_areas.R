#' Calculate area of marking/stroke for pain drawings
#'
#' Calculate the area of each marking/stroke as a closed polygon. Either on a per-stroke basis, per-drawing basis or both.
#' 
#' @param p A list of tibbles of i,x and y columns (as integers) -- see [pd_check_data] for more detail.
#' @param by A string of either 'strokes', 'drawings' or 'both'. 
#'
#' @returns If `by` is "strokes", the function will return a list of tibbles of stroke areas (as integers) -- one list element (tibble) for each row in `p`. 
#' If `by` is "drawings", the function returns a vector of collated drawing areas (as integers) of the same length as `p`. 
#' If `by` is "both", it resutns a named list of both.
#'
#' @export
#' @examples
#' overlap <- pd_poly_anatomy_overlap(pd_demo_data[1:3,], pd_Back_right_leg)
#' pd_poly_areas(overlap, by="both")
#'  
pd_poly_areas <- function(p, by="drawings") {

  # Calculate the area of each stroke/polygon -- this is necessary irrespective of 'by' parameter
  p <- p |> 
    purrr::map(\(df, i_df) {
      if(tibble::is_tibble(df) && nrow(df)>2) {
        # It has more than two coordinate pairs
        df |>
          dplyr::group_by(i) |>
          dplyr::group_modify(\(dfgr, i_dfgr) {
            tibble::tibble(area = as.integer(round(geometry::polyarea(c(dfgr$x,dfgr$x[[1]]), c(dfgr$y,dfgr$y[[1]]))))) ## dfgr$y[[1]] to close poly          
          }) |> 
          dplyr::ungroup()
      } else if(tibble::is_tibble(df) && nrow(df)<3) {
        # It is a single point or a two point line
        0
      } else {
        NA
      } # end if
    }) # end map
  
  if(by=="drawings") {
    p |> purrr::map_int(\(df) {
      if(tibble::is_tibble(df)) {
        sum(df$area)
      } else {
        as.integer(0)
      }      
    })
  } else if (by=="strokes") {
    p
  } else {
    list(drawings = {
      p |> purrr::map_int(\(df) {
        if(tibble::is_tibble(df)) {
          sum(df$area)
        } else {
          as.integer(0)
        }
      })
    },
    strokes = p)
  }
}