#' Clean pain drawings for self-intersections and no-area polygons.
#' 
#' The function `pd_poly_cleanup()` removes self-intersections from pain drawing polygons. 
#' It also identifies _no-area_ polygons, i.e. single points and two-point lines,
#' which have no area -- these can be either deleted or buffered into squares with areas.
#' 
#' @param pd A valid pain drawing data structure -- see [pd_check_data] for more detail.
#' @param noarea_action A string specifying how to manage polygons with no area (points and lines) -- accepted values: "drop" and "buffer"
#' @param delta A numeric value representing the buffering zone -- only relevant if noarea is set to "buffer"
#'
#' @returns A pain drawing data structure
#'
#' @export
#' @examples
#' pd_demo_data <- pd_demo_data |> mutate(p=pd_poly_cleanup(p))
#' 
pd_poly_cleanup <- function(p, noarea_action="buffer", delta=5) {
  # This function takes a single p column from a valid pain drawing data set
  # p: a list of tibbles of i,x,y

  delta <- as.integer(round(delta))
  p <- p |> 
    purrr::map(\(df, i_df) {
      if(!tibble::is_tibble(df) || nrow(df)==0) {
        NA 
      } else {
        if(noarea_action=="drop") {
          # Drop the strokes from coordinate data, if point or two-points 
          df |> 
            dplyr::group_by(i) |> 
            dplyr::filter(dplyr::n()>2) |>
            dplyr::ungroup() # exit map iteration
        } else if(noarea_action=="buffer") {
          # Buffer the strokes from coordinate data, if a point or two points
          df |> 
            dplyr::group_by(i) |>
            dplyr::group_modify(\(dfgr, i_dfgr) {
              if(nrow(dfgr)==1) {
                tibble::tibble(
                  x=as.integer(round(dfgr$x + c(-delta,delta,delta,-delta))), 
                  y=as.integer(round(dfgr$y + c(-delta,-delta,delta,delta)))) # exit map-in-map iteration
              } else if (nrow(dfgr)==2) {
                polyclip::polylineoffset(list(x=dfgr$x, y=dfgr$y), delta=delta, jointype="square", endtype="square") |> 
                  purrr::map_dfr(\(q) {
                    tibble::tibble(x=as.integer(round(q$x)),
                                   y=as.integer(round(q$y)))  # exit map-in-map iteration
                  })
              } else {
                dfgr  # exit map-in-map iteration
              }
            }) |>
            dplyr::ungroup()
        }
      }
  }) # purrr:map

  p <- p |> purrr::map(\(df, i_df) {
    if(!tibble::is_tibble(df)) {
      NA
    } else {
      df |> 
        dplyr::group_by(i) |>
        dplyr::group_modify(\(dfgr, i_dfgr) {
          polyclip::polysimplify(list(x=dfgr$x, y=dfgr$y), filltype="nonzero") |> 
          purrr::map_dfr(\(q) {
            tibble::tibble(x=as.integer(round(q$x)), 
                           y=as.integer(round(q$y)))})
        }) |> 
        dplyr::ungroup()
    }
  }) 
  
  p
}

