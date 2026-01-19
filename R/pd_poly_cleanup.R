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
  # which must be a list of dataframes of x,y and i (all int)

  p <- p |> 
    purrr::map(\(df, i_df) {
      if(any(is.na(df) | nrow(df)==0)) {
        warning("Data contains paindrawings with no strokes/coordinates")
        tibble::tibble(x=as.integer(), y=as.integer(), i=as.integer()) # exit map iteration
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
                  x=dfgr[[1,'x']] + c(-delta,delta,delta,-delta), 
                  y=dfgr[[1,'y']] + c(-delta,-delta,delta,delta)) # exit map-in-map iteration
              } else if (nrow(dfgr)==2) {
                polyclip::polylineoffset(list(x=dfgr$x, y=dfgr$y), delta=delta, jointype="square", endtype="square") |> 
                  purrr::map_dfr(\(q) {q}) # exit map-in-map iteration
              } else {
                dfgr  # exit map-in-map iteration
              }
            })
        }
      }
  }) # purrr:map

  p <- p |> purrr::map(\(df, i_df) {
    df |> 
      dplyr::group_by(i) |>
      dplyr::group_modify(\(dfgr, i_dfgr) {
        polyclip::polysimplify(list(x=dfgr$x, y=dfgr$y), filltype="nonzero") |> 
        purrr::map_dfr(\(q) {q}) 
      }) |> 
      dplyr::ungroup()
  }) 
  
  p
}

