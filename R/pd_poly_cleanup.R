#' Clean pain drawings for self-intersections and no-area polygons.
#' 
#' The function `pd_poly_cleanup()` removes self-intersections from pain drawing polygons by
#' reducing them to their convex hull polygons.
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
pd_poly_cleanup <- function(p, chull=TRUE, noarea_action="buffer", delta=5, overlaps="nothing") {
  # This function takes a single p column from a valid pain drawing data set
  # p: a list of tibbles of i,x,y

  if(overlaps != "nothing") { 
    warning(("Calling `pd_poly_cleanup()` with the parameter `overlaps` specified will return a (potentially) different set of strokes -- stroke data in the `s` column may become invalid. We recommend to `mutate()` to a new column rather than replacing col `p`."))
  }

  # Here, we should probably check for polygons with multiple vertices but no actual area
  # ..i.e duplicate coordinates and/or straight lines...

  # Now manage stroke polygons which are points or two-point lines:
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
            dplyr::filter(dplyr::n()>2) |> # only retain of more than 2 rows of coordinates
            dplyr::ungroup() # exit map iteration
        } else if(noarea_action=="buffer") {
          # Buffer the strokes' coordinate data, if a point or two points
          df |> 
            dplyr::group_by(i) |>
            dplyr::group_modify(\(dfgr, i_dfgr) {
              if(nrow(dfgr)<3) { # its a point or a line
                if(nrow(dfgr) == 1) {dfgr <- rbind(dfgr, dfgr+c(1,0)) } # Its a point -- add another point, 1 pixel away and move on
                polyclip::polylineoffset(list(x=dfgr$x, y=dfgr$y), delta=delta, endtype="round") |> 
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

  # Remove self-intersections and duplicated vertices
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
  
  # Replace each stroke polygon by its own convex hull
  if (chull) {
    p <- p |>
    purrr::map(\(df) {
      df |> 
        dplyr::group_by(i) |>
        dplyr::group_modify(\(stroke, indx) {
          stroke[chull(stroke$x, stroke$y)]
        })
    })
  }

  
  if (overlaps != "nothing") {
    p <- p |> pd_poly_manage_overlaps()
  }

  return(p)
}

