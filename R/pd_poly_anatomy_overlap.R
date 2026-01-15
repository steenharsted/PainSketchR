
#' Subset pain drawings to the template outline
#'
#' To be completed
#' 
#' @param pd A valid pain drawing data structure -- see [pd_check] for more detail.
#' @param template A valid pain drawing data structure -- see [pd_check] for more detail.
#'
#' @returns A valid pain drawing data structure
#'
#' @export
#' @examples

pd_poly_anatomy_overlap <- function(pd, ar) {
  # pd is a pain drawing -- tibble with cols id,i,x,y
  # ar is anatomical region, e.g. 'Back_of_head'
  
  if(nrow(pd)>1) {warning("Function `pd_poly_anatomy_area` expects a pain drawing data frame with a single row.") ; return(NA)}

  # The purpose is to reduce the anatomical region to its intersections with pd polygons
  # in practical terms, e.g. the subset of the polygon 'front-upper-arm' which is an
  # intersection to one or more polygons from the paindrawing pd

  # All possible combinations of strokes/polygons from the paindrawings (pd) and the template
  
  points <- pd$p[[1]]
  strokes <- pd$s[[1]]
  # Define some variables to hold the result in a new data frame (to replace points and strokes)
  new_drawing_id <- paste0(pd$id,"_",ar$id) # New id to contain both pd and ar info
  new_i <- 1 # Start at the beginning - this is a row pointer for new_strokes and i for new_points
  new_points <- points[FALSE,] # Same structure as points but with no rows
  new_strokes <- strokes[FALSE,] # Same structure as strokes but with no rows
  
  # For this we need three counters (!) 'j' for each stroke in the pain drawing, 'new_i' for the strokes in
  # the intersection results and 'jj' for each intersection stroke per pain drawing strokes (there may be multiple - not likely though!)
  for(j in unique(points$i)) {
    intersection <- pd_poly_clip(points[points$i==j,], ar$p[[1]], op = "intersection") # Note this may contain multiple subset polygons
    if(nrow(intersection)==0) { # No intersection 
      # do nothing 
    } else {
      intersection$i <- intersection$i + new_i - 1 # shift the intersection stroke i's to fit new i's in result tibble
      for(jj in unique(intersection$i)) {
        new_strokes <- rbind(new_strokes, strokes[strokes$i == j,] |> dplyr::mutate(i=jj))
        new_points <- rbind(new_points, intersection[intersection$i==jj,])
        new_i <- new_i + 1
      }
    }
  }
  
  pd$id <- new_drawing_id
  pd$s <- list(new_strokes)
  pd$p <- list(new_points)
  return(pd)
}

