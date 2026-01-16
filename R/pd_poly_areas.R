#' Calculate marking/stroke areas for pain drawings
#'
#' Calculate the area of each marking/stroke as a closed polygon and add the result to the 'strokes' element, before adding the cumulated area for each pain drawing to the 'drawings' element.
#' 
#' @param pd A valid pain drawing data structure -- see [pd_check] for more detail.
#'
#' @returns A valid pain drawing data structure with added area data ('drawings' and 'strokes')
#'
#' @export
#' @examples
#' overlap <- pd_poly_anatomy_overlap(pd_demo_data[1,], pd_Back_right_leg)
#' pd_poly_areas(overlap)#' 
pd_multipoly_area <- function(pd) {
  # This function will take a pd data structure and for each
  # individual pain drawing calculates the area for each strokes
  # and for each drawing in total
  # This is returned as a new column in the pd data frame

  # Check that pd is a valid pain drawing
  # ...to be done

  # Recursive self-calling in case pd is more than a single row (pain drawing):
  if(nrow(pd)>1) {
    pd |> 
      dplyr::rowwise() |>
      group_modify(\(r,i) {pd_poly_areas(r)}) |>
      dplyr::ungroup()
    return(pd)
  }

  # This only reached, if pd is a single row
  pd <- pd_poly_areas(pd)

  return(pd)
}

pd_poly_areas <- function(pd1) {
  # Calculate the area of strokes/polygons

  if(nrow(pd1)>1) {warning("Function `pd_poly_area` expects a pain drawing data frame with a single row -- did you want pd_multiply_area?") ; return(NA)}
  points <- pd1$p[[1]]
  strokes <- pd1$s[[1]]
  areas <- points |> 
    dplyr::group_by(i) |>
    dplyr::summarize(area = as.integer(geometry::polyarea(x, y)))
  strokes <- dplyr::left_join(strokes, areas, by="i")
  pd1$area <- sum(areas$area)
  pd1$s <- list(strokes)
  return(pd1)
}