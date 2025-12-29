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
#' pd_geom_areas(my_paindrawings)
pd_geom_areas <- function(pd) {
  # Calculate the combined (summed) area of stroke polygons

  # Calculate the area of each stroke/points polygon
  stroke_areas <- pd$points |> 
    dplyr::group_by(id,i) |>
    dplyr::group_modify(\(points_data, grp_vars) {
      dplyr::tibble(stroke_area = geometry::polyarea(points_data$x, points_data$y))
    }) |>
    ungroup()
  
  # Add this area as a column to the strokes tibble
  pd$strokes <- dplyr::left_join(pd$strokes, stroke_areas, by=c("id","i"))
  # Add the accumulated area of strokes/points polygons to the drawings tibble
  pd$drawings <- dplyr::left_join(pd$drawings, stroke_areas |> dplyr::group_by(id) |> dplyr::summarize(drawing_area = sum(stroke_area)), by="id")

  return(pd)
}