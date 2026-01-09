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
#' 
#' 
pd_geom_areas <- function(pd) {
  # Calculate the area of strokes/polygons

  pd |> 
    dplyr::group_by(id,i) |>
    dplyr::group_modify(\(points, grp_vars) {
      dplyr::tibble(area = geometry::polyarea(points$x, points$y))
    }) |>
    dplyr::ungroup()
}