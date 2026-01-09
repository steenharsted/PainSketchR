#' Close polygons
#' 
#' 'Closing' a polygon refers to ensuring, that the first and last coordinate
#' set (x,y) are identical.
#' 
#' Generally speaking, polygon definitions should not be closed, as the order
#' of points in the geometric defintions should be irrelevant. 
#'
#' @param pd
#'
#' @returns A pain drawing definition
#'
#' @export
#' @examples
#' 
#' 
pd_geom_close_polygons <- function(pd) {
  pd <- pd |> 
    dplyr::group_by(id,i) |>
    dplyr::group_modify(\(points, grp_vars) {
      if(!identical(points[nrow(points),], points[1,])) {
        points[nrow(points)+1,] <- points[1,]
      }
      points
    })
    pd
}