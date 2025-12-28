pd_geom_areas <- function(pd) {
  # Calculate the combined (summed) area of stroke polygons

  stroke_areas <- pd$points |> 
    dplyr::group_by(id,i) |>
    dplyr::group_modify(\(points_data, grp_vars) {
      dplyr::tibble(stroke_area = geometry::polyarea(points_data$x, points_data$y))
    }) |>
    ungroup()
  
  pd$strokes <- dplyr::left_join(pd$strokes, stroke_areas, by=c("id","i"))
  pd$drawings <- dplyr::left_join(pd$drawings, stroke_areas |> dplyr::group_by(id) |> dplyr::summarize(drawing_area = sum(stroke_area)), by="id")

  return(pd)
}