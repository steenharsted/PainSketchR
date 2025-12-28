pd_geom_cleanup <- function(pd, minarea=0, pointline="buffer", delta=1) {
  # pd is assumed to be a valid pd data structure -- list of three tibbles, etc
  # delta is the value (px) we want to add as buffer around
  # points and lines with no area

  # Run data sanity check ... to be completed

  if (minarea>0) {
    # Should we ...
    # run an area calculation of each polygon and drop if less than some minimum area
    # ...?
  }

  if (pointline=="drop" | pointline=="buffer") {
    # Identify the strokes to be dropped or buffered
    pointline_id_i <- pd$points |> 
      dplyr::count(id, i) |>
      dplyr::filter(n<3) # points or two-point lines
  }

  if(pointline=="drop" && nrow(pointline_id_i)>0) {
    # Drop the strokes without changing stroke numbering (i)
    pd$points <- dplyr::anti_join(pd$points, pointline_id_i, by=c("id","i"))
    pd$strokes <- dplyr::anti_join(pd$strokes, pointline_id_i, by=c("id","i"))
  }

  if (pointline=="buffer" && nrow(pointline_id_i)>0) {
    # Buffer the strokes 
    # First, get the coordinates to buffer
    coor2buff <- dplyr::semi_join(pd$points, pointline_id_i, by=c("id","i"))
    # Second, buffer them...
    pd$points <- pd$points |>
      dplyr::group_by(id, i) |>
      dplyr::group_modify(\(pointline_stroke, grp_vars) {
        # For each stroke, check whether it is a point, a line or 3+ vertex polygon
        if (nrow(pointline_stroke)==1) {
          # It's a point - replace the point with four points
          dplyr::tibble(x=pointline_stroke[[1,'x']] + c(-delta,delta,delta,-delta), y=pointline_stroke[[1,'y']] + c(-delta,-delta,delta,delta))
        } else if (nrow(pointline_stroke)==2) {
          # It's a line - replace the two points with 
          polyclip::polylineoffset(list(x=pointline_stroke$x, y=pointline_stroke$y), delta=delta, jointype="square", endtype="square") |> map_dfr(\(x) {x})
        } else {
          # It's a 3+ vertex polygon - do nothing
          pointline_stroke
        }
      }) |>
      dplyr::ungroup()
  }
  return(pd)
}
