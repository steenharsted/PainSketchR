#' Clean pain drawings for duplicates, no-area polygons, etc
#'
#' Pain drawings are subject to whatever input users have seen fit to add. This may include
#' markings or 'stroke' like isolated points and two-point lines, which have no area. 
#' Users may also draw highly complicated markings/strokes which self-intersect.
#' 
#' @param pd A valid pain drawing data structure -- see [pd_check] for more detail.
#' @param minarea A numeric value representing the minimum value of polygon areas to retain
#' @param noarea_action A string specifying how to manage polygons with no area (points and lines) -- accepted values: "drop" and "buffer"
#' @param delta A numeric value representing the buffering zone -- only relevant if noarea is set to "buffer"
#'
#' @returns A pain drawing data structure
#'
#' @export
#' @examples
#' 
#' 
pd_geom_cleanup <- function(pd, minarea=0, noarea_action="buffer", delta=1) {
  # pd is assumed to be a valid pd data structure -- a tibble with cols id, i, x, y
  # Run data sanity check ? ... to be completed
  # delta is the value (px) we want to add as buffer around points and lines with no area

  if (minarea>0) {
    # To be considered: Should we run an area calculation of each polygon and drop 
    # if less than some minimum area -- for example a 3 point polygon which is almost a line
    # or a multipoint polygon where all points a clustered very closely
    # ...?
  }

  if (noarea_action=="drop" || noarea_action=="buffer") {
    # Identify the strokes to be dropped or buffered
    noarea_id_i <- pd$points |> 
      dplyr::count(id, i) |>
      dplyr::filter(n<3) # points or two-point lines
  }

  if(noarea_action=="drop" && nrow(noarea_id_i)>0) {
    # Drop the strokes without changing stroke numbering (i)
    pd$points <- dplyr::anti_join(pd$points, noarea_id_i, by=c("id","i"))
    pd$strokes <- dplyr::anti_join(pd$strokes, noarea_id_i, by=c("id","i"))
  }

  if (noarea_action=="buffer" && nrow(noarea_id_i)>0) {
    # Buffer the strokes 
    # First, get the coordinates to buffer
    coor2buff <- dplyr::semi_join(pd$points, noarea_id_i, by=c("id","i"))
    # Second, buffer them...
    pd$points <- pd$points |>
      dplyr::group_by(id, i) |>
      dplyr::group_modify(\(stroke_coordinates, grp_vars) {
        # For each stroke, check whether it is a point, a line or 3+ vertex polygon
        if (nrow(stroke_coordinates)==1) {
          # It's a point - replace the point with four points
          dplyr::tibble(x=stroke_coordinates[[1,'x']] + c(-delta,delta,delta,-delta), y=stroke_coordinates[[1,'y']] + c(-delta,-delta,delta,delta))
        } else if (nrow(stroke_coordinates)==2) {
          # It's a line - replace the two points with 
          polyclip::polylineoffset(list(x=stroke_coordinates$x, y=stroke_coordinates$y), delta=delta, jointype="square", endtype="square") |> purrr::map_dfr(\(x) {x})
        } else {
          # It's a 3+ vertex polygon - do nothing
          stroke_coordinates
        }
      }) |>
      dplyr::ungroup()
  }

  # Deal with self-intersections
  pd$points <- pd$points |>
    dplyr::group_by(id,i) |>
    dplyr::group_modify(\(stroke_coordinates, grp_vars) {
      polyclip::polysimplify(list(x=stroke_coordinates$x, y=stroke_coordinates$y), filltype="nonzero") |> 
        purrr::map_dfr(\(x) {x})
    })
  return(pd)
}
