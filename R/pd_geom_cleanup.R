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
pd_geom_cleanup <- function(pd, noarea_action="buffer", delta=1, parallel=FALSE) {
  # pd is assumed to be a valid pd data structure -- a tibble with cols id, i, x, y
  # Run data sanity check ? ... to be completed
  # delta is the value (px) we want to add as buffer around points and lines with no area

  # We will need this more than once ...
  pd <- pd |> dplyr::group_by(id,i)

  if (parallel) {
    # To be considered: Should we run these operations as parallelize operations
    # ...?
  }

  if(noarea_action=="drop") {
    # Drop the strokes without changing stroke numbering (i)
    pd <- pd |> dplyr::filter(dplyr::n()>2)  # Already grouped by id and i
  }

  if (noarea_action=="buffer") {
    # Buffer the strokes without chaning stroke numbering (i)
    pd <- pd |> # Already grouped by id and i
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
      }) 
  }

  # Deal with self-intersections
  pd <- pd |>  # Already grouped by id and i
    dplyr::group_modify(\(stroke_coordinates, grp_vars) {
      polyclip::polysimplify(list(x=stroke_coordinates$x, y=stroke_coordinates$y), filltype="nonzero") |> 
        purrr::map_dfr(\(x) {x})
    })
  
  return(dplyr::ungroup(pd))
}
