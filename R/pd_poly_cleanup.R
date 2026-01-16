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
#' pd_poly_cleanup(pd_demo_data[1,])
#' 
pd_multipoly_cleanup <- function(pd, noarea_action="buffer", delta=1, parallel=FALSE) {
  # This function will take a pd data structure and for each
  # individual pain drawing clean up no-area polygons, self-intersection, etc.

  # Check that pd is a valid pain drawing
  # ...to be done

  # Recursive self-calling in case pd is more than a single row (pain drawing):
  if(nrow(pd)>1) {
    pd <- pd |> 
      dplyr::rowwise() |>
      dplyr::group_modify(\(r,i) {pd_poly_cleanup(r)}) 
    return(dplyr::ungroup(pd))
  }

  # This only reached, if pd is a single row
  pd <- pd_poly_cleanup(pd)

  return(pd)
}

pd_poly_cleanup <- function(pd1, noarea_action="buffer", delta=1, parallel=FALSE) {
  # pd is assumed to be a valid pd data structure -- a tibble with cols id, i, x, y
  # Run data sanity check ? ... to be completed
  # delta is the value (px) we want to add as buffer around points and lines with no area

  if (nrow(pd1)>1) { warning("Function `pd_poly_cleanup` expects a pain drawing data frame with only 1 row -- did you want `pd_multipoly_cleanup()`?");return(NA)}

  points <- pd1$p[[1]] |> dplyr::group_by(i)
  strokes <- pd1$s[[1]] |> dplyr::group_by(i)

  if(noarea_action=="drop") {
    # Drop the strokes without changing stroke numbering (i)
    rows_to_drop <- points |> dplyr::filter(dplyr::n()<3) |> dplyr::pull(i)
    strokes <- strokes |> dplyr::filter(!i %in% dplyr::all_of(rows_to_drop))
    points <- points |> dplyr::filter(!i %in% dplyr::all_of(rows_to_drop))
  }

  if (noarea_action=="buffer") {
    # Buffer the strokes without changing stroke numbering (i)
    points <- points |> # Already grouped by i
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
  points <- points |>  # Already grouped by i
    dplyr::group_modify(\(stroke_coordinates, grp_vars) {
      polyclip::polysimplify(list(x=stroke_coordinates$x, y=stroke_coordinates$y), filltype="nonzero") |> 
        purrr::map_dfr(\(x) {x})
    })
  pd1$s <- list(dplyr::ungroup(strokes))
  pd1$p <- list(dplyr::ungroup(points))
  return(pd1)
}
