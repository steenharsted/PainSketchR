
#' Subset pain drawings to the canvas outline
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
#' pd_geom_in_canvas_outline(my_paindrawings, anatomy_area_outline)

pd_geom_in_canvas_outline <- function(pd, template) {
  # pd is assumed to be a valid pain drawing data structure: list of 3 tibbles, etc
  # run sanity check? ... to be completed

  # template is assumed to be of the same data structure as pd, but probably with only one id
  
  # the purpose is to reduce the template polygons to their intersections with pd polygons
  # in practical terms, e.g. the subset of the polygon 'anterior-upper-arm' which is an
  # intersection to a polygon from the paindrawing pd

  # All possible combinations of strokes/polygons from the paindrawings (pd) and the template
  combos <- dplyr::cross_join(pd$strokes |> dplyr::select(id, i), template$strokes |> dplyr::select(id, i))

  #result_drawings <- dplyr::left_join(pd$drawings, combos |> dplyr::select(id_a, id_b) |> dplyr::mutate(id = paste0(id_a,"_",id_b)), by=c("id_a"))
  #result_strokes <- dplyr::left_joint(pd$strokes, combos |> dplyr::select(id_a, id_b) |> dplyr::mutate(id = paste0(id_a,"_",id_b)), by=c("id_a"))
  result_points <- tibble::tibble(id=character(), i=integer(), x=integer(), y=integer())
  
  for (c in 1:nrow(combos)) {
    a <- pd$points |> dplyr::filter(id == combos[[c,'id.x']] & i == combos[[c,'i.x']])
    b <- template$points |> dplyr::filter(id == combos[[c,'id.y']] & i == combos[[c,'i.y']])
    
    # Do a quick check to see whether bounding boxes overlap
    if(min(a$x)>max(b$x) || min(b$x)>max(a$x) || min(a$y)>max(b$y) || min(b$y)>max(a$y)) { 
      next # skip to next iteration in the inner loop, as there is no bounding box overlap
    }

    intersection <- pd_polyclip(a, b, op = "intersection") # Note this may contain multiple subsets
    
    if(nrow(intersection)>0) {
      new_id <- paste0(combos[[c,'id.x']],"_",combos[[c,'id.y']],"_",combos[[c,'i.x']],"_",combos[[c,'i.y']])
      result_points <- dplyr::bind_rows(result_points, tibble::tibble(id = new_id, intersection))
      #print(str(result_points))
      
      #print(paste0(length(unique(intersection$i))," overlap(s) between ",combos[[c,'i.x']]," and ",combos[[c,'i.y']]," with ",nrow(intersection)," vertices."))
      
    }
  }
  list(points=result_points)
}

