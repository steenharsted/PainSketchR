
#' Subset pain drawings to the template outline
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

pd_geom_in_template_outline <- function(pd, template) {
  # pd is a pain drawing -- tibble with cols id,i,x,y
  # template is also a pain drawing but consists of area definitions, e.g. 'Back_of_head'
  
  # The purpose is to reduce the template polygons to their intersections with pd polygons
  # in practical terms, e.g. the subset of the polygon 'fron-upper-arm' which is an
  # intersection to a polygon from the paindrawing pd

  # All possible combinations of strokes/polygons from the paindrawings (pd) and the template
  combos <- dplyr::cross_join(pd |> dplyr::distinct(id, i), template |> dplyr::distinct(id, i))
  
  result <- tibble::tibble(id=character(), i=integer(), x=integer(), y=integer())
  
  for (c in 1:nrow(combos)) {
    a <- pd |> dplyr::filter(id == combos[[c,'id.x']] & i == combos[[c,'i.x']])
    b <- template |> dplyr::filter(id == combos[[c,'id.y']] & i == combos[[c,'i.y']])
    
    intersection <- pd_polyclip(a, b, op = "intersection") # Note this may contain multiple subsets
    
    if(tibble::is_tibble(intersection) && nrow(intersection)>0) {
      new_id <- paste0(combos[[c,'id.x']],"_",combos[[c,'i.x']],"_",combos[[c,'id.y']],"_",combos[[c,'i.y']])
      result <- dplyr::bind_rows(result, tibble::tibble(id = new_id, intersection))
      #print(str(result))
      
      #print(paste0(length(unique(intersection$i))," overlap(s) between ",combos[[c,'i.x']]," and ",combos[[c,'i.y']]," with ",nrow(intersection)," vertices."))
      
    }
  }
  result
}

