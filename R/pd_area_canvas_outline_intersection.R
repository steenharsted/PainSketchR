
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
#' pd_area_canvas_outline_intersection(my_paindrawings, body_area_outline)
pd_area_canvas_outline_intersection <- function(pd, template) {
  # pd is assumed to be a valid pain drawing data structure: list of 3 tibbles, etc
  # run sanity check? ... to be completed

  # template is assume to be of the same data structure as pd
  
  # the purpose is to reduce the template polygons to their intersections with pd polygons
  # in practical terms, e.g. the subset of the polygon 'anterior-upper-arm' which is an
  # intersection to a polygon from the paindrawing pd


  
}

