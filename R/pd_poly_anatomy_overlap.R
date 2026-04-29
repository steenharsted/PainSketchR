
#' Subset pain drawings to intersection with an anatomical region
#'
#' This function ...
#'
#' @param pd A valid pain drawing data structure -- see [pdr_check_data] for more detail.
#' @param c The column of pd which holds stroke/polygon coordinates
#' @param anatomy_pd A valid pain drawing data structure -- see [pdr_check_data] for more detail.
#' @param anatomy_id A unique id from the anatomy_pd
#'
#' @returns A valid pain drawing data structure
#'
#' @export
#' @examples
#' pd_poly_anatomy_overlap(pd_demo_data, p, pd_demo_anatomy,  "Back_right_calf")
#'
#'
#'
pd_poly_anatomy_overlap <- function(p, anatomy_pd, anatomy_id="Not specified") {
  # p is from a pain drawing data structure: a list of tibbles with cols i,x,y
  # anatomy_pd is a similar data structure: a list of tibbles with cols i,x,y

  # The purpose is to reduce the anatomical region to its intersections with pd polygons
  # in practical terms, e.g. the subset of the polygon 'front-upper-arm' which is an
  # intersection to one or more polygons from the paindrawing pd

  # This function will accept anatomy_pd in one of two types: either a tibble of x,y
  # or a full tibble of pain drawing data (id, s and p) which then requires a anatomy_id
  # If user provides an anatomy_id -- select that row from anatomy_pd and unlist it
  if(anatomy_id!="Not specified") {
    anatomy_pd <- anatomy_pd |>
    dplyr::filter(id==anatomy_id) |>
    dplyr::pull(p) |>
    purrr::pluck(1) # there should be only 1 tibble though
    # it is now a tibble of two columns x and y
  } 
  if(!tibble::is_tibble(anatomy_pd) || !all(c("x","y") %in% names(anatomy_pd))) {
      stop("Please provide valid data for the anatomy region.")
  }

  # Iterate each element in the list p
  # We MUST return a list a same length as p

  result <- p |>
    purrr::map(\(df, i_df) {
      if(!tibble::is_tibble(df)) {
        NA # if there is no tibble in this p-element set the result-element to NA
      } else {
        # If the element of p is a tibble, we need to look at each stroke in
        # that tibble and look for intersections with the anatomy region:
        df |>
          dplyr::group_by(i) |> # strokes are defined by i
          dplyr::group_map(\(dfgr, i_dfgr) {
            # We recast each tibble group as a list element because i-values get scrambled.
            # We cannot simply convert the existing i values -- weird strokes could intersect
            # an anatomy region more than once .. thus convert output to a list (without i)
            pd_poly_clip(dfgr, anatomy_pd, op = "intersection") |>
              dplyr::select(-i)
          }) |> # Finally, re-cast the list of lists as a list of either NA or tibbles with new i values
          purrr::imap_dfr(\(df, i_df) {
            mutate(df, i=i_df)
          })
      } # end if
    }) # end map
  return(result)
}

