#' Implode several pain drawwings into one.
#'
#' This function takes as input a pain drawing data structure, where each row (pain drawing)
#' represents e.g. an anatomical region. The function merges these into a single pain drawing
#' (row) with multiple strokes/polygons.
#' 
#' The function is useful for merging anatomcial regions (represented in pd as pain drawings) 
#' which are connected or overlap.
#' 
#' @param pd must be a valid pain drawing data structure. Furthermore, each row in `pd` 
#' should consist of a pain drawing with just a single stroke/polygon -- if not, the pain
#' drawing will be exploded into multiple paindrawings before imploding.
#' @returns A new pain drawing data structure where the column `p` has been imploded into one 
#' set of coordinates, of multiple strokes. THe `s` columns retains all information from the
#' original strokes (in multiple pain drawings), but all other columns are replaced so the
#' result contains a single row: `w` and `h` are replaced by the maximum value observed, 
#' id is placed with 'imploded', `coord` and `app` are retained or replace with 'multiple' if
#' muliple values are observed, `ts` is replaced by the maximum (latest) value observed. All
#' other columns (if present) are deleted.
#'
#' @export
#' @examples
#' pdr_implode(pd_demo_anatomy)
#' 
#' bi <- pdr_example("mird_body_background")
#' pd_demo_anatomy |> pd_recreate_drawing(background_image = bi) # Note: execution time > 5 sec
#' pd_demo_anatomy |> pd_implode() |> pd_recreate_drawing(background_image = bi)
#' pd_demo_anatomy |> pd_implode() |> pd_sanitize(overlaps="merge") |> pd_recreate_drawing(background_image = bi)
#' 
#' pd_demo_anatomy |> dplyr::filter(id %in% c("Front_left_thigh", "Front_left_leg", "Front_left_foot")) |> pd_recreate_drawing(background_image = bi)#' 
#' pd_demo_anatomy |> dplyr::filter(id %in% c("Front_left_thigh", "Front_left_leg", "Front_left_foot")) |> pd_implode() |> pd_recreate_drawing(background_image = bi)
#' 
pdr_implode <- function(pd) {
  if (!pdr_check_data(pd, verbose=FALSE)) { stop("`pd` must be a valid pain drawing data structure")}

  ## Handle rest-of-data where it is not all the same, e.g id and canvas sizes ...
  ## sensible defaults or NA?  
  if(length(unique(pd$w))>1 | length(unique(pd$h))>1) {
    stop("More than one width or height value of pain drawing canvas found.")
  }

  # If there are pain drawings (rows) with more than one stroke
  # explode the data first, before we implode it
  if (any(pd$s |> purrr::map_lgl(\(s_elements) {nrow(s_elements) > 1}))) {
    pd <-  pdr_explode(pd)#stop("One or more rows contain more than one stroke/polygons -- perhaps run `pdr_unnest_strokes()` before `pdr_nest_strokes()`")
  }

  # Now, each row in pd is a pain drawing with just one stroke/polygon
  # We can thus collapse this into a pd data structure with just one pain drawing of
  # multiple strokes/polygons, numbered (i) 1:n, where n is nrow(pd).

  # s <- pd |> dplyr::select(s) |> tidyr::unnest(cols=s)
  # p <- pd |> dplyr::select(p) |> tidyr::unnest(cols=p)
  # Retain all other columns in `pd` but `p`...
  all_but_p_and_s <- pd |> dplyr::select(-p, -s) |>
    dplyr::summarize(
      id = "imploded",
      w = max(w),
      h = max(h),
      coord = ifelse(length(unique(coord))>1, "multiple", coord[1]),
      ts = max(ts), 
      app = ifelse(length(unique(app))>1, "multiple", app[1])
    )
  # Create a new tibble ... pd collapsed into just column p with only one element - a tibble with a unique 'i' for each pain drawing in pd
  just_p_collapsed <- tibble::tibble(p = pd |> dplyr::pull(p) |> purrr::list_rbind(names_to = 'i') |> list() )
  just_s_collapsed <- tibble::tibble(s = pd |> dplyr::pull(s) |> purrr::list_rbind(names_to = 'i') |> list() )

  return(
    dplyr::bind_cols(
      all_but_p_and_s,
      just_s_collapsed,
      just_p_collapsed
    )
  )
}
