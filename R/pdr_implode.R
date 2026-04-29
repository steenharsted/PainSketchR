#' Merge connected and overlapping anatomical regions
#'
#' This function merges all connected or overlapping anatomical regions into one region. Any
#' unconnected or none-overlapping regions are retained as separate regions.
#' 
#' This function takes as input a pain drawing data structure, where each row (pain drawing)
#' represents an anatomical region, and each anatomical region is defined by a single 
#' stroke/polygon.
#' 
#' The function merges those anatomcial regions (represented in pd as pain drawings) which 
#' are connected or overlap.
#' 
#' * Any connected or overlapping anatomical areas will be merged. 
#' * Any discrete (groups of) unconnected and not-overlapping anatomical areas will be returned 
#' as separate resulting pain drawings (rows).
#' 
#' Thus if `pd` consists of three pain drawings (rows) with id = 'A', 'B' and 'C', where only 
#' 'A' and 'B' overlap, the resulting pain drawing data structure will consist of two pain 
#' drawings (rows): 'A' (the resulting merger of 'A' and 'B') and 'C'. Note, that mergers will 
#' retain all other variables (columns) from the first pain drawing in that subset of rows, e.g. 
#' the `id` 'A'.
#' 
#' This function is useful for creating anatomical templates -- e.g. the anterior aspect of a 
#' full body template may consist of n different anatomical areas which all border or overlap 
#' each other. The function can be used to collapse these into a single pain drawing of n 
#' polygons, which are then merged. 
#' The resulting pain drawing structure with a single polygon (anterior aspect of the body 
#' outline) can be used to define a new anatomical region for analyses. 
#' Similarly, a pain drawing data structure with discrete groups of anatomical areas such as
#' e.g. both the anterior and posterior aspects will return separate regions where subregions
#' are merged within the anterior and posterior regions.
#' 
#' @param pd must be a valid pain drawing data structure. Furthermore, each row in `pd` 
#' should consist of a pain drawing with just a single stroke/polygon.
#' @returns A new pain drawing data structure where the column `p` has been collapsed into a 
#' merged pain area -- all other columns retain values from the first pain drawing of that merger.
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
  # unnest the data first
  if (any(pd$s |> purrr::map_lgl(\(t) nrow(t)) > 1)) {
    # just call unnest ...
    #stop("One or more rows contain more than one stroke/polygons -- perhaps run `pdr_unnest_strokes()` before `pdr_nest_strokes()`")
  }

  # Now, each row in pd is a pain drawing with just one stroke/polygon
  # We can thus collapse this into a pd data structure with just one pain drawing of
  # multiple strokes/polygons, numbered (i) 1:n, where n is nrow(pd).

  p <- pd$p 
  # Retain all other columns in `pd` but `p`...
  all_but_p_and_s <- pd |> dplyr::select(-p, -s)
  # Create a new tibble ... pd collapsed into just column p with only one element - a tibble with a unique 'i' for each pain drawing in pd
  just_p_collapsed <- tibble::tibble(p = pd |> dplyr::pull(p) |> purrr::list_rbind(names_to = 'i') |> list() )
  just_s_collapsed <- tibble::tibble(s = pd |> dplyr::pull(s) |> purrr::list_rbind(names_to = 'i') |> list() )

  # just_p <- 
  #   just_p_collapsed$p[[1]] |> 
  #   dplyr::group_by(i) |> 
  #   dplyr::group_split() |>
  #   purrr::map(\(tbl) {
  #     tbl$i <- as.integer(1)
  #     tbl
  #   })
  # # Just latch `p` onto `result`
  # result$p <- just_p
  # return(result) 
}