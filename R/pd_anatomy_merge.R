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
#' pd_anatomy_merge(pd_demo_anatomy)
#' 
#' bi <- system.file("extdata", "mird_body_background.png", package = "paindrawings")
#' pd_demo_anatomy |> pd_recreate_drawing(background_image = bi)
#' pd_demo_anatomy |> pd_anatomy_merge() |> pd_recreate_drawing(background_image = bi)
#' 
#' pd_demo_anatomy |> dplyr::filter(id %in% c("Front_left_thigh", "Front_left_leg", "Front_left_foot")) |> pd_recreate_drawing(background_image = bi)#' 
#' pd_demo_anatomy |> dplyr::filter(id %in% c("Front_left_thigh", "Front_left_leg", "Front_left_foot")) |> pd_anatomy_merge() |> pd_recreate_drawing(background_image = bi)
#' 
pd_anatomy_merge <- function(pd) {
  if (!pd_check_data(pd, verbose=FALSE)) { stop("`pd` must be a valid pain drawing data structure")}

  if(length(unique(pd$w))>1 | length(unique(pd$h))>1) {
    stop("More than one width or height value of pain drawing canvas found.")
  }

  if (pd$s |> purrr::map_int(\(t) nrow(t)) |> max() > 1) {
    stop("One or more rows contain more than one stroke/polygons.")
  }

  # Each row in pd is a pain drawing with just one stroke/polygon
  # We can thus collapse this into a pd data structure with just one pain drawing of
  # multiple strokes/polygons, numbered (i) 1:n, where n is nrow(pd).

  # Retain all other columns in `pd` but `p`...
  all_but_p <- pd |> dplyr::select(-p)
  # Create a new tibble ... pd collapsed into just column p with only one element - a tibble with a unique 'i' for each pain drawing in pd
  just_p_collapsed <- tibble::tibble(p = pd |> dplyr::pull(p) |> purrr::list_rbind(names_to = 'i') |> list() )
  # Now merge overlapping strokes/polygons ... this is time consuming!
  just_p_collapsed <- just_p_collapsed |> mutate(p=pd_poly_manage_overlaps(p, method="union"))
  # Get the surviving `i` values from the merged pain drawings -- which correspond to the row number in pd
  i_survivors <- unique(just_p_collapsed$p[[1]]$i)
  # Reduce `all_but_p` to the rows which survived pain drawing merger
  result <- all_but_p[i_survivors,]
  # Un-collapse/split just_p into a list and set the i to 1 in each tibble
  just_p <- 
    just_p_collapsed$p[[1]] |> 
    dplyr::group_by(i) |> 
    dplyr::group_split() |>
    purrr::map(\(tbl) {
      tbl$i <- as.integer(1)
      tbl
    })
  # Just latch `p` onto `result`
  result$p <- just_p
  return(result) 
}