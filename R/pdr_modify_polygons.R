
#' Modify pain drawing elements
#' 
#' This function takes a valid pain drawing list as input
#' and performs one or more operations sequentially on those 
#' data before returning a valid pain drawing list-column of 
#' the same length. It is thus suited for `mutate()` operations.
#' 
#' 
#' @param pdr_data
#' @param ops
#'
#' @returns
#'
#' @export
#' @examples


pdr_modify_polygons <- function(paindrawr_data, ops) {
  lcol <- paindrawr_data
  rm(paindrawr_data)


  ########## Sanity checks ##########
  accepted_ops <- c("reduce_to_chull", "merge_overlaps", "make_valid", "merge_edges", "reduce_by_template")

  # This function takes a valid pain drawing data set as input
  # if (!pdr_check_data(pdr, verbose = FALSE)) {
  #   pdr_check_data(pdr, verbose = TRUE) # Give user some info
  #   stop("Stopped: The function `pdr_modify()` expects valid pain data as parameter 'pdr'. Use `pdr_check_data()` for more details.")
  # }
  # # Require 'ops' to be a character vector of fixed options
  # if(!is.character(ops)) {
  #   stop("Stopped: The function `pdr_modify()` expects a character vector as parameter 'ops'.")
  # } 
  # if(any(identical(NA, ops)) || any(ops == "")) {
  #   warning("Unknown ops specified in function `pdr_modify()`.")
  #   return(pdr)
  # }
  # if(any(identical(ops, "reduce_by_template")) && is.null(templates)) {
  #   stop("Stopped: The function `pdr_modify()` expects a templates parameter when 'ops' includes 'reduce_to_template'.")
  # }
  # if(any(!{ops %in% accepted_ops})) {
  #   warning("Unknown ops specified in function `pdr_modify()`.")
  # }
  # if(all(c("merge_overlaps", "merge_edges") %in% ops)) {
  #   warning("Both 'merge_overlaps' and 'merge_edges' specified in 'ops' of `pdr_modify()` -- defaulting to 'merge_overlaps'.")
  #   ops <- ops[-which(ops == "merge_edges")]
  # }


  ########## End sanity checks ##########

  ########## Helper functions ##########

  ########## End helper functions ########## 

  ########## ops functions ##########


  ########## End ops functions ########## 

  if("reduce_to_chull" %in% ops) {
    lcol <- lcol |>
      purrr::map_depth(.depth=1, \(pd) {
        pd <- pd |> 
          purrr::imap(\(element,indx) {
            if(indx==".polygons") {
              element |> 
                purrr::map(\(poly) {sf::st_convex_hull(poly)}) |> 
                sf::st_sfc()
            } else {
              element
            }
          })
      })
  }

  if("merge_overlaps" %in% ops) {
    lcol <- lcol |>
      purrr::map_depth(.depth=1, \(pd) {
        pd <- pd |> 
          purrr::imap(\(element,indx) {
            if(indx==".polygons") {
              element |> 
                sf::st_union() |>
                sf::st_cast("POLYGON")
            } else {
              element
            }
          })
      })
  }

  if("make_valid" %in% ops) {
    lcol <- lcol |>
      purrr::map_depth(.depth=1, \(pd) {
        pd <- pd |> 
          purrr::imap(\(element,indx) {
            if(indx==".polygons") {
              element |> 
                purrr::map(\(poly) {sf::st_make_valid(poly)}) |>
                sf::st_sfc()
            } else {
              element
            }
          })
      })
  }

  # if("merge_edges" %in% ops) {
  #   pdr <- merge_edge(pdr)
  # }

  # if("reduce_by_templates" %in% ops) {
  #   pdr <- reduce_by_template(pdr, template)
  # }

  lcol # return
}