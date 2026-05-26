
#' Modify pain drawing elements
#' 
#' This function takes a valid pain drawing list as input
#' and performs one or more operations sequentially on those 
#' data before returning a valid pain drawing list-column of 
#' the same length. It is thus suited for `mutate()` operations.
#' 
#' 
#' @param pdr
#' @param operations
#'
#' @returns
#'
#' @export
#' @examples


pdr_modify <- function(pdr, operations, delta=5) {
  # This function will:
  # a) perform sanity checks on input parameters
  # b) perform each of the operations specified in sequence
  # 
  # Note that the operations are defined in separate functions
  # each of which works recursively.



  { ########## Sanity checks >> ##########
    accepted_operations <- c("remove_duplicates", "drop_noarea", 
      "buffer_noarea", "remove_selfintersections", "reduce_to_chull",
    "close_polygons", "merge_overlaps")

    # This function takes a valid pain drawing data set as input
    if (!pdr_check_data(pdr, verbose = FALSE)) {
      pdr_check_data(pdr, verbose = TRUE) # Give user some info
      stop("Stopped: The function `pdr_modify()` expects valid pain data as parameter 'pdr'. Use `pdr_check_data()` for more details.")
    }
    # Require 'operations' to be a character vector of fixed options
    if(any(identical(NA, operations)) || any(operations == "")) {return(pdr)}
    if(!is.character(operations)) {
      stop("Stopped: The function `pdr_modify()` expects a character vector as parameter 'operations'.")
    }  
    if(any(!{operations %in% accepted_operations})) {
      warning(paste0("Unknown operations specified in function `pdr_modify()`."))
    }
    if(all(c("drop_noarea", "buffer_noarea") %in% operations)) {
      warning(paste0("Both 'drop_noarea' and 'buffer_noarea' specified in 'operations' of
      `pdr_modify()` -- defaulting to 'drop_noarea'."))
      operations <- intersect(operations, accepted_operations) 
    }


  } ########## << Sanity checks ##########

  { ########## Helper functions >> ##########
  is_blank_drawing <- function(entry) {
    if(identical(NA, entry$.points)) { return(TRUE)}
    if(nrow(entry$.points)==0) {return(TRUE)} 
    return(FALSE)
  }

  make_blank_na <- function(entry) {
    # This function changes the point and stroke column elements 
    # to NA if they are empty (blank pain drawing) or NA

    if(is_blank_drawing(entry)) { 
      entry$.points <- NA
      entry$.strokes <- NA
    }
    
    return(entry)
  }  
  } ########## << Helper functions ########## 

  { ########## Operations functions >> ##########
  remove_duplicates <- function(lcol) {
    # This function removes sequential duplicates of
    # coordinates. 

    # lcol is a list-col of multiple pain drawings 
    # ldata is an element from lcol - a list of data elements

    lcol |> 
      purrr::map(\(ldata) {
        if(is_blank_drawing(ldata)) {
          make_blank_na(ldata)
        } else {
          ldata$.points <- ldata$.points |>
            dplyr::group_by(.index) |>
            dplyr::group_modify(\(grp, indx) {
              grp <- grp |>
              dplyr::filter_out(
                dplyr::when_all(
                  .x == dplyr::lag(.x),
                  .y == dplyr::lag(.y)
                )
              ) # end filter_out
            }) |> # end group_modify
            dplyr::ungroup()
          ldata
        } # end if
      }) # end map 
  } 
    
  drop_noarea <- function(lcol) {
    # This functions removes those strokes in pain drawings
    # which have no geometric area, i.e. points and two-vertex
    # line segments

    # This is were the self-referencing recursion happens:
    # Recursion if lcol is a single pain drawing with a '.points' element:
    if(is.list(lcol) && ".points" %in% names(lcol)) {
      # If no coordinates - just return
      if(is_blank_drawing(lcol)) {
        return(lcol)
      }
      # ..otherwise find and keep only strokes consisting of 2+ points
      lcol$.points <- lcol$.points |>
        dplyr::group_by(.index) |> 
        dplyr::filter(dplyr::n()>2) |> 
        dplyr::ungroup()
      # ..and realign index in .strokes
      lcol$.strokes <- lcol$.strokes |>
        dplyr::filter(.index %in% lcol$.points$.index)
      return(lcol) 
    # Recursion if lcol is a list of pain drawings:
    } else {
      lcol <- lcol |>
        purrr::map(\(e) {e=drop_noarea(e)})
      return(lcol) 
    }  
  }  
    
  buffer_noarea <- function(lcol, delta) {
    # This functions buffers those strokes in pain drawings
    # which have no geometric area, i.e. points and two-vertex
    # line segments -- delta is the buffer size

    # This is were the self-referencing recursion happens:
    # Recursion if lcol is a single pain drawing with a '.points' element:
    if(is.list(lcol) && ".points" %in% names(lcol)) {
      # If no coordinates - just return
      if(is_blank_drawing(lcol)) {
        return(lcol)
      }
      # ..otherwise find and buffer strokes consisting of 1 or 2 points
      lcol$.points <- lcol$.points |>
      dplyr::group_by(.index) |> 
      dplyr::group_modify(\(grp, indx) {
        if(nrow(grp)<3) { 
          # ..stroke is a point or a line
          if(nrow(grp) == 1) {
            # If it's a point -- add another point and shift
            # it 1 pixel to make it a line .. then move on
            grp <- rbind(grp, grp)
            grp[2,'.x'] <- grp[2,'.x']+1
          } 
          # ..stroke is (now) a line
          polyclip::polylineoffset(list(x=grp$.x, y=grp$.y), delta=delta, endtype="round") |> 
            purrr::map_dfr(\(q) {
              tibble::tibble(.x=as.integer(round(q$x)),
                             .y=as.integer(round(q$y))) 
            })
        } else {
          grp  # exit map-in-map iteration
        }
      })
      return(lcol) 
    # Recursion if lcol is a list of pain drawings:
    } else {
      lcol <- lcol |>
        purrr::map(\(e) {e=buffer_noarea(e, delta)})
      return(lcol) 
    } 
  }
    
  remove_selfintersections <- function(lcol) {
  # This function relies on the polyclip simplify function
  # to remove self intersections from polygons

  # This is were the self-referencing recursion happens:
  # Recursion if lcol is a single pain drawing with a '.points' element:
    if(is.list(lcol) && ".points" %in% names(lcol)) {
      # If no coordinates - just return
      if(is_blank_drawing(lcol)) {
        return(lcol)
      }
      # ..otherwise simplify via polyclip
      lcol$.points <- lcol$.points |>
        dplyr::group_by(.index) |>
        dplyr::group_modify(\(grp, indx) {
          polyclip::polysimplify(list(x=grp$.x, y=grp$.y), filltype="nonzero") |> # nonzero
          purrr::map_dfr(\(q) {
            tibble::tibble(x=as.integer(round(q$x)), 
                          y=as.integer(round(q$y)))})      
        }) |>
        dplyr::ungroup()
      return(lcol)
    # Recursion if lcol is a list of pain drawings:
    } else  {
      lcol <- lcol |>
        purrr::map(\(e) {e=remove_selfintersections(e)})
      return(lcol)
    }     
  }
    
  reduce_to_chull <- function(lcol) {
  # This function reduces each polygopn to their convex hull

  # This is were the self-referencing recursion happens:
  # Recursion if lcol is a single pain drawing with a '.points' element:
    if(is.list(lcol) && ".points" %in% names(lcol)) {
      # If no coordinates - just return
      if(is_blank_drawing(lcol)) {
        return(lcol)
      }
      # ..otherwise reduce to convex hull
      lcol$.points <- lcol$.points |>
        dplyr::group_by(.index) |>
        dplyr::group_modify(\(grp, indx) {
          grp <- dplyr::slice(chull(grp$.x, grp$.y)) 
        }) |>
        dplyr::ungroup()
      return(lcol)
    # Recursion if lcol is a list of pain drawings:
    } else  {
      lcol <- lcol |>
        purrr::map(\(e) {e=reduce_to_chull(e)})
      return(lcol)
    }     
  }
    
  close_polygons <- function(lcol) {
  # This function reduces each polygopn to their convex hull

  # This is were the self-referencing recursion happens:
  # Recursion if lcol is a single pain drawing with a '.points' element:
    if(is.list(lcol) && ".points" %in% names(lcol)) {
      # If no coordinates - just return
      if(is_blank_drawing(lcol)) {
        return(lcol)
      }
      # ..otherwise close polygons (if open)
      lcol$.points <- lcol$.points |>
        dplyr::group_by(.index) |>
        dplyr::group_modify(\(grp, indx) {
          if(!identical(grp[nrow(grp),], grp[1,])) {
            grp[nrow(grp)+1,] <- grp[1,]
          }
          grp
        }) |>
        dplyr::ungroup()
      return(lcol)
    # Recursion if lcol is a list of pain drawings:
    } else  {
      lcol <- lcol |>
        purrr::map(\(e) {e=close_polygons(e)})
      return(lcol)
    }     
  }  
  
  merge_overlaps <- function(lcol) {
  # This function merges any overlapping polygons in each drawing

  # This is were the self-referencing recursion happens:
  # Recursion if lcol is a single pain drawing with a '.points' element:
    if(is.list(lcol) && ".points" %in% names(lcol)) {
      # If no coordinates - just return
      if(is_blank_drawing(lcol)) {
        return(lcol)
      }
      # If exactly one polygon - just return
      if(length(unique(lcol$.strokes$.index))==1) {
        return(lcol)
      }
      # ..otherwise mange overlaps with custom helper function
      lcol$.points <- lcol$.points |>
        pdr_poly_manage_overlaps()
      ############################################################ WHAT TO DO ABOUT .strokes ??
      return(lcol)
    # Recursion if lcol is a list of pain drawings:
    } else  {
      lcol <- lcol |>
        purrr::map(\(e) {e=merge_overlaps(e)})
      return(lcol)
    }     
  }  
    
  } ########## << Operations functions ########## 

  if("remove_duplicates" %in% operations) {
    pdr <- remove_duplicates(pdr)
  }

  if("drop_noarea" %in% operations) {
    pdr <- drop_noarea(pdr)
  }

  if("buffer_noarea" %in% operations) {
    pdr <- buffer_noarea(pdr, delta)
  }

  if("remove_selfintersections" %in% operations) {
    pdr <- remove_selfintersections(pdr)
  }

  if("reduce_to_chull" %in% operations) {
    pdr <- reduce_to_chull(pdr)
  }

  if("close_polygons" %in% operations) {
    pdr <- close_polygons(pdr)
  }

  if("merge_overlaps" %in% operations) {
    pdr <- merge_overlaps(pdr)
  }
  return(pdr)
}