
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
  # b) perform each of the operations specified 
  # 
  # Note that the operations are defined in separate functions
  # and are performed in a fixed sequence


  { ########## Sanity checks ##########
    accepted_operations <- c("drop_noarea", "buffer_noarea", "sanitize", "reduce_to_chull", "merge_overlaps")

    # This function takes a valid pain drawing data set as input
    if (!pdr_check_data(pdr, verbose = FALSE)) {
      pdr_check_data(pdr, verbose = TRUE) # Give user some info
      stop("Stopped: The function `pdr_modify()` expects valid pain data as parameter 'pdr'. Use `pdr_check_data()` for more details.")
    }
    # Require 'operations' to be a character vector of fixed options
    if(!is.character(operations)) {
      stop("Stopped: The function `pdr_modify()` expects a character vector as parameter 'operations'.")
    } 
    if(any(identical(NA, operations)) || any(operations == "")) {
      warning("Unknown operations specified in function `pdr_modify()`.")
      return(pdr)}     
    if(any(!{operations %in% accepted_operations})) {
      warning("Unknown operations specified in function `pdr_modify()`.")
    }
    if(all(c("drop_noarea", "buffer_noarea") %in% operations)) {
      warning(paste0("Both 'drop_noarea' and 'buffer_noarea' specified in 'operations' of
      `pdr_modify()` -- defaulting to 'drop_noarea'."))
      operations <- intersect(operations, accepted_operations) 
    }


  } ########## End sanity checks ##########

  { ########## Helper functions ##########
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
  } ########## End helper functions ########## 

  { ########## Operations functions ##########

    
  drop_noarea <- function(pdr) {
    # This functions removes those strokes in pain drawings
    # which have no geometric area, i.e. points and two-vertex
    # line segments ... not it is possible for other geometries
    # to also have no area

    pdr <- pdr |>
      purrr::map(\(e) {
        if(is_blank_drawing(e)) {
          e # As no strokes just keep as is
        } else {
          # ..else remove points and two-segment-lines
          e$.points <- e.points |>
            dplyr::group_by(.index) |> 
            dplyr::filter(dplyr::n()>2) |> 
            dplyr::ungroup()
          # ..update the strokes tibble
          e$.strokes <- e$.strokes |>
            dplyr::filter(.index %in% e$.points$.index)
          e # ...keep this element
        }
      })
    return(pdr)
  }  
    
  buffer_noarea <- function(pdr, delta) {
    # This functions buffers those strokes in pain drawings
    # which have no geometric area, i.e. points and two-vertex
    # line segments -- delta is the buffer size

    pdr <- pdr |>
      purrr::map(\(e) {
        if(is_blank_drawing(e)) {
          e # As no strokes just keep as is
        } else {
          # ..otherwise find and buffer strokes without area
          e$.points <- e$.points |>
            dplyr::group_by(.index) |> 
            dplyr::group_modify(\(grp, indx) {
              if(nrow(grp)>2) {
                grp # is a 3+ vertex polygon, so just keep it
              } else { 
                # grp is a point or a 2-point line
                # if it is a point, make it a 2-point line
                if(nrow(grp) == 1) {
                  grp <- rbind(grp, grp)
                  grp[2,'.x'] <- grp[2,'.x']+as.integer(1)
                } 
                # Now offset the two-point line and make tibble
                wrap_pc_offset(list(x=grp$.x, y=grp$.y), d=delta, e="round") |>
                  tibble::as_tibble() |>
                  dplyr::rename(.x=x, .y=y)
              } # end if 
            }) # end group_modify
        } # end if
      }) # end map
        
    return(pdr)    
  }
    
  reduce_to_chull <- function(pdr) {
    # This function reduces each polygon to their convex hull

    pdr <- pdr |>
    purrr::map(\(e) {
      if(is_blank_drawing(e)) {
        e # As no strokes just keep as is
      } else {
        # ..else remove points and two-segment-lines
        e$.points <- e.points |>
          dplyr::group_by(.index) |>
          dplyr::group_modify(\(grp, indx) {
            grp <- dplyr::slice(chull(grp$.x, grp$.y)) 
          }) |>
          dplyr::ungroup()
        e # ...keep this element
      }
    })
    return(pdr) 
  } 
  
  sanitize <- function(pdr) {
    # This functions is essentially a wrapper for the 
    # polyclip simplify function -- removes self-intersections
    # and sequential coordinate duplicates -- this includes
    # un-closing the polygon if it is closed

    pdr <- pdr |> # A list-col from pdr data (tibble)
      purrr::map(\(e) { # A list element (i.e length==1 from a single line of pdr data)
        e$.points <- e$.points |> # All coordinates for that drawing
          dplyr::group_by(.index) |>  # Now grouped by stroke index
          dplyr::group_modify(\(grp, indx) { # 
            if(nrow(grp)<3) { # Stroke is point or line segment
              grp # Don't change anything
            } else {
              polyclip::polysimplify(
                A = list(
                      list(x=grp$.x,
                           y=grp$.y)
                    ), eps=1) |> 
                purrr::pluck(1) |>
              tibble::as_tibble() |> 
              dplyr::mutate_all(as.integer)
            }
          }) |>
          dplyr::ungroup()
        e # return
      })
    return(pdr)
  }
    
  merge_overlaps <- function(pdr) {
    # This function merges any overlapping polygons in each drawing
    # It iterates the elements of pdr ... i.e one function
    # call per pain drawing

    pdr <- pdr |> purrr::map(\(e) {
      e$.points <- pdr_help_merge_overlapping_polygons(e$.points)
      e$.strokes <- pdr_help_reduce_stroke_data(e$.strokes, unique(e$.points$.index))
      e
    }) # endmap

    return(pdr)    
  }  
    
  } ########## << Operations functions ########## 

  if("drop_noarea" %in% operations) {
    pdr <- drop_noarea(pdr)
  }

  if("buffer_noarea" %in% operations) {
    pdr <- buffer_noarea(pdr, delta)
  }

  if("sanitize" %in% operations) {
    pdr <- sanitize(pdr)
  }

  if("reduce_to_chull" %in% operations) {
    pdr <- reduce_to_chull(pdr)
  }

  if("merge_overlaps" %in% operations) {
    # ..must be sanitized before attempting overlap merging
    if (!{"sanitize" %in% operations}) {
      pdr <- sanitize(pdr) # If we didn't already...
    }
    pdr <- merge_overlaps(pdr)
  }
  return(pdr)
}