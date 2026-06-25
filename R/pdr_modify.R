
#' Modify pain drawing elements
#' 
#' This function takes a valid pain drawing list as input
#' and performs one or more operations sequentially on those 
#' data before returning a valid pain drawing list-column of 
#' the same length. It is thus suited for `mutate()` operations.
#' 
#' 
#' @param pdr
#' @param ops
#'
#' @returns
#'
#' @export
#' @examples


pdr_modify <- function(pdr, ops, delta=5, templates = NULL) {
  # This function will:
  # a) perform sanity checks on input parameters
  # b) perform each of the ops specified 
  # 
  # Note that the ops are defined in separate functions
  # and are performed in a fixed sequence


  ########## Sanity checks ##########
  accepted_ops <- c("flipy", "drop_noarea", "buffer_noarea", "sanitize", "reduce_to_chull", "merge_overlaps", "merge_edges", "reduce_to_templates")

  # This function takes a valid pain drawing data set as input
  if (!pdr_check_data(pdr, verbose = FALSE)) {
    pdr_check_data(pdr, verbose = TRUE) # Give user some info
    stop("Stopped: The function `pdr_modify()` expects valid pain data as parameter 'pdr'. Use `pdr_check_data()` for more details.")
  }
  # Require 'ops' to be a character vector of fixed options
  if(!is.character(ops)) {
    stop("Stopped: The function `pdr_modify()` expects a character vector as parameter 'ops'.")
  } 
  if(any(identical(NA, ops)) || any(ops == "")) {
    warning("Unknown ops specified in function `pdr_modify()`.")
    return(pdr)
  }
  if(any(identical(ops, "reduce_to_templates")) && is.null(templates)) {
    stop("Stopped: The function `pdr_modify()` expects a templates parameter when 'ops' includes 'reduce_to_templates.")
  }
  if(any(!{ops %in% accepted_ops})) {
    warning("Unknown ops specified in function `pdr_modify()`.")
  }
  if(all(c("drop_noarea", "buffer_noarea") %in% ops)) {
    warning("Both 'drop_noarea' and 'buffer_noarea' specified in 'ops' of `pdr_modify()` -- defaulting to 'drop_noarea'.")
    ops <- ops[-which(ops == "buffer_noarea")]
  }
  if(all(c("merge_overlaps", "merge_edges") %in% ops)) {
    warning("Both 'merge_overlaps' and 'merge_edges' specified in 'ops' of `pdr_modify()` -- defaulting to 'merge_overlaps'.")
    ops <- ops[-which(ops == "merge_edges")]
  }
  if(any(c("merge_overlaps", "merge_edges", "reduce_to_templates") %in% ops) && !all("sanitize" %in% ops)) {
    ops <- c("sanitize", ops)
  }


  ########## End sanity checks ##########

  ########## Helper functions ##########
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
  ########## End helper functions ########## 

  ########## ops functions ##########
  flipy <- function(pdr) {
    # This function flips (mirrors) the y-axis
    # This solves a common problem because different platforms
    # tend to use a traditional cartesion coordinates system
    # with origo at the _lower_ left whereas computer screens
    # typically place the origo in the _upper_ left

    pdr <- pdr |> 
      purrr::map(\(e) {
        e$.points$.y <- e$.height - e$.points$.y
        e
      })
    return(pdr)
  }
    
  drop_noarea <- function(pdr) {
    # This functions removes those strokes in pain drawings
    # which have no geometric area, i.e. points and two-vertex
    # line segments ... NOTE! it is possible for other geometries
    # to also have no area

    pdr <- pdr |>
      purrr::map(\(e) {
        if(is_blank_drawing(e)) {
          e # As there are no strokes just keep as is
        } else {
          # .. data (polygons) with no area (points, etc)
          e$.points <- e$.points |>
            dplyr::group_by(.index) |> 
            dplyr::group_modify(\(grp, indx) {
              if(is_polygon(list(x=grp$.x, y=grp$.y))) {
                grp
              } else {
                tibble::tibble(.x=as.integer(), .y=as.integer())
              }
            }) |>
            dplyr::ungroup()
          # ..update the strokes tibble
          e$.strokes <- pdr_help_reduce_stroke_data(e$.strokes, unique(e$.points$.index))
          e # ...keep this element
        }
      })
    return(pdr)
  }  
    
  buffer_noarea <- function(pdr, delta=5) {
    # This functions buffers those strokes in pain drawings
    # which have no geometric area, i.e. points and two-vertex
    # line segments -- delta is the buffer size

    pdr <- pdr |> # list of pdr data
      purrr::map(\(e) { # a single pdr
        if(is_blank_drawing(e)) {
          e # As no strokes just keep as is
        } else {
          # ..otherwise find and buffer strokes without area
          e$.points <- e$.points |>
            dplyr::group_by(.index) |> 
            dplyr::group_modify(\(grp, indx) {
              if(is_polygon(list(x=grp$.x, y=grp$.y))) {
                grp # Looks fine - just keep it
              } else {
                # If grp is a single point make it a 2-point line
                if(nrow(grp) == 1) {
                  grp <- rbind(grp, grp)
                  grp[2,'.x'] <- grp[2,'.x']+as.integer(1)
                } 
                # Now offset the points line and make tibble
                wrap_pc_offset(list(x=grp$.x, y=grp$.y), d=delta, e="round") |>
                  purrr::pluck(1) |>
                  tibble::as_tibble() |>
                  dplyr::rename(.x=x, .y=y)
              }
            }) # end group_modify
          e # return
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
        e$.points <- e$.points |>
          dplyr::group_by(.index) |>
          dplyr::group_modify(\(grp, indx) {
            grp <- grp |> dplyr::slice(chull(grp$.x, grp$.y)) 
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
        # First sanitize the coordinate points for each index
        e$.points <- e$.points |> # All coordinates for that drawing
          dplyr::group_by(.index) |>  # Now grouped by stroke index
          dplyr::group_modify(\(grp, indx) { # 
            # We store wrap_pc_simplify in tmp to avoid double work
            tmp <- wrap_pc_simplify(list(x=grp$.x, y=grp$.y))
            if(length(tmp)==0) {
              # Sanitizing this stroke yielded an empty result
              # Probably, the stroke is not a valid polygon (e.g a point)
              # So return an empty tibble
              tibble::tibble(.x=as.integer(), .y=as.integer())
            } else if (length(tmp)==1) {
              # After sanitizing, this stroke, we have 
              # coordinates of a single polygon
              tmp |>
                purrr::pluck(1) |>
                tibble::as_tibble() |>
                dplyr::rename(.x=x, .y=y)
            } else {
              # After sanitizing this stroke, we have
              # coordinates of mulitple polygons -- the 
              # outermost must have the largest bounding box
              tmp_indx <- tmp |> purrr::map_int(\(e) {
                bb_area = (max(e$x)-min(e$x))*(max(e$y)-min(e$y))
              }) |> purrr::as_vector() |> which.max()
              tmp[[tmp_indx]] |> tibble::as_tibble() |> dplyr::rename(.x=x, .y=y)
            }
          }) |>
          dplyr::ungroup()
        e$.strokes <- pdr_help_reduce_stroke_data(e$.strokes, unique(e$.points$.index))
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
    
  merge_edges <- function(pdr) {
    # This function merges any polygons that share aborder 
    # in each drawing. It is identical to merge_overlaps
    # except for the edges=TRUE parameter
    
    pdr <- pdr |> purrr::map(\(e) {
      e$.points <- pdr_help_merge_overlapping_polygons(e$.points, edges=TRUE)
      e$.strokes <- pdr_help_reduce_stroke_data(e$.strokes, unique(e$.points$.index))
      e
    }) # endmap

    return(pdr)    
  }

  reduce_to_templates <- function(pdr, templates) {
    # This function reduces each polygon to the subset which
    # is contained within a number of templates. This is 
    # useful for, e.g. reducing pain drawings to that within
    # the background template outline.
    
    # pdr <- pdr |> purrr::map(\(e) {
    #   e$.points <- ..function..(e$.points, templates)
    #   e$.strokes <- ..function..(e$.strokes, unique(e$.points$.index))
    #   e
    # }) # endmap
    return(pdr)    
  }

  ########## End ops functions ########## 


  if("flipy" %in% ops) {
    pdr <- flipy(pdr)
  }

  if("drop_noarea" %in% ops) {
    pdr <- drop_noarea(pdr)
  }

  if("buffer_noarea" %in% ops) {
    pdr <- buffer_noarea(pdr, delta)
  }

  if("sanitize" %in% ops) {
    pdr <- sanitize(pdr)
  }

  if("reduce_to_chull" %in% ops) {
    pdr <- reduce_to_chull(pdr)
  }

  if("merge_overlaps" %in% ops) {
    pdr <- merge_overlaps(pdr)
  }

  if("merge_edges" %in% ops) {
    pdr <- merge_edges(pdr)
  }

  if("reduce_to_templates" %in% ops) {
    pdr <- reduce_to_templates(pdr, templates)
  }

  return(pdr)
}