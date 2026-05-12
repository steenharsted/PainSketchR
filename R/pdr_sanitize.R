#' Clean and normalize pain drawing stroke geometries
#'
#' Processes stroke coordinate data (`p` column) by removing 
#' self-intersections, optionally buffering strokes with 
#' no area, simplifying polygons, and applying additional 
#' geometric transformations.
#'
#' The function accepts a valid pain drawing data structure
#' as input (see [pdr_check_data()]) as well as a number of
#' parameters specified below:
#'
#' @details
#' The following operations are applied in order:
#'
#' 1. **Sequential duplicates** of coordinate points are removed.
#'    This is always enforced.
#' 
#' 2. **Blank drawings** (i.e. no drawing data) are converted to NA
#' 
#' 3. **No-area-stroke** Whether to remove or buffer strokes with no area.
#'
#' 4. **Buffer delta** Size of no-area-strokes buffering.
#' 
#' 5. **Polygon simplification** Whether to remove self-intersections.
#'
#' 6. **Convex hull transformation** Whether to convert all strokes into their
#'    convex hull.
#'
#' 7. **Polygon closure** Whether to close stroke polygons.
#' 
#' 8. **Overlap handling** If and how to handle overlapping strokes
#'
#'
#'
#' @param pd A pain drawing data structure (see [pdr_check_data()]).
#' 
#' @empty2na Convert empty drawings to NA, TRUE (default) or FALSE.
#'
#' @param noarea Character string specifying how to handle strokes with
#'   fewer than 3 points. One of:
#'   * `"drop"`
#'   * `"buffer"` (default)
#'
#' @param buffer_delta Integer buffer radius (in coordinate units) used when
#'   `noarea_action = "buffer"`.
#'
#' @param simplify Clean strokes/polygons of self-intersections and duplicate vertices
#' 
#' @param chull Logical. If `TRUE`, replace each stroke with its convex hull.
#'
#' @param close_polygon Logical. If `TRUE`, ensures that the first and last
#'   coordinate of each stroke are identical.
#'
#' @param overlaps Character string controlling how overlapping polygons are handled:
#'    * `"nothing"` leaves overlaps untouched
#'    * `"union"` merges strokes that connect or overlap
#'    * `"intersection"` merges strokes that overlap
#' 
#' @returns
#' Return as pain drawing data structure of the same length as input `pd`.
#'
#' @export
#'
#' @examples
#' # Clean only the p column
#' pdr_demo_data$p <- pdr_sanitize(pdr_demo_data$p)
#'
#' # Clean within a pipeline
#' pdr_demo_data <- pdr_demo_data |>
#'   dplyr::mutate(p = pdr_sanitize(p))
#'
#' # Apply convex hulls and close polygons
#' pdr_demo_data <- pdr_demo_data |>
#'   dplyr::mutate(p = pdr_sanitize(p, chull = TRUE, close_polygon = TRUE))


pdr_sanitize <- function(
  pd, 
  blank2na=TRUE, 
  noarea="buffer", 
  buffer_delta=as.integer(5), 
  simplify=TRUE, 
  chull=FALSE, 
  overlaps="union", 
  close_polygon=TRUE) {
  # This function takes as input a valid pain drawing data
  # structure and 'sanitixes' it -- removes self-intersection,
  # and similar operations -- see documentation.

  # This function takes a valid pain drawing data set as input
  if (!pdr_check_data(pd, verbose = FALSE)) {
    stop("Stopped: The function `pdr_sanitize()` expects a valid pain data set as input.")
  }

  # Sanity check of parameters
  if (dplyr::when_all(blank2na != TRUE, blank2na != FALSE)) {
    stop("Stopped: `blan2na` in function `pdr_sanitize()` must be logical TRUE or FALSE.")
  }
  if (dplyr::when_all(noarea != "drop", noarea != "buffer")) {
    stop("Stopped: `noarea` in function `pdr_sanitize()` must be either 'drop' or 'buffer'.")
  }
  if (dplyr::when_any(!is.integer(buffer_delta), buffer_delta < 1)) {
    stop("Stopped: `buffer_delta` in function `pdr_sanitize()` must be an integer greater than 0.")
  }
  if (dplyr::when_all(simplify != TRUE, simplify != FALSE)) {
    stop("Stopped: `simplify` in function `pdr_sanitize()` must be logical TRUE or FALSE.")
  }
  if (dplyr::when_all(chull != TRUE, chull != FALSE)) {
    stop("Stopped: `chull` in function `pdr_sanitize()` must be logical TRUE or FALSE.")
  }
  if (dplyr::when_all(overlaps != "union", overlaps != "intersection")) {
    stop("Stopped: `overlaps` in function `pdr_sanitize()` must be either 'union' or 'intersection'.")
  }
  if (dplyr::when_all(close_polygon != TRUE, close_polygon != FALSE)) {
    stop("Stopped: `close_polygon` in function `pdr_sanitize()` must be logical TRUE or FALSE.")
  }

  # Some simple helper functions
  # 'entry' should be a list with just named elements
  # 'p' and 's' which are tibbles (or NA)
  is_blank_drawing <- function(entry) {
    if(identical(NA, entry$p)) { return(TRUE)}
    if(nrow(entry$p)==0) {return(TRUE)} 
    return(FALSE)
  }

  make_blank_na <- function(entry) {
    # This function changes the p and s column elements 
    # to NA if they are empty (blank pain drawing) or NA

    if(is_blank_drawing(entry)) { 
      entry$p <- NA
      entry$s <- NA
    }
    
    return(entry)
  }

  remove_duplicates <- function(entry) {
    # This function removes sequential duplicates of
    # coordinates

    # If no coordinates - just return
    if(is_blank_drawing(entry)) {
      return(entry)
    }

    entry$p <- entry$p |>
      dplyr::group_by(i) |>
      dplyr::group_modify(\(grp, indx) {
        grp <- grp |>
          dplyr::filter_out(
            dplyr::when_all(
              x == dplyr::lag(x),
              y == dplyr::lag(y)
            )
          )
      })
    
    return(entry)
  }

  drop_noarea <- function(entry) {
    # This functions removes those strokes in pain drawings
    # which have no geometric area, i.e. points and two-vertex
    # line segments

    # If no coordinates - just return
    if(is_blank_drawing(entry)) {
      return(entry)
    }

    # Iterate p coordinates by stroke and keep only if 2+ vertices
    entry$p <- entry$p |>
      dplyr::group_by(i) |> 
      dplyr::filter(dplyr::n()>2) |> 
      dplyr::ungroup()
    # Adjust s to only keep stroke info on strokes still in p
    entry$s <- entry$s |>
      dplyr::group_by(i) |>
      dplyr::filter(i %in% entry$p$i) |>
      dplyr::ungroup()
    # Reset index i in p and s 
    entry$p <- entry$p |>
      dplyr::group_by(i) |>
      dplyr::group_modify(\(grp, indx) { # indx = i
        grp |> mutate(ii=dplyr::cur_group_id())
      }) |>
      dplyr::ungroup() |>
      dplyr::mutate(i = ii) |> 
      dplyr::select(-ii)
    entry$s <- entry$s |>
      dplyr::group_by(i) |>
      dplyr::group_modify(\(grp, indx) { # indx = i
        grp |> mutate(ii=dplyr::cur_group_id())
      }) |>
      dplyr::ungroup() |>
      dplyr::mutate(i = ii) |> 
      dplyr::select(-ii)

    return(entry)  
  }

  buffer_noarea <- function(entry, buffer_delta) {
    # This functions buffers those strokes in pain drawings
    # which have no geometric area, i.e. points and two-vertex
    # line segments -- delta is the buffer size

    # If no coordinates - just return
    if(is_blank_drawing(entry)) {
      return(entry)
    }

    entry$p <- entry$p |>
      dplyr::group_by(i) |> 
      dplyr::group_modify(\(grp, indx) {
        if(nrow(grp)<3) { 
          # ..stroke is a point or a line
          if(nrow(grp) == 1) {
            # If it's a point -- add another point and shift
            # it 1 pixel to make it a line .. then move on
            grp <- rbind(grp, grp)
            grp[1,'x'] <- grp[1,'x']
          } 
          # ..stroke is (now) a line
          polyclip::polylineoffset(list(x=grp$x, y=grp$y), delta=buffer_delta, endtype="round") |> 
            purrr::map_dfr(\(q) {
              tibble::tibble(x=as.integer(round(q$x)),
                            y=as.integer(round(q$y))) 
            })
        } else {
          grp  # exit map-in-map iteration
        }
    })
    return(entry)
  }
 
  remove_selfintersections <- function(entry) {
    # This function relies on the polyclip simplify function
    # to remove self intersections from polygns

    # If no coordinates - just return
    if(is_blank_drawing(entry)) {
      return(entry)
    }
    
    entry$p <- entry$p |>
      dplyr::group_by(id) |>
      dplyr::group_modify(\(grp, indx) {
        polyclip::polysimplify(list(x=grp$x, y=grp$y), filltype="nonzero") |> # nonzero
            purrr::map_dfr(\(q) {
              tibble::tibble(x=as.integer(round(q$x)), 
                            y=as.integer(round(q$y)))})      
      }) |>
      dplyr::ungroup()

    return(entry) 
  }

  reduce_to_chull <- function(entry) {
    # This function replaces the polygon with its convex hull

    # If no coordinates - just return
    if(is_blank_drawing(entry)) {
      return(entry)
    }

    entry$p <- entry$p |>
      dplyr::group_by(i) |>
      dplyr::group_modify(\(grp, indx) {
        grp <- dplyr::slice(chull(grp$x, grp$y)) 
      }) |>
      dplyr::ungroup()

    return(entry)
  }

  make_closed_polygon <- function(entry)  {
    # This function closes a polygon 

    # If no coordinates - just return
    if(is_blank_drawing(entry)) {
      return(entry)
    }

    entry$p <- entry$p |>
      dplyr::group_by(i) |>
      dplyr::group_modify(\(grp, indx) {
        if(!identical(grp[nrow(grp),], grp[1,])) {
          grp[nrow(grp)+1,] <- grp[1,]
        }
        grp
      }) |>
      dplyr::ungroup()

    return(entry)
  }  
    
    
  merge_overlaps <- function(entry) {
    # This functions merges (by union) any overlapping polygons

    # If no coordinates - just return
    if(is_blank_drawing(entry)) {
      return(entry)
    }

    # If only 1 stroke - just return
    if(length(unique(entry$p$i)) < 2) {
      return(entry)
    }

    entry$p <- entry$p |>
      pdr_poly_manage_overlaps(method = overlaps)

    ## MUST FIX col s

    return(entry)
  }

# sanitize pain drawings
  pd <- pd |>
    purrr::map(\(entry) {
      # Enforce: removing sequential duplicate coordinates
      entry <- remove_duplicates(entry)

      # Replace blank data set with NA?
      if (blank2na) {entry <- make_blank_na(entry)}

      # Drop strokes with no area?
      #if (noarea=="drop") {entry <- drop_noarea(entry)}

  #     # Buffer strokes with no area?
  #     if (noarea=="buffer") {row <- buffer_noarea(row)}

  #     # Remove self-intersections?
  #     if (simplify) {row <- remove_selfintersections(row)}

  #     # Reduce to convex hull?
  #     if (chull) {row <- reduce_to_hull}

  #     # Close polygons?
  #     if (close_polygon) { row <- make_closed_polygon(row)}

  #     # Merge overlapping strokes
  #     # if (overlaps == "union") { ... }

    })

      


  return(pd)
}

