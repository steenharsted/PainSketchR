#' Clean and normalize pain drawing stroke geometries
#'
#' Processes stroke coordinate data (`p` column) by removing 
#' self-intersections, optionally buffering strokes with 
#' no area, simplifying polygons, and applying additional 
#' geometric transformations.
#'
#' The function accepts either:
#' * a full pain drawing data structure (see [pdr_check_data()])
#'   - In this case the `p` column is modified and the full data structure is returned.
#' * a `p` list-column from such a structure
#'   - In this case a replacement `p` column is return, useful for `mutate()` calls
#'
#' @details
#' The following operations are applied in order:
#'
#' 1. **No-area-stroke handling** (points or lines with < 3 vertices)
#'    Controlled by `noarea_action`:
#'    * `"drop"`: remove such strokes
#'    * `"buffer"`: expand them into polygons using a buffer of size `buffer_delta`
#'
#' 2. **Polygon simplification**
#'    * Removes self-intersections
#'    * Removes duplicated vertices
#'
#' 3. **Convex hull transformation** (optional)
#'    * If `chull = TRUE`, each stroke is replaced by its convex hull
#'
#' 4. **Overlap handling** (optional)
#'    * Controlled by `overlaps`, 
#'
#' 5. **Polygon closure** (optional)
#'    * If `close_polygon = TRUE`, ensures each stroke forms a closed ring
#'
#' @param p A pain drawing data structure or a `p` list-column.
#' 
#' @empty2na TRUE or FALSE -- convert empty drawings to NA
#'
#' @param noarea_action Character string specifying how to handle strokes with
#'   fewer than 3 points. One of:
#'   * `"drop"`
#'   * `"buffer"`
#'
#' @param buffer_delta Integer buffer radius (in coordinate units) used when
#'   `noarea_action = "buffer"`.
#'
#' @param simplify Clean strokes/polygons of self-intersections and duplicate vertices
#' 
#' @param chull Logical. If `TRUE`, replace each stroke with its convex hull.
#'
#' @param overlaps Character string controlling how overlapping polygons are handled.
#'   See [pdr_poly_manage_overlaps()] for available options.
#'
#' @param close_polygon Logical. If `TRUE`, ensures that the first and last
#'   coordinate of each stroke are identical.
#'
#' @returns
#' If `p` is a pain drawing data structure, returns the same structure with a
#' modified `p` column.
#'
#' If `p` is a list-column, returns a cleaned list-column of the same length.
#'
#' @section Warning:
#' Using `overlaps != "nothing"` may invalidate the correspondence between the
#' `p` and `s` columns (stroke indices). Consider writing results to a new column.
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
pdr_sanitize <- function(pd, empty2na=TRUE, noarea_action="buffer", buffer_delta=5, simplify=TRUE, chull=FALSE, overlaps="union", close_polygon=TRUE) {
  # This function takes a valid pain drawing data set as input
  if (!pdr_check_data(pd, verbose = FALSE)) {
    pdr_check_data(pd, verbose = TRUE)
    stop("The function `pdr_sanitize()` expects a valid pain data set as input.")
  }

  # A simple helper function
  is_not_tibble_with_content <- function(t) {
    any(
          !tibble::is_tibble(t),
          nrow(t)==0
        )
  }
 

  # Convert empty pain drawings (no coordinates) to NA
  if (empty2na) {
    pd <- pd |>
      # ...one row (by id) at a time
      dplyr::group_by(id) |>
      dplyr::group_modify(\(pd_row, indx) {
        # In a single row, 'p' and 's' are list of just 1 element
        if(pd_row$p[[1]] |> is_not_tibble_with_content()) { 
          pd_row$p[[1]] <- NA
          pd_row$s[[1]] <- NA
        }
      })
  }
  

  
  # Now manage stroke polygons which are points or two-point lines:
  # We run this BEFORE the polyclip::simplify because, it always deletes points and lines
  
  # Drop pain drawing strokes with no area
  if (noarea_action = "drop") {
    pd <- pd |>
    # ...one row (by id) at a time
      dplyr::group_by(id) |>
      dplyr::group_modify(\(pd_row, indx) {
        if (!is.na(pd_row$p[[1]])) {
          # Only keep strokes with 3+ coordinate pairs (rows)
          pd_row$p[[1]] <- pd_row$p[[1]] |>
            dplyr::group_by(i) |> 
            dplyr::filter(dplyr::n()>2) |> 
            dplyr::ungroup()
          # Only keep stroke info in strokes still have coordinates
          pd_row$s[[1]] <- pd_row$s[[1]] |>
            dplyr::group_by(i) |>
            dplyr::filter(i %in% pd_row$p[[1]]$i) |>
            dplyr::ungroup()
          pd_row
        } else {
          pd_row
        }
      })
  }

    
  # Buffer pain drawing strokes with no area
  if (noarea_action = "buffer") {
    buffer_delta <- as.integer(round(buffer_delta))
    pd <- pd |>
    # ...one row (by id) at a time
      dplyr::group_by(id) |>
      dplyr::group_modify(\(pd_row, indx) {
        if(!is.na(pd_row$p[[1]])) {
          pd_row$p[[1]] <- pd_row$p[[1]] |> 
            dplyr::group_by(i) |>
            dplyr::group_modify(\(dfgr, i_dfgr) {
              if(nrow(dfgr)<3) { # its a point or a line
                # Its a point -- add another point, 1 pixel away and move on
                if(nrow(dfgr) == 1) {dfgr <- rbind(dfgr, dfgr+c(1,0)) } 
                # Its (now) a line
                polyclip::polylineoffset(list(x=dfgr$x, y=dfgr$y), delta=buffer_delta, endtype="round") |> 
                  purrr::map_dfr(\(q) {
                    tibble::tibble(x=as.integer(round(q$x)),
                                  y=as.integer(round(q$y)))  # exit map-in-map iteration
                  })
              } else {
                dfgr  # exit map-in-map iteration
              }
            }) |>
            dplyr::ungroup()
          pd_row
        } else {
          pd_row
        }
      })
  }

  # Remove self-intersections and duplicated vertices
  if (simplify) {
    pd <- pd |>
    # ...one row (by id) at a time
      dplyr::group_by(id) |>
      dplyr::group_modify(\(pd_row, indx) {

      })
  }

  pd <- pd |> purrr::map(\(df, i_df) {
    if(!tibble::is_tibble(df) || nrow(df)==0) {
      NA
    } else {
      df |> 
        dplyr::group_by(i) |>
        dplyr::group_modify(\(dfgr, i_dfgr) {
          polyclip::polysimplify(list(x=dfgr$x, y=dfgr$y), filltype="nonzero") |> # nonzero
          purrr::map_dfr(\(q) {
            tibble::tibble(x=as.integer(round(q$x)), 
                           y=as.integer(round(q$y)))})
        }) |> 
        dplyr::ungroup()
    }
  }) 
  
  # Replace each stroke polygon by its own convex hull
  if (chull) {
    pd <- pd |>
    purrr::map(\(df) {
      if(!tibble::is_tibble(df) || nrow(df)==0) {
        NA
      } else {
        df |> 
          dplyr::group_by(i) |>
          dplyr::group_modify(\(stroke, indx) {
            stroke |> dplyr::slice(chull(stroke$x, stroke$y)) # chull() returns indices (not coordinates)
          }) |>
          dplyr::ungroup()
      }
    })
  }

  # Close polygon
  if (close_polygon) {
    pd <- pd |>
      purrr::map(\(df) {
        if(!tibble::is_tibble(df) || nrow(df)==0) {
          NA
        } else {
          df <- df |> 
            dplyr::group_by(i) |>
            dplyr::group_modify(\(points, grp_vars) {
              if(!identical(points[nrow(points),], points[1,])) {
                points[nrow(points)+1,] <- points[1,]
              }
              points
            }) |>
            dplyr::ungroup()
        } # if
      }) # map
  } # if

  if (overlaps != "nothing") {
    pd <- pd |> pdr_poly_manage_overlaps(method = overlaps)
    # Must fix 's' column now
  }

  return(pd)
    }

