#' Clean and normalize pain drawing stroke geometries
#'
#' Processes stroke coordinate data (`p` column) by removing self-intersections,
#' optionally buffering degenerate strokes, simplifying polygons, and applying
#' additional geometric transformations.
#'
#' The function accepts either:
#' * a full pain drawing data structure (see [pd_check_data()])
#' * or a `p` list-column from such a structure
#'
#' In the former case, the `p` column is modified and the full data structure is returned.
#'
#' @details
#' The following operations are applied in order:
#'
#' 1. **Degenerate stroke handling** (points or lines with < 3 vertices)
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
#'    * Controlled by `overlaps`, see [pd_poly_manage_overlaps()]
#'
#' 5. **Polygon closure** (optional)
#'    * If `close_polygon = TRUE`, ensures each stroke forms a closed ring
#'
#' @param p A pain drawing data structure or a `p` list-column.
#'
#' @param noarea_action Character string specifying how to handle strokes with
#'   fewer than 3 points. One of:
#'   * `"drop"`
#'   * `"buffer"`
#'
#' @param buffer_delta Integer buffer radius (in coordinate units) used when
#'   `noarea_action = "buffer"`.
#'
#' @param chull Logical. If `TRUE`, replace each stroke with its convex hull.
#'
#' @param overlaps Character string controlling how overlapping polygons are handled.
#'   See [pd_poly_manage_overlaps()] for available options.
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
#' pd_demo_data$p <- pd_sanitize(pd_demo_data$p)
#'
#' # Clean within a pipeline
#' pd_demo_data <- pd_demo_data |>
#'   dplyr::mutate(p = pd_sanitize(p))
#'
#' # Apply convex hulls and close polygons
#' pd_demo_data <- pd_demo_data |>
#'   dplyr::mutate(p = pd_sanitize(p, chull = TRUE, close_polygon = TRUE))
pd_sanitize <- function(p, noarea_action="buffer", buffer_delta=5, chull=FALSE, overlaps="nothing", close_polygon=FALSE) {
  # This function takes a single p column from a valid pain drawing data set
  # p: a list of tibbles of i,x,y
  # However, if an entire pain drawing data structure is passed as `p`, simply mutate that column:
  if (pd_check_data(p)) {
    return(p |> dplyr::mutate(p=pd_sanitize(p, noarea_action, buffer_delta, chull, overlaps, close_polygon)))
  }

  # As we made it this far, `p` is a probably a p-column from a pain drawing data structure called by mutate
  # Sanity check!
  if (!is.list(p)) {
    stop("`p` is not a valid list-column")
  } 
  if (!all(p |> purrr::map_lgl(\(tib) {identical(tib, NA) || tibble::is_tibble(tib)}))) {
    stop("Every element of `p` should be a tibble -- perhaps you should use `pd_sanitize()` in mutate calls?")
  } 
  if (!all(p |> purrr::map_lgl(\(tib) {identical(tib, NA) || all(c("i","x","y") %in% names(tib))}))) {
    stop("Every tibble element of `p` must include columns i, x and y")
  } 
  if (!all(p |> purrr::map_lgl(\(tib) {identical(tib, NA) || all(is.integer(c(tib$i, tib$x, tib$y)))}))) {
    stop("One or more tibble element of `p` includes columns i,x and/or y which are not integers")
  }

  if(overlaps != "nothing") { 
    warning(("Calling `pd_sanitize()` with the parameter `overlaps` specified will return a (potentially) different set of brush stroke data (column `p`) -- brush stroke data in the `s` column may thus become invalid. We recommend to `mutate()` to a new column rather than replacing col `p`."))
  }

  # Now manage brush stroke polygons which are points or two-point lines:
  # We run this BEFORE the polyclip::simplify because, it always deletes points and lines
  buffer_delta <- as.integer(round(buffer_delta))
  p <- p |> 
    purrr::map(\(df, i_df) {
      if(!tibble::is_tibble(df) || nrow(df)==0) {
        NA 
      } else {
        if(noarea_action=="drop") {
          # Drop the strokes from coordinate data, if point or two-points 
          df |> 
            dplyr::group_by(i) |> 
            dplyr::filter(dplyr::n()>2) |> # only retain of more than 2 rows of coordinates
            dplyr::ungroup() # exit map iteration
        } else if(noarea_action=="buffer") {
          # Buffer the strokes' coordinate data, if a point or two points
          df |> 
            dplyr::group_by(i) |>
            dplyr::group_modify(\(dfgr, i_dfgr) {
              if(nrow(dfgr)<3) { # its a point or a line
                if(nrow(dfgr) == 1) {dfgr <- rbind(dfgr, dfgr+c(1,0)) } # Its a point -- add another point, 1 pixel away and move on
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
        }
      }
  }) # purrr:map

  # Remove self-intersections and duplicated vertices
  p <- p |> purrr::map(\(df, i_df) {
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
    p <- p |>
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

  if (overlaps != "nothing") {
    p <- p |> pd_poly_manage_overlaps()
  }

    if (close_polygon) {
    p <- p |>
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

  return(p)
}

