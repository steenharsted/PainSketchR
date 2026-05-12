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


pdr_sanitize <- function(pd, blank2na=TRUE, noarea="buffer", buffer_delta=5, simplify=TRUE, chull=FALSE, overlaps="union", close_polygon=TRUE) {
  
}

