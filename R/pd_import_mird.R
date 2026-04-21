#' Convert MiRD pain drawing strings to coordinate data
#'
#' Parses pain drawing data stored in the legacy *MiRD* string format and
#' converts it into a list-column of stroke coordinate tibbles compatible with
#' the `p` column of a pain drawing data structure (see [pd_check_data()]).
#'
#' Each input string represents one pain drawing and is converted into a tibble
#' with columns `i`, `x`, and `y`, where `i` indexes individual strokes.
#'
#' @details
#' **MiRD string format**
#'
#' A MiRD pain drawing is encoded as a character string containing one or more
#' strokes. Each stroke is represented as a sequence of x/y coordinate pairs:
#'
#' * Coordinates are stored as a flat sequence: `(,x1,y1,x2,y2,...)`
#' * Multiple strokes are concatenated as:
#'   `(,x1,y1,...,),(,x1,y1,...)`
#'
#' This function:
#'
#' * Splits the string into individual strokes
#' * Converts coordinate sequences into `(x, y)` pairs
#' * Assigns a stroke index `i` to each group of coordinates
#'
#' **Coordinate system**
#'
#' MiRD uses a coordinate system with origin in the upper-left corner.
#' If `flip_y = TRUE`, the y-axis is flipped to match a lower-left origin
#' (e.g. for plotting), using a fixed canvas height of 500 units.
#'
#' **Missing or invalid input**
#'
#' * `NA` or malformed strings return `NA`
#' * No validation of coordinate consistency (e.g. odd-length sequences) is performed
#'
#' @param mird_string Character vector of MiRD-encoded pain drawing strings.
#'
#' @param flip_y Logical. If `TRUE` (default), flip y-coordinates to a
#'   lower-left origin coordinate system.
#'
#' @returns
#' A list of tibbles (one per input element). Each tibble contains:
#' * `i` (integer): stroke index
#' * `x`, `y` (integer): coordinates
#'
#' Elements corresponding to missing or invalid input are `NA`.
#'
#' @export
#'
#' @examples
#' mird <- c(
#'   NA,
#'   "(,293,201,300,202,303,202,)",
#'   "(,198,104,206,103,),(,203,100,209,108,)",
#'   "(,335,215,336,215,),(,331,211,331,212,331,213,331,214,),(,341,263,),(,341,272,),(,341,287,)"
#' )
#'
#' # Convert to list-column of coordinate tibbles
#' p <- pd_import_mird(mird)
#'
#' # Use inside a data pipeline
#' pd_df <- tibble::tibble(id = c("A", "B", "C"), mird_string = mird)
#' pd_df <- pd_df |>
#'   dplyr::mutate(p = pd_import_mird(mird_string))
#' 
pd_import_mird <- function(mird_string, flip_y=TRUE) {
  if (!is.character(mird_string) || length(mird_string) < 1) {
    stop("Parameter `mird_string` must be a vector of characters of length 1+")
  }

  # This function is vectorized and return a list
  # Each string in mird_string becomes and element in the list
  # Each element is a tibble of i,x,y values
  p <- mird_string |> purrr::map(\(ms, i) {    
    # Check parameter sanity
    if(!is.na(ms) && is.character(ms) && stringr::str_length(ms)>6) { # Absolute minimum mird string with data: "(,0,0,)"
      ms <- stringr::str_replace_all(ms, ",\\),\\(,", ";")
      ms <- stringr::str_replace_all(ms, "\\(,", "")
      ms <- stringr::str_replace_all(ms, ",\\)", "")
      points <- stringr::str_split(ms, ";", simplify=TRUE)
      points <- stringr::str_split(points, ",")
      # Check for even number of points?
      points <- points |> purrr::imap_dfr(\(xy,i) {
        xy <- as.integer(xy) |> matrix(ncol=2, byrow=TRUE) |> tibble::as_tibble(.name_repair = ~c("x","y")) 
        xy$i <- as.integer(i)
        xy
      })    
      # MiRD drawings have origo at top left corner -- ggplot at lower left corner
      if(flip_y) {points$y <- as.integer(500-points$y)}
      points # End map iteration here
    } else {
      #warning("Empty or invalid mird string detected.")
      NA
    }
  })

  s <- p |>
    purrr::map(\(tib) {
      if(!tibble::is_tibble(tib) || nrow(tib)==0) {
        NA
      } else {
        tibble::tibble(
          i = as.integer(unique(tib$i)),
          q = as.integer(unique(tib$i)),
          t = "pen",
          bw = 5,
          c ="#000000",
          a = 1
        )
        
      }
    })

  result <- tibble::tibble(
    id = as.character(1:length(p)),
    w = as.integer(450),
    h = as.integer(500),
    coord = "px",
    ts = as.character(Sys.time()),
    s = s,
    p = p
  )
  
  return(result)
}