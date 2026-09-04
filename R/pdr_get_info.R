#' Extract information from pain drawing objects
#'
#' Extracts metadata or variables from a pain drawing (`data_col`) list-column.
#' Simple elements (such as `".id"` or `".timestamp"`) are returned as a vector,
#' whereas columns within the nested `.strokes` or `.points` tibbles are
#' returned as a list with one element per pain drawing.
#'
#' @param data_col A valid pain drawing object (typically a list-column) as checked
#'   by [pdr_check_data()].
#' @param var A character vector of length 1 or 2 specifying what information
#'   to extract.
#'
#'   If `var` has length 1, the value should be the name of an element in each
#'   pain drawing (e.g. `".id"`).
#'
#'   If `var` has length 2, the first element must be either `".strokes"` or
#'   `".points"`, and the second element must be the name of a column in the
#'   corresponding tibble (e.g. `c(".strokes", ".alpha")`).
#'
#' @return
#' If `var[1]` refers to a regular element, a vector of length `length(data_col)` is
#' returned.
#'
#' If `var[1]` is `".strokes"` or `".points"`, a list of length `length(data_col)`
#' is returned, where each element contains the selected column from the
#' corresponding tibble.
#'
#' @examples
#' # Extract ids
#' pdr_example_data |> pdr_get_info(pdr_data, ".id")
#'
#' # Extract x coordinates from the .points tibble
#' pdr_example_data |> pdr_get_info(pdr_data, c(".strokes", ".alpha"))
#'#'
#' @seealso [pdr_check_data()]
#'
#' @export

pdr_get_info <- function(paindrawr_data, cols=".id") {
  # data_col is expected to be a valid data_col list-col
  # var should be a vector of length 1 or 2 -- first element
  # should be an element name in data_col (list), second element 
  # (if present) should be a column name in .strokes or 
  # .points (which should be element 1 or 2)

  if(length(cols)==1) {
    paindrawr_data |> 
      purrr::map(cols) |>    # Just the one element
      purrr::list_simplify() # Convert to int, chr, num, ..
  } else if(length(cols)==2) {
    if(cols[[2]]=="") {
      paindrawr_data |>
        purrr::map(cols[[1]])
    } else {
      paindrawr_data |>
        purrr::map(cols[[1]]) |>
        purrr::map(~.x |> dplyr::pull(cols[[2]]))  
    }
  } else {
    NA
  }
}