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

pdr_get_info <- function(data_col, var=".id") {
  # data_col is expected to be a valid data_col list-col
  # var should be a vector of length 1 or 2 -- first element
  # should be an element name in data_col (list), second element 
  # (if present) should be a column name in .strokes or 
  # .points (which should be element 1 or 2)

  

  var1 <- var[1]
  if(length(var)>1) {var2 <- var[2]}

  # Sanity check
  # Is data_col valid pain drawing data?
  if(!pdr_check_data(data_col, verbose=FALSE)) {
    pdr_check_data(data_col, verbose=TRUE) # Get some debug info for user
    stop("Invalid 'data_col' parameter in functional call pdr_get_info()")
  }
  # Is the var parameters (first element) a name in data_col?
  if(!{{var1}} %in% names(data_col[[1]])) {
    stop("Invalid first 'var' parameter in pdr_get_info()
     -- should be an element name of the data_col parameter")
  }
  if({{var1}} %in% c(".strokes", ".points")) {
    # Get col names from .strokes or .points tibbles of 1st list element
    tmp_names <- data_col[[1]] |> purrr::pluck(var1) |> names()
    if(!var2 %in% tmp_names) {
      stop("Invalid second 'var' parameter in pdr_get_info()
      -- should be an column name of the first var element of 'data_col'.")
    }
  }

  # The following could be made more generic by testing the data type of {{var1}}
  # so as to return a vector if data is <int> or <char>, etc, but a list if matrix...

  if(!{{var1}} %in% c(".strokes", ".points")) {
    # This should return a vector of same length as data_col
    if({{var1}} == ".rgba") {
      return(data_col |> purrr::map_depth(.depth=1, {{var1}}) |> purrr::map(\(e) {e}))
    } else {
      return(data_col |> purrr::map_depth(.depth=1, {{var1}}) |> purrr::as_vector())
    }    
  } else {
    # This should return a list of same length as data_col
    return(data_col |> purrr::map_depth(.depth=1, {{var1}}) |> purrr::map(\(e) {e |> dplyr::pull( {{var2}} )}))
  }
}