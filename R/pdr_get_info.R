pdr_get_info <- function(pdr, var="id") {
  # pdr is expected to be a valid pdr list-col
  # var should be a vector of length 1 or 2 -- first element
  # should be an element name in pdr (list), second element 
  # (if present) should be a column name in .strokes or 
  # .points (which should be element 1 or 2)

  var1 <- var[1]
  if(length(var)>1) {var2 <- var[2]}

  # Sanity check
  # Is pdr valid pain drawing data?
  if(!pdr_check_data(pdr, verbose=FALSE)) {
    pdr_check_data(pdr, verbose=TRUE) # Get some debug info for user
    stop("Invalid 'pdr' parameter in functional call pdr_get_info()")
  }
  # Is the var parameters (first element) a name in pdr?
  if(!{{var1}} %in% names(pdr[[1]])) {
    stop("Invalid first 'var' parameter in pdr_get_info()
     -- should be an element name of the pdr parameter")
  }
  if({{var1}} %in% c(".strokes", ".points")) {
    # Get col names from .strokes or .points tibbles of 1st list element
    tmp_names <- pdr[[1]] |> purrr::pluck(var1) |> names()
    if(!var2 %in% tmp_names) {
      stop("Invalid second 'var' parameter in pdr_get_info()
      -- should be an column name of the first var element of 'pdr'.")
    }
  }


  if(!{{var1}} %in% c(".strokes", ".points")) {
    # This should return a vector of same length as pdr
    return(pdr |> purrr::map_depth(.depth=1, {{var1}}) |> purrr::as_vector())
  } else {
    # This should return a list of same length as pdr
    return(pdr |> purrr::map_depth(.depth=1, {{var1}}) |> purrr::map(\(e) {e |> dplyr::pull( {{var2}} )}))
  }
}