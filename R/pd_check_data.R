#' Check whether an R object is a valid pain drawing data structure
#'
#' A pain drawing data structure must comply with certain criteria -- see Details below.
#'
#' @details
#'
#' This function will check whether an R object fulfills the following criteria and provide some feedback.
#'
#' For an R object to be a valid pain drawing data object:
#'
#' * must be a tibble where each row represents a pain drawing
#' * it must have three named columns: `id`, `s`, and `p` -- users are free to add additional columns.
#' * the column `id` must be of type character
#' * the columns `s` and `p` must be list columns where each element (corresponding to each row):
#'     - must be of type tibble
#'     - the `s` tibbles must contain a col `i` of type integer
#'     - the `p` tibble must contain cols `i`, `x` and `y` of types integer
#'
#' The `id` column of the data object must represent a unique identifier for each pain drawing.
#'
#' The `i` column in each of the `s` tibbles must represent an index number for each stroke or marking, in that pain drawing. Thus, the `s` tibbles should contain exactly one row for each stroke/marking with a unique `i`.
#'
#' The `i` column in each of the `p` tibbles will be repeated for each coordinate x,y pair in each stroke/marking.
#'
#' The `x` and `y` columns must represent coordinates of each point in the pain drawing strokes.
#'
#' Users may store any other data in the data structure as relevant. Any information pertaining to the _top level_ pain drawing (e.g. date or project name) should be stored as columns in the top level tibble.
#' Any information pertaining to the individual stroke/marking (e.g. colour or thickness) should be stored as columns in the tibbles in the `s` list column. Similarly, in the unlikely event, that any information needs to be stored pertaining to each coordinate pair, it should be store as columns in the `p` list column tibbles.
#'
#' @param d The R object to examine
#'
#' @returns TRUE or FALSE. The function will also provide output to stderr pertaining to any issues detected.
#'
#' @export
#' @examples
#' pd_check_data(pd_demo_data, verbose = FALSE) # Will return TRUE
#' 
#' pd_check_data(letter[1:10]) # Will return FALSE and provide details in stderr
#'
pd_check_data <- function(d) {
  ok <- TRUE
  if(tibble::is_tibble(d)) {
    if (verbose) {message("Data is a tibble: OK")}
  } else {
    warning("Data is a tibble: FAIL")
    ok <- FALSE
  }

  ## Should we insist that each pain drawing has `w` and `h` columns? ..if width and height?
  if(all(c("id","s","p") %in% names(d))) {
    if (verbose) {message("Data has columns 'id', 's' and 'p': OK")}
  } else {
    warning("Data has columns 'id', 's' and 'p': FAIL")
    ok <- FALSE
  }

  if(is.character(d$id) && is.list(d$s) && is.list(d$p)) {
    if (verbose) {message("Data columns 'id', 's' and 'p' are <chr>, <list> and <list>: OK")}
  } else {
    warning("Data columns 'id', 's' and 'p' are <chr>, <list> and <list>: FAIL")
    ok <- FALSE
  }

  if(any(duplicated(d$id))) {
    if (verbose) {warning("All elements of 's' are unique: FAIL")}
    ok <- FALSE
  } else {
    message("All elements of 'id' are unique: OK")
  }

  if(all(sapply(d$s, tibble::is_tibble))) {
    if (verbose) {message("All elements of 's' are tibbles: OK")}
  } else {
    warning("All elements of 's' are tibbles: FAIL")
    ok <- FALSE
  }

  if(all(sapply(d$p, tibble::is_tibble))) {
    if (verbose) {message("All elements of 'p' are tibbles: OK")}
  } else {
    warning("All elements of 'p' are tibbles: FAIL")
    ok <- FALSE
  }

  if(d$s |> purrr::every(\(x) {all("i" %in% names(x))})) {
    if (verbose) {message("All elements of 's' have column 'i': OK")}
  } else {
    warning("All elements of 's' have column 'i': FAIL")
    ok <- FALSE
  }
  
  if(d$p |> purrr::every(\(x) {all(c("i","x", "y") %in% names(x))})) {
    if (verbose) {message("All elements of 's' have columns 's' and 'p': OK")}
  } else {
    warning("All elements of 's' have columns 's' and 'p': FAIL")
    ok <- FALSE
  }

  if(purrr::every(d$s$i, {is.integer})) {
    if (verbose) {message("All elements of 's' have column 'i' which is integer: OK")}
  } else {
    warning("All elements of 's' have column 'i' which is integer: FAIL")
    ok <- FALSE
  }

  if(purrr::every(d$p$i, {is.integer})) {
    if (verbose) {message("All elements of 'p' have column 'i' which is integer: OK")}
  } else {
    warning("All elements of 'p' have column 'i' which is integer: FAIL")
    ok <- FALSE
  }

  if(purrr::every(d$p$x, {is.integer})) {
    if (verbose) {message("All elements of 'p' have column 'x' which is integer: OK")}
  } else {
    warning("All elements of 'p' have column 'x' which is integer: FAIL")
    ok <- FALSE
  }

  if(purrr::every(d$p$y, {is.integer})) {
    if (verbose) {message("All elements of 'p' have column 'y' which is integer: OK")}
  } else {
    warning("All elements of 'p' have column 'y' which is integer: FAIL")
    ok <- FALSE
  }

  if(ok) {
    if (verbose) {message("Data structure is valid pain drawing data structure: OK")}
  } else {
    warning("Data structure is valid pain drawing data structure: FAIL")
  }

  return(ok)
}
