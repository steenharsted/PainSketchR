#' Validate a pain drawing data structure
#'
#' A pain drawing data structure must comply with certain criteria -- see Details below. 
#' This function returns TRUE or FALSE and optionally emits diagnostic messages.
#'
#' #' @details
#' A valid pain drawing data structure must satisfy all of the following:
#'
#' **Top-level structure**
#' * An unnamed list with one element per pain drawing
#' 
#' **Second-level structure**
#' * A list with one named elment per variable
#'     - Must contain elements: `.id`, `.units`, `.width`, `.height`, `.timestamp`, `.strokes`, `.points`
#'
#' **Second-level element types**
#' * `.id`: character (unique identifier)
#' * `.width`, `.height`: integer
#' * `.units`, `.timestamp`: character or NA
#' * `.strokes`, `.points`: tibble or NA
#'
#' **Element constraints**
#' * `.id` values must be unique
#'
#' **`.strokes` tibble or NA (stroke metadata)**
#' If tibble, must be structured as:
#' * Columns: `.index`, `.q`, `.tool`, `.tool_width`, `.color`, `.alpha`
#' * Types: integer, integer, character, integer, character, integer
#' * One row per stroke (`.index` must be unique)
#' * Exactly one unique value of `.color` per tibble
#'
#' **`.points` tibble or NA (stroke coordinates)**
#' If tibble, must be structured as:
#' * Columns: `.index`, `.x`, `.y`
#' * All integer
#' * `.index` values correspond to those in the matching `.strokes` tibble
#' 
#' #' **Relationship between `.strokes` and `.points`**
#' * The `.strokes` tibble defines strokes (one row per stroke)
#' * The `.points` tibble defines coordinates (multiple rows per stroke)
#' * The `.index` column links them:
#'   - `.strokes$.index`: unique stroke identifiers
#'   - `.points$.index`: repeats to associate coordinates with a stroke
#' 
#' 
#' @param d An object to validate.
#'
#' @returns
#' `TRUE` if `d` is valid, otherwise `FALSE`.
#' If `verbose = TRUE`, diagnostic messages are emitted.
#'
#' @export
#' @examples
#' pdr_check_data(pdr_demo_data, verbose = FALSE) # Will return TRUE
#'
#' pdr_check_data(letters[1:10]) # Will return FALSE and provide details
#'
pdr_check_data <- function(d, verbose = TRUE) {
  if (!is.list(d)) {
    warning("Data 'd' is not a list", call. = FALSE)
    return(FALSE)
  }

  # ---- helpers ----
  fail <- function(msg) {
    warning(msg, call. = FALSE)
    return(FALSE)
  }

  ok <- function(msg) {
    if (verbose) message(msg)
  }

  is_na_scalar <- function(x) {
    identical(x, NA)
  }

  is_list_or_na <- function(x) {
    is.list(x) || is_na_scalar(x)
  }

  has_names <- function(x, required) {
    all(required %in% names(x))
  }

  # ---- validators for nested tibbles ----
  valid_strokes_tbl <- function(x) {
    tibble::is_tibble(x) &&
      setequal(names(x), c(".index", ".q", ".tool", ".tool_width", ".color", ".alpha")) &&
      is.integer(x$.index) &&
      is.integer(x$.q) &&
      is.character(x$.tool) &&
      is.integer(x$.tool_width) &&
      is.character(x$.color) &&
      is.integer(x$.alpha) &&
      length(unique(x$.color)) == 1 &&
      !any(duplicated(x$.index))
  }

  valid_points_tbl <- function(x) {
    tibble::is_tibble(x) &&
      setequal(names(x), c(".index", ".x", ".y")) &&
      is.integer(x$.index) &&
      is.integer(x$.x) &&
      is.integer(x$.y)
  }

int_checker <- function(d) {
  # This function checks each element of the pain drawing
  # data structure (which is a list) -- each element should
  # also be a list (of named elements)

  if(verbose) {
    try({    
      message("") # Empty line
      message(paste0("Element with ID ", d$.id, ":"))
    })
  }

  # ---- top-level list ----
  if (!is.list(d)) {
    return(fail("Data is a list: FAIL -- NOTE data should not simply be a list of columns '.id', '.units', '.width', '.height', '.timestamp', '.strokes', '.points', but a list of such lists."))
  }
  ok("Data is a list: OK")

  # ---- required columns ----
  required_cols <- c(".id", ".units", ".width", ".height", ".timestamp", ".strokes", ".points")
  if (!has_names(d, required_cols)) {
    return(fail("Missing required elements"))
  }
  ok("Required elements present: OK")

  # ---- column types ----
  if (!(is.character(d$.id) &&
        is.integer(d$.width) &&
        is.integer(d$.height) &&
        is.character(d$.units) &&
        is.character(d$.timestamp) &&
        is.list(d$.strokes) &&
        is.list(d$.points))) {
    return(fail("Element types incorrect"))
  }
  ok("Element types: OK")

  # ---- unique IDs ----
  if (any(duplicated(d$.id))) {
    return(fail("IDs must be unique"))
  }
  ok("IDs unique: OK")

  # ---- list column '.strokes' (allow NA) ----
  if (!is_na_scalar(d$.strokes) && !valid_strokes_tbl(d$.strokes)) {
    return(fail("Invalid column in '.strokes'"))
  }
  ok("'s' column valid: OK")

  # ---- list column '.points' (allow NA) ----
  if (!is_na_scalar(d$.points) && !valid_points_tbl(d$.points)) {
    return(fail("Invalid column in '.points'"))
  }
  ok("'p' column valid: OK")

  # ---- identical '.index' in list column '.points' and '.strokes' 
  if (!setequal(unique(d$.points$.index), d$.strokes$.index)) {
    return(fail("Discrepancies in column '.index' in columns '.points' and '.strokes'"))
  }
  ok("Column '.index' in list columns '.poins' and '.strokes' have same values")

  ok("Data structure is valid pain drawing data structure: OK")
  TRUE
  }
  # ---- end helpers ----

  purrr::every(d, int_checker)
}

