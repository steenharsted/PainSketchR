#' Validate a pain drawing data structure
#'
#' A pain drawing data structure must comply with certain criteria -- see Details below. This function returns TRUE or FALSE and optionally emits diagnostic messages.
#'
#' #' @details
#' A valid pain drawing data structure must satisfy all of the following:
#'
#' **Top-level structure**
#' * A tibble with one row per pain drawing
#' * Must contain columns: `id`, `coord`, `w`, `h`, `ts`, `s`, `p`
#'
#' **Column types**
#' * `id`: character (unique identifier)
#' * `w`, `h`: integer
#' * `coord`, `ts`: character
#' * `s`, `p`: list columns
#'
#' **Column constraints**
#' * `id` values must be unique
#'
#' **`s` column (stroke metadata)**
#' Each element must be either `NA` or a tibble with:
#' * Columns: `i`, `q`, `t`, `bw`, `c`, `a`
#' * Types: integer, integer, character, integer, character, integer
#' * One row per stroke (`i` must be unique)
#' * Exactly one unique value of `c` per tibble
#'
#' **`p` column (stroke coordinates)**
#' Each element must be either `NA` or a tibble with:
#' * Columns: `i`, `x`, `y`
#' * All integer
#' * `i` values correspond to those in the matching `s` tibble
#' 
#' #' **Relationship between `s` and `p`**
#' * Each row represents one pain drawing
#' * The `s` tibble defines strokes (one row per stroke)
#' * The `p` tibble defines coordinates (multiple rows per stroke)
#' * The `i` column links them:
#'   - `s$i`: unique stroke identifiers
#'   - `p$i`: repeats to associate coordinates with a stroke
#' 
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
#' pd_check_data(pd_demo_data, verbose = FALSE) # Will return TRUE
#'
#' pd_check_data(letters[1:10]) # Will return FALSE and provide details in stderr
#'
pd_check_data <- function(d, verbose = TRUE) {

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

  is_tibble_or_na <- function(x) {
    tibble::is_tibble(x) || is_na_scalar(x)
  }

  has_names <- function(x, required) {
    all(required %in% names(x))
  }

  # ---- validators for nested tibbles ----
  valid_s_tbl <- function(x) {
    tibble::is_tibble(x) &&
      has_names(x, c("i", "q", "t", "bw", "c", "a")) &&
      is.integer(x$i) &&
      is.integer(x$q) &&
      is.character(x$t) &&
      is.integer(x$bw) &&
      is.character(x$c) &&
      is.integer(x$a) &&
      length(unique(x$c)) == 1 &&
      !any(duplicated(x$i))
  }

  valid_p_tbl <- function(x) {
    tibble::is_tibble(x) &&
      has_names(x, c("i", "x", "y")) &&
      is.integer(x$i) &&
      is.integer(x$x) &&
      is.integer(x$y)
  }

  # ---- top-level tibble ----
  if (!tibble::is_tibble(d)) {
    return(fail("Data is a tibble: FAIL"))
  }
  ok("Data is a tibble: OK")

  # ---- required columns ----
  required_cols <- c("id", "coord", "w", "h", "ts", "s", "p")
  if (!has_names(d, required_cols)) {
    return(fail("Missing required columns"))
  }
  ok("Required columns present: OK")

  # ---- column types ----
  if (!(is.character(d$id) &&
        is.integer(d$w) &&
        is.integer(d$h) &&
        is.character(d$coord) &&
        is.character(d$ts) &&
        is.list(d$s) &&
        is.list(d$p))) {
    return(fail("Column types incorrect"))
  }
  ok("Column types: OK")

  # ---- unique IDs ----
  if (any(duplicated(d$id))) {
    return(fail("IDs must be unique"))
  }
  ok("IDs unique: OK")

  # ---- list column 's' (allow NA) ----
  if (!purrr::every(d$s, ~ is_na_scalar(.x) || valid_s_tbl(.x))) {
    return(fail("Invalid elements in 's' column"))
  }
  ok("'s' column valid: OK")

  # ---- list column 'p' (allow NA) ----
  if (!purrr::every(d$p, ~ is_na_scalar(.x) || valid_p_tbl(.x))) {
    return(fail("Invalid elements in 'p' column"))
  }
  ok("'p' column valid: OK")

  # ---- identical 'i' in list column 'p' and 's' 
  if (!all(purrr::map2_lgl(d$p, d$s, ~ setequal(unique(.x$i), .y$i)))) {
    return(fail("Discrepancies in column 'i' in columns 'p' and 's'"))
  }
  ok("Column 'i' in list columns 'p' and 's' have same values")

  ok("Data structure is valid pain drawing data structure: OK")
  TRUE
}

