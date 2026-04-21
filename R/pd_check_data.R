#' Check whether an R object is a valid pain drawing data structure
#'
#' A pain drawing data structure must comply with certain criteria -- see Details below. This function return TRUE or FALSE and helpful warnings as a byproduct.
#'
#' @details
#'
#' This function will check whether an R object fulfills the following criteria and provide some feedback if it does not and `verbose` is `TRUE`.
#'
#' For an R object to be a valid pain drawing data object:
#'
#' * it must be a tibble where each row represents a pain drawing
#' * it must have named columns: `id`, `coord`, `w`, `h`, `ts`, `s`, and `p` -- users are free to add additional columns.
#' * the columns `id`, `w` and `h` must be of type integer (not nummeric)
#' * the column `coord` and `ts` must be of type character
#' * the columns `s` and `p` must be list columns of the same length as number of rows in the pain drawing data structure. Each list element:
#'     - must be of type tibble
#'     - the `s` tibbles must contain columns `i`, `q`, `t`, `bw`, `c`, `a`, which are int int chr int chr int
#'     - the `p` tibble must contain columns `i`, `x`, `y`, which are int int int
#'
#' The `id` column of the data object must represent a unique identifier for each pain drawing.
#'
#' The `i` column in each of the `s` tibbles must represent an index number for each stroke or marking, in that pain drawing. Thus, the `s` tibbles should contain exactly one row for each stroke/marking with a unique `i`.
#'
#' The `i` column in each of the `p` tibbles will be repeated for each coordinate x,y pair in each stroke/marking and should correspond to the `i` values of the `s` list column element of that row.
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
#' pd_check_data(letters[1:10]) # Will return FALSE and provide details in stderr
#'
pd_check_data <- function(d, verbose = TRUE) {
  ok <- TRUE

  ##### CHECK DATA FRAME #####

  # pd data is a tibble
  if (tibble::is_tibble(d)) {
    if (verbose) {
      message("Data is a tibble: OK")
    }
  } else {
    warning("Data is a tibble: FAIL")
    ok <- FALSE
  }

  # pd data has expected column names
  if (all(c("id", "coord", "w", "h", "coord", "ts", "s", "p") %in% names(d))) {
    if (verbose) {
      message(
        "Data has columns 'id', 'w', 'h', 'coord', 'ts', 'p', and 's': OK"
      )
    }
  } else {
    warning(
      "Data has columns 'id', 'w', 'h', 'coord', 'ts', 'p', and 's': FAIL"
    )
    ok <- FALSE
  }

  ##### CHECK COLUMNS IN DATA FRAME #####

  # pd data columns has expected type ( Should we also check for 'f', 'v' and 'app'? )
  if (
    is.character(d$id) &&
      is.integer(d$w) &&
      is.integer(d$h) &&
      is.character(d$coord) &&
      is.character(d$ts) &&
      is.list(d$s) &&
      is.list(d$p)
  ) {
    if (verbose) {
      message(
        "Data columns 'id', 'w', 'h', 'coord', 'ts', 's' and 'p' are <chr>, <int>, <int>, <chr>, <chr>, <list> and <list>: OK"
      )
    }
  } else {
    warning(
      "Data columns 'id', 'w', 'h', 'coord', 'ts', 's' and 'p' are <chr>, <int>, <int>, <chr>, <chr>, <list> and <list>: FAIL"
    )
    ok <- FALSE
  }

  # Check for duplicated pd id's
  # EXAMPLE FAILS HERE - WE NEED TO TEST IF d$id exists before testing duplicates
  # I think these tests only makes sense if # pd data has expected column names is TRUE
  if (any(duplicated(d$id))) {
    warning("All elements of 'id' are unique: FAIL")
    ok <- FALSE
  } else {
    if (verbose) {
      message("All elements of 'id' are unique: OK")
    }
  }

  # Check all elements in the 's' column are tibbles
  if (purrr::every(d$s, tibble::is_tibble)) {
    # WHAT SHOULD WE DO WITH NA's?
    if (verbose) {
      message("All elements of 's' are tibbles: OK")
    }
  } else {
    warning("All elements of 's' are tibbles: FAIL")
    ok <- FALSE
  }

  # Check all elements in the 'p' column are tibbles
  if (purrr::every(d$p, tibble::is_tibble)) {
    # WHAT SHOULD WE DO WITH NA's?
    if (verbose) {
      message("All elements of 'p' are tibbles: OK")
    }
  } else {
    warning("All elements of 'p' are tibbles: FAIL")
    ok <- FALSE
  }

  ##### CHECK COLUMNS IN TIBBLES IN LIST-COL OF DATA FRAME #####

  # Check that all tibbles in the list-column 's' have expected names
  if (
    d$s |>
      purrr::every(\(x) {
        # x is a list element of d$s, i.e a tibble
        all(c("i", "q", "t", "bw", "c", "a") %in% names(x))
      })
  ) {
    if (verbose) {
      message(
        "All tibbles in list-col 's' have columns 'i', 'q', 't', 'bw', 'c', 'a': OK"
      )
    }
  } else {
    warning(
      "All tibbles in list-col 's' have columns 'i', 'q', 't', 'bw', 'c', 'a': FAIL"
    )
    ok <- FALSE
  }

  # Check that all tibbles in the list-column 'p' have expected names
  if (
    d$p |>
      purrr::every(\(x) {
        # x is a list element of d$s, i.e a tibble
        all(c("i", "x", "y") %in% names(x))
      })
  ) {
    if (verbose) {
      message("All tibbles in list-col 'p' have columns 'i', 'x' and 'y': OK")
    }
  } else {
    warning("All tibbles in list-col 'p' have columns 'i', 'x' and 'y': FAIL")
    ok <- FALSE
  }

  # Check that all tibble columns in the list-column 's' have expected types
  if (
    d$s |>
      purrr::every(\(x) {
        # x is a list element of d$s, i.e a tibble
        is.integer(c(x$i, x$q, x$bw, x$a)) &
          is.character(c(x$t, x$c))
      })
  ) {
    if (verbose) {
      message(
        "All tibbles in list-col 's' have columns 'i', 'q', 't', 'bw', 'c', 'a', which are <int> <int> <chr> <int> <chr> <int>: OK"
      )
    }
  } else {
    warning(
      "All tibbles in list-col 's' have columns 'i', 'q', 't', 'bw', 'c', 'a', which are <int> <int> <chr> <int> <chr> <int>: FAIL"
    )
    ok <- FALSE
  }

  # Check that all tibble columns in the list-column 'p' have expected types
  if (
    d$p |>
      purrr::every(\(z) {
        # z is a list element of d$s, i.e a tibble
        is.integer(c(z$i, z$x, z$y))
      })
  ) {
    if (verbose) {
      message(
        "All tibbles in list-col 'p' have columns 'i', 'x', and 'y', which are <int> <int> <int>: OK"
      )
    }
  } else {
    warning(
      "All tibbles in list-col 'p' have columns 'i', 'x', and 'y', which are <int> <int> <int>: FAIL"
    )
    ok <- FALSE
  }

  # Check that all colors specified in 'c' column of each tibble in list-col 's' are the same
  if (
    d$s |>
      purrr::every(\(z) {
        # z is a list element of d$s, i.e a tibble
        length(unique(z$c)) == 1
      })
  ) {
    if (verbose) {
      message(
        "All tibbles in list-col 's' have a column 'c', which holds only a single color specification for each tibble: OK"
      )
    }
  } else {
    warning(
      "All tibbles in list-col 's' have a column 'c', which holds only a single color specification for each tibble: FAIL"
    )
    ok <- FALSE
  }

  # Check that none of the id's specified in 'i' column of each tibble in list-col 's' are the same
  if (
    d$s |>
      purrr::every(\(z) {
        # z is a list element of d$s, i.e a tibble
        all(!duplicated(z$i))
      })
  ) {
    if (verbose) {
      message(
        "All tibbles in list-col 's' have a column 'i', which are all unique for each tibble: OK"
      )
    }
  } else {
    warning(
      "All tibbles in list-col 's' have a column 'i', which are all unique for each tibble: FAIL"
    )
    ok <- FALSE
  }

  if (ok) {
    if (verbose) {
      message("Data structure is valid pain drawing data structure: OK")
    }
  } else {
    warning("Data structure is valid pain drawing data structure: FAIL")
  }

  return(ok)
}
