pdr_example_file <- function(path = NULL) {
  # This function provides users with easy access to example
  # data stored in the inst/extdata folder
  if (is.null(path)) {
    dir(system.file("extdata", package = "paindrawr"))
  } else {
    system.file("extdata", path, package = "paindrawr", mustWork = TRUE)
  }
}