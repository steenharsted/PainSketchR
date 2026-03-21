#' Trim spray pain drawing to tempalte
#'
#' This function trims a spray pain drawing (png file) to a template (png) file. 
#' This is useful in a number of scenarios, e.g. to eliminate spray outside the outline of the underlying background image or
#' to limit the spray pain drawing to the subset within a particular anatomical region.
#' 
#' @returns A new png spray pain drawing
#'
#' @export
#' @examples
pd_spray_trim <- function(png, template) {

  ## THIS IS UNFINISHED ... ! 

    # Should we allow for png in memory (as opposed to specifying a png file)?
  if (!is.character(png)) {
    stop("At least one png file path must be specified")
  }

  result <- tibble::tibble(file = png, R = NA, G = NA, B = NA)

  # ADD: If png_is a vector of files, read them into memory
  png_data <- list()

  for (i in seq_along(png_files)) {
    if (fs::is_file(png_files[i])) {
      png_data <- append(png_data, 
        tryCatch(
        expr = {
          png::readPNG(png_files[i])
        },
        error = \(e) {
          NA
        })
      )
    }
  }
  return(result)
}