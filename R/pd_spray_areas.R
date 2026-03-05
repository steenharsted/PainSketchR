#' Calculate area of pain drawing created with spraying
#'
#' Area is calculated on the basis of colour and not on a per-stroke basis.
#' To calculate areas on a per-stroke basis, the pain drawing should be split into
#' separate drawings for each stroke.
#'
#' @param png_files A vector of valid file paths to a pain drawing png files
#'
#' @returns A tibble with columns `R`, `G` and `B` and a row for each element of the `png_files`. Cells hold the calculated area.
#'
#' @export
#' @examples
#' pd_spray_areas()
#'
pd_spray_areas <- function(png_files, alpha_multiply = TRUE) {
  if (!is.character(png_files)) {
    stop("One of more png file paths (character) must be specified")
  }

  result <- tibble::tibble(file = png_files, R = NA, G = NA, B = NA)

  for (i in seq_along(png_files)) {
    if (fs::is_file(png_files[i])) {
      png_data <- tryCatch(
        expr = {
          png::readPNG(png_files[i])
        },
        error = \(e) {
          NA
        }
      )
      if (is.array(png_data)) {
        if (alpha_multiply) {
          result[, 'R'] <- sum(png_data[,, 1] * png_data[,, 4])
          result[, 'G'] <- sum(png_data[,, 2] * png_data[,, 4])
          result[, 'B'] <- sum(png_data[,, 3] * png_data[,, 4])
        } else {
          result[, 'R'] <- sum(png_data[,, 1])
          result[, 'G'] <- sum(png_data[,, 2])
          result[, 'B'] <- sum(png_data[,, 3])
        }
      }
    }
  }
  return(result)
}
