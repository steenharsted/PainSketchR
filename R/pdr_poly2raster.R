#' Title
#'
#' @param pdr
#'
#' @returns
#'
#' @export
#' @examples
#' 
pdr_poly2raster_template <- function(
  .data, # Valid pain drawing data tibble,
  paindrawr_data = pdr_data,
  dest_folder,
  col_filenames = id,
  invert=FALSE) {
  # This function takes as its input a paindrawing data 
  # structure and creates a png raster file.
  # The idea is to input, e.g. an anatomical region like
  # 'Back_head' or 'Right_thumb_palmar' and plot this as a
  # polygon where everything inside the polygon is #000000
  # and everything outside the polygon is #ffffff (or vice
  # versa - 'invert'). This raster image can then simply be
  # multiplied with another png raster file OF THE SAME DIMs
  # to reduce that other png to retain only pain drawings
  # inside (or outside 'invert') the anatomy region. 

  # We will often need to apply the same anatomical region
  # template to several pain drawings - so this function
  # allows users to save the raster template to an external
  # file.

  
  if(method=="memory") {
    result <- list()
  }
  if(method=="file") {
    col_filenames <- match.arg(col_filenames, choices = names(.data))
    if(!is.character())
    if(!fs::is_dir(dest_folder)) {
      fs::dir_create(dest_folder)
    }
    # Do more sanity checks on file names?
  }
  
  for(i in seq(nrow(.data))) {
    p <- pdr_plot_drawing(.data[i,], type = "polygon")
    ggplot2::ggsave(
      plot = p,
      file = fs::path(
        dest_folder, 
        paste0(.data[i, {{col_filenames}}], ".png")),
      width = width_mm,
      height = height_mm,
      units = "mm",
      dpi = dpi
    )
  }
  


}