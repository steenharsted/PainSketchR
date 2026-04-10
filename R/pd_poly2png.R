#' Generate png templates from pain drawing polygon data
#' 
#' These are useful for generating png templates of anatomical regions which
#' can be superimposed by matrix multiplication onto spray pain drawings, thus
#' getting the intersection of spray pain drawings and anatomical regions
#'
#' @returns
#'
#' @export
#' @examples
pd_poly2png <- function(pd, to_file=TRUE, filenames = "id", path = "./", width=-1, height=-1, by="id") {
  # We must have a canvas size for each plot, either from pd$w pd$h or as parameter
  if (height==-1 & !("h" %in% names(ds))) { stop("Canvas height not specfied in function call, and 'h' col not in pain drawing data")}
  if (width==-1 & !("w" %in% names(ds))) { stop("Canvas width not specfied in function call, and 'w' col not in pain drawing data")}
  if (height>0 && width>0) {
    width = rep(width, nrow(pd))
    height = rep(height, nrow(height))
  } else {
    stop("Invalid canvas dimensions")
  }
  # We must have a filename for each row of pd, either from pd$filename or from a vector of same length
  if (filenames=="id") {pd$filename <- fs::path(path, pd$id)}
  if (filenames!="id" & length(failname != nrow(pd))) stop("The 'filenames' parameter should be a vector of same length as the column 'p', or set to 'id'")
  if (is.character(filenames) & length(filenames)==nrow(pd)) {pd$filename <- filenames}
  # Currently, we can only implement this function using external files (not in RAM)
  if(!to_file) {stop("The 'not to file' (i.e. straight to RAM) is not yet implemented")}
  # We can generate a png file for each stroke (i) or for each pain drawing (id)
  if(by!="id" | by!="i") {stop("The 'by' parameter must be either 'id' or 'i'")}

  pd |> 
    dplyr::ungroup() |>
    dplyr::group_by(dplyr::row_number()) |>
    dplyr::group_walk(\(row) {
      if (by=="id") {
        # Is ggplot really the best way to plot polygons in all white to black background?
        ggplot2::ggplot(row$p[[1]], aes(x,y,group=i, fill="white")) + ggplot2::geom_polygon() |>
          ggplot2::ggsave(filename = row$filename)
      }
    })
}