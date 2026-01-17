#' Generate a valid paindrawing data structure from a MiRD paindrawing string
#'
#' @returns A valid paindrawing data structure
#'
#' @export
#' @examples
#' mird_string <- "(,293,201,300,202,303,202,305,202,308,202,),(,198,104,206,103,203,100,209,108,202,109,)"
#' pd_mird2pd(mird_string, id="New id", flip_y=TRUE)
pd_mird2pd <- function(mird_string, id="*", flip_y=TRUE) {
  if(id=="*") {id=paste0(sample(letters[1:26],10,replace=TRUE))} # If no id is given, just random text

  mird_string <- stringr::str_replace_all(mird_string, ",\\),\\(,", ";")
  mird_string <- stringr::str_replace_all(mird_string, "\\(,", "")
  mird_string <- stringr::str_replace_all(mird_string, ",\\)", "")
  points <- stringr::str_split(mird_string, ";", simplify=TRUE)
  points <- stringr::str_split(points, ",")
  points <- points |> purrr::imap_dfr(\(xy,i) {
    xy <- as.integer(xy) |> matrix(ncol=2, byrow=TRUE) |> tibble::as_tibble(.name_repair = ~c("x","y")) 
    xy$i <- i
    xy
  })
 
  # MiRD drawings have origo at top left corner -- gplot at lower let corner
  if(flip_y) {points$y <- 500-points$y}

  return(tibble::tibble(id=id, s=list(tibble::tibble(i=unique(points$i))), p=list(points)))
}