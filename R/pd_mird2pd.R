#' Generate a valid paindrawing data structure from a MiRD paindrawing string
#'
#' @param mird -- A data frame which must contain columns `id` (character) and `pd` (character), which represent a unique id and coordinates for each pain drawing.
#' @param flip_y -- TRUE (default) or FALSE -- should data be y-axis flipped to correspond to the MiRD pain drawing canvas
#' @param simplify -- If TRUE function will return a simple data frame. If FALSE (default) will return a valid pain drawing data structure -- see [pd_check_data].
#'
#' @returns A valid paindrawing data structure
#'
#' @export
#' @examples
#' pd_df <- tibble::tibble(id=c("A", "B", "C"), mird_string = c(NA, "(,293,201,300,202,303,202,305,202,308,202,),(,198,104,206,103,203,100,209,108,202,109,)", "(,335,215,336,215,),(,331,211,331,212,331,213,331,214,),(,341,263,),(,341,272,),(,341,287,)"))
#' pd_df <- pd_df |> dplyr::mutate(p = pd_mird2pd(mird_string, flip_y=TRUE))
pd_mird2pd <- function(mird_string, flip_y=TRUE) {
  # This function should be vectorized and return a list of data frames
  result <- mird_string |> purrr::map(\(ms, i) {    
    # Check parameter sanity
    if(!is.na(ms) && is.character(ms) && stringr::str_length(ms)>6) { # Absolute minimum mird string with data: "(,0,0,)"
      ms <- stringr::str_replace_all(ms, ",\\),\\(,", ";")
      ms <- stringr::str_replace_all(ms, "\\(,", "")
      ms <- stringr::str_replace_all(ms, ",\\)", "")
      points <- stringr::str_split(ms, ";", simplify=TRUE)
      points <- stringr::str_split(points, ",")
      # Check for even number of points?
      points <- points |> purrr::imap_dfr(\(xy,i) {
        xy <- as.integer(xy) |> matrix(ncol=2, byrow=TRUE) |> tibble::as_tibble(.name_repair = ~c("x","y")) 
        xy$i <- as.integer(i)
        xy
      })    
      # MiRD drawings have origo at top left corner -- ggplot at lower left corner
      if(flip_y) {points$y <- as.integer(500-points$y)}
      points # End map iteration here
    } else {
      #warning("Empty mird string detected.")
      NA
      #tibble::tibble(x=as.integer(), y=as.integer(), i=as.integer()) # End map iteration here
    }
  })
  return(result)
}