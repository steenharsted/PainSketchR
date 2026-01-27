#' Generate an `s` column of stroke information (just `i` indeces) from the `p` column.
#'
#' @param p A list column where each element is a data frame with an `i` column
#'
#' @returns A list column where each element is a data frame with the unique `i` values from the input
#'
#' @export
#' @examples
#' pd_df <- tibble::tibble(id=c("A", "B", "C"), mird_string = c(NA, "(,293,201,300,202,303,202,305,202,308,202,),(,198,104,206,103,203,100,209,108,202,109,)", "(,335,215,336,215,),(,331,211,331,212,331,213,331,214,),(,341,263,),(,341,272,),(,341,287,)"))
#' pd_df <- pd_df |> dplyr::mutate(p = pd_mird2pd(mird_string, flip_y=TRUE))
#' pd_df <- pd_df |> dplyr::mutate(s = pd_mird_add_s(p))
pd_mird_add_s <- function(p) {
  # p should be a list of tibbles (of x,y,i)
  # This function should be vectorized and return a list of the same length, of data frames
  p |> purrr::map(\(df, i) {    
    # Check sanity of each points data frame?
    if(!tibble::is_tibble(df)) {
      NA
    } else {
      tibble::tibble(i=unique(df$i))
    }
  })
}