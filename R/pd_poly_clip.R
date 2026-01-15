#' Wrapper for polyclip::polyclip
#'
#' @param df_a A dataframe/tibble to pass to the A parameter of polyclip
#' @param df_b A dataframe/tibble to pass to the B parameter of polyclip
#' @param operation A string to pass to the op parameter of polyclip
#'
#' @returns A dataframe of polygon definitions. Column `i` is an identifier <int> of the polygons. Columns `x` and `y` are coordinates.
#'
#' @export
#' @examples
#' 
#' 
pd_poly_clip <- function(A, B, operation="intersection") {
  # A and B should be dataframes which contain two columns x and y
  
  result <- polyclip::polyclip(
    A = list(x=A$x , y=A$y),
    B = list(x=B$x , y=B$y),
    op=operation) 
  if(!purrr::is_empty(result)) {
      result <- result |> purrr::map_dfr(\(x) {x}, .id="i") |>
      dplyr::mutate(i = as.integer(i),
                    x = as.integer(x),
                    y = as.integer(y)) 
  } else {
    result <- tibble::tibble()
  }
  result
  }