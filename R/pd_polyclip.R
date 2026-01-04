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
#' pd_polyclip(my_paindrawing$points |> filter(i==1), my_paindrawing$points |> filter(i==2), op="union")
#' 
#' 
pd_polyclip <- function(A, B, operation="intersection") {
  # A and B should contain two columns x and y
  
  polyclip::polyclip(
    A = list(x=A$x , y=A$y),
    B = list(x=B$x , y=B$y),
    op=operation) |>
      purrr::map_dfr(\(x) {x}, .id="i") |>
      dplyr::mutate(i = as.integer(i),
                    x = as.integer(x),
                    y = as.integer(y)) 
  
}