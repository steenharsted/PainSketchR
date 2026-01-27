pd_poly_clip <- function(A, B, operation="intersection") {
  # A and B should be dataframes which contain two columns x and y
  result <- polyclip::polyclip(
    A = list(x=A$x , y=A$y),
    B = list(x=B$x , y=B$y),
    op=operation) 
  
  if(purrr::is_empty(result)) {
    result <- tibble::tibble(i=as.integer(), x=as.integer(), y=as.integer())
  } else {
      result <- result |> purrr::map_dfr(\(q) {q}, .id="i") |>
      dplyr::mutate(i = as.integer(i),
                    x = as.integer(round(x)),
                    y = as.integer(round(y))) 
  }
  result
}
