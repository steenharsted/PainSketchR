pd_poly_clip <- function(A, B, operation="intersection") {
  # A and B should be dataframes which contain two columns x and y
  result <- polyclip::polyclip(
    A = list(x=A$x , y=A$y),
    B = list(x=B$x , y=B$y),
    op=operation) 
  if(!purrr::is_empty(result)) {
      result <- result |> purrr::map_dfr(\(q) {q}, .id="i") |>
      dplyr::mutate(i = as.integer(i),
                    x = as.integer(round(x)),
                    y = as.integer(round(y))) 
  } else {
    result <- tibble::tibble()
  }
  result
}

# intersection <- polyclip::polyclip(
#         A=list(
#           x = df |> dplyr::filter(i==i_strokes[p1]) |> dplyr::pull(x),
#           y = df |> dplyr::filter(i==i_strokes[p1]) |> dplyr::pull(y)
#         ),
#         B=list(
#           x = df |> dplyr::filter(i==i_strokes[p2]) |> dplyr::pull(x),
#           y = df |> dplyr::filter(i==i_strokes[p2]) |> dplyr::pull(y)
#         ), op="intersection")