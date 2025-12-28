pd_json2df <- function(l) {
  l <- pd_json2list(l)

  # These lines converts list to a single data frame with 's' and 'p' being list
  # columns of dataframes -- this is an alternative data structure where all is
  # stored in a single data frame (with a complex 3-layer structure).
  # l <- l |> purrr::map(\(pd) { 
  #     pd$s$p <- pd$s$p |> purrr::map(\(m) {dplyr::tibble(x=m[,1], y=m[,2])})
  #     pd$s <- list(pd$s)
  #   pd }) |> dplyr:::bind_rows() 
  list(
    drawings = l |> purrr::map_dfr(\(le) {le |> purrr::discard_at("s")}),
    strokes = l |> purrr::map_dfr(\(le) {le |> purrr::keep_at(c("id", "s"))}) |> dplyr::mutate(s = s |> purrr::discard_at("p")) |> tidyr:::unnest(cols=c(s)),
    points  = l |> purrr::map_dfr(\(le) {dplyr::tibble(id=le$id, i=le$s$i, p=le$s$p)}) |> tidyr::unnest(cols=c(p)) |> dplyr::mutate(x = p[,1], y=p[,2]) |> dplyr::select(-p)
  )
}

pd_json2list <- function(i) {
  # Read json files into a list of pd data

  # if input 'i' is not a list -- it's probably a vector of file names
  # if so convert to a list - that way we manipulate that list into result
  if(!is.list(i) && is.character(i)) { i <- purrr::map(i, \(x) {x})} 

  for (j in 1:length(i)) {
    if (file.exists(i[[j]])) {
      i[[j]] <- c(file=i[[j]], jsonlite::fromJSON(i[[j]]))
    } else {
      warning(paste0("File ", i[[j]], " not found"))
    }
  }
  return(i)
}
