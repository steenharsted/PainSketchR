pd_mird2pd <- function(f_name) {  
  # Sanity check -- is f_name a string
  if(!is.character(f_name)) {return(data.frame())}
  # Sanity check -- more than one filename?
  if(length(f_name)>1) {
    result <- tibble::tibble()
    for(f in f_name) {
      result <- dplyr::bind_rows(result, pd_mird2pd(f))
    }
    return(result)
  } 
  # Sanity check -- does such a file exist?
  if(!file.exists(f_name)) {return(data.frame())}
  
  # Get the file content as a simple string and check whether it is valid pd data
  content <- read

  # })
  # # regexp explanation:
  # #   (?<=\\(,)                   = preceded by (,
  # #   ([:digit:]+,[:digit:]+)+    = 1 or more of : 1+ digits , 1+ digits
  # #   (?=,\\))                    = followed by ,)

  # # Return a list with an element for each string in st which is a vector of strings
  # # Each element in the result is a data frame with three columns: stroke, x, y

  # result <- list()
  # result$drawings <- files |> dplyr::select(Id = KEY)


  # ids <- df |> dplyr::pull( {{.id}} )
  # df |>
  #   dplyr::pull({{ .st }}) |>
  #   purrr::imap(\(pd,i) {
  #     if (is.na(pd)) {
  #       data.frame()
  #     } else if (pd=="") {
  #       data.frame()
  #     } else {
  #       pd |>
  #         stringr::str_extract_all("(?<=\\(,)([:digit:]+,[:digit:]+)+(?=,\\))") |>
  #         purrr::map(\(x) string_r::str_split(x,",")) |> # now a list of lists of char vectors
  #         purrr::map_depth(2, \(df) {as.integer(df) |> matrix(ncol=2, byrow=TRUE) |> as.data.frame() |> purrr::set_names(c("x","y")) |> dplyr::mutate(i=i)} ) |>
  #         purrr::map(\(x) {purrr::list_rbind(x, names_to = "stroke")}) |>
  #         purrr::list_rbind()
  #     }
  #   }) |>
  #   purrr::list_rbind() |>
  #   dplyr::mutate(i = ids[i] ) |>
  #   dplyr::rename(id = i) |>
  #   dplyr::relocate(id, .before=1) 
  #   #mutate() stroke and id to factors


  ## POSSIBLY REPLACE WITH
#   pd<- list()
# pd$points <- complete_data |> select(NEW_KEY, paindrawing_LBP) |> 
#   mutate(paindrawing_LBP = str_replace_all(paindrawing_LBP, ",\\),\\(,", ";")) |> 
#   mutate(paindrawing_LBP = str_replace_all(paindrawing_LBP, "\\(,", "")) |>
#   mutate(paindrawing_LBP = str_replace_all(paindrawing_LBP, ",\\)", "")) |>
#   mutate(paindrawing_LBP = str_split(paindrawing_LBP, ";")) |> 
#   mutate(paindrawing_LBP = paindrawing_LBP |> map(\(x) {
#     str_split(x,",") |> imap_dfr(\(xy,i) {
#       as.integer(xy) |> matrix(ncol=2, byrow=TRUE) |> as.data.frame() |> set_names(c("x","y")) |> mutate(i=i)  
#     })
#   })) |> 
#   rename(id = NEW_KEY) |> 
#   unnest(paindrawing_LBP) 

# pd$strokes <- pd$points |> distinct(id,i)
# pd$drawings <- pd$points |> distinct(id)

# saveRDS(pd, file=here("clean_data", "pd_data.RDS"))
}