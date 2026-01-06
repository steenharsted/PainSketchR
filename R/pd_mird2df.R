pd_mird2pd <- function(files) {  
  # if(!is.list(files) && is.character(files)) { files <- purrr::map(files, \(x) {x})} 

  # for (j in 1:length(files)) {
  #   if (file.exists(files[[j]])) {
  #     files[[j]] <- c(file=files[[j]], read.csv2(files[[j]]))
  #   } else {
  #     warning(paste0("File ", files[[j]], " not found"))
  #   }
  # }

  # files <- files |> purrr::map_dfr(\(x) {x |> purrr::map_dfr(\(z) {z})}) 

  # # Get rid of NA values...
  # files <- files |> dplyr::filter(!is.na(KEY) & !is.na(paindrawing_LBP)) |> str()

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