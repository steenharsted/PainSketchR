## code to prepare `DATASET` dataset goes here

# Generate the MiRD full body template
# This is based on csv files in folder regions_of_full_body_tempalte
result <- tibble::tibble(id=as.character(), i=as.integer(), x=as.integer(), y=as.integer())
for(c in fs::dir_ls("data-raw/regions_of_full_body_template/")) {
  d <- read.csv(c, sep=";")
  result <- rbind(result, tibble::tibble(id=fs::path_ext_remove(fs::path_file(c)), i=1, x=d[,1], y=d[,2]))
}
save(result, file="data-raw/pd_demo_body_template.rda")

# Generate a demo data set of pain drawings
# This is based on real-world data collection (mird)
pd_demo_data <- list()
pd_demo_data$points <- read.csv("data-raw/demo_data.csv") |> 
  dplyr::mutate(paindrawing_LBP = stringr::str_replace_all(paindrawing_LBP, ",\\),\\(,", ";")) |> 
  dplyr::mutate(paindrawing_LBP = stringr::str_replace_all(paindrawing_LBP, "\\(,", "")) |>
  dplyr::mutate(paindrawing_LBP = stringr::str_replace_all(paindrawing_LBP, ",\\)", "")) |>
  dplyr::mutate(paindrawing_LBP = stringr::str_split(paindrawing_LBP, ";")) |> 
  dplyr::mutate(paindrawing_LBP = paindrawing_LBP |> purrr::map(\(x) {
    stringr::str_split(x,",") |> purrr::imap_dfr(\(xy,i) {
      as.integer(xy) |> matrix(ncol=2, byrow=TRUE) |> as.data.frame() |> purrr::set_names(c("x","y")) |> dplyr::mutate(i=i)  
    })
  })) |> 
  tidyr::unnest(paindrawing_LBP) |> dplyr::select(-X)
pd_demo_data$strokes <- pd_demo_data$points |> distinct(id,i)
pd_demo_data$drawings <- pd_demo_data$points |> distinct(id)
save(pd_demo_data, file="data-raw/pd_demo_data.rda")
