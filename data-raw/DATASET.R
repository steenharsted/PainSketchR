## code to prepare `DATASET` dataset goes here

# Generate the MiRD full body template
# This is based on csv files in folder regions_of_full_body_tempalte
pd_demo_body_template <- tibble::tibble(id=as.character(), i=as.integer(), x=as.integer(), y=as.integer())
for(c in fs::dir_ls("data-raw/regions_of_full_body_template/")) {
  d <- read.csv(c, sep=";", header = FALSE)
  pd_demo_body_template <- rbind(pd_demo_body_template, tibble::tibble(id=fs::path_ext_remove(fs::path_file(c)), i=1, x=d[,1], y=d[,2]))
}
pd_demo_body_template <- pd_demo_body_template |> dplyr::mutate(i=as.integer(i), x=as.integer(x), y=as.integer(y))
pd_demo_body_template <- pd_demo_body_template |> 
  dplyr::group_by(id) |>
  dplyr::group_modify(\(d,indx) {
    d <- tibble::tibble(
      s = list(tibble::tibble(i=unique(d$i))),
      p = list(tibble::tibble(i=d$i, x=d$x, y=d$y))
    )
  }) |>
  dplyr::ungroup() 

save(pd_demo_body_template, file="data-raw/pd_demo_body_template.rda")

pd_demo_anatomy_lower_back <- pd_poly_manage_overlaps(
  pd_demo_body_template |> dplyr::filter(id=="Mid_back_bottom"),
  pd_demo_body_template |> dplyr::filter(id=="Back_right_buttock")
)


# Generate a demo data set of pain drawings
# This is based on real-world data collection (mird)
pd_demo_data <- read.csv("data-raw/demo_data.csv") |> 
  dplyr::mutate(paindrawing_LBP = stringr::str_replace_all(paindrawing_LBP, ",\\),\\(,", ";")) |> 
  dplyr::mutate(paindrawing_LBP = stringr::str_replace_all(paindrawing_LBP, "\\(,", "")) |>
  dplyr::mutate(paindrawing_LBP = stringr::str_replace_all(paindrawing_LBP, ",\\)", "")) |>
  dplyr::mutate(paindrawing_LBP = stringr::str_split(paindrawing_LBP, ";")) |> 
  dplyr::mutate(paindrawing_LBP = paindrawing_LBP |> purrr::map(\(x) {
    stringr::str_split(x,",") |> purrr::imap_dfr(\(xy,i) {
      as.integer(xy) |> matrix(ncol=2, byrow=TRUE) |> as.data.frame() |> purrr::set_names(c("x","y")) |> dplyr::mutate(i=i)  
    })
  })) |> 
  tidyr::unnest(paindrawing_LBP) |> dplyr::select(-X) |> dplyr::relocate(i, .after=id) |>
  dplyr::group_by(id) |>
  dplyr::group_modify(\(d, indx) {
    d <- tibble::tibble(
      s = list(tibble::tibble(i=unique(d$i))),
      p = list(tibble::tibble(i=d$i, x=d$x, y=d$y))
    )
  }) |>
  dplyr::ungroup() 

save(pd_demo_data, file="data-raw/pd_demo_data.rda")
  