## code to prepare `DATASET` dataset goes here

# Generate some demo data of anatomical regions from full body outline/template
for(c in fs::dir_ls("data-raw/regions_of_full_body_template/")) {
  d <- read.csv(c, sep=";", header = FALSE)
  pd_d <- tibble::tibble(id=fs::path_ext_remove(fs::path_file(c)), 
                         s=list(tibble::tibble(i=1)), 
                         p=list(tibble::tibble(x=d[,1], y=d[,2])))
  # This is a bit quirky ... but necessary to ensure the variable is name correctly in rda:
  var_name <- paste0("pd_",fs::path_ext_remove(fs::path_file(c)))
  assign(var_name, pd_d)
  do.call(save, list(var_name, file=paste0("data/pd_",fs::path_ext_remove(fs::path_file(c)),".rda")))
}

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
  