## code to prepare `DATASET` dataset goes here

# Generate some demo data of anatomical regions from full body outline/template
pd_d <- tibble::tibble(id=as.integer(), s=list(), p=list())
for(c in fs::dir_ls("data-raw/regions_of_full_body_template/")) {
  d <- read.csv(c, sep=";", header = FALSE)
  pd_d <- rbind(pd_d, tibble::tibble(id=fs::path_ext_remove(fs::path_file(c)), 
                         w=450,
                         h=500,
                         s=list(tibble::tibble(i=1)), 
                         p=list(tibble::tibble(i=1, x=d[,1], y=d[,2]))))
}  # This is a bit quirky ... but necessary to ensure the variable is name correctly in rda:
  var_name <- "pd_demo_anatomy"
  assign(var_name, pd_d)
  do.call(save, list(var_name, file="data/pd_demo_anatomy.rda"))

# Generate png stencils for each anatomical region (to allow for finding intersections in spray pain drawings)
pd_d$p |> purrr::map(\(x) {})

# Generate a demo data set of pain drawings
# This is based on real-world data collection (mird)
# With an added observation where paindrawing is NA
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

pd_demo_data <- rbind(tibble::tibble(id="No pain drawing", s=list(NA), p=list(NA)), pd_demo_data)

save(pd_demo_data, file="data/pd_demo_data.rda")
  
