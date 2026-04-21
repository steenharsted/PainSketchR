## code to prepare the demo `DATASET` goes here

# Generate some demo data of anatomical regions from full body outline/template
pd_d <- tibble::tibble(id=as.integer(), s=list(), p=list())
for(c in fs::dir_ls("data-raw/regions_of_full_body_template/")) {
  d <- read.csv(c, sep=";", header = FALSE)
  pd_d <- rbind(pd_d, tibble::tibble(
    id=fs::path_ext_remove(fs::path_file(c)), 
    w=as.integer(450),
    h=as.integer(500),
    coord="px",
    ts=as.character(Sys.time()),
    app="PainDraweR package",
    s=list(tibble::tibble(i=as.integer(1), q=as.integer(1), t="pen", bw=as.integer(1), c="#FF0000", a=as.integer(256))), 
    p=list(tibble::tibble(i=as.integer(1), x=d[,1], y=d[,2]))))
} 

# This is a bit quirky ... but necessary to ensure the variable is named correctly in rda:
var_name <- "pd_demo_anatomy"
assign(var_name, pd_d)
do.call(save, list(var_name, file="data/pd_demo_anatomy.rda"))

# Generate png stencils for each anatomical region (to allow for finding intersections in spray pain drawings)
# pd_d$p |> purrr::map(\(x) {})

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

pd_demo_data <- pd_demo_data |>
  dplyr::mutate(f="data/pd_demo_data.csv") |>
  dplyr::mutate(v=as.integer(2)) |>
  dplyr::mutate(w=as.integer(450), h=as.integer(500)) |>
  dplyr::mutate(coord="px") |>
  dplyr::mutate(ts = as.character(Sys.time())) |>
  dplyr::mutate(app = "PainDrawR package") 
  
pd_demo_data$s <- pd_demo_data$s |> purrr::map(\(tb) {
  if(tibble::is_tibble(tb)) {
    tb |> mutate(
      q=i,
      t="pen",
      bw=as.integer(1),
      c="#FF0000",
      a=as.integer(128)
    )
  } else {
    tb
  }
})

#pd_demo_data <- rbind(tibble::tibble(id="No pain drawing", s=list(NA), p=list(NA), f="", v=as.integer(2), w=as.integer(0), h=as.integer(0), coord="", ts=as.character(Sys.time()), app="PainDraweR package"), pd_demo_data)

save(pd_demo_data, file="data/pd_demo_data.rda")
  

# Generate to simple text strings, which represent file paths to json files in the package
pd_demo_filepath_json_file_1 <- system.file("extdata", "four_geoms.json", package = "paindrawings")
save(pd_demo_filepath_json_file_1, file="data/pd_demo_filepath_json_file_1.rda")
pd_demo_filepath_json_file_2 <- system.file("extdata", "two_geoms.json", package = "paindrawings")
save(pd_demo_filepath_json_file_2, file="data/pd_demo_filepath_json_file_2.rda")

pd_demo_filepath_background_image_body <- system.file("extdata", "mird_body_background.png", package = "paindrawings")
save(pd_demo_filepath_background_image_body, file="data/pd_demo_filepath_background_image_body.rda")
pd_demo_filepath_background_image_head <- system.file("extdata", "heads.png", package = "paindrawings")
save(pd_demo_filepath_background_image_head, file="data/pd_demo_filepath_background_image_head.rda")
