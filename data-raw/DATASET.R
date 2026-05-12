## code to prepare the demo `DATASET` goes here

# Generate some demo data of anatomical regions from full body outline/template
pdr_d <- tibble::tibble(id=as.integer(), s=list(), p=list())
for(c in fs::dir_ls("data-raw/regions_of_full_body_template/")) {
  d <- read.csv(c, sep=";", header = FALSE)
  pdr_d <- rbind(pdr_d, tibble::tibble(
    id=fs::path_ext_remove(fs::path_file(c)), 
    w=as.integer(450),
    h=as.integer(500),
    coord="px",
    ts=as.character(Sys.time()),
    app="PainDraweR package",
    s=list(tibble::tibble(i=as.integer(1), q=as.integer(1), t="pen", bw=as.integer(1), c="#FF0000", a=as.integer(256))), 
    p=list(tibble::tibble(i=as.integer(1), x=d[,1], y=d[,2]))))
} 
# The following was added by SON to change this to the new pdr data format format 
pdr_d <- pdr_d |> purrr::transpose()

# This is a bit quirky ... but necessary to ensure the variable is named correctly in rda:
var_name <- "pdr_example_anatomy"
assign(var_name, pdr_d)
do.call(save, list(var_name, file="data/pdr_example_anatomy.rda"))

# Generate png stencils for each anatomical region (to allow for finding intersections in spray pain drawings)
# pdr_d$p |> purrr::map(\(x) {})

# Generate a demo data set of pain drawings
# This is based on real-world data collection (mird)
# With an added observation where paindrawing is NA
pdr_example_data <- read.csv("data-raw/demo_data.csv") |> 
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

pdr_example_data <- pdr_example_data |>
  dplyr::mutate(f="data/pdr_example_data.csv") |>
  dplyr::mutate(v=as.integer(2)) |>
  dplyr::mutate(w=as.integer(450), h=as.integer(500)) |>
  dplyr::mutate(coord="px") |>
  dplyr::mutate(ts = as.character(Sys.time())) |>
  dplyr::mutate(app = "paindrawr package") 
  
pdr_example_data$s <- pdr_example_data$s |> purrr::map(\(tb) {
  if(tibble::is_tibble(tb)) {
    tb |> dplyr::mutate(
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

pdr_example_data <- pdr_example_data |>
  dplyr::relocate(s, .after=last_col()) |>
  dplyr::relocate(p, .after=last_col()) |>
  purrr::transpose()

save(pdr_example_data, file="data/pdr_example_data.rda")
  
