## code to prepare the demo `DATASET` goes here

# Generate some demo data of anatomical regions from full body outline/template
###pdr_d <- tibble::tibble(.id=as.integer(), .strokes=list(), .points=list())
pdr_xa <- list()
for(c in fs::dir_ls("data-raw/regions_of_full_body_template/")) {
  d <- read.csv(c, sep=";", header = FALSE)
  pdr_xa[[length(pdr_xa) + 1]] <- list(
    .id= as.character(fs::path_ext_remove(fs::path_file(c))), 
    .file = as.character(fs::path_file(c)),
    .width=as.integer(450),
    .height=as.integer(500),
    .units="px",
    .timestamp=as.character(Sys.time()),
    .app="PainDraweR package",
    .strokes=tibble::tibble(.index=as.integer(1), .q=as.integer(1), .tool="pen", .tool_width=as.integer(1), .color="#FF0000", .alpha=as.integer(256)), 
    .points=tibble::tibble(.index=as.integer(1), .x=d[,1], .y=d[,2])
  )
} 

# This is a bit quirky ... but necessary to ensure the variable is named correctly in rda:
var_name <- "pdr_example_anatomy"
assign(var_name, pdr_xa)
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
  tidyr::unnest(paindrawing_LBP) |> dplyr::select(-X) |> 
  dplyr::rename(.id=id, .x=x, .y=y, .index=i) |>
  dplyr::relocate(.index, .after=.id) |>
  dplyr::group_by(.id) |>
  dplyr::group_modify(\(d, indx) {
    d <- tibble::tibble(
      .strokes = list(tibble::tibble(.index=unique(d$.index))),
      .points = list(tibble::tibble(.index=d$.index, .x=d$.x, .y=d$.y))
    )
  }) |>
  dplyr::ungroup() 

pdr_example_data <- pdr_example_data |>
  dplyr::mutate(.file="data/pdr_example_data.csv") |>
  dplyr::mutate(.version=as.integer(2)) |>
  dplyr::mutate(.width=as.integer(450)) |>
  dplyr::mutate(.height=as.integer(500)) |>
  dplyr::mutate(.units="px") |>
  dplyr::mutate(.timestamp = as.character(Sys.time())) |>
  dplyr::mutate(.app = "paindrawr package") 
  
pdr_example_data$.strokes <- pdr_example_data$.strokes |> purrr::map(\(tb) {
  if(tibble::is_tibble(tb)) {
    tb |> dplyr::mutate(
      .q=.index,
      .tool="pen",
      .tool_width=as.integer(1),
      .color="#FF0000",
      .alpha=as.integer(128)
    )
  } else {
    tb
  }
})

pdr_example_data <- pdr_example_data |>
  dplyr::relocate(.strokes, .after=last_col()) |>
  dplyr::relocate(.points, .after=last_col()) |>
  purrr::transpose()

save(pdr_example_data, file="data/pdr_example_data.rda")
  
