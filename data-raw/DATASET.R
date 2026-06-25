## code to prepare the demo `DATASET` goes here

# Generate some demo data of anatomical regions from full body outline/template
###pdr_d <- tibble::tibble(.id=as.integer(), .strokes=list(), .points=list())
pdr_xa <- list()
for(c in fs::dir_ls("data-raw/regions_of_full_body_template/")) {
  d <- read.csv(c, sep=";", header = FALSE)
  pdr_xa[[length(pdr_xa) + 1]] <- list(
    .id= as.character(fs::path_ext_remove(fs::path_file(c))), 
    .file = as.character(fs::path_file(c)),
    .version = "1",
    .width=as.integer(450),
    .height=as.integer(500),
    .units="px",
    .timestamp=as.character(Sys.time()),
    .app="PainDraweR package",
    .strokes=tibble::tibble(.index=as.integer(1), .draw_input_type="mouse", .tool="pen", .tool_width=as.integer(1), .color="#FF0000", .point_density=as.integer(1), .spray_radius=as.integer(0),  .alpha=as.integer(255)), 
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
# and some constructed drawings to illustrate special cases
# With an added observation where paindrawing is NA
special_cases <- tibble::tibble(
    id="no_area",
    paindrawing_LBP="(,100,100,),(,150,150,175,175,)") |>
  dplyr::bind_rows(tibble::tibble(
    id="duplicates",
    paindrawing_LBP="(,100,100,100,100,100,100,100,130,130,130,130,100,100,100,)"
  )) |>
  dplyr::bind_rows(tibble::tibble(
    id="selfintersection",
    paindrawing_LBP="(,120,115,100,100,100,130,130,130,130,100,110,115,)"
  )) |>
  dplyr::bind_rows(tibble::tibble(
    id="convex_hull",
    paindrawing_LBP="(,115,115,100,100,100,130,130,130,130,100,115,115,)"
  )) |>
  dplyr::bind_rows(tibble::tibble(
    id="closed_polygon",
    paindrawing_LBP="(,100,100,100,130,130,130,130,100,)"
  )) |>
  dplyr::bind_rows(tibble::tibble(
    id="merge_overlaps",
    paindrawing_LBP="(,100,100,100,130,130,130,130,100,100,100,),(,115,115,115,145,145,145,145,115,115,115,),(,110,110,110,120,120,120,120,110,110,110,),(,150,150,150,170,170,170,170,150,150,150,)"
  )) |>
  dplyr::bind_rows(tibble::tibble(
    id="no_area_line",
    paindrawing_LBP="(,100,100,100,105,100,115,100,130,100,140,100,135,)"
  ))|>
  dplyr::bind_rows(tibble::tibble(
    id="intersection",
    paindrawing_LBP="(,130,50,130,100,180,100,180,50,),(,100,100,100,130,130,130,130,100,),(,100,130,100,160,130,160,130,130,),(,100,170,100,200,130,200,130,170,)"
  ))


pdr_example_data <- dplyr::bind_rows(
  special_cases, 
  read.csv("data-raw/demo_data.csv", ) |> dplyr::select(-X)) |> 
  dplyr::mutate(paindrawing_LBP = stringr::str_replace_all(paindrawing_LBP, ",\\),\\(,", ";")) |> 
  dplyr::mutate(paindrawing_LBP = stringr::str_replace_all(paindrawing_LBP, "\\(,", "")) |>
  dplyr::mutate(paindrawing_LBP = stringr::str_replace_all(paindrawing_LBP, ",\\)", "")) |>
  dplyr::mutate(paindrawing_LBP = stringr::str_split(paindrawing_LBP, ";")) |> 
  dplyr::mutate(paindrawing_LBP = paindrawing_LBP |> purrr::map(\(x) {
    stringr::str_split(x,",") |> purrr::imap_dfr(\(xy,i) {
      as.integer(xy) |> matrix(ncol=2, byrow=TRUE) |> as.data.frame() |> purrr::set_names(c("x","y")) |> dplyr::mutate(i=i)  
    })
  })) |> 
  tidyr::unnest(paindrawing_LBP) |> 
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
  dplyr::mutate(.version="2") |>
  dplyr::mutate(.width=as.integer(450)) |>
  dplyr::mutate(.height=as.integer(500)) |>
  dplyr::mutate(.units="px") |>
  dplyr::mutate(.timestamp = as.character(Sys.time())) |>
  dplyr::mutate(.app = "paindrawr package") 
  
pdr_example_data$.strokes <- pdr_example_data$.strokes |> purrr::map(\(tb) {
  if(tibble::is_tibble(tb)) {
    tb |> dplyr::mutate(
      .tool="pen",
      .draw_input_type = "mouse",
      .tool_width=as.integer(1),
      .color="#FF0000",
      .point_density = as.integer(1),
      .spray_radius = as.integer(0),
      .alpha=as.integer(255)
    )
  } else {
    tb
  }
})



pdr_example_data <- pdr_example_data |>
  dplyr::relocate(.strokes, .after=last_col()) |>
  dplyr::relocate(.points, .after=last_col()) |>
  purrr::transpose()

# We merged geometric example data and actual patient data into one
# lets split them up again (as it is probably easier that way)

pdr_example_geometry <- pdr_example_data |> 
  purrr::keep(\(e) {e$.id %in% c("no_area", "duplicates", "convex_hull", "closed_polygon", "merge_overlaps", "no_area_line", "intersection", "selfintersection")})
save(
  pdr_example_geometry,
  file="data/pdr_example_geometry.rda"
)

pdr_example_data <- pdr_example_data |> 
  purrr::keep(\(e) {!e$.id %in% c("no_area", "duplicates", "convex_hull", "closed_polygon", "merge_overlaps", "no_area_line", "intersection", "selfintersection")})
save( 
  pdr_example_data,
  file="data/pdr_example_data.rda"
)
  
