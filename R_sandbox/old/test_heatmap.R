library(devtools)
library(tidyverse)

load_all()

# Load sample data

pd <- pd_json2pd(c(
  "data-raw/two_geoms.json",
  "data-raw/four_geoms.json",
  "data-raw/test_spray.json",
  "data-raw/test_spray_bw.json",
  "data-raw/Anon_pen_spray.json",
  "data-raw/two_geoms.json",
  "data-raw/four_geoms.json",
  "data-raw/test_spray.json",
  "data-raw/test_spray_bw.json",
  "data-raw/Anon_pen_spray.json",
  "data-raw/two_geoms.json",
  "data-raw/four_geoms.json",
  "data-raw/test_spray.json",
  "data-raw/test_spray_bw.json",
  "data-raw/Anon_pen_spray.json",
  "data-raw/two_geoms.json",
  "data-raw/four_geoms.json",
  "data-raw/test_spray.json",
  "data-raw/test_spray_bw.json",
  "data-raw/Anon_pen_spray.json"
))

pd <- pd |>
  mutate(
    id = row_number(),
    sex = sample(c("man", "woman"), nrow(pd), replace = TRUE),
    age = sample(20:100, nrow(pd))
  )


pd |>
  pd_create_heatmap(
    id_col = "id",
    variables = c("sex", "age")
  )
