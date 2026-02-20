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

# .data <- pd
# id_col <- "id"
# variables <- c("sex", "age")
# n_groups = c(2, 2)
# equal_n = TRUE
# max_n = 1000
# tool = "pen"
# background_image = NULL
# grid_size = 10
# point_size = 0.25
# min_alpha = 0.1
# color_scale = "max"
# label_format = "pct"
# show_n = TRUE
# save_plot = FALSE
# filename = "heatmap_%03d.png"
# scale = 1.5
# width = NA
# height = NA

pd |>
  pd_create_heatmap(
    id_col = "id",
    variables = c("sex", "age")
  )
