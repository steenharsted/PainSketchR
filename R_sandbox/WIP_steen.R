library(devtools)
library(tidyverse)

load_all()

# Load sample data

pd <- pd_json2pd(c(
  "data-raw/two_geoms.json",
  "data-raw/four_geoms.json",
  "data-raw/test_spray.json",
  "data-raw/test_spray_bw.json",
  "data-raw/Anon_pen_spray.json"
))

background_image <- png::readPNG("inst/extdata/feet_background.png")

pd |>
  pd_recreate_drawing(
    background_image = background_image,
    filename = "R_sandbox/drawing.png"
  )


pd |>
  filter(id == "Anon") |>
  pd_recreate_drawing(save_plot = FALSE)
