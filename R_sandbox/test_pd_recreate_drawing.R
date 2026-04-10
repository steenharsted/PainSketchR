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


# No background image
pd |>
  dplyr::filter(id == "Anon") |>
  pd_recreate_drawing()

# Background image
pd |>
  dplyr::filter(id == "Anon") |>
  pd_recreate_drawing(
    background_image = background_image
  )

# Background image with id
pd |>
  dplyr::filter(id == "Anon") |>
  pd_recreate_drawing(
    background_image = background_image,
    include_id = TRUE
  )


# Background image no rasterize
pd |>
  dplyr::filter(id == "Anon") |>
  pd_recreate_drawing(
    background_image = "inst/extdata/feet_background.png",
    rasterize = FALSE
  )

# Multiple drawings
pd |>
  pd_recreate_drawing(
    background_image = background_image
  )


pd |>
  pd_recreate_drawing(
    background_image = background_image,
    rasterize = FALSE
  )


pd |>
  pd_recreate_drawing(
    background_image = background_image,
    clean_up = FALSE,
    dpi = 300
  )

pd |>
  pd_recreate_drawing(
    background_image = background_image,
    clean_up = FALSE,
    dpi = 96
  )

# Drawings with rasterize = TRUE will create temporary files
# These are automatically removed if clean_up = TRUE
# To inspect the files set clean_up to FALSE and look in tempdir()

pd |>
  pd_recreate_drawing(
    background_image = background_image,
    clean_up = FALSE,
    include_id = TRUE
  )

tempdir()
