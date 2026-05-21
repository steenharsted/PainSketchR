library(devtools)
library(tidyverse)

load_all()

# Load sample data

# pd <- pdr_import_json(c(
#   "data-raw/test_spray.json",
#   "data-raw/test_spray_bw.json",
#   "data-raw/Anon_pen_spray.json",
#   "data-raw/col_alpha_01_to_10.json"
# ))


# pd <- pdr_import_json(c(
#   "data-raw/test_spray.json",
#   "data-raw/col_alpha_01_to_10.json"
# ))


pd <- tibble(paindrawr_data = pdr_example_data)

# Get ID
pd <- pd |> 
  mutate(
    id = map_chr(.x = paindrawr_data, 
    .f = \(x) x$id), 
    .before = paindrawr_data
  )

pd

background_image <- png::readPNG("inst/extdata/feet_background.png")


pd$paindrawr_data




pd |> 
  unnest(paindrawr_data)


# https://www.robwiederstein.org/2022/04/13/lists-into-tibbles-part-03/



















# No background image
pd |>
  dplyr::filter(id == "test_spray") |>
  pdr_recreate_drawing()

# Background image
pd |>
  dplyr::filter(id == "test_spray") |>
  pdr_recreate_drawing(
    background_image = background_image
  )

# Background image with id
pd |>
  dplyr::filter(id == "test_spray") |>
  pdr_recreate_drawing(
    background_image = background_image,
    include_id = TRUE
  )


# Background image no rasterize
pd |>
  dplyr::filter(id == "test_spray") |>
  pdr_recreate_drawing(
    background_image = "inst/extdata/feet_background.png",
    rasterize = TRUE
  )

# Multiple drawings
pd |>
  pdr_recreate_drawing(
    background_image = background_image
  )


pd |>
  pdr_recreate_drawing(
    background_image = background_image,
    rasterize = FALSE
  )


pd |>
  pdr_recreate_drawing(
    background_image = background_image,
    clean_up = FALSE,
    dpi = 300
  )

pd |>
  pdr_recreate_drawing(
    background_image = background_image,
    clean_up = FALSE,
    dpi = 96
  )

# Drawings with rasterize = TRUE will create temporary files
# These are automatically removed if clean_up = TRUE
# To inspect the files set clean_up to FALSE and look in tempdir()

pd |>
  pdr_recreate_drawing(
    background_image = background_image,
    clean_up = FALSE,
    include_id = TRUE
  )

tempdir()


pd
