library(devtools)
library(tidyverse)

load_all()
background_image <- png::readPNG("inst/extdata/mird_body_background.png")

# Load sample data

pd <- tibble(pdr_data = pdr_example_data)

# Get ID
pd <- pd |>
  mutate(
    id = map_chr(.x = pdr_data, .f = \(x) x$.id),
    .before = pdr_data
  )

pd

pd |>
  filter(row_number() == 2) |>
  pdr_plot_drawing(
    background_image = background_image
  )


## Test new function
pd |>
  filter(row_number() < 5) |>
  pdr_plot_drawing(
    background_image = background_image
  )

## Vizualize with new function and changed default name
pd |>
  filter(row_number() < 5) |>
  rename(new_name = pdr_data) |>
  pdr_plot_drawing(
    background_image = background_image,
    paindrawr_data = new_name
  )


##### Sample data with spray

pd <-
  tibble(
    pdr_data = pdr_import_json(c(
      "data-raw/test_spray.json",
      "data-raw/test_spray_bw.json",
      "data-raw/Anon_pen_spray.json",
      "data-raw/col_alpha_01_to_10.json"
    ))
  )

background_image <- png::readPNG("inst/extdata/feet_background.png")

# pd <- pdr_import_json(c(
#   "data-raw/test_spray.json",
#   "data-raw/col_alpha_01_to_10.json"
# ))

# No background image
pd |>
  dplyr::filter(row_number() == 1) |>
  pdr_plot_drawing()

# Background image
pd |>
  dplyr::filter(row_number() == 2) |>
  pdr_plot_drawing(
    background_image = background_image
  )

# Background image with id
pd |>
  dplyr::filter(row_number() == 3) |>
  pdr_plot_drawing(
    background_image = background_image,
    include_id = TRUE
  )


# Background image no rasterize
pd |>
  pdr_plot_drawing(
    background_image = "inst/extdata/feet_background.png",
    rasterize = TRUE
  )

# Multiple drawings
pd |>
  pdr_plot_drawing(
    background_image = background_image
  )


pd |>
  pdr_plot_drawing(
    background_image = background_image,
    rasterize = FALSE
  )


pd |>
  pdr_plot_drawing(
    background_image = background_image,
    clean_up = FALSE,
    dpi = 300
  )

pd |>
  pdr_plot_drawing(
    background_image = background_image,
    clean_up = FALSE,
    dpi = 96
  )

# Drawings with rasterize = TRUE will create temporary files
# These are automatically removed if clean_up = TRUE
# To inspect the files set clean_up to FALSE and look in tempdir()

pd |>
  pdr_plot_drawing(
    background_image = background_image,
    clean_up = FALSE,
    include_id = TRUE
  )

tempdir()


pd
