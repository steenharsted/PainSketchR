library(tidyverse)
library(devtools)
load_all()

# Quick test -------------------------------------------------------------------

# Get some data
pd <- pd_json2pd(c(
  "data-raw/two_geoms.json",
  "data-raw/four_geoms.json",
  "data-raw/test_spray.json",
  "data-raw/test_spray_bw.json",
  "data-raw/Anon_pen_spray.json"
))


# Normal workflow is with clean_up = TRUE (default)
pd[1, ] |> pd_to_png_single()
pd <- pd |> pd_add_png()
pd


# To inspect the .png files created use clean_up = FALSE
# The .png files can be found in a temp directory
tempdir()
pd[1, ] |> pd_to_png_single(clean_up = FALSE)
pd |> pd_add_png(clean_up = FALSE)

# Can we recreate the .png drawing from the .png column?
grid::grid.newpage()
pd[3, ] |> pd_to_png_single(clean_up = FALSE) |> grid::grid.raster()


# pd_to_png_single will complain if given multible rows
pd |> pd_to_png_single(clean_up = FALSE)
