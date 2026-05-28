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


load_all()
pdr_add_rgba_single(paindrawr_data = pdr_example_data)

map_dbl(.x = pdr_example_data, .f = \(list) list$.width )


pd |> 
  unnest_wider(pdr_data) |> 
  pdr_add_rgba()


pdr_example_data[[1]] 

tibble(
  list_col = pdr_example_data
) |> 
  unnest_wider(list_col)
