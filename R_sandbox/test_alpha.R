library(devtools)
library(tidyverse)

load_all()


alpha_data <- pd_json2pd("data-raw/Alpha_01_til_10.json")

alpha_data |> pd_recreate_drawing()

grid::grid.newpage()
alpha_data |> pd_to_png_single() |> grid::grid.raster()

grid::grid.newpage()
alpha_data |> pd_to_png_single(grey_scale = TRUE) |> grid::grid.raster()


col_alpha_data <- pd_json2pd("data-raw/col_alpha_01_to_10.json")

col_alpha_data |> pd_recreate_drawing()

grid::grid.newpage()
col_alpha_data |> pd_to_png_single() |> grid::grid.raster()


grid::grid.newpage()
col_alpha_data |> pd_to_png_single(grey_scale = TRUE) |> grid::grid.raster()


my_array_col <- col_alpha_data |> pd_to_png_single()
my_array_grey <- col_alpha_data |> pd_to_png_single(grey_scale = TRUE)

str(my_array_col)
alpha_channel_col <- my_array_col[,, 4] # Extracts the alpha layer
alpha_channel_grey <- my_array_grey[,, 4] # Extracts the alpha layer

identical(alpha_channel_col, alpha_channel_grey)
