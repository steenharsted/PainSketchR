library(devtools)
library(tidyverse)

load_all()

col_alpha_data <- pd_json2pd("data-raw/col_alpha_01_to_10.json")

col_alpha_data |> pd_recreate_drawing()

grid::grid.newpage()
col_alpha_data |> pd_to_png_single() |> grid::grid.raster()


grid::grid.newpage()
col_alpha_data |> pd_to_png_single(grey_scale = TRUE) |> grid::grid.raster()

set.seed(1)
my_array_col <- col_alpha_data |> pd_to_png_single()
set.seed(1)
my_array_grey <- col_alpha_data |> pd_to_png_single(grey_scale = TRUE)

str(my_array_col)
alpha_channel_col <- my_array_col[,, 4] # Extracts the alpha layer
alpha_channel_grey <- my_array_grey[,, 4] # Extracts the alpha layer

# THE ALPHA LAYER IS IDENTICAL REGARDLESS OF COLOR!!!!
identical(alpha_channel_col, alpha_channel_grey)

# THE LAYERS WITH COLOR ARE NOT IDENTICAL !!!
identical(my_array_col[,, 3], my_array_grey[,, 3])
identical(my_array_col[,, 2], my_array_grey[,, 2])
identical(my_array_col[,, 1], my_array_grey[,, 1])

# ALPHA LAYER AGAIN
identical(my_array_col[,, 4], my_array_grey[,, 4])
