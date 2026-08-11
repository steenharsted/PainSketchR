library(tidyverse)
library(devtools)
library(here)

load_all()

# Get standard data
base_data <- read_csv(here("R_sandbox", "REDCap_import", "test.csv")) |>
  mutate(
    sex = factor(sex, levels = c(1, 2), labels = c("Male", "Female")),
    parkinsons = factor(parkinsons, levels = c(1, 2), labels = c("No", "Yes"))
  )

# Keep only relevant columns
base_data <- base_data |>
  select(
    record_id,
    sex,
    hand_nrs,
    parkinsons
  )


json_files <- list.files(
  "R_sandbox/REDCap_import/test/",
  pattern = "json$",
  recursive = TRUE
)

paindraw_data <- tibble(
  pdr_data = pdr_import_json(here(
    "R_sandbox",
    "REDCap_import",
    "test",
    json_files
  ))
)


# Extract ID
## First get drawing id
paindraw_data <- paindraw_data |>
  mutate(id = pdr_get_info(pdr_data, ".id"))


## Id is nested in the drawing id string
paindraw_data <- paindraw_data |>
  mutate(
    record_id = str_extract(id, "R[0-9]+F") |>
      str_remove("^R") |>
      str_remove("F$") |>
      as.double()
  )


# Because we have multible drawings pr id we go wide before joining

### First extract image type
paindraw_data_wide <- paindraw_data |>
  mutate(
    view = str_extract(id, "F-.+-") |>
      str_remove("^F-") |>
      str_remove("-E.+$") |>
      str_remove("^background_image_")
  ) |>
  select(-id) |>
  pivot_wider(
    names_from = view,
    values_from = pdr_data
  )

base_data_with_paindrawings <- base_data |>
  left_join(paindraw_data_wide)

# Test heatmap
base_data_with_paindrawings |>
  pdr_plot_heatmap(
    paindrawr_data = dorsal,
    variables = c("sex", "hand_nrs"),
    background_image = "inst/extdata/lower_arm_dorsal.png"
  )


base_data_with_paindrawings |>
  mutate(" " = "") |>
  pdr_plot_heatmap(
    paindrawr_data = spiral,
    variables = c(" ", "parkinsons"),
    background_image = "inst/extdata/spiral.png",
    point_size = 3,
    min_alpha = 1,
    equal_n = FALSE
  )


base_data_with_paindrawings |>
  mutate(" " = "") |>
  pdr_plot_heatmap(
    paindrawr_data = spiral,
    variables = c("parkinsons"),
    background_image = "inst/extdata/spiral.png",
    point_size = 3,
    min_alpha = 1,
    equal_n = FALSE
  )


# Recreate drawing
data_with_paindrawings |>
  filter_out(is.na(sex)) |>
  filter(row_number() == 2) |>
  pdr_plot_drawing(
    paindrawr_data = dorsal,
    background_image = "inst/extdata/lower_arm_dorsal.png",
  )


# Recreate drawingS
data_with_paindrawings |>
  filter_out(is.na(sex)) |>
  pdr_plot_drawing(
    paindrawr_data = dorsal,
    background_image = "inst/extdata/lower_arm_dorsal.png",
  )


## Add RGBA arrays

data_with_paindrawings <- data_with_paindrawings |>
  filter_out(is.na(sex)) |>
  mutate(
    dorsal = pdr_add_rgba(dorsal)
  )


# TEST THAT RGBAs look correct
grid::grid.newpage()
data_with_paindrawings$dorsal[[2]]$.rgba |> grid::grid.raster()

grid::grid.newpage()
data_with_paindrawings$dorsal[[3]]$.rgba |> grid::grid.raster()


# Extract alpha
data_with_paindrawings |>
  mutate(
    dorsal_area = pdr_get_alpha_area(dorsal),
    dorsal_intensity = pdr_get_alpha_intensity(dorsal)
  )


data_with_paindrawings |>
  filter_out(is.na(sex)) |>
  mutate(
    id = pdr_get_info(pdr = dorsal, ".id")
  )
