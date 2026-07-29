library(tidyverse)
library(devtools)
library(here)

load_all()

# Get standard data

data <- read_csv(here("R_sandbox", "REDCap_export", "2026_06_18_test.csv")) |>
  mutate(sex = factor(sex, levels = c(1, 2), labels = c("Male", "Female")))


json_files <- list.files("R_sandbox/REDCap_export/", pattern = "json$")

paindraw_data <- tibble(
  pdr_data = pdr_import_json(here("R_sandbox", "REDCap_export", json_files))
)


paindraw_data |>
  mutate(id = pdr_get_info(pdr_data, ".id"))

# Extract ID
## First get drawing id

paindraw_data <- paindraw_data |>
  mutate(
    drawing_id = map_chr(
      .x = pdr_data,
      .f = \(list) list$.id
    )
  )

## Id is nested in the drawing id string
paindraw_data <- paindraw_data |>
  mutate(
    record_id = str_extract(drawing_id, "R[0-9]+F") |>
      str_remove("^R") |>
      str_remove("F$") |>
      as.double()
  )

### !!!! NOTICE THAT INFORMATION IS ALSO IN FILENAME !!! ###

paindraw_data |> left_join(data)


## Maybe a better option is to go wide before joining

### First extract image type
paindraw_data_wide <- paindraw_data |>
  mutate(
    view = str_extract(drawing_id, "_image_.+-") |>
      str_remove("^_image_") |>
      str_remove("-.+$")
  ) |>
  select(-drawing_id) |>
  pivot_wider(
    names_from = view,
    values_from = pdr_data
  )

data_with_paindrawings <- data |>
  left_join(paindraw_data_wide) |>
  select(record_id, sex, age, dorsal, palmar)

# Test heatmap
data_with_paindrawings |>
  pdr_plot_heatmap(
    paindrawr_data = dorsal,
    variables = c("sex", "age")
  )


data_with_paindrawings |>
  filter_out(is.na(sex)) |> # OTHERWISE ERROR
  pdr_plot_heatmap(
    paindrawr_data = dorsal,
    background_image = "inst/extdata/lower_arm_dorsal.png",
    variables = c("sex", "age")
  )


data_with_paindrawings |>
  filter_out(is.na(sex)) |> # OTHERWISE ERROR
  mutate(" " = " ") |> # Dummy variable
  pdr_plot_heatmap(
    paindrawr_data = dorsal,
    background_image = "inst/extdata/lower_arm_dorsal.png",
    variables = c(" ", "sex"),
    show_n = FALSE
  )


data_with_paindrawings |>
  filter_out(is.na(sex)) |> # OTHERWISE ERROR
  mutate(" " = " ") |> # Dummy variable
  pdr_plot_heatmap(
    paindrawr_data = palmar,
    background_image = "inst/extdata/lower_arm_palmar.png",
    variables = c(" ", "sex"),
    show_n = FALSE
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
