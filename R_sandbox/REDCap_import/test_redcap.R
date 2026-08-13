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
#paindraw_data <- paindraw_data |>
#  mutate(id = pdr_get_info(pdr_data, ".id"))

paindraw_data <- paindraw_data |>
  mutate(id = map_chr(.x = pdr_data, .f = ~ .x$.id |> unique()))


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


### START HERE
base_data_with_paindrawings


# Test heatmap
base_data_with_paindrawings |>
  pdr_plot_heatmap(
    paindrawr_data = dorsal,
    background_image = "inst/extdata/lower_arm_dorsal.png"
  )

# Split by sex
base_data_with_paindrawings |>
  pdr_plot_heatmap(
    paindrawr_data = dorsal,
    variables = c("sex"),
    background_image = "inst/extdata/lower_arm_dorsal.png"
  )

# Split by pain score
base_data_with_paindrawings |>
  pdr_plot_heatmap(
    paindrawr_data = dorsal,
    variables = c("hand_nrs"),
    background_image = "inst/extdata/lower_arm_dorsal.png"
  )

# Split by pain score multible groups
base_data_with_paindrawings |>
  pdr_plot_heatmap(
    paindrawr_data = dorsal,
    variables = c("hand_nrs"),
    n_groups = c(3, 1),
    background_image = "inst/extdata/lower_arm_dorsal.png"
  )


# Split by pain score multible groups
base_data_with_paindrawings |>
  pdr_plot_heatmap(
    paindrawr_data = dorsal,
    variables = c(NA, "hand_nrs"),
    n_groups = c(1, 4),
    label_format = "both",
    background_image = "inst/extdata/lower_arm_dorsal.png"
  )


# Spiral
base_data_with_paindrawings |>
  pdr_plot_heatmap(
    paindrawr_data = spiral,
    background_image = "inst/extdata/spiral.png",
    point_size = 3,
    show_n = TRUE
  )


base_data_with_paindrawings |>
  pdr_plot_heatmap(
    paindrawr_data = spiral,
    variables = c(NA, "parkinsons"),
    background_image = "inst/extdata/spiral.png",
    point_size = 4,
    equal_n = FALSE
  )


base_data_with_paindrawings |>
  pdr_plot_heatmap(
    paindrawr_data = spiral,
    variables = c("parkinsons"),
    background_image = "inst/extdata/spiral.png",
    point_size = 3,
    equal_n = FALSE
  )


# Recreate drawing
base_data_with_paindrawings |>
  filter(record_id == 6) |>
  pdr_plot_drawing(
    paindrawr_data = dorsal,
    background_image = "inst/extdata/lower_arm_dorsal.png",
  )


# Recreate drawingS
base_data_with_paindrawings |>
  filter(record_id < 4) |>
  pdr_plot_drawing(
    paindrawr_data = dorsal,
    background_image = "inst/extdata/lower_arm_dorsal.png"
  )


## Add RGBA arrays
base_data_with_paindrawings <- base_data_with_paindrawings |>
  mutate(
    dorsal = pdr_add_rgba(dorsal),
    spiral = pdr_add_rgba(spiral)
  )

base_data_with_paindrawings

# Extract alpha area and intensity
base_data_with_paindrawings <- base_data_with_paindrawings |>
  mutate(
    dorsal_area = pdr_get_alpha_area(dorsal),
    dorsal_intensity = pdr_get_alpha_intensity(dorsal),
    spiral_area = pdr_get_alpha_area(spiral),
    spiral_intensity = pdr_get_alpha_intensity(spiral)
  )


base_data_with_paindrawings


# Look at drawing with the smallest area
base_data_with_paindrawings |>
  filter(dorsal_area == min(dorsal_area)) |>
  pdr_plot_drawing(
    paindrawr_data = dorsal,
    background_image = "inst/extdata/lower_arm_dorsal.png"
  )

# Summarise area and intensity dorsal drawing
base_data_with_paindrawings |>
  summarise(
    mean_area = mean(dorsal_area),
    mean_intensity = mean(dorsal_intensity),
    .by = sex
  )


# Spiral measures
# Summarise area and intensity dorsal drawing
base_data_with_paindrawings |>
  summarise(
    mean_area = mean(spiral_area),
    mean_intensity = mean(spiral_intensity),
    .by = parkinsons
  )


## TEST OF DRAWING
base_data_with_paindrawings |>
  filter(record_id == 15) |>
  pdr_plot_drawing(
    paindrawr_data = palmar,
    background_image = "inst/extdata/lower_arm_palmar.png"
  )


base_data_with_paindrawings |>
  filter(record_id == 16) |>
  pdr_plot_drawing(
    paindrawr_data = spiral,
    background_image = "inst/extdata/spiral_grey.png",
    type = "path"
  )


# Test of RGBA arrays
## Plot a spiral (pen) RGBA arrays
grid::grid.newpage()
base_data_with_paindrawings$spiral[[16]]$.rgba |> grid::grid.raster()

## Plot a dorsal (spray)  RGBA arrays
grid::grid.newpage()
base_data_with_paindrawings$dorsal[[16]]$.rgba |> grid::grid.raster()


# Test of heatmap plot
base_data_with_paindrawings |>
  pdr_plot_heatmap(
    paindrawr_data = spiral,
    background_image = "inst/extdata/spiral.png",
    variables = "parkinsons",
    point_size = 4,
    show_n = TRUE
  )


base_data_with_paindrawings |>
  pdr_plot_heatmap(
    paindrawr_data = spiral,
    background_image = "inst/extdata/spiral.png",
    variables = "parkinsons",
    point_size = 4,
    show_n = TRUE,
    alpha_scale = c(1, 1)
  )


base_data_with_paindrawings |>
  pdr_plot_heatmap(
    paindrawr_data = spiral,
    background_image = "inst/extdata/spiral.png",
    variables = c("parkinsons", "hand_nrs"),
    n_groups = c(NA, 3),
    point_size = 4,
    show_n = TRUE,
    alpha_scale = c(0.01, 1)
  )


base_data_with_paindrawings |>
  pdr_plot_heatmap(
    paindrawr_data = spiral,
    background_image = "inst/extdata/spiral.png",
    variables = c("hand_nrs", "parkinsons"),
    n_groups = c(3, 1),
    point_size = 4,
    equal_n = FALSE,
    show_n = TRUE,
    alpha_scale = c(0.01, 1),
    label_format = "both"
  )


base_data_with_paindrawings |>
  pdr_plot_heatmap(
    paindrawr_data = spiral,
    background_image = "inst/extdata/spiral.png",
    variables = "parkinsons",
    point_size = 4,
    show_n = TRUE,
    alpha_scale = c(0.01, 0.2)
  )
