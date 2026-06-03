library(tidyverse)
library(devtools)

load_all()

load("data/pdr_example_data.rda")
background_image <- png::readPNG("inst/extdata/mird_body_background.png")

demo <-
  tibble(pdr_data = pdr_example_data) |>
  filter(row_number() != 1) |>
  mutate(
    cat_1 = sample(c("Mouse", "Cat", "Dog"), n(), replace = TRUE),
    cat_2 = sample(c("Cocaine", "Heroin"), n(), replace = TRUE),
    quant_1 = map_dbl(
      .x = pdr_data,
      .f = function(tbl) {
        nrow(tbl$.points)
      }
    ),
    quant_2 = sample(1:10, n(), replace = TRUE)
  )


# Test pdr_plot_heatmap

demo |>
  pdr_plot_heatmap(
    variables = c("cat_1", "cat_2"),
    n_groups = c(4, 2),
    background_image = background_image
  )


demo |>
  pdr_plot_heatmap(
    variables = c("quant_1", "cat_2"),
    n_groups = c(4, 2),
    background_image = background_image
  )


demo |>
  pdr_plot_heatmap(
    variables = c("quant_1", "cat_2"),
    n_groups = c(4, 2),
    background_image = background_image,
    label_format = "both"
  )


demo |>
  pdr_plot_heatmap(
    variables = c("quant_2", "quant_1"),
    n_groups = c(2, 4),
    background_image = background_image,
    label_format = "both",
    equal_n = FALSE,
    grid_size = 10
  )


# Test one var
# Not super happy with this :/
demo |>
  pdr_plot_heatmap(
    variables = c("cat_1"),
    n_groups = c(3),
    background_image = background_image
  )

demo |>
  pdr_plot_heatmap(
    variables = c("quant_1"),
    n_groups = c(4),
    background_image = background_image
  )


# Better looking, but clumsy code?
demo |>
  # create dummy
  mutate(
    " " = " "
  ) |>
  pdr_plot_heatmap(
    variables = c(" ", "quant_1"),
    n_groups = c(1, 4),
    background_image = background_image
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
  ) |>
  mutate(
    cat_1 = sample(c("Mouse", "Cat", "Dog"), n(), replace = TRUE),
    cat_2 = sample(c("Cocaine", "Heroin"), n(), replace = TRUE),
    quant_1 = map_dbl(
      .x = pdr_data,
      .f = function(tbl) {
        nrow(tbl$.points)
      }
    ),
    quant_2 = sample(1:10, n(), replace = TRUE)
  )

background_image <- png::readPNG("inst/extdata/feet_background.png")


pd |>
  pdr_plot_heatmap(
    variables = c("cat_1"),
    n_groups = c(2),
    background_image = background_image
  )


pd |>
  mutate(
    " " = " ",
    "  " = " "
  ) |>
  pdr_plot_heatmap(
    variables = c(" ", "  "),
    n_groups = c(1, 1),
    background_image = background_image,
    tool = "spray"
  )
