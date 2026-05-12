library(tidyverse)
library(devtools)

load_all()

load("data/pdr_example_data.rda")
background_image <- png::readPNG("inst/extdata/mird_body_background.png")

demo <- pdr_example_data |>

  filter(row_number() != 1) |>
  mutate(
    cat_1 = sample(c("Mouse", "Cat", "Dog"), n(), replace = TRUE),
    cat_2 = sample(c("Cocaine", "Heroin"), n(), replace = TRUE),
    quant_1 = map_dbl(
      .x = p,
      .f = function(tbl) {
        nrow(tbl)
      }
    ),
    quant_2 = sample(1:10, n(), replace = TRUE)
  )


# Test pdr_create_heatmap

demo |>
  pdr_create_heatmap(
    variables = c("cat_1", "cat_2"),
    n_groups = c(4, 2),
    background_image = background_image
  )


demo |>
  pdr_create_heatmap(
    variables = c("quant_1", "cat_2"),
    n_groups = c(4, 2),
    background_image = background_image
  )


demo |>
  pdr_create_heatmap(
    variables = c("quant_1", "cat_2"),
    n_groups = c(4, 2),
    background_image = background_image,
    label_format = "both"
  )


demo |>
  pdr_create_heatmap(
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
  pdr_create_heatmap(
    variables = c("cat_1"),
    n_groups = c(3),
    background_image = background_image
  )

demo |>
  pdr_create_heatmap(
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
  pdr_create_heatmap(
    variables = c(" ", "quant_1"),
    n_groups = c(1, 4),
    background_image = background_image
  )
