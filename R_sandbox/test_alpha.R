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


## Explore gglot

plot_data <- tibble(
    x = c(rep(1, 10), rep(2, 10)),
    y = c(1:10, 1:10),
    alpha = c(rep(0.1, 10), seq(0.1, 1, by = 0.1)),
    n = c(1:10, rep(1, 10))
) |>
    mutate(label = paste0(alpha, " * ", n)) |>
    uncount(n)

plot_data |>
    ggplot(aes(x = x, y = y, alpha = alpha)) +
    geom_point(color = "red", size = 40) +
    theme_void() +
    scale_size_identity() +
    scale_alpha_identity() +
    scale_x_continuous(limits = c(0.5, 2.5)) +
    geom_text(aes(label = label), alpha = 1)


# Resulting alpha should follow this formula
# α = F_alpha + B_alpha * (1 - F_alpha)

plot_data_extra <- tibble(
    x = rep(1.5, 10),
    y = c(1:10),
    alpha_base = rep(0.1, 10),
    nn = 1:10,
    n = rep(1, 10)
) |>
    mutate(
        alpha = 1 - (1 - alpha_base)^nn,
        label = paste0(round(alpha, 3), " * ", n),

        # Other ways of calucating alpha (for my understanding)
        alpha_purr = purrr::accumulate(alpha_base, \(bg, fg) {
            fg + bg * (1 - fg)
        })
    )

plot_data_extra

bind_rows(plot_data, plot_data_extra) |>
    ggplot(aes(x = x, y = y, alpha = alpha)) +
    geom_point(color = "red", size = 40) +
    theme_void() +
    scale_size_identity() +
    scale_alpha_identity() +
    scale_x_continuous(limits = c(0.5, 2.5)) +
    geom_text(aes(label = label), alpha = 1)


# Lets try compare alphas

## Explore gglot

plot_data_cum <- tibble(
    x = rep(1, 10),
    y = 1:10,
    alpha = rep(0.1, 10),
    n = 1:10
) |>
    uncount(n)

plot_cum <- plot_data_cum |>
    ggplot(aes(x = x, y = y, alpha = alpha)) +
    geom_point(color = "red", size = 40, stroke = 0, shape = 19) +
    theme_void() +
    scale_size_identity() +
    scale_alpha_identity() +
    scale_x_continuous(limits = c(0.5, 2.5))


plot_data_mul <- tibble(
    x = rep(1, 10),
    y = 1:10,
    alpha_base = rep(0.1, 10),
    nn = 1:10,
    n = rep(1, 10)
) |>
    mutate(
        alpha = 1 - (1 - alpha_base)^nn,
    )

plot_data_mul

plot_mul <- plot_data_mul |>
    ggplot(aes(x = x, y = y, alpha = alpha)) +
    geom_point(color = "red", size = 40, stroke = 0, shape = 19) +
    theme_void() +
    scale_size_identity() +
    scale_alpha_identity() +
    scale_x_continuous(limits = c(0.5, 2.5))


plot_cum
plot_mul

ggsave("cum.png", plot = plot_cum)
ggsave("mul.png", plot = plot_mul)

# Read Arrays
cum_png <- png::readPNG("cum.png")
mul_png <- png::readPNG("mul.png")

identical(cum_png[,, 4], mul_png[,, 4])


# Compare with tolerance
tolerance <- 1e-5
comparison <- abs(cum_png[,, 4] - mul_png[,, 4]) < tolerance
all(comparison) # Should be TRUE if all values are close enough

# Extract all alpha values for comparison
diff <- cum_png[,, 4] - mul_png[,, 4]
print(diff[abs(diff) > 1e-5])

diff |> as.double() |> quantile(probs = seq(0, 1, 0.05))
