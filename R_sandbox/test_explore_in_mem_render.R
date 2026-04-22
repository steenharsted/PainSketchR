### 2026 04 21
# Exploring in memory render. We should be able to do this using unigd::ugd_render()
# First we need to be able to 100% recreate the ggsave output using png() og ugd_save

# library(ggplot2)
# a <- penguins |>
#   ggplot(aes(x = bill_len, y = bill_dep, color = species)) +
#   geom_point()

# # Save with base
# png("test_base.png", width = 600, height = 400, units = "px")
# print(a)
# dev.off()

# # Save with ggsave
# ggsave("test_ggsave.png", a, width = 600, height = 400, units = "px")

# # WHY ARE THE OUTPUTS SO DIFFERENT?

# ####
# # ggsave default: 300 dpi, so 600px / 300dpi = 2 inches wide
# png("test_base_match.png", width = 600, height = 400, units = "px", res = 300)
# print(a)
# dev.off()

# ragg::agg_png(
#   "test_ragg_match.png",
#   width = 600 / 300, # inches
#   height = 400 / 300, # inches
#   res = 300,
#   units = "in"
# )
# a
# dev.off()

# # in memeory render

# arr <- ragg::agg_capture(width = 600, height = 400, units = "px", res = 300)
# a
# cap <- arr() # chr matrix of hex/named colours
# dev.off()

# # Convert colour names → integer RGBA → numeric [0,1] array
# rgba_int <- col2rgb(cap, alpha = TRUE) # 4 × (600*400) matrix, values 0-255
# rgba_arr <- array(
#   t(rgba_int) / 255, # normalise to [0,1]
#   dim = c(400, 600, 4) # height × width × RGBA
# )

# rgba_arr

# grid::grid.newpage()
# rgba_arr |> grid::grid.raster()

library(tidyverse)
library(devtools)
load_all()

# data
pd <- pd_import_json(c(
  "data-raw/two_geoms.json",
  "data-raw/four_geoms.json",
  "data-raw/test_spray.json",
  "data-raw/test_spray_bw.json",
  "data-raw/Anon_pen_spray.json"
))

set.seed(1)
# memory
raster_mem <- pd_to_png_single_in_mem(pd[3, ])
grid::grid.newpage()
grid::grid.raster(raster_mem)

set.seed(1)
# original
raster_org <- pd_to_png_single(pd[3, ])
grid::grid.newpage()
grid::grid.raster(raster_org)

str(raster_mem)
str(raster_org)
identical(raster_mem, raster_org)


# Test recreate_in_mem()

background_image <- png::readPNG("inst/extdata/feet_background.png")


# No background image
pd |>
  dplyr::filter(id == "Anon") |>
  pd_recreate_drawing_in_mem()

# Background image
pd |>
  dplyr::filter(id == "Anon") |>
  pd_recreate_drawing_in_mem(
    background_image = background_image
  )

# Background image no rasterize
pd |>
  dplyr::filter(id == "Anon") |>
  pd_recreate_drawing_in_mem(
    background_image = "inst/extdata/feet_background.png",
    rasterize = TRUE
  )

# Multiple drawings
pd |>
  pd_recreate_drawing_in_mem(
    background_image = background_image
  )

pd |>
  pd_recreate_drawing(
    background_image = background_image
  )


pd |>
  pd_recreate_drawing_in_mem(
    background_image = background_image,
    rasterize = FALSE
  )


pd |>
  pd_recreate_drawing_in_mem(
    background_image = background_image,
    clean_up = FALSE,
    dpi = 300
  )

pd |>
  pd_recreate_drawing_in_mem(
    background_image = background_image,
    clean_up = FALSE,
    dpi = 96
  )


### Benchmark
bench::mark(
  file_based = {
    set.seed(1)
    pd |> pd_recreate_drawing(background_image = background_image)
  },
  in_memory = {
    set.seed(1)
    pd |> pd_recreate_drawing_in_mem(background_image = background_image)
  },
  iterations = 10,
  check = FALSE,
  memory = FALSE
)

### Output on Windows labtop 2026_04_22
# A tibble: 2 × 13
#   expression      min   median `itr/sec` mem_alloc `gc/sec` n_itr  n_gc total_time result memory
#   <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl> <int> <dbl>   <bch:tm> <list> <list>
# 1 file_based 741.32ms 741.32ms     1.35         NA    12.1      1     9   741.32ms <NULL> <NULL>
# 2 in_memory     1.54s    1.58s     0.633        NA     2.53     2     8      3.16s <NULL> <NULL>
# # ℹ 2 more variables: time <list>, gc <list>

#### LETS BENCHMARK pd_to_png() file vs mem
## SIGNLE ROWS

bench::mark(
  file_based = {
    set.seed(1)
    pd_to_png_single(pd[3, ])
  },
  in_memory = {
    set.seed(1)
    pd_to_png_single_in_mem(pd[3, ])
  },
  iterations = 20,
  check = FALSE,
  memory = FALSE
)

# Output of Linux machines 2026_04_22
# A tibble: 2 × 13
#   expression      min   median `itr/sec` mem_alloc `gc/sec` n_itr  n_gc total_time result memory time            gc
#   <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl> <int> <dbl>   <bch:tm> <list> <list> <list>          <list>
# 1 file_based   54.3ms   55.8ms      17.1        NA     14.0    11     9      642ms <NULL> <NULL> <bench_tm [20]> <tibble [20 × 3]>
# 2 in_memory    57.4ms   58.8ms      16.8        NA     16.8    10    10      596ms <NULL> <NULL> <bench_tm [20]> <tibble [20 × 3]>

bench::mark(
  file_based = {
    set.seed(1)
    pd[1,] |> pd_to_png_single()
  },
  in_memory = {
    set.seed(1)
    pd[1,] |> pd_to_png_single_in_mem()
  },
  iterations = 20,
  check = FALSE,
  memory = FALSE
)

## RESULTS FROM WINDOWS 2026_04_22
# A tibble: 2 × 13
#   expression    min median `itr/sec` mem_alloc `gc/sec` n_itr  n_gc total_time result memory time       gc      
#   <bch:expr> <bch:> <bch:>     <dbl> <bch:byt>    <dbl> <int> <dbl>   <bch:tm> <list> <list> <list>     <list>  
# 1 file_based  124ms  128ms      7.66        NA     4.13    13     7       1.7s <NULL> <NULL> <bench_tm> <tibble>
# 2 in_memory   129ms  136ms      7.30        NA     4.87    12     8      1.64s <NULL> <NULL> <bench_tm> <tibble>

pd <- pd_import_json(c(
  "data-raw/two_geoms.json",
  "data-raw/four_geoms.json",
  "data-raw/test_spray.json",
  "data-raw/test_spray_bw.json",
  "data-raw/Anon_pen_spray.json"
))

bench::mark(
  file_based = {
    set.seed(1)
    pd |> mutate(.png = pd_to_png(pd))
  },
  in_memory = {
    set.seed(1)
    pd |> mutate(.png = pd_to_png_in_mem(pd))
  },
  iterations = 20,
  check = FALSE,
  memory = FALSE
)

# output on Linux 2026_04_22
# A tibble: 2 × 13
#   expression      min   median `itr/sec` mem_alloc `gc/sec` n_itr  n_gc total_time result memory time            gc
#   <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl> <int> <dbl>   <bch:tm> <list> <list> <list>          <list>
# 1 file_based    275ms    286ms      3.34        NA     6.17    20    37      5.99s <NULL> <NULL> <bench_tm [20]> <tibble [20 × 3]>
# 2 in_memory     315ms    326ms      2.89        NA     6.35    20    44      6.93s <NULL> <NULL> <bench_tm [20]> <tibble [20 × 3]>

# output on Windows 2026_04_22
# A tibble: 2 × 13
#   expression      min   median `itr/sec` mem_alloc `gc/sec` n_itr  n_gc total_time result memory
#   <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl> <int> <dbl>   <bch:tm> <list> <list>
# 1 file_based 746.12ms 770.25ms     1.29         NA     2.64    20    41      15.5s <NULL> <NULL>
# 2 in_memory     1.05s    1.08s     0.922        NA     1.89    20    41      21.7s <NULL> <NULL>
