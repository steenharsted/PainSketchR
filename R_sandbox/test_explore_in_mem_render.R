### 2026 04 21
# Exploring in memory render. We should be able to do this using unigd::ugd_render()
# First we need to be able to 100% recreate the ggsave output using png() og ugd_save

library(ggplot2)
library(unigd)

a <- penguins |>
  ggplot(aes(x = bill_len, y = bill_dep, color = species)) +
  geom_point()

# Save with base
png("test_base.png", width = 600, height = 400, units = "px")
print(a)
dev.off()

# Save with ggsave
ggsave("test_ggsave.png", a, width = 600, height = 400, units = "px")

# Save with unigd
ugd()
a
ugd_save(file = "test_ugd.png", width = 600, height = 400, as = "png")
dev.off()

# WHY ARE THE OUTPUTS SO DIFFERENT?

####
# ggsave default: 300 dpi, so 600px / 300dpi = 2 inches wide
png("test_base_match.png", width = 600, height = 400, units = "px", res = 300)
print(a)
dev.off()


ragg::agg_png(
  "test_ragg_match.png",
  width = 600 / 300, # inches
  height = 400 / 300, # inches
  res = 300,
  units = "in"
)
a
dev.off()


ugd()
a
ugd_save(
  file = "test_ugd_match.png",
  width = 600,
  height = 400,
  zoom = 300 / 96, # ~3.125 — matches ragg's 300 dpi rendering
  as = "png"
)
dev.off()


# Physical size in ugd pixels: 2in * 96dpi = 192 x 128
# zoom = 300/96 scales that up to 600x400 output pixels
ugd(width = 192, height = 128)
a
ugd_save(
  file = "test_ugd_match.png",
  width = 600,
  height = 400,
  zoom = 300 / 96,
  as = "png"
)
dev.off()


# Open ugd at full output resolution, no zoom
ugd(width = 600, height = 400)
a
ugd_save(
  file = "test_ugd_match.png",
  width = 600,
  height = 400,
  zoom = 1,
  as = "png"
)
dev.off()


# in memeory render
ugd()
a
a_render <- ugd_render(
  width = 600,
  height = 400,
  zoom = 300 / 96, # ~3.125 — matches ragg's 300 dpi rendering
  as = "png"
)
dev.off()


a_render_png <- png::readPNG(a_render)

grid::grid.newpage()
a_render_png |> grid::grid.raster()


arr <- ragg::agg_capture(width = 600, height = 400, units = "px", res = 300)
a
cap <- arr() # chr matrix of hex/named colours
dev.off()

# Convert colour names → integer RGBA → numeric [0,1] array
rgba_int <- col2rgb(cap, alpha = TRUE) # 4 × (600*400) matrix, values 0-255
rgba_arr <- array(
  t(rgba_int) / 255, # normalise to [0,1]
  dim = c(400, 600, 4) # height × width × RGBA
)

rgba_arr

grid::grid.newpage()
rgba_arr |> grid::grid.raster()


# data
pd <- pd_json2pd(c(
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
