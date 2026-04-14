pd <- pd_json2pd(c("data-raw/two_geoms.json", "data-raw/four_geoms.json"))
pd <- pd |> pd_add_png()

# Alpha is easy to extract now

pd$.png[[1]][,, 4]

dim(pd$.png[[1]]) # Should show [height, width, 4]

# add a lsit column?
pd |>
  mutate(
    alpha = purrr:::map(.x = .png, .f = ~ .x[,, 4] |> as.double()),

    # Values for each
    alpha_decile = purrr:::map(
      .x = .png,
      .f = ~ .x[,, 4] |> as.double() |> quantile(probs = seq(0, 1, 0.1))
    ),

    # should it be
  )


image(pd$.png[[1]][,, 4], col = gray(seq(0, 1, length.out = 100)))
