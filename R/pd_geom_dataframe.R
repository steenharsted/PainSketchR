pd_geom_dataframe <- function(pd) {
  # Run data sanity check ... to be completed

  list_of_df_to_df <- pd |> 
    map_dfr(function(pd_schematic) 
    { # This map function should return a simple data frame of all pd's
      df <- pd_schematic$s$p |> 
        # This map function should return a simple data frame of a single pd
        imap_dfr(function(list_of_matrices, i) {d <- as.data.frame(list_of_matrices) |> setNames(c("x", "y")) |> mutate(s=i, .before=1)}) |>
        mutate(id=pd_schematic$id, .before=1) |>
        mutate(w=pd_schematic$w) |>
        mutate(h=pd_schematic$h) 
      pd_schematic <- df        
    })
  list_of_df_to_df
}

