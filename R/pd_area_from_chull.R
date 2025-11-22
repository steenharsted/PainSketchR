pd_area_from_chull <- function(df) {
  df_checkup <- pd_check_df(df)
  if (df_checkup!="ok") {abort(message=df_checkup)}
  #require(geometry)

  # If df contains id and/or stroke, perform function on a group-by basis
  if ("stroke" %in% names(df)) { df <- df |> group_by(stroke) }
  if (all(c("id","stroke") %in% names(df))) { df <- df |> group_by(id, stroke) }
  
  df |> 
    group_modify(\(grp_obs, grp_vas) {
      chull_indices <- chull(grp_obs |> ungroup() |> select(x,y) |> as.matrix())
      grp_obs |> filter(row_number() %in% chull_indices) |> # reduced to points on chull
        as_tibble() |> pd_area_close()
    }) |> ungroup()
}