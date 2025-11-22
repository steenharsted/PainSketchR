pd_area_from_point <- function(df, delta=1) {
  df_checkup <- pd_check_df(df)
  if (df_checkup!="ok") {abort(message=df_checkup)}

  require(dplyr)
  
  # If df contains id and/or stroke, perform function on a group-by basis
  if ("stroke" %in% names(df)) { df <- df |> group_by(stroke) }
  if (all(c("id","stroke") %in% names(df))) { df <- df |> group_by(id, stroke) }

  require(dplyr)
  df |> 
    group_modify(\(grp_obs,grp_var) {
      if(nrow(grp_obs) == 1) {
        # It's a point ! (only one row of x,y coordinates)
        # change to four points at x,y +/- delta
        tibble(
          x = grp_obs |> pull(x) + c(-delta,delta,delta,-delta,-delta),
          y = grp_obs |> pull(y) + c(-delta,-delta,delta,delta,-delta)
        )
      } else {
        grp_obs
      }
    }) |> ungroup()
}
