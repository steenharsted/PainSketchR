pd_line2area <- function(df, delta=1) {
  df_checkup <- pd_check_df(df)
  if (df_checkup!="ok") {abort(message=df_checkup)}

  # If df contains id and/or stroke, perform function on a group-by basis
  if ("stroke" %in% names(df)) { df <- df |> group_by(stroke) }
  if (all(c("id","stroke") %in% names(df))) { df <- df |> group_by(id, stroke) }

  df |>
    group_modify(\(grp_obs,grp_var) {
      if(nrow(grp_obs) == 2) {
        # It's a line ! (only two rows of x,y coordinates)
        # Change to 4 points (first and last are identifcal to close polygon)
        angle <- atan2(
          grp_obs[[2,'y']]-grp_obs[[1,'y']],
          grp_obs[[2,'x']]-grp_obs[[1,'x']]) # line angle in rads
        tibble(
          x=c(
            grp_obs[[1,'x']]+delta*cos(angle + 0.5*pi),
            grp_obs[[1,'x']]+delta*cos(angle - 0.5*pi),
            grp_obs[[2,'x']]+delta*cos(angle - 0.5*pi),
            grp_obs[[2,'x']]+delta*cos(angle + 0.5*pi),
            grp_obs[[1,'x']]+delta*cos(angle + 0.5*pi)),
          y=c(
            grp_obs[[1,'y']]+delta*sin(angle + 0.5*pi),
            grp_obs[[1,'y']]+delta*sin(angle - 0.5*pi),
            grp_obs[[2,'y']]+delta*sin(angle - 0.5*pi),
            grp_obs[[2,'y']]+delta*sin(angle + 0.5*pi),
            grp_obs[[1,'y']]+delta*sin(angle + 0.5*pi))
        )
      } else {
        grp_obs
      }
    }) |> ungroup()
}
