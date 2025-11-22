pd_area_close <- function(df) {
  df_checkup <- pd_check_df(df)
  if (df_checkup!="ok") {abort(message=df_checkup)}
  #require(geometry)

  # If df contains id and/or stroke, perform function on a group-by basis
  if ("stroke" %in% names(df)) { df <- df |> group_by(stroke) }
  if (all(c("id","stroke") %in% names(df))) { df <- df |> group_by(id, stroke) }

  df |>
    group_modify(\(grp_obs, grp_var) {
      first_row <- grp_obs |> filter(row_number()==1)
      last_row <- grp_obs |> filter(row_number()==nrow(grp_obs))
      if (identical(first_row, last_row)) {
        grp_obs
      } else {
        # add first coordinate point to the end
        grp_obs |> rows_append(first_row)
      }
    })
}
