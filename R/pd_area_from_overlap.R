pd_overlap_resolve <- function(df) {
  # This functions assumes df is data frame with 3 or 4 four columns
  # (id,) stroke, x and y. Where stroke is a series of integers from
  # 1 to n - the number of strokes. Each stroke should
  # represents a polygon -- we need to test each combination of
  # polygons (strokes) and remove the overlap if there is any
  # overlap -- most of this can be handled by polyclip

  # Do some sanity checks
  df_checkup <- pd_check_df(df)
  if (df_checkup!="ok") {abort(message=df_checkup)}
  df <- ungroup(df)

  # Do a sanity check for stroke? ..should be 1:n -- i.e. all number 1 to total n of strokes
  # OR SHOULD STROKES BE A FACTOR RATHER THAN AN INTEGER WITH THE 1:n ASSUMPTION ?

  # If df contains id, perform function on a group-by basis
  if ("id" %in% names(df)) { df <- df |> group_by(id) }

  require(polyclip)
  require(dplyr)

df |> 
  group_modify(\(grp_obs, grp_var) { # already grouped by 'id' (or nothing at all)
    # This defines the pairwise combinations of polygons to check for overlap
    poly_combinations <- combn(grp_obs |> pull(stroke), 2) |> t() |> as_tibble() |> setNames(c("a","b"))
    if (nrow(poly_combinations)==0) { break }

    repeat {
      # Treat pair_of_polys as a stack and pop off the first row
      pair_of_polys <- poly_combinations |> filter(row_number() == 1)
      poly_combinations <- poly_combinations |> filter(row_number() != 1)
      
      coordinates_a <- grp_obs |> filter(stroke == pair_of_polys[,'a']) |> select(x,y)
      coordinates_b <- grp_obs |> filter(stroke == pair_of_polys[,'b']) |> select(x,y)
      polygon_overlap <- polyclip(coordinates_a,coordinates_b, "intersection")

      if (length(polygon_overlap)>0) {
        # There is an overlap -- deal with it 
        # Incomplete! ..remove all elements from poly_combos which have been dealth with already?

        # Remove the overlap from a and b and add the overlap as a new polygon
        grp_obs <- grp_obs |> filter(stroke != pair_of_polys[,'a'] & stroke != pair_of_polys[,'b']) 
        grp_obs <- bind_rows(stroke = pair_of_polys[,'a'], polyclip(coordinates_a, coordinates_b, "minus") |> flatten() |> as.data.frame())
        grp_obs <- bind_rows(stroke = pair_of_polys[,'b'], polyclip(coordinates_b, coordinates_a, "minus") |> flatten() |> as.data.frame())
        grp_obs <- bind_rows(stroke = )
        
      } else {
        grp_obs
      }
    }
    grp_obs # return this
  })
}