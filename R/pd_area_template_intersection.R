pd_area_template_intersection <- function(df, template) {
  # This functions assumes df is data frame with 3 or 4 four columns
  # (id,) stroke, x and y. Where stroke is a series of char strings
  # Each stroke should a represents a polygon.
  # We assume template is a data frame of similar structure which represents a
  # background template -- i.e. the body outline onto which the
  # paindrawing was skecthed. Instead of 'stroke' it has an identifier called
  # 'anamtomy' and x,y coordinates
  # E.g. anatomy could be `lower_leg_left_front` or `head_front`

  require(dplyr)

  # Do some sanity checks
  df_checkup <- pd_check_df(df)
  if (df_checkup!="ok") {break()}
  df <- ungroup(df)

  # If df contains id, perform function on a group-by basis
  if ("id" %in% names(df)) { df <- df |> group_by(id) }

  require(polyclip) 
  print(template)
df |> 
  group_modify(\(grp_obs, grp_var) { # grouped by 'id' (or nothing at all)
    
    result <- grp_obs[0,] # create an empty data frame of the same structure as input polygon data frame
    stroke_combinations <- expand.grid(
                                        grp_obs |> distinct(stroke) |> pull(stroke), 
                                        template |> distinct(anatomy) |> pull(anatomy)
                                      ) |> 
          as_tibble(.name_repair = ~ c("s", "a")) 
    
    if (nrow(stroke_combinations)>0){
      for (i in 1:nrow(stroke_combinations)){
        # This loop iterates (pairwise) all the possible combinations of pain drawing strokes/polygons and
        # template strokes/polygons to look for overlapping areas
        # If overlap is found, it retains their intersection as part of the result

        # Look at the stroke combinations at the pointer and get polygon coordinates    
        stroke_s <- stroke_combinations[i,'s']
        stroke_a <- stroke_combinations[i,'a']

        coordinates_s <- grp_obs |> filter(stroke == stroke_s) |> select(x,y)
        coordinates_a <- template |> filter(anatomy == stroke_a) |> select(x,y)
        polygon_overlap <- polyclip(coordinates_s,coordinates_a, "intersection")
        
        if (length(polygon_overlap)>0) {
          # There is an intersection -- now deal with it
  
          # The add the intersection of the two overlapping polygons
          if (length(polygon_overlap)==1) {
            # There is (typically) only one overlapping area
            result <- rbind(result,
                            data.frame(
                            stroke=paste0(stroke_a,"+",stroke_b), polygon_overlap[[1]]))
          } else {
            # But there could be several
            for (j in 1:length(polygon_overlap)) {
            result <- rbind(result,
                            data.frame(
                            stroke=paste0(stroke_s,"+",stroke_a,"_",j), polygon_overlap[[j]]))
            }
          }
        }
      } # end for loop
        
    } # end if       
    grp_obs <- result # return this
    
  })
}

