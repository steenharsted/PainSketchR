pdr_poly_manage_overlaps <- function(p, method="intersection") {
  # This function takes the column p from a pain drawing data structure and for each pain drawing (row)
  # it handles any overlapping polygons

  if (!is.list(p)) {stop("The data 'p' is not a list of tibbles")}  

  p <- p |> # p is a list of tibbles (of x,y,i)
    purrr::map(\(df, i_df) {
      if(!tibble::is_tibble(df)) { # Should we accept data frames as well?
        NA
      } else {
        # We have coordinates in the data frame, but how many strokes        
        if (length(unique(df$i))<2) {
          # There is only a single stroke in data frame - thus no overlaps
          df # ..so just stick with current data frame
        } else {
          # There are multiple strokes in data frame - check for overlaps in external function          
          loop_pairwise(df, method) # Handle the overlaps pairwise
        }
      }
    })
  
  # Fix column 's' ? When we merge polygons, strokes are deleted ... this is not trivial! What if stroke 1 and 2 have different pen size and colour?
}

loop_pairwise <- function(df, method="intersection") {
  i_strokes <- unique(df$i) # Hold the actual identifiers of the discrete strokes
  n_strokes <- length(i_strokes) # How many of them there are (this may change in the while loop)
  p1 <- 1 # pointer 1
  p2 <- 2 # pointer 2

  # These loops will iterate each combination of pairs of strokes for overlap - merging as we go
  # The order of the strokes is not important, thus the runtime will be O(½n²-½n) which
  # is half of the nxn matrix. We can represent these stroke combinations with a single
  # vector of n elements if we use two pointers to iterate the vector - we will simply id
  # the strokes by their index in the list-of-strokes (los) list
  
  while (p1 < n_strokes) { # Loop all polygons as the first polygon in pairwise comparison
    while (p2 <= n_strokes) { # ..for each, loop remaining polygons as the second polygon in pairwise comparison
      # print(
      #   paste0(
      #     "There are ",n_strokes," strokes. P1 is ",p1," and points to ", i_strokes[p1],". P2 is ",p2," and points to ", i_strokes[p2],". All remaining strokes are: ", paste0(i_strokes, collapse=",")
      #   )
      # )
      a <- df |> dplyr::filter(i==i_strokes[p1]) # coordinates of first polygon
      b <- df |> dplyr::filter(i==i_strokes[p2]) # coordinates of second polygon

      # The following will return a df of x and y polygon coordinates (one for each intersection)
      if (method=="intersection") {
        intersection <- pd_poly_clip(a, b, op = "intersection") 
        they_overlap <- nrow(intersection)>1 # TRUE or FALSE -- intersection is false if no area to overlap (e.g. overlapping vertices)
      } else if (method=="union") {
        intersection <- pd_poly_clip(a, b, op = "union") 
        they_overlap <- length(unique(intersection$i))==1 # If no overlap: 2
      }
      
      if (they_overlap) {
        if (method=="intersection" | method=="union") {
          # Replace p1 by the union of p1 and p2, delete p2, reset length of strokes and reset p2=p1+1
          merge_result <- pd_poly_clip(a, b, op = "union") |> 
            dplyr::filter(i==1) # Use only first polygon, the rest are holes
          df <- df |>
            dplyr::filter(i != i_strokes[p1] & i != i_strokes[p2]) |> # Remove p1 and p2 from pd1 points data frame
            dplyr::bind_rows(dplyr::tibble(i=i_strokes[p1], x=merge_result$x, y=merge_result$y)) # Add the merge result as p1 in the pd1 points data frame
          #print(paste0("Merged stroke ", i_strokes[p1] ," and ", i_strokes[p2]," with ",nrow(a), " and ",nrow(b), " points respectively into a new strokes with ",df|>dplyr::filter(i==i_strokes[p1])|>nrow()," points"))
          i_strokes <- i_strokes[-p2] # Remove p2 from vector of stroke indices we are iterating          
          n_strokes <- n_strokes-1
          p2 <- p1+1
        } else {
          # The method is not 'merge', but something else (e.g. split) -- not yet implemented
        }
      } else { 
        # if the do not overlap just move to the next stroke polygon
        p2 <- p2+1
      }
    } # end of inner loop (p2)
    p1 <- p1+1 # Advance p1
    p2 <- p1+1 # Reset p2
  } # end of out loop (p1)
    
    return(df)
}
