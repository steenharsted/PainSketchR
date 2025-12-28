pd_geom_manage_overlaps <- function(pd) {
  # This function will take a pd data structure and for each
  # individual pain drawing merge strokes.

  # Run data sanity check ... to be completed


  pd$points <- pd$points |>
    group_by(id) |>
    group_modify(\(tos, grp_vars) {
      tos <- manage_stroke_overlaps(tos)
    })
  
  warning("We need to decide how to handle $strokes which are added/deleted from $points")
  
  return(pd)
}

manage_stroke_overlaps <- function(tos, method="merge") {
  # This function takes a tibble of strokes ('points' from a single pain drawing)
  # and manages any overlapping strokes

  # Expect only a single unique id -- i.e. points data from one and just one pain drawing
  # if(length(unique(tos$id))!=1) {tos <- tos |> dplyr::filter(id == tos[1,'id'])}

  strokes_i <- unique(tos$i) # Hold the actual identifier of the discrete strokes
  n_strokes <- length(strokes_i) # How many of them there are (this may change in the while loop)
  p1 <- 1 # pointer 1
  p2 <- 2 # pointer 2


  # If there are not at least two strokes - there can be no overlaps so just return
  if (n_strokes<2) {return(tos)}

  # This function needs to investigate each combination of pairs of strokes for overlap
  # The order of the strokes is not important, thus the runtime will be O(½n²-½n) which
  # is half of the nxn matrix. We can represent these stroke combinations with a single
  # vector of n elements if we use two pointers to iterate the vector - we will simply id
  # the strokes by their index in the list-of-strokes (los) list

  while (p1 < n_strokes) {
    while (p2 <=n_strokes) {
      # We need to check length of strokes (number of coordinates) and decide what to
      # to do with lengths of 0, 1 and 2. 

      # We could check whether bounding boxes overlap -- if no, there is no overlap, if yes there MIGHT an overlap
      a <- list(x=tos |> dplyr::filter(i==strokes_i[p1]) |> dplyr::pull(x), y=tos |> dplyr::filter(i==strokes_i[p1]) |> dplyr::pull(y))
      b <- list(x=tos |> dplyr::filter(i==strokes_i[p2]) |> dplyr::pull(x), y=tos |> dplyr::filter(i==strokes_i[p2]) |> dplyr::pull(y))

      they_intersect <- !purrr::is_empty(polyclip::polyclip(A = a, B = b, op = "intersect")) # TRUE or FALSE
      #print(paste0("n:",n_strokes," p1:",p1, " p2:",p2, " overlap:",they_intersect))
      if (they_intersect) {
        if (method=="merge") {
          # Replace p1 by the union of p1 and p2, delete p2, reset length of tos and reset p2=p1+1
          merge_result <- polyclip::polyclip(A = a, B = b, op = "union")[[1]] # Warning we assume there is a result and the first element is the relevant one
          tos <- tos |>
            dplyr::filter(i != strokes_i[p1] & i!= strokes_i[p2]) |>
            dplyr::bind_rows(dplyr::tibble(i=strokes_i[p1], x=merge_result$x, y=merge_result$y)) |>
            dplyr::arrange(i)
          strokes_i <- strokes_i[-p2] # Remove p2 
          n_strokes <- n_strokes-1
        } else if (method=="split") {
          # split overlaps into separate (typically 3) distinct polygons
        } else if (method=="adoptA") {
          # let the overlap become part of polygon A and remove from B
        } else if (method=="adoptB") {
          # let the overlap become part of polygon B and remove from A
        }
      } else {
        p2 <- p2+1
      }
    }
    p1 <- p1+1 # Advance p1
    p2 <- p1+1 # Reset p2
  }
  return(tos)
}