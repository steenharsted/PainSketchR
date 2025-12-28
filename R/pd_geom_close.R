pd_geom_close <- function(pd) {
  # Run data sanity check ... to be completed

  require(purrr)
  pd |> map(function(pd_schematic) 
  { # This chunk must return a valid pd element, i.e. a paindrawing schematic
    pd_schematic$s$p <- pd_schematic$s$p |> map(function(p) 
    { # This chunk must return a valid p element i.e an n⏺2 matrix
      first_x <- p[[1,1]]
      first_y <- p[[2,1]]
      last_x <- p[[nrow(p),1]]
      last_y <-p[[nrow(p),2]]
      if (first_x == last_x && first_y==last_y) {
        p
      } else {
        # add first coordinate point to the end
        p <- rbind(p, c(first_x, first_y))
      } # end if
    }) # end map
    pd_schematic
  }) # end map
}
