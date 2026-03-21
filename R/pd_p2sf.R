pd_p2sf <- function(p, buffer=5, rm_selfintersection=TRUE, merge_overlaps=FALSE, rm_points=FALSE, rm_lines=FALSE) {
  # This function takes the 'p' column of valid pain drawing data structures
  # and converts it into a sf object - specifically a list of multipolygons
  # It should typically be used as `mutate(sf_col = p2sf(p))` 

  # Internal helper function to close polygons, if they're open
  close_poly <- function(m) {
    if(all(m[1,]==m[nrow(m),])) { m } else { rbind(m, m[1,]) }
  }

  # The 'p' column may contain points and/or straight (1-dimensional) line segments of
  # two coordinate points. These can not be coerced into valid sf polygons. We should thus 
  # either handle 'p' as an sf geometry collections or   # manually fix such issues first - we do the later:
  p <- p |> 
    purrr::map(\(df) {
      if (!tibble::is_tibble(df)) {
        NA
      } else {
      df |>
        dplyr::group_by(i) |>
        dplyr::group_modify(\(dfgr, indx) {
          if(nrow(dfgr)==1) {
            # It is a point -- convert to sf object point and buffer it before recasting to tibble .. or NA
            if (rm_points) {
              NA
            } else {
              sf::st_point(c(dfgr$x, dfgr$y)) |> 
              sf::st_buffer(dist = buffer, nQuadSegs = 16) |>
              sf::st_coordinates() |>
              tibble::as_tibble() |>
              select(x=X, y=Y)  
            }        
          } else if (nrow(dfgr)==2) {
            # It is a line -- convert to sf object line and buffer it before recasting to tibble .. or NA
            if (rm_lines) {
              NA
            } else {
              sf::st_linestring(matrix(c(dfgr$x, dfgr$y), ncol=2)) |>
              sf::st_buffer(dist = buffer, nQuadSegs = 16) |>
              sf::st_coordinates() |>
              tibble::as_tibble() |>
              select(x=X, y=Y)  
            }
          } else {
            # It is a polygon with 3+ vertices
            dfgr
          }
        }) # End group_modify
      } # End if (!is_tibble)
    }) # End map

  # The variable 'p' now contains only NA values or tibbles (i,x,y) with more than 2 coordinates  
  # The following creates an sf_multipolygon object from pain drawing data (col 'p')
  sf_p <- p |>
    purrr::map(\(df) {
      if(!tibble::is_tibble(df)) { # Should we accept data frames as well?
        NA
      } else {
        df |>
          dplyr::group_by(i) |> 
          dplyr::group_modify(\(p, indx) {close_poly(p)}) |> # sf package expects polygons to be closed
          tidyr::nest() |> # Now split the data frame into list elements, by grouping variable 
          dplyr::pull(data) |> # 'data' is a new col created by nest function -- it holds non-grouping variables (x,y)
          purrr::map(\(a_tib) {as.matrix(a_tib, ncol=2, byrow=FALSE)}) |> # 'data' col is list of matrices 
          list() |> # now a list of lists of matrices
          sf::st_multipolygon() # Finally create sf object
      }
    }) 
  # sf is now a list of lists (of length 1) of sf_multipolygon objects
  
  # Polygons may include self-intersections which produce negative areas. We remove any self-intersections
  # by zero-length buffering
  # if (rm_selfintersection) {
  #   sf_p <- sf_p |> 
  #     purrr::map(\(pd_poly) {
  #       if(!is.na(pd_poly)) {
  #         sf::st_boundary(pd_poly)    
  #       } else {
  #         NA
  #       }      
  #     }) 
  # }
  
  sf_p
  }