pdr_add_poly_rgba <- function(pdr, invert=FALSE) {
  
  one <- 1L
  zero = 0L
  if(invert) {
    one <- 0L
    zero = 1L
  }

  result <- pdr |> 
    purrr::map(\(e) {
      xmax = e$.width
      ymax = e$.height

      v <- terra::vect(e$.polygons)

      r <- terra::rast(
        nrows = ymax,
        ncols = xmax,
        xmin = 0,
        xmax = xmax,
        ymin = 0,
        ymax = ymax
      )
      
      r <- terra::rasterize(
        v, 
        r,
        field = rep(one, length(v))
      )
      
      tmp <- terra::as.matrix(r, wide = TRUE)
      tmp[is.na(tmp)] <- zero
      storage.mode(tmp) <- "integer"
      tmp
    })

  result # return
}