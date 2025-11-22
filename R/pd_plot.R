pd_plot <- function(df) {
  # Add a parameter to determine whether to split into multiple facets
  # when data contains multiple ids?

  require(ggplot2)
  p <- ggplot(df, aes(x=x,y=y,colour=as.factor(stroke))) + geom_path() # geom_polygon(fill=NA) 

  # If df contains id, create a plot facet for each pain drawing
  if ("id" %in% names(df)) { p <- p + facet_wrap(~id) }
  p
}
