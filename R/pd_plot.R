pd_geom_plot <- function(pd) {
  # We could add some more paramters to this function (e.g alpha, how to facet, etc)
  # ...or just leave it up to the user to create their own ggplots from pain drawing data structure

  pd$points |> 
    ggplot2::ggplot(aes(x=x,y=y,group=as.factor(i), fill=as.factor(i))) +  geom_polygon(alpha=0.5) + facet_wrap(~id)  
}
