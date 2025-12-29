#' Generate a simple pain drawing plot
#'
#' This function is a simple wrapper of the ggplot function with pain drawing points represented as geom_polygon in separate colours (alpha=0.5) and
#' each separate pain drawing presented as a wrapped facet.
#' 
#' @param pd A valid pain drawing data structure -- see [pd_check] for more detail.
#'
#' @returns A ggplot2 plot
#'
#' @export
#' @examples
#' pd_geom_plot(my_paindrawings)
pd_geom_plot <- function(pd) {
  # We could add some more paramters to this function (e.g alpha, how to facet, etc)
  # ...or just leave it up to the user to create their own ggplots from pain drawing data structure

  pd$points |> 
    ggplot2::ggplot(aes(x=x,y=y,group=as.factor(i), fill=as.factor(i))) +  geom_polygon(alpha=0.5) + facet_wrap(~id)  
}
