
#' @importFrom ggplot2 ggplot element_text rel
fun_2d_plot<-function(datin,colors,title.name,group.name){

  p2<-ggplot2::ggplot(datin, ggplot2::aes(x=datin[,'V1'], y=datin[,'V2'],color=datin[,'group'])) +
    ggplot2::geom_point(size=0.5)+
    ggplot2::labs(x="First SDR Direction",y="Second SDR Direction",color='Group',
         title=title.name)+
    ggplot2::theme_bw()+
    ggplot2::theme(legend.direction = 'horizontal',
          legend.position = 'top',
          plot.title = ggplot2::element_text(size = ggplot2::rel(1)))
  if (is.null(colors)){
    p2<-p2+ggsci::scale_color_jama()
  } else{
    p2<-p2+ggplot2::scale_color_manual(values=colors)
  }
  p2
}

