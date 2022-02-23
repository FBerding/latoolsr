#'Create plot of filter values
#'
#'Function for creating a visualization of the relevant filter values.
#'
#' @param trained_learner Trained learner for predicting the data. The object
#' must be generated via the function \code{\link{ai_train}}.
#' @return Returns a plot of the relevant values.
#' @export
create_filter_plot<-function(trained_learner){
  data_set<-trained_learner$filter_summary$Scores
  filter_plot<-ggplot2::ggplot(data=data_set)+
    ggplot2::geom_ribbon(ggplot2::aes(x=data_set$Rank,ymin=data_set$LCI95,ymax=data_set$UCI95),fill="blue",alpha=.5)+
    ggplot2::geom_line(ggplot2::aes(x=data_set$Rank,y=data_set$Mean))+
    ggplot2::geom_line(ggplot2::aes(x=data_set$Rank,y=data_set$LCI95))+
    ggplot2::geom_line(ggplot2::aes(x=data_set$Rank,y=data_set$UCI95))+
    ggplot2::geom_vline(ggplot2::aes(xintercept=trained_learner$n_input_variables),size=1)+
    ggplot2::labs(x="Rank of Input Variable",y="Filter Score",
         title = paste("Filter", trained_learner$filter_summary$Filter,"with",trained_learner$n_iteration,"Iterations and Sample Size of",trained_learner$n_p_train)
    )
  return(filter_plot)
}

#'Create plot of performance values
#'
#'Function for creating a visualization of Iota values of a learner.
#'
#' @param trained_learner Trained learner for predicting the data. The object
#' must be generated via the function \code{\link{ai_train}}.
#' @return Returns a plot of the relevant values.
#' @export
create_performance_plot<-function(trained_learner){
  plot_data_performance<-trained_learner$reliability$iota1
  plot_data_performance["Category_Number"]<-seq(1:nrow(plot_data_performance))

  performance_plot<-ggplot2::ggplot(data=plot_data_performance)+
    ggplot2::geom_rect(ggplot2::aes(fill="Not Reliable",xmin=1,xmax=(nrow(plot_data_performance)+0.9),ymin=0,ymax=.377),alpha=0.2)+
    ggplot2::geom_rect(ggplot2::aes(fill="Minimal Reliable", xmin=1,xmax=(nrow(plot_data_performance)+0.9),ymin=.377,ymax=.478),alpha=0.2)+
    ggplot2::geom_rect(ggplot2::aes(fill="Reliable", xmin=1,xmax=(nrow(plot_data_performance)+0.9),ymin=.478,ymax=1),alpha=0.2)+

    ggplot2::geom_rect(ggplot2::aes(xmin=plot_data_performance$Category_Number,xmax=(plot_data_performance$Category_Number+0.9),ymin=plot_data_performance$LCI95,ymax=plot_data_performance$UCI95))+
    ggplot2::geom_segment(ggplot2::aes(x=plot_data_performance$Category_Number,xend=(plot_data_performance$Category_Number+0.9),y =plot_data_performance$Mean,yend = plot_data_performance$Mean),size=1)+

    ggplot2::coord_cartesian(ylim=c(0,1),xlim=c(1,nrow(plot_data_performance)+0.9))+
    ggplot2::labs(y="Iota 1",x=trained_learner$category_Name,title="Estimated Iota 1 for Each Category")+
    ggplot2::scale_fill_manual(values=c("Reliable"="green","Minimal Reliable"="yellow","Not Reliable"="red" ),
                      name="Reliability",
    )+
    ggplot2::scale_x_continuous(breaks = c(seq(1:nrow(plot_data_performance))),
                       labels=plot_data_performance$Label)

  return(performance_plot)
}
