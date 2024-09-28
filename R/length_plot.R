sm_length_plot_base <- function(ylab = NULL) {
  ggplot2::ggplot() +
    ggplot2::theme_grey(base_size = 14) +
    ggplot2::labs(x = NULL, y = ylab) +
    ggplot2::scale_x_continuous(breaks = seq(10, 200, by = 20))
}


#' Length frequency plot
#'
#' @param d A length dataframe
#' @param ylab Y-axis label
#'
#' @return a ggplot
#' @export
#'
sm_length_plot <- function(d, ylab = "xx") {
  
  d <- 
    d |> 
    dplyr::select(ar, lengd, y) |> 
    tidyr::drop_na()
  if(!any(d$lengd %% 1 != 0)) {
    d <- 
      d |> 
      tidyr::complete(ar, lengd = tidyr::full_seq(lengd, 1),
               fill = list(y = 0)) 
  }
  sm_length_plot_base(ylab) +
    ggplot2::geom_ribbon(data = 
                           d |> 
                           dplyr::group_by(lengd) |> 
                           dplyr::reframe(y = stats::median(y)),
                         ggplot2::aes(lengd, ymax = y, ymin = 0), fill = "grey") +
    ggplot2::geom_line(data = d,
                       ggplot2::aes(lengd, y)) +
    ggplot2::facet_wrap(. ~ ar, dir = "v", ncol = 3)
}

