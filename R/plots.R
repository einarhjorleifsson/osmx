#' Length frequency plot
#'
#' @param d A length dataframe
#'
#' @return a ggplot
#' @export
#'
sm_length_plot <- function(d) {
  
  var <- unique(d$var)
  my.lab <- dplyr::case_when(var == "n" ~ "Fjoldi i hverju lengdarbili",
                             var == "b" ~ "Thyngd [kg] i hverju lengdarbili",
                             .default = "what")
  d <- 
    d |> 
    dplyr::select(ar, lengd, val) |> 
    tidyr::drop_na()
  
  # bin data if all are integers (so all but shrimp and capelin)
  if(!any(d$lengd %% 1 != 0)) {
    d <- 
      d |> 
      tidyr::complete(ar, lengd = tidyr::full_seq(lengd, 1),
                      fill = list(val = 0)) 
  }
  
  ggplot2::ggplot() +
    ggplot2::theme_bw(base_size = 14) +
    ggplot2::geom_ribbon(data = 
                           d |> 
                           dplyr::group_by(lengd) |> 
                           dplyr::reframe(val = stats::median(val)),
                         ggplot2::aes(lengd, ymax = val, ymin = 0), fill = "grey") +
    ggplot2::geom_line(data = d,
                       ggplot2::aes(lengd, val)) +
    ggplot2::facet_wrap(~ ar, dir = "v", ncol = 3, strip.position = "right") +
    ggplot2::labs(x = NULL, y = my.lab) +
    ggplot2::scale_x_continuous(breaks = seq(10, 200, by = 20)) +
    ggplot2::guides(x = ggplot2::guide_axis(n.dodge = 2))
  
  
}



sm_bubble_plot_base <- function(xlim = c(-30, -10), ylim = c(62.5, 67.5)) {
  ggplot2::ggplot() +
    ggplot2::theme_grey(base_size = 16) +
    ggplot2::geom_path(data = geo::island, ggplot2::aes(lon, lat)) +
    ggplot2::scale_size_area(max_size = 30) +
    ggplot2::coord_quickmap(xlim = xlim,
                            ylim = ylim)
}

#' Plot abundance or biomass
#'
#' @param d A tibble containing lon, lat, abundance an biomass
# '
#' @return a plot
#' @export
#'
sm_bubble_plot <- function(d) { 
  
  var <- unique(d$var)
  zlab <- dplyr::case_when(var == "n" ~ "Stykki",
                           var == "b" ~ "Afli [kg]",
                           .default = "what")
  
  sm_bubble_plot_base() +
    ggplot2::geom_point(data = d,
                        ggplot2::aes(lon, lat, size = val),
                        alpha = 0.25, colour = "red") +
    ggplot2::labs(x = NULL, y = NULL, size = zlab) +
    ggplot2::facet_wrap(~ ar, nrow = 3)
  
}

#' Plott bootstap results
#'
#' @param d A tibble
#'
#' @return a plot
#' @export
#'
sm_boot_plot <- function(d) {
  
  var <- unique(d$var)
  ylab <- dplyr::case_when(var == "n" ~ "Fjoldi i togi",
                           var == "b" ~ "Afli [kg] i togi",
                           .default = "what")
  
  d |> 
    ggplot2::ggplot() +
    ggplot2::theme_bw(base_size = 16) +
    ggplot2::geom_pointrange(ggplot2::aes(ar, mean, ymin = lower.ci, ymax = upper.ci)) +
    ggplot2::scale_x_continuous(breaks = seq(1985, 2050, by = 5)) +
    ggplot2::expand_limits(y = 0) +
    ggplot2::labs(x = NULL, y = ylab) +
    ggplot2::guides(x = ggplot2::guide_axis(n.dodge = 2))
  
  
}
