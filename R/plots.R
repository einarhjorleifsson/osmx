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
    ggplot2::geom_point(data = d |> dplyr::filter(val == 0),
                        ggplot2::aes(lon, lat),
                        colour = "blue",
                        size = 0.2) +
    ggplot2::geom_point(data = d |> dplyr::filter(val > 0),
                        ggplot2::aes(lon, lat, size = val),
                        alpha = 0.25,
                        colour = "red") +
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


#' Length ungutted weight qc-plot
#'
#' @param res list of munged hafvog's tables
#' @param species Numerical, species to plot
#'
#' @return A plot
#' @export
#'
sm_luw_plot <- function(res, species = 1) {
  
  d <- 
    res$kv.this.year |> 
    dplyr::filter(tegund == species)
  ggplot2::ggplot() +
    ggplot2::theme_grey(base_size = 16) +
    ggplot2::geom_ribbon(data = res$qc$lw |> 
                           dplyr::filter(tegund == species),
                         ggplot2::aes(lengd, ymin = osl1, ymax = osl2),
                         fill = "pink") +
    ggplot2::geom_point(data = d |> 
                          dplyr::filter(.l_osl == "ok"),
                        ggplot2::aes(lengd, oslaegt), 
                        size = 1, alpha = 0.5, colour = "blue") +
    ggplot2::geom_point(data = d |> dplyr::filter(.l_osl == "check"), 
                        ggplot2::aes(lengd, oslaegt), colour = "red") +
    ggrepel::geom_text_repel(data = d |> dplyr::filter(.l_osl == "check"), 
                             ggplot2::aes(lengd, oslaegt, label = lestnr)) +
    ggplot2::scale_x_log10(breaks = c(seq(5, 50, by = 5), seq(60, 100, by = 10), 120, 140, 160, 200)) +
    ggplot2::scale_y_log10(breaks = c(seq(5, 50, by = 5),
                                      seq(60, 100, by = 10),
                                      seq(120, 200, by = 20),
                                      seq(300, 1000, by = 100),
                                      seq(1500, 10000, by = 500),
                                      seq(15000, 30000, by = 1000))) +
    ggplot2::coord_cartesian(xlim = range(d$lengd, na.rm = TRUE),
                             ylim = range(d$oslaegt, na.rm = TRUE))
}


#' Length gutted weight qc-plot
#'
#' @param res list of munged hafvog's tables
#' @param species Numerical, species to plot
#'
#' @return A plot
#' @export
#'
sm_lgw_plot <- function(res, species = 1) {
  
  d <- 
    res$kv.this.year |> 
    dplyr::filter(tegund == species)
  ggplot2::ggplot() +
    ggplot2::theme_grey(base_size = 16) +
    ggplot2::geom_ribbon(data = res$qc$lw |> 
                           dplyr::filter(tegund == species),
                         ggplot2::aes(lengd, ymin = sl1, ymax = sl2),
                         fill = "pink") +
    ggplot2::geom_point(data = d |> 
                          dplyr::filter(.l_sl == "ok"),
                        ggplot2::aes(lengd, slaegt), 
                        size = 1, alpha = 0.5, colour = "blue") +
    ggplot2::geom_point(data = d |> dplyr::filter(.l_sl == "check"), 
                        ggplot2::aes(lengd, slaegt), colour = "red") +
    ggrepel::geom_text_repel(data = d |> dplyr::filter(.l_sl == "check"), 
                             ggplot2::aes(lengd, slaegt, label = lestnr)) +
    ggplot2::scale_x_log10(breaks = c(seq(5, 50, by = 5), seq(60, 100, by = 10), 120, 140, 160, 200)) +
    ggplot2::scale_y_log10(breaks = c(seq(5, 50, by = 5),
                                      seq(60, 100, by = 10),
                                      seq(120, 200, by = 20),
                                      seq(300, 1000, by = 100),
                                      seq(1500, 10000, by = 500),
                                      seq(15000, 30000, by = 1000))) +
    ggplot2::coord_cartesian(xlim = range(d$lengd, na.rm = TRUE),
                             ylim = range(d$oslaegt, na.rm = TRUE))
}

# NOTE: Should make the following 3 plot construction more simple
#       This may require prior revamping the data

#' Ratio of gutted to ungutted as a function of length
#'
#' @param res list of munged hafvog's tables
#' @param species Numerical, species to plot
#'
#' @return A plot
#' @export
#'
sm_l_gvu_plot <- function(res, species = 1) {
  
  d <- 
    res$kv.this.year |> 
    dplyr::filter(tegund == species)
  ggplot2::ggplot() +
    ggplot2::theme_grey(base_size = 16) +
    ggplot2::geom_rect(data = res$qc$range |> 
                           dplyr::filter(tegund == species),
                         ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = slaegt_low, ymax = slaegt_high),
                         fill = "pink") +
    ggplot2::geom_point(data = d |> dplyr::filter(.sl_osl == "ok"),
                        ggplot2::aes(lengd, slaegt/oslaegt), 
                        size = 1, alpha = 0.5, colour = "blue") +
    ggplot2::geom_point(data = d |> dplyr::filter(.sl_osl == "check"), 
                        ggplot2::aes(lengd, slaegt/oslaegt), colour = "red") +
    ggrepel::geom_text_repel(data = d |> dplyr::filter(.sl_osl == "check"), 
                             ggplot2::aes(lengd, slaegt/oslaegt, label = lestnr)) +
    ggplot2::coord_cartesian(xlim = range(d$lengd, na.rm = TRUE),
                             ylim = range(d$slaegt/d$oslaegt, na.rm = TRUE))
}


#' Ratio of liver to ungutted as a function of length
#'
#' @param res list of munged hafvog's tables
#' @param species Numerical, species to plot
#'
#' @return A plot
#' @export
#'
sm_l_lvu_plot <- function(res, species = 1) {
  
  d <- 
    res$kv.this.year |> 
    dplyr::filter(tegund == species)
  ggplot2::ggplot() +
    ggplot2::theme_grey(base_size = 16) +
    ggplot2::geom_rect(data = res$qc$range |> 
                         dplyr::filter(tegund == species),
                       ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = lifur_low, ymax = lifur_high),
                       fill = "pink") +
    ggplot2::geom_point(data = d |> dplyr::filter(.sl_osl == "ok"),
                        ggplot2::aes(lengd, lifur/oslaegt), 
                        size = 1, alpha = 0.5, colour = "blue") +
    ggplot2::geom_point(data = d |> dplyr::filter(.sl_osl == "check"), 
                        ggplot2::aes(lengd, lifur/oslaegt), colour = "red") +
    ggrepel::geom_text_repel(data = d |> dplyr::filter(.sl_osl == "check"), 
                             ggplot2::aes(lengd, lifur/oslaegt, label = lestnr)) +
    ggplot2::coord_cartesian(xlim = range(d$lengd, na.rm = TRUE),
                             ylim = range(d$lifur/d$oslaegt, na.rm = TRUE))
}

#' Ratio of gonads to ungutted as a function of length
#'
#' @param res list of munged hafvog's tables
#' @param species Numerical, species to plot
#'
#' @return A plot
#' @export
#'
sm_l_gonadsvu_plot <- function(res, species = 1) {
  
  d <- 
    res$kv.this.year |> 
    dplyr::filter(tegund == species)
  ggplot2::ggplot() +
    ggplot2::theme_grey(base_size = 16) +
    ggplot2::geom_rect(data = res$qc$range |> 
                         dplyr::filter(tegund == species),
                       ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = kynkirtlar_low, ymax = kynkirtlar_high),
                       fill = "pink") +
    ggplot2::geom_point(data = d |> dplyr::filter(.kyn == "ok"),
                        ggplot2::aes(lengd, kynfaeri/oslaegt), 
                        size = 1, alpha = 0.5, colour = "blue") +
    ggplot2::geom_point(data = d |> dplyr::filter(.kyn == "check"), 
                        ggplot2::aes(lengd, kynfaeri/oslaegt), colour = "red") +
    ggrepel::geom_text_repel(data = d |> dplyr::filter(.kyn == "check"), 
                             ggplot2::aes(lengd, kynfaeri/oslaegt, label = lestnr)) +
    ggplot2::coord_cartesian(xlim = range(d$lengd, na.rm = TRUE),
                             ylim = range(d$kynfaeri/d$oslaegt, na.rm = TRUE))
}