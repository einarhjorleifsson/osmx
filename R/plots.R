#' Length frequency plot
#'
#' @param d A length dataframe
#'
#' @return a ggplot
#' @export
#'
sm_plot_lengths <- function(d) {
  
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


sm_plot_bubble_base <- function(xlim = c(-30, -10), ylim = c(62.5, 67.5)) {
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
sm_plot_bubble <- function(d) { 
  
  var <- unique(d$var)
  zlab <- dplyr::case_when(var == "n" ~ "Stykki",
                           var == "b" ~ "Afli [kg]",
                           .default = "what")
  
  sm_plot_bubble_base() +
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
sm_plot_boot <- function(d) {
  
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
#' @param cruise Which cruise to plot
#'
#' @return A plot
#' @export
#'
sm_plot_length_ungutted <- function(res, species = 1, cruise) {
  
  d <- 
    res$kv.this.year |> 
    dplyr::filter(tegund == species)
  if(!missing(cruise)) d <- d |> dplyr::filter(leidangur %in% cruise)

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
#' @param cruise Which cruise to plot
#'
#' @return A plot
#' @export
#'
sm_plot_length_gutted <- function(res, species = 1, cruise) {
  
  d <- 
    res$kv.this.year |> 
    dplyr::filter(tegund == species)
  if(!missing(cruise)) d <- d |> dplyr::filter(leidangur %in% cruise)
  
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
#' @param cruise Which cruise to plot
#'
#' @return A plot
#' @export
#'
sm_plot_weights <- function(res, species = 1, cruise) {
  
  d <- 
    res$kv.this.year |> 
    dplyr::filter(tegund == species)
  if(!missing(cruise)) d <- d |> dplyr::filter(leidangur %in% cruise)
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
#' @param cruise Which cruise to plot
#'
#' @return A plot
#' @export
#'
sm_plot_liver <- function(res, species = 1, cruise) {
  
  d <- 
    res$kv.this.year |> 
    dplyr::filter(tegund == species)
  if(!missing(cruise)) d <- d |> dplyr::filter(leidangur %in% cruise)
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
#' @param cruise Which cruise to plot
#'
#' @return A plot
#' @export
#'
sm_plot_gonads <- function(res, species = 1, cruise) {
  
  d <- 
    res$kv.this.year |> 
    dplyr::filter(tegund == species)
  if(!missing(cruise)) d <- d |> dplyr::filter(leidangur %in% cruise)
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

#' Violin plot
#'
#' @param timetrend A tibble
#'
#' @return ggplot2
#' @export
#'
sm_plot_violin <- function(timetrend) {
  
  d.median <- 
    timetrend |> 
    dplyr::group_by(ar, var)  |>  
    dplyr::reframe(val = stats::median(val, na.rm = TRUE))
  p <- 
    timetrend |>  
    ggplot2::ggplot(ggplot2::aes(ar, val)) +
    ggplot2::theme_bw(base_size = 24) +
    ggplot2::geom_violin(ggplot2::aes(group = ar), scale = "width") +
    ggplot2::geom_jitter(ggplot2::aes(group = ar), alpha = 0.2, colour = "red", size = 0.5) + 
    ggplot2::geom_line(data = d.median,
                       colour = "blue") +
    ggplot2::facet_wrap(~ var, scale = "free_y") +
    ggplot2::labs(x = NULL, y = NULL)
  return(p)
}

#' Last 20 plot
#'
#' @param d A tibble
#'
#' @return ggplot2
#' @export
#'
sm_plot_last20 <- function(d) {
  
  x <- max(d$ar)
  yrs <- (x-1):(x - 10)
  
  p <- 
    d |> 
    dplyr::filter(ar %in% yrs) |> 
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(stod, val, colour = factor(ar))) +
    ggplot2::geom_line(data = d |> 
                         dplyr::filter(ar == max(ar)),
                       ggplot2::aes(stod, val), 
              linewidth = 1,
              colour = "red") +
    ggplot2::facet_grid(leidangur ~ var, scales = "free") +
    ggplot2::labs(y = "Stodvarnumer", y = NULL, colour = "Ar") +
    ggplot2::coord_flip() +
    ggplot2::scale_color_viridis_d(direction = -1)
  
  return(p)
  
}

#' Species probability of capture
#'
#' @param d A tibble
#'
#' @return A plot
#' @export
#'
sm_plot_probability <- function(d) {
  d |> 
    dplyr::mutate(p = ifelse(p == 0, NA, p)) |> 
    ggplot2::ggplot(ggplot2::aes(lon, lat)) +
    ggplot2::theme_bw() +
    ggplot2::geom_tile(ggplot2::aes(fill =p)) +
    ggplot2::geom_polygon(data = geo::island, fill = "grey") +
    ggplot2::coord_quickmap() +
    ggplot2::scale_fill_viridis_c(option  = "magma", direction = -1, limits = c(0, 1)) +
    ggplot2::labs(x = NULL, y = NULL, fill = "Probability",
                  caption = "Cyan: Caught this year")
}


#' Species indices by rectangle
#'
#' @param d A tibble with data by rectangle
#'
#' @return A plot
#' @export
#'
sm_plot_glyph <- function(d) {
  
  n.glyph <- 
    d |> 
    GGally::glyphs(x_major = "lon", 
                   y_major = "lat",
                   x_minor = "ar", 
                   y_minor = "val", 
                   width = 1, 
                   height = 0.5)
  
  n.glyph |> 
    dplyr::mutate(years = ifelse(ar < max(ar), "history", "current"),
                  pos = ifelse(val != 0, TRUE, FALSE),
                  base = lat - 0.25,
                  gy = ifelse(val == 0, gy + 0.005, gy)) |> 
    ggplot2::ggplot() +
    ggplot2::theme_bw() +
    ggplot2::geom_linerange(ggplot2::aes(x = gx, ymin = base, ymax = gy,
                                         colour = years)) +
    ggplot2::geom_path(data = geo::island, ggplot2::aes(lon, lat)) +
    ggplot2::coord_quickmap() +
    scale_longitude_ices() +
    scale_latitude_ices() +
    ggplot2::scale_colour_manual(values = c("history" = "#377EB8", "current" = "#E41A1C")) +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_line(size = 1),
                   axis.ticks = ggplot2::element_blank(),
                   legend.position = "none") 
  
}