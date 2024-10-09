# from: https://stackoverflow.com/questions/10802806/is-there-a-way-to-output-text-to-the-r-console-in-color

#' Convert plain text colour to ANSI code
#'
#' @param colour colour in plain text ("red", "green", etc.) to convert to ANSI
#'
#' @return string representing provided colour as ANSI encoding
#'
colour_to_ansi <- function(colour) {
  # Note ANSI colour codes
  colour_codes <- list(
    "black" = 30,
    "red" = 31,
    "green" = 32,
    "yellow" = 33,
    "blue" = 34,
    "magenta" = 35,
    "cyan" = 36,
    "white" = 37
  )
  
  # Check colour provided in codes above
  if (colour %in% names(colour_codes) == FALSE) {
    stop(
      paste0(
        "Colour provided (", colour, ") can't be converted to ANSI. ",
        "Must be one of: \n", paste(names(colour_codes), collapse = ",")
      )
    )
  }
  
  # Create ANSI version of colour
  ansi_colour <- paste0("\033[", colour_codes[[colour]], "m")
  
  return(ansi_colour)
}

#' Print (cat) progress text as coloured text
#'
#' @param text string to print using cat()
#' @param colour plain text colour ("red", "green", etc.). Defaults to "green"
#'
coloured_print <- function(text, colour = "green") {
  cat(colour_to_ansi(colour), text, "\033[0m\n")
}


grade <- function (x, dx) {
  if (dx > 1) 
    warning("Not tested for grids larger than one")
  brks <- seq(floor(min(x)), ceiling(max(x)), dx)
  ints <- findInterval(x, brks, all.inside = TRUE)
  x <- (brks[ints] + brks[ints + 1])/2
  return(x)
}


scale_longitude_ices <- function(min = -44, max = 68.5, step = 1, ...) {
  breaks <- seq(min + 0.5, max - 0.5, step)
  labels <- geo::d2ir(60, breaks) |>  stringr::str_sub(3)
  return(ggplot2::scale_x_continuous(name = NULL, breaks = breaks, labels = labels, ...))
}
scale_latitude_ices <- function(min = 36, max = 84.5, step = 0.5, ...) {
  breaks <- seq(min + 0.25, max - 0.25, step)
  labels <- geo::d2ir(breaks, 0) |>  stringr::str_sub(1, 2)
  return(ggplot2::scale_y_continuous(name = NULL, breaks = breaks, labels = labels, ...))
}