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
