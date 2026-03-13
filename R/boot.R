# Internal helper: bootstrap mean and 95% CI for a numeric vector
boot_ci <- function(x, times = 100) {
  cis <- stats::quantile(
    replicate(times, mean(sample(x, replace = TRUE))),
    probs = c(0.025, 0.975)
  )
  tibble::tibble(
    n        = length(x),
    mean     = mean(x),
    lower.ci = unname(cis[1]),
    upper.ci = unname(cis[2])
  )
}

#' Bootstrap catch per station per year
#'
#' @param res a list
#'
#' @return a list
#' @export
#'
sm_boot <- function(res) {
  coloured_print("Bootstrapping", "green")
  res$boot <-
    res$by.station |>
    dplyr::group_by(tegund, ar, var) |>
    dplyr::reframe(boot_ci(val))
  return(res)
}
