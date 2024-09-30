boot = function(x, times=100) {
  
  # Get column name from input object
  var = deparse(substitute(x))
  var = gsub("^\\.\\$","", var)
  
  # Bootstrap 95% CI
  cis = stats::quantile(replicate(times, mean(sample(x, replace=TRUE))), probs=c(0.025,0.975))
  
  # Return data frame of results
  data.frame(var, n=length(x), mean=mean(x), lower.ci=cis[1], upper.ci=cis[2])
}

#' Bootstrap catch per station per year
#'
#' @param res a list
#'
#' @return a list
#' @export
#'
sm_boot <- function(res) {
  
  coloured_print("Bootstrapping abundance", "green")
  by.station.boot.n <-
    res$by.station |>
    dplyr::filter(var == "n") |> 
    dplyr::group_by(tegund, ar) |>
    dplyr::do(boot(.$val)) |> 
    dplyr::mutate(var = "n")
  
  coloured_print("\nBootstrapping biomass", "green")
  
  by.station.boot.b <-
    res$by.station |>
    dplyr::filter(var == "b") |> 
    dplyr::group_by(tegund, ar) |>
    dplyr::do(boot(.$val)) |> 
    dplyr::mutate(var = "b")
  
  res$boot <-
    dplyr::bind_rows(by.station.boot.n, by.station.boot.b)

  return(res)
}

