#' Calculate a binomial Monte Carlo confidence interval
#'
#' @param successes Number of successful simulations.
#' @param trials Number of simulations in the denominator.
#' @param conf_level Confidence level for the interval.
#'
#' @return A named numeric vector containing `lower` and `upper`.
#'
#' @noRd
binomial_ci <- function(successes, trials, conf_level) {

  #handle incorrect inputs
  if (
    length(successes) != 1 ||
    length(trials) != 1 ||
    is.na(successes) ||
    is.na(trials) ||
    trials < 1
  ) {
    return(c(lower = NA, upper = NA))
  }

  #compute ci
  ci <- stats::binom.test(
    x = successes,
    n = trials,
    conf.level = conf_level
  )$conf.int

  #return
  c(lower = unname(ci[1]),upper = unname(ci[2]))
}
