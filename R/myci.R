#' Calculate confidence interval for a sample mean
#'
#' @param x Numeric vector of data
#'
#' @return Named vector containing lower and upper bounds of 95% confidence interval
#' @export
#'
#' @examples
#' data <- rnorm(30)
#' myci(data)
myci <- function(x) {
  n <- length(x)
  x_bar <- mean(x)
  s <- stats::sd(x)
  alpha <- 0.05
  t_crit <- stats::qt(1 - alpha/2, df = n - 1)
  margin_error <- t_crit * s / sqrt(n)
  lower <- x_bar - margin_error
  upper <- x_bar + margin_error
  return(c(lower = lower, upper = upper))
}
