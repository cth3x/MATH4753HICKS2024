#' Plot normal distribution with shaded area
#'
#' @param mu Mean of the normal distribution
#' @param sigma Standard deviation of the normal distribution
#' @param a Point up to which to calculate and shade the cumulative probability
#'
#' @return A list containing:
#'   \item{mu}{Input mean}
#'   \item{sigma}{Input standard deviation}
#'   \item{a}{Input point}
#'   \item{area}{Cumulative probability up to point a}
#' @export
#'
#' @examples
#' myncurve(0, 1, 1.96)
#' myncurve(100, 15, 120)
myncurve <- function(mu, sigma, a) {
  x <- seq(mu - 3 * sigma, mu + 3 * sigma, length.out = 1000)
  graphics::curve(stats::dnorm(x, mean = mu, sd = sigma),
                  xlim = c(mu - 3 * sigma, mu + 3 * sigma),
                  main = "Normal Distribution",
                  ylab = "Density",
                  xlab = "x")
  x_fill <- seq(mu - 3 * sigma, a, length.out = 100)
  y_fill <- stats::dnorm(x_fill, mean = mu, sd = sigma)
  graphics::polygon(c(x_fill, a, mu - 3 * sigma),
                    c(y_fill, 0, 0),
                    col = "skyblue")
  prob <- stats::pnorm(a, mean = mu, sd = sigma)
  return(list(mu = mu, sigma = sigma, a = a, area = prob))
}
