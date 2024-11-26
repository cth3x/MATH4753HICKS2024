#' Histogram of Sample Means with Overlaid Density Curves
#'
#' @param n Numeric. The sample size for each iteration.
#' @param iter Numeric. The number of iterations (number of samples drawn).
#' @param a Numeric. The lower bound of the uniform distribution. Default is 0.
#' @param b Numeric. The upper bound of the uniform distribution. Default is 10.
#'
#' @return None. The function generates a plot with a histogram and density curves.
#'
#' @export
#'
#' @examples
#' # Generate a histogram of sample means with 100 samples, 1000 iterations,
#' # and a uniform distribution between 0 and 10
#' newfunctionfromabove(n = 100, iter = 1000, a = 0, b = 10)
#'
#' # Change the bounds of the uniform distribution
#' newfunctionfromabove(n = 50, iter = 500, a = -5, b = 5)
newfunctionfromabove <- function(n, iter, a = 0, b = 10) {
  y <- stats::runif(n * iter, a, b)
  data <- matrix(y, nrow = n, ncol = iter, byrow = TRUE)
  w <- apply(data, 2, mean)
  param <- graphics::hist(w, plot = FALSE)
  ymax <- max(param$density)
  ymax <- 1.1 * ymax
  graphics::hist(w, freq = FALSE, ylim = c(0, ymax),
                 main = paste("Histogram of sample mean", "\n", "sample size= ", n, sep = ""),
                 xlab = "Sample mean")
  graphics::lines(stats::density(w), col = "Blue", lwd = 3)
  norm_curve <- function(x) {
    stats::dnorm(x, mean = (a + b) / 2, sd = (b - a) / (sqrt(12 * n)))
  }
  unif_curve <- function(x) {
    stats::dunif(x, a, b)
  }
  graphics::curve(norm_curve, add = TRUE, col = "Red", lty = 2, lwd = 3)
  graphics::curve(unif_curve, add = TRUE, lwd = 4)
}
