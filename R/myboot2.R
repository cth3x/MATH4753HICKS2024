#' Bootstrap Simulation with Visualization
#'
#' @param iter Number of bootstrap iterations (default = 10000)
#' @param x Numeric vector of data
#' @param fun Function to apply to bootstrap samples (default = mean)
#' @param alpha Significance level for confidence interval (default = 0.05)
#' @param cx Character expansion factor for plot text (default = 1.5)
#' @param ... Additional parameters passed to hist()
#'
#' @return Invisibly returns a list containing:
#'   \item{ci}{Confidence interval}
#'   \item{fun}{Function used}
#'   \item{x}{Original data}
#' @export
#'
#' @examples
#' data <- rnorm(100)
#' myboot2(1000, data)
#' myboot2(1000, data, fun = median, alpha = 0.1)
myboot2 <- function(iter = 10000, x, fun = mean, alpha = 0.05, cx = 1.5, ...) {
  n <- length(x)
  y <- sample(x, n * iter, replace = TRUE)
  rs.mat <- matrix(y, nrow = n, ncol = iter, byrow = TRUE)
  xstat <- apply(rs.mat, 2, fun)
  ci <- stats::quantile(xstat, c(alpha / 2, 1 - alpha / 2))
  para <- graphics::hist(
    xstat,
    freq = FALSE,
    las = 1,
    main = paste(
      "Histogram of Bootstrap sample statistics",
      "\n",
      "alpha=", alpha, " iter=", iter, sep = ""
    ),
    ...
  )
  mat <- matrix(x, nrow = length(x), ncol = 1, byrow = TRUE)
  pte <- apply(mat, 2, fun)
  graphics::abline(v = pte, lwd = 3, col = "Black")
  graphics::segments(ci[1], 0, ci[2], 0, lwd = 4)
  graphics::text(ci[1], 0, paste("(", round(ci[1], 2), sep = ""), col = "Red", cex = cx)
  graphics::text(ci[2], 0, paste(round(ci[2], 2), ")", sep = ""), col = "Red", cex = cx)
  graphics::text(pte, max(para$density) / 2, round(pte, 2), cex = cx)
  invisible(list(ci = ci, fun = fun, x = x))
}
