#' Optimal Ticket Allocation for Overbooking
#'
#' @param N Numeric. The capacity (number of available seats).
#' @param gamma Numeric. The target probability that the number of ticket holders showing up
#'   will not exceed the capacity. Must be between 0 and 1 (exclusive).
#' @param p Numeric. The probability that an individual ticket holder shows up. Must be between
#'   0 and 1 (exclusive).
#'
#' @return A list containing:
#' \item{nd}{Optimal number of tickets for the discrete case.}
#' \item{nc}{Optimal number of tickets for the normal approximation case.}
#' \item{N}{The capacity used for calculations.}
#' \item{p}{The probability of a ticket holder showing up.}
#' \item{gamma}{The target probability used for calculations.}
#'
#' @export
#'
#' @examples
#' # Example usage with capacity 100, gamma 0.05, and show-up probability 0.9
#' ntickets(N = 100, gamma = 0.05, p = 0.9)
#'
#' # Example with a different show-up probability
#' ntickets(N = 120, gamma = 0.1, p = 0.85)
ntickets <- function(N, gamma, p) {
  # Validate inputs
  if (!is.numeric(N) || N <= 0 || round(N) != N) {
    stop("N must be a positive integer.")
  }
  if (!is.numeric(gamma) || gamma <= 0 || gamma >= 1) {
    stop("gamma must be a numeric value between 0 and 1 (exclusive).")
  }
  if (!is.numeric(p) || p <= 0 || p >= 1) {
    stop("p must be a numeric value between 0 and 1 (exclusive).")
  }
  # Normal continuous approximation
  normal_continuous_dist <- function(N, gamma, p) {
    expected_value <- N * p
    stddev <- sqrt(N * p * (1 - p))
    return(stats::qnorm(1 - gamma, mean = expected_value, sd = stddev))
  }
  # Normal discrete case
  normal_discrete_dist <- function(N, gamma, p) {
    n <- N
    while (TRUE) {
      prob <- sum(stats::dbinom(0:N, n, p))
      if (1 - prob <= gamma) {
        return(n)
      }
      n <- n + 1
    }
    return(NULL)
  }
  # Plotting function
  plotting <- function(N, gamma, p, is_discrete = TRUE) {
    vals <- seq(N, N + 20)
    obj_vals <- numeric(length(vals))
    if (is_discrete) {
      for (i in seq_along(vals)) {
        prob <- sum(stats::dbinom(0:N, floor(vals[i]), p))
        obj_vals[i] <- 1 - prob
      }
      difference <- abs(obj_vals - gamma)
      intersect <- vals[which.min(difference)]
      plot(vals, obj_vals, type = "b",
           main = paste("Objective Vs n for Discrete Case\n",
                        "Optimal nd: ", intersect, "\ngamma=", gamma, " N=", N, sep = ""),
           ylab = "Objective (1 - P(X <= N))", xlab = "n")
      graphics::abline(h = gamma, col = "red", lwd = 2)
      graphics::abline(v = intersect, col = "red", lwd = 2)
    } else {
      for (i in seq_along(vals)) {
        expected_value <- vals[i] * p
        stddev <- sqrt(vals[i] * p * (1 - p))
        prob <- stats::pnorm(N, mean = expected_value, sd = stddev)
        obj_vals[i] <- 1 - prob
      }
      difference <- abs(obj_vals - gamma)
      intersect <- vals[which.min(difference)]
      plot(vals, obj_vals, type = "l",
           main = paste("Objective Vs n for Normal Approximation\n",
                        "Optimal nc: ", intersect, "\ngamma=", gamma, " N=", N, sep = ""),
           ylab = "Objective (1 - P(X <= N))", xlab = "n")
      graphics::abline(h = gamma, col = "blue", lwd = 2)
      graphics::abline(v = intersect, col = "blue", lwd = 2)
    }
    return(intersect)
  }
  # Calculate optimal values
  nd <- plotting(N, gamma, p, is_discrete = TRUE)
  nc <- plotting(N, gamma, p, is_discrete = FALSE)
  result <- list(nd = nd, nc = nc, N = N, p = p, gamma = gamma)
  return(result)
}
