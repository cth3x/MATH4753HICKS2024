#' Simulate binomial trials and plot results
#'
#' @param iter Number of iterations to simulate (default = 100)
#' @param n Number of trials per iteration (default = 10)
#' @param p Probability of success for each trial (default = 0.5)
#'
#' @return A table of relative frequencies for number of successes, normalized by iterations
#' @export
#'
#' @examples
#' # Simulate 100 iterations of 10 trials with p=0.5
#' mybin()
#'
#' # Simulate with different parameters
#' mybin(iter=1000, n=20, p=0.3)
mybin <- function(iter = 100, n = 10, p = 0.5) {
  sam.mat = matrix(NA, nrow = n, ncol = iter, byrow = TRUE)
  succ = c()
  for (i in 1:iter) {
    sam.mat[, i] = sample(c(1, 0), n, replace = TRUE, prob = c(p, 1 - p))
    succ[i] = sum(sam.mat[, i])
  }
  succ.tab = table(factor(succ, levels = 0:n))
  graphics::barplot(succ.tab / iter,
                    col = grDevices::rainbow(n + 1),
                    main = "Binomial Simulation",
                    xlab = "Number of Successes")
  return(succ.tab / iter)
}
