#' Calculate Z-scores and their summary statistics
#'
#' @param x A numeric vector of values to standardize
#'
#' @return A list containing:
#'   \item{mean_z}{The mean of the z-scores, rounded to 4 decimal places}
#'   \item{var_z}{The variance of the z-scores}
#'
#' @export
#'
#' @examples
#' data <- c(2, 4, 6, 8, 10)
#' z_scores(data)
z_scores <- function(x) {
  scores <- (x - mean(x)) / stats::sd(x)
  mean_z <- round(mean(scores), 4)
  var_z <- stats::var(scores)
  return(list(mean_z = mean_z, var_z = var_z))
}
