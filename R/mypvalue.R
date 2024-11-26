#' Calculate p-value and critical value for a t-test
#'
#' This function calculates the p-value and critical value for a two-tailed t-test
#' based on a given t-statistic, sample size, and significance level. It also
#' determines whether to reject the null hypothesis.
#'
#' @param tcalc A numeric value representing the calculated t-statistic.
#' @param n A numeric value representing the sample size.
#' @param alpha A numeric value representing the significance level, default is 0.05.
#'
#' @return A list with the following components:
#'   \item{p_value}{The two-tailed p-value associated with the t-statistic.}
#'   \item{critical_value}{The critical t-value for the specified alpha level.}
#'   \item{reject_null}{A logical value indicating whether the null hypothesis is rejected (TRUE) or not (FALSE).}
#'
#' @export
#'
#' @examples
#' # Example usage with t-statistic of 2.5, sample size of 30, and alpha = 0.05
#' result <- mypvalue(tcalc = 2.5, n = 30, alpha = 0.05)
#' print(result)
mypvalue <- function(tcalc, n, alpha = 0.05) {
  df <- n - 1
  # Explicitly qualify calls to stats functions
  p_value <- 2 * stats::pt(-abs(tcalc), df)
  critical_value <- stats::qt(1 - alpha / 2, df)
  list(
    p_value = p_value,
    critical_value = critical_value,
    reject_null = p_value < alpha
  )
}
