#' Plot fire damage vs distance with fitted curve
#'
#' @param x Vector of distance values
#' @param y Vector of damage values
#' @param title Optional plot title
#'
#' @return A plot of the fire damage data with fitted line
#' @export
#'
#' @examples
#' data(fire)
#' plot_fire(fire$DISTANCE, fire$DAMAGE)
plot_fire <- function(x, y, title = "Fire Damage vs Distance") {
  # Create the scatter plot
  graphics::plot(x, y,
                 xlab = "Distance",
                 ylab = "Damage",
                 main = title,
                 pch = 16,
                 col = "blue")

  # Fit a linear model
  fit <- stats::lm(y ~ x)

  # Add the fitted line
  graphics::abline(fit, col = "red", lwd = 2)

  # Add a legend
  graphics::legend("topleft",
                   legend = c("Data points", "Fitted line"),
                   col = c("blue", "red"),
                   pch = c(16, NA),
                   lty = c(NA, 1))

  # Return the fit invisibly
  invisible(fit)
}
