#' Calculate Statistics for Medicare Payments
#'
#' This function calculates \code{statistic} over all of the DRG codes for average Medicare payments
#'
#' @param data a data frame
#' @param statistic a type of statistic to calculate
#'
#' @return a table shows \code{statistic} of average Medicare payments over all of the DRG codes
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom knitr kable
#'
#' @examples
#' calculate_statistics(DRG_data, "mean")
#' calculate_statistics(DRG_data, "median")
#' calculate_statistics(DRG_data, "standard deviation")
#'
calculate_statistics <- function(data, statistics) {
  if (statistics == "mean") {
    i <- 2
  } else if (statistics == "median") {
    i <- 3
  } else if (statistics == "standard deviation") {
    i <- 4
  } else {
    stop("Invalid choice for 'statistics'. Choose from 'mean', 'median', or 'standard deviation'.")
  }

  # Calculate the mean, median, and standard deviation of each group
  data <- data %>%
    group_by(DRG.Definition) %>%
    summarise(mean = mean(`Average Medicare Payments`),
              median = median(`Average Medicare Payments`),
              standard_deviation = sd(`Average Medicare Payments`, na.rm = TRUE))

  # Return the statistics that we choose
  table_result <- knitr::kable(data[, c(1, i)])
  return(table_result)
}
