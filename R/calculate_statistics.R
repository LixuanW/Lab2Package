#' Calculate Statistics for Medicare Payments
#'
#' @param data
#' @param statistic
#'
#' @return
#' @export
#'
#' @examples
calculate_statistics <- function(data, statistic) {
  if (statistic == "mean") {
    result <- data %>%
      summarise(mean_medicare_payment = mean(Average.Medicare.Payments, na.rm = TRUE))
  } else if (statistic == "median") {
    result <- data %>%
      summarise(median_medicare_payment = median(Average.Medicare.Payments, na.rm = TRUE))
  } else if (statistic == "sd") {
    result <- data %>%
      summarise(sd_medicare_payment = sd(Average.Medicare.Payments, na.rm = TRUE))
  } else {
    stop("Invalid statistic. Please choose 'mean', 'median', or 'sd'.")
  }

  return(result)
}
