#' Make Boxplot of Payments by DRG Code
#'
#' This function creates ggplot boxplot of \code{variable} based on the dataset \code{data}
#' @param data a data frame
#' @param variable a type of payment that the boxplot will build on
#'
#' @return boxplot of \code{variable} based on the dataset \code{data}
#' @export
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_boxplot
#'
#' @examples
#' payment_boxplot(DRG_data, "Average Total Payments")
#'
payment_boxplot <- function(data, variable){

  # Check if the input variable is one of the payment variables
  if (variable %in% c("Average Covered Charges",
                      "Average Total Payments",
                      "Average Medicare Payments")) {

    # Make a boxplot using ggplot2
    drg_plot <- ggplot(data,
                       mapping = aes(x = `DRG Definition`, y = .data[[variable]])
    ) +
      geom_boxplot() +

      # Define labels for the axes and title using the format_label function
      labs(x = paste("DRG Definition"),
           y = paste(variable),
           title = paste("Boxplot of", variable, "by DRG Code")
      ) +

      # Adjust x-axis text size, angle, and justification
      theme(axis.text.x = element_text(size = 1, angle = 70, hjust = 1),

            # Center the plot title
            plot.title = element_text(hjust = 0.5)
      )

  } else {
    # Stop the function and throw an error if the variable is not a payment
    stop("Input variable is not available!")
  }

  # Return the boxplot
  return(drg_plot)
}
