#' One-Way ANOVA Test
#'
#' This function performs a one-way ANOVA test and returns the results.
#'
#' @param data A data frame containing the data.
#' @param response The name of the response variable (dependent variable).
#' @param group The name of the grouping variable (independent variable).
#' @return A list containing the ANOVA table and summary statistics.
#' @examples
#' data <- data.frame(
#'   response = c(5.1, 5.2, 5.4, 6.5, 6.8, 7.0),
#'   group = c("A", "A", "B", "B", "C", "C")
#' )
#' one_way_anova(data, "response", "group")
#' @export
one_way_anova <- function(data, response, group) {
  # Ensure the response and group columns exist
  if (!(response %in% names(data)) || !(group %in% names(data))) {
    stop("Response or group variable not found in the dataset.")
  }

  # Perform ANOVA
  formula <- as.formula(paste(response, "~", group))
  model <- aov(formula, data = data)
  anova_table <- summary(model)

  # Return results
  list(
    anova_table = anova_table,
    model = model
  )
}
