#' @title Create summary statistics and boxplot across groups
#'
#' @description This function calculates the summary statistics (min, max, mean, median) of a variable in a data frame
#' across different groups. It also creates a boxplot where x-axis represents groups and y-axis represents the variable.
#'
#' @param data A data frame containing the data.
#' @param group The column containing different groups, which decides how the data will be grouped.
#' @param var A numeric vector. Summary statistics are computed based on it.
#' @param drop_na A logical value indicating whether NA values should be stripped before the computation proceeds.
#' If \code{TRUE}, NA values will be removed. If \code{FALSE}, NA values will be kept.
#' The default value is \code{TRUE}.
#' @param alpha A numeric value representing the transparency of the boxplot.
#' The default value is 0.5,
#' @param scale_y A logical value indicating whether the y-axis of the boxplot should be transformed to log10 scale.
#' If \code{TRUE}, y-axis will be scaled, otherwise \code{FALSE}. The default value is \code{FALSE}.
#' @return A list containing (1) a tibble of summary statistics and, (2) the boxplot.
#'
#' @examples
#' summary_and_boxplot_by_group(apt_buildings, property_type, no_of_units)
#' summary_and_boxplot_by_group(apt_buildings, ward, no_of_storeys, alpha = 0.3, scale_y = TRUE)
#'
#' @export
#'
summary_and_boxplot_by_group <- function(data, group, var, drop_na = TRUE, alpha = 0.5, scale_y = FALSE) {
  calculations <- dplyr::summarise(data, is_numeric = is.numeric({{ var }}),
                            class = class({{ var }}))

  if(!calculations$is_numeric) {
    stop('Sorry, this function only works for numeric column!\n',
         'You have provided an object of class: ', calculations$class)
  }
  if (!is.numeric(alpha)) {
    stop('Sorry, `alpha` has to be numeric!\n',
         'You have provided `alpha` of class: ', class(alpha)[1])
  }
  if (!is.logical(drop_na) || !is.logical(scale_y)) {
    stop('Sorry, `drop_na` and `scale_y` has to be logical!\n',
         'You have provided `drop_na` of class: ', class(drop_na)[1], '\n',
         'You have provided `scale_y` of class: ', class(scale_y)[1])
  }

  # compute min, max, mean and median of variable `var` grouped by `group`
  summary <- data %>%
    dplyr::group_by({{group}}) %>%
    dplyr::summarise(min = min({{var}}, na.rm = drop_na),
              max = max({{var}}, na.rm = drop_na),
              mean = mean({{var}}, na.rm = drop_na),
              median = stats::median({{var}}, na.rm = drop_na),
              n = dplyr::n()
    )

  # create boxplot
  plot <- data %>%
    ggplot2::ggplot(ggplot2::aes({{group}}, {{var}})) +
    ggplot2::geom_boxplot(alpha = alpha)

  if (scale_y) {
    plot <- plot + ggplot2::scale_y_log10()
  }

  result <- list(summary, plot)
  return (result)
}
