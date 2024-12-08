#' Calculate Confidence Interval Width
#'
#' This function calculates the width of the confidence intervals (CI) for contraceptive prevalence rates.
#'
#' @param data A data frame containing contraceptive data.
#' @param iso_code The numeric ISO code to filter data by.
#' @param coverage The coverage level for the CI (must be 80 or 95).
#' @return A tibble containing the year and calculated CI widths.
#' @export

get_width_ci <- function(data, iso_code, coverage) {

  assertthat::assert_that(coverage %in% c(80, 95),
                          msg = "Coverage must be 80 or 95.")
  assertthat::assert_that("iso" %in% colnames(data),
                          msg = "Data must contain 'iso' column.")

  # Filter data by ISO code
  filtered_data <- data %>%
    filter(iso == iso_code)

  # Determine interval width
  if (coverage == 95) {
    filtered_data <- filtered_data %>%
      mutate(width = U95 - L95)
  } else if (coverage == 80) {
    filtered_data <- filtered_data %>%
      mutate(width = U80 - L80)
  }

  # Return the result as a tibble
  return(filtered_data %>%
           select(year = Year, width) %>%
           arrange(year))
}
