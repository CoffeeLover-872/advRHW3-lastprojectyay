#' Plot mCPR Data and Estimates
#'
#' @description
#' Function plots modern contraceptive prevalence rates (mCPR) among married women from observed data and estimates.
#'
#' @param dat A tibble containing observed mCPR values. Columns: iso, year, cp, division_numeric_code & start_date/end_date and is_in_union == Y
#' @param est A tibble containing mCPR estimates. Columns: "Country or area", iso, Year, Median, U95, L95, U80, L80.
#' @param iso_code Country ISO code for filtering data.
#' @param CI Confidence intervals to be plotted, options: 95 (default), 80, or NA (no CI plotted).
#'
#' @return A ggplot object displaying the mCPR data and estimates, with or without confidence intervals.
#'
#' @examples
#' plot_cp(dat, est, iso_code = 4)
#' plot_cp(dat, est, iso_code = 404, CI = 80)
#'
#' @export

# actual coding time (well more like copy-paste Hw#1 then work it out i guess)
plot_cp <- function(dat, est, iso_code, CI = 95) {
  library(ggplot2)
  library(dplyr)
  library(devtools)

  dat_processed <- dat %>%
    filter(division_numeric_code == iso_code & is_in_union == "Y") %>%
    mutate(year = (start_date + end_date) / 2,
           cp = contraceptive_use_modern * 100) %>%
    select(division_numeric_code, year, cp)

  # filter data for the specified ISO code
  est_filtered <- est %>% filter(iso == iso_code)
  dat_filtered <- dat_processed %>% filter(division_numeric_code == iso_code)

  # plotty plotty
  p <- ggplot(est_filtered, aes(x = Year, y = Median)) +
    geom_line(color = "blue", size = 1) +
    geom_point(data = dat_filtered, aes(x = year, y = cp), color = "black") +
    labs(x = "Time", y = "Modern use (%)", title = est_filtered$`Country or area`[1])

  # add confidence intervals if `CI` is provided
  if (!is.na(CI)) {
    if (CI == 95) {
      p <- p + geom_ribbon(aes(ymin = L95, ymax = U95), fill = "grey", alpha = 0.4)
    } else if (CI == 80) {
      p <- p + geom_ribbon(aes(ymin = L80, ymax = U80), fill = "grey", alpha = 0.4)
    }
  }

  return(p)
}

