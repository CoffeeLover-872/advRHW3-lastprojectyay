library(testthat)
library(dplyr)

source("C:/Users/danie/OneDrive/Desktop/Repository 3 (Adv R HW #3)/advRHW3-lastprojectyay/GetWidthCI/R/get_width_ci.R")

# Dummy data for testing
est_dummy <- tibble::tibble(
  iso = c(4, 4, 4),
  Year = c(2020, 2021, 2022),
  U95 = c(60, 65, 70),
  L95 = c(40, 45, 50),
  U80 = c(55, 60, 65),
  L80 = c(45, 50, 55)
)

# Test 1: Correctly calculates 95% confidence interval width
test_that("get_width_ci correctly calculates 95% CI width", {
  result <- get_width_ci(est_dummy, iso_code = 4, coverage = 95)
  expect_equal(result$width, c(20, 20, 20))  # 60-40, 65-45, 70-50
})

# Test 2: Returns NA for missing U95 or L95 when coverage = 95
test_that("get_width_ci returns NA for missing U95 or L95", {
  est_dummy_with_na <- est_dummy
  est_dummy_with_na$U95[2] <- NA  # Introduce an NA value in U95
  
  result <- get_width_ci(est_dummy_with_na, iso_code = 4, coverage = 95)
  expect_true(is.na(result$width[2]))  # Second width should be NA
})

# Test 3: Error when coverage is not 80 or 95
test_that("get_width_ci errors for invalid coverage", {
  expect_error(get_width_ci(est_dummy, iso_code = 4, coverage = 99),
               "Coverage must be 80 or 95.")
})

# Test 4: Error when data does not contain 'iso' column
test_that("get_width_ci errors for missing 'iso' column", {
  invalid_data <- est_dummy %>% select(-iso)  # Remove 'iso' column
  expect_error(get_width_ci(invalid_data, iso_code = 4, coverage = 95),
               "Data must contain 'iso' column.")
})
