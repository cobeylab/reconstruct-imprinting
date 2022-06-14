setwd("../..")

test_that("Observation year less than 13 years from birth year gives known-good probabilities.", {

  this_epi_data = get_country_cocirculation_data("United States", 2010)
  INTENSITY_DATA = get_country_intensity_data("United States", 2010, min_samples_processed_per_year = 50)

  # Birth Year 2007
  expect_equal(get_p_infection_year(2007, 2010, 0.28, 2010, INTENSITY_DATA), c(0.42098152, 0.16439171, 0.29023874, 0.03097275))
  # Birth Year 2010
  expect_equal(get_p_infection_year(2010, 2010, 0.28, 2010, INTENSITY_DATA), c(0.2490017), tolerance = 0.00001)

})


test_that("Number of probabilities are same as number of years in input.", {

  this_epi_data = get_country_cocirculation_data("United States", 2010)
  INTENSITY_DATA = get_country_intensity_data("United States", 2010, min_samples_processed_per_year = 50)

  # Long span
  birth_year = 1998
  obs_year = 2010

  expect_equal(length(get_p_infection_year(birth_year, obs_year, 0.28, obs_year, INTENSITY_DATA)), obs_year - birth_year + 1)

  # Short span
  birth_year = 2000
  obs_year = 2003

  expect_equal(length(get_p_infection_year(birth_year, obs_year, 0.28, obs_year, INTENSITY_DATA)), obs_year - birth_year + 1)

  # One year
  birth_year = 2008
  obs_year = 2008

  expect_equal(length(get_p_infection_year(birth_year, obs_year, 0.28, obs_year, INTENSITY_DATA)), obs_year - birth_year + 1)

})


test_that("Misspelled country name gives an error.", {

  expect_error(get_country_cocirculation_data("United States?", 2010))
  expect_error(get_country_intensity_data("UnitedStates", 2010, min_samples_processed_per_year = 50))
  expect_error(get_imprinting_probabilities(observation_years = obs_year, countries = c('UnitedStates')))

})

test_that("Invalid year gives an error.", {

  expect_error(get_country_cocirculation_data("United States", -1))
  expect_error(get_country_intensity_data("United States", 1900, min_samples_processed_per_year = 50))

})

