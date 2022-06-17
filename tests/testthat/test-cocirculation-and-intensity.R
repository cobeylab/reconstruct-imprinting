setwd("../..")

test_that("Observation year less than 13 years from birth year gives known-good probabilities.", {

  this_epi_data = get_country_cocirculation_data("United States", 2010)
  INTENSITY_DATA = get_country_intensity_data("United States", 2010, min_samples_processed_per_year = 50)

  # Birth Year 2007
  expect_equal(get_p_infection_year(2007, 2010, 0.28, 2010, INTENSITY_DATA), c(0.19899714, 0.18616278, 0.43038805, 0.03884563))
  # Birth Year 2010
  expect_equal(get_p_infection_year(2010, 2010, 0.28, 2010, INTENSITY_DATA), c(0.2106002), tolerance = 0.000001)

})


test_that("Observation year greater than 13 years from birth year gives known-good probabilities.", {
  this_epi_data = get_country_cocirculation_data("United States", 2010)
  INTENSITY_DATA = get_country_intensity_data("United States", 2010, min_samples_processed_per_year = 50)

  # Birth Year 1995
  expect_equal(get_p_infection_year(1995, 2010, 0.28, 2010, INTENSITY_DATA), c(0.22467298, 0.24317721, 0.07694539, 0.02963517, 0.11150953, 0.05575555, 0.03564507, 0.04825983, 0.06960915, 0.01243207, 0.02032434, 0.01586563, 0.01117729), tolerance = 0.000001)

  # Birth Year 1998
  expect_equal(get_p_infection_year(1998, 2010, 0.28, 2010, INTENSITY_DATA), c(0.065103001, 0.244965840, 0.122484638, 0.078305628, 0.106017928, 0.152918439, 0.027310965, 0.044648818, 0.034853868, 0.024554432, 0.022970789, 0.053105959, 0.004793197), tolerance = 0.000001)
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

test_that("Numeric (non-NA) probabilities are returned for post-2017 observation years.", {

  obs_year = 2022
  min_year = 1918

  probs = get_imprinting_probabilities(observation_years = obs_year, countries = c('United States'))

  expect_false(any(is.na(probs$imprinting_prob)))
})

test_that("Countries with low-quality intensity data return appropriate intensity values.", {

  intensities_Germany = get_country_intensity_data(country = c('Germany'),
                                                   max_year = 2022,
                                                   min_samples_processed_per_year = 50)
  intensities_Iraq = get_country_intensity_data(country = c('Iraq'),
                                                   max_year = 2022,
                                                   min_samples_processed_per_year = 50)

  expect_false(any(is.na(intensities_Germany$intensity)))
  expect_false(any(is.na(intensities_Iraq$intensity)))
  expect_true(all(intensities_Germany$intensity >= -2.5 & intensities_Germany$intensity <= 2.5))
  expect_true(all(intensities_Iraq$intensity >= -2.5 & intensities_Iraq$intensity <= 2.5))
})
