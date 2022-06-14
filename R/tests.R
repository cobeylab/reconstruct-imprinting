library(testthat)

test_that("Observation year less than 13 years from birth year gives known-good probabilities.", {
  library(tidyverse)
  source("calculation_funs.R")
  source("data_import_funs.R")

  this_epi_data = get_country_cocirculation_data("United States", 2010)
  INTENSITY_DATA = get_country_intensity_data("United States", 2010, min_samples_processed_per_year = 50)

  # Birth Year 2007
  expect_equal(get_p_infection_year(2007, 2010, 0.28, 2010, INTENSITY_DATA), c(0.42098152, 0.16439171, 0.29023874, 0.03097275))
  # Birth Year 2010
  expect_equal(get_p_infection_year(2010, 2010, 0.28, 2010, INTENSITY_DATA), c(0.2490017), tolerance = 0.000001)

})

test_that("Observation year greater than 13 years from birth year gives known-good probabilities.", {
  library(tidyverse)
  source("calculation_funs.R")
  source("data_import_funs.R")

  this_epi_data = get_country_cocirculation_data("United States", 2010)
  INTENSITY_DATA = get_country_intensity_data("United States", 2010, min_samples_processed_per_year = 50)

  # Birth Year 1995
  expect_equal(get_p_infection_year(1995, 2010, 0.28, 2010, INTENSITY_DATA), c(0.224672982, 0.243177207, 0.076945392, 0.008765326, 0.073416962, 0.090385447, 0.040737142, 0.127933613, 0.019213806, 0.030645188, 0.020023422, 0.010588820, 0.014100647))

  # Birth Year 1998
  expect_equal(get_p_infection_year(1998, 2010, 0.28, 2010, INTENSITY_DATA), c(0.019255801, 0.161283500, 0.198560127, 0.089491975, 0.281046509, 0.042209182, 0.067321817, 0.043987758, 0.023261680, 0.030976516, 0.012096213, 0.021356245, 0.002279026))
})

test_that("Number of probabilities are same as number of years in input.", {
  library(tidyverse)
  source("calculation_funs.R")
  source("data_import_funs.R")

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
  library(tidyverse)
  source("calculation_funs.R")
  source("data_import_funs.R")

  expect_error(get_country_cocirculation_data("United States?", 2010))
  expect_error(get_country_intensity_data("UnitedStates", 2010, min_samples_processed_per_year = 50))
  expect_error(get_imprinting_probabilities(observation_years = obs_year, countries = c('UnitedStates')))

})

test_that("Invalid year gives an error.", {
  library(tidyverse)
  source("calculation_funs.R")
  source("data_import_funs.R")

  expect_error(get_country_cocirculation_data("United States", -1))
  expect_error(get_country_intensity_data("United States", 1900, min_samples_processed_per_year = 50))

})

test_that("Observation years greater than current year raises error.", {
  library(tidyverse)
  source('calculation_funs.R')
  source('data_import_funs.R')

  obs_year = as.numeric(format(Sys.Date(), '%Y')) + 1
  expect_error(get_imprinting_probabilities(observation_years = obs_year, countries = c('United States')))

})

test_that("Range of years returned equals range of years passed.", {
  library(tidyverse)
  source('calculation_funs.R')
  source('data_import_funs.R')

  obs_year = 2018
  min_year = 1918

  probs = get_imprinting_probabilities(observation_years = obs_year, countries = c('United States'))

  expect_equal(max(probs$birth_year), obs_year)
  expect_equal(min(probs$birth_year), min_year)

})

