setwd("../..")

test_that("Observation years greater than current year raises error.", {

  obs_year = as.numeric(format(Sys.Date(), '%Y')) + 1
  expect_error(get_imprinting_probabilities(observation_years = obs_year, countries = c('United States')))

})

test_that("Range of years returned equals range of years passed.", {

  obs_year = 2018
  min_year = 1918

  probs = get_imprinting_probabilities(observation_years = obs_year, countries = c('United States'))

  expect_equal(max(probs$birth_year), obs_year)
  expect_equal(min(probs$birth_year), min_year)

})


