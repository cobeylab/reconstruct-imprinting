INTENSITY_DATA <- readRDS("inst/extdata/INTENSITY_DATA.rds")



#' Calculate the probability imprinting occurs n years after birth
#'
#' Given an individual's birth year, the year of observation, and pre-calculated influenza circulation intensities, calculate the probability that the first influenza infection occurs exactly 0, 1, 2, ... 12 years after birth.
#'
#' @details The probability of primary influenza infection n years after birth is calculated based on a modified [geometric distribution](https://en.wikipedia.org/wiki/Geometric_distribution): let p be the average annual probability of a primary influenza infection. Then the probability that primary infection occurs n=0,1,2,... years after birth is \eqn{p*(1-p)^{n}}.
#'
#' This function modifies the geometric model above to account for changes in annual circulation intensity, so that annual probabilities of primary infection \eqn{p_i} are scaled by the intensity in calendar year i. Details are given in [Gostic et al. Science, 2016](https://www.science.org/doi/10.1126/science.aag1322).
#'
#' @param birth_year year of birth (numeric). Must be between 1918 and the current calendar year.
#' @param observation_year year of observation, which affects the birth cohort's age.
#' @param intensity_df data frame of annual intensities, output by [get_country_intensity_data()].
#' @param max_year maximum year for which to output probabilities. Must be greater than or equal to observation_year.
#' @param baseline_annual_p_infection average annual probability of primary infection. The default, 0.28, was estimated using age-seroprevalence data in [Gostic et al. 2016](https://www.science.org/doi/10.1126/science.aag1322).
#'
#' @return a vector whose entries show the probability that a person born in year 0 was first infected by influenza in year 0, 1, 2, 3, ...  We only consider the first 13 probabilities. These outputs are not normalized, so the vector sum asymptotically approaches one, but is not exactly equal to one. For cohorts born <13 years prior to the year of observation, the output vector will have less than 13 entries.
#'
#' @examples
#' # For a cohort >12  years old and born in 2000, return the probabilities of primary infection in 2000, 2001, ... 2012:
#' get_p_infection_year(
#'   birth_year = 2000,
#'   observation_year = 2022,
#'   intensity_df = get_country_intensity_data("Canada", 2022),
#'   max_year = 2022
#' )
#'
#' #' # For a cohort <12  years old, born in 2020, return the probabilities of primary infection in 2020, 2021, and 2022:
#' get_p_infection_year(
#'   birth_year = 2020,
#'   observation_year = 2022,
#'   intensity_df = get_country_intensity_data("Mexico", 2022),
#'   max_year = 2022
#' )
get_p_infection_year <- function(birth_year,
                                 observation_year, ## Year of data collection, which matters if observation_year is shortly after birth_year
                                 intensity_df,
                                 max_year,
                                 baseline_annual_p_infection = 0.28) { ## max calendar year for which to output estimates
  ## Function to calculate probs of first exposure in year x, given birth in year y
  ## INPUTS
  ##    - year in which an individual was born (birth.year)
  ##    - year in which the individual became infected with bird flu (infection year)
  ## OUTPUTS
  ##    - vector of 13 probabilities, the first representing the probability of first flu infection in the first year of life (age 0), the second representing the probability of first flu infection in the second year of life (age 1), and so on up to the 13th year of life (age 12)
  stopifnot(observation_year <= max_year)
  # Weighted attack rate = annual prob infection weighted by circulation intensity
  weighted.attack.rate <- baseline_annual_p_infection * (intensity_df$intensity)
  names(weighted.attack.rate) <- intensity_df$year
  ################# Calculations ---------------
  possible_imprinting_years <- birth_year:min(birth_year + 12, observation_year) # Calendar years of first infection (ages 0-12)
  nn <- length(possible_imprinting_years) # How many possible years of first infection? (should be 13)
  valid_attack_rates <- weighted.attack.rate[as.character(possible_imprinting_years)] # Get weighted attack rates corresponding to possible years of first infection
  attack_rate_complements <- matrix(rep(1 - valid_attack_rates, nn), nn, nn, byrow = T)
  ## Create matrices of 0s and 1s, which will be used below to vectorize and speed calculations
  infection_year <- not_infection_year <- matrix(0, nn, nn)
  diag(infection_year) <- 1 # Fill in diagonal of one with 1s for years of first infection
  not_infection_year[lower.tri(not_infection_year)] <- 1 # Fill in sub-diagonal for all the years since birth in which the individual escaped infection.
  # Exact probability of escaping infection in the previous (x-1) years, and becoming infected in year x
  prod.mat <- (valid_attack_rates * infection_year) + (attack_rate_complements * not_infection_year)
  # Fill in upper triangle with 1s to make multiplication possible
  prod.mat[upper.tri(prod.mat)] <- 1
  # Take product across rows
  p_ij <- apply(prod.mat, 1, prod)
  p_ij # Output probability of first infection in year i given birth year
}


to_long_df <- function(outlist) {
  ## Reformat the list of matrix outputs into a long data frame
  reformat_one_list_element <- function(ll) {
    ## ll is a matrix whose columns represent birth years, and rows represent unique countries and years of observation
    mat_rownames <- rownames(ll) ## Extract the country-year rownames
    as_tibble(ll) %>% ## cast to tibble
      mutate(year_country = mat_rownames) %>% ## Make the country-year rownames into a column
      extract(year_country, into = c("year", "country"), regex = "(\\d{4})(\\w.+)", convert = T) %>%
      pivot_longer(-c(year, country), values_to = "imprinting_prob", names_to = "birth_year") %>%
      mutate(birth_year = as.integer(birth_year))
  }
  ## Apply the reformatting function to all list elements
  list_names <- names(outlist)
  subtypes <- gsub("(.+)_probs", "\\1", list_names) ## extract the imprinted subtype from list names
  list_of_dfs <- lapply(outlist, reformat_one_list_element) ## reformat each matrix into a data frame
  names(list_of_dfs) <- subtypes ## Get the list of the subtype represented by each matrix in the list
  bind_rows(list_of_dfs, .id = "subtype") ## Bind all subtypes into a single long data frame and return
}


#' Calculate imprinting probabilities
#'
#' For each country and year of observation, calculate the probability that cohorts born in each year from 1918 through the year of observation imprinted to a specific influenza A virus subtype (H1N1, H2N2, or H3N2), or group (group 1 contains H1N1 and H2N2; group 2 contains H3N2).
#'
#' @param observation_years year(s) of observation in which to output imprinting probabilities. The observation year, together with the birth year, determines the birth cohort's age when calculating imprinting probabilities. Cohorts <=12 years old at the time of observation have some probability of being naive to influenza.
#' @param countries a vector of countries for which to calculate imprinting probabilities. Run `show_available_countries()` for a list of valid inputs, and proper spellings.
#' @param df_format must be either 'long' (default) or 'wide'. Controls whether the output data frame is in long format (with a single column for calculated probabilities and a second column for imprinting subtype), or wide format (with four columns, H1N1, H2N2, H3N2, and naive) showing the probability of each imprinting status.
#'
#' @details Imprinting probabilities are calculated following [Gostic et al. 2016](https://www.science.org/doi/10.1126/science.aag1322). Briefly, the model first calculates the probability that an individual's first influenza infection occurs 0, 1, 2, ... 12 years after birth using a modified geometric waiting time model. The annual circulation intensities output by [get_country_intensity_data()] scale the probability of primary infection in each calendar year.
#'
#' Then, after calculating the probability of imprinting 0, 1, 2, ... calendar years after birth, the model uses data on which subtypes circulated in each calendar year (from [get_country_cocirculation_data()]) to estimate that probability that a first infection was caused by each subtype. See [get_country_cocirculation_data()] for details about the underlying data sources.
#'
#' @return
#' * If `format=long` (the default), a long tibble with columns showing the imprinting subtype (H1N1, H2N2, H3N2, or naive), the year of observation, the country, the birth year, and the imprinting probability.
#' * If `format=wide`, a wide tibble with each row representing a country, observation year, and birth year, and with a column for each influenza A subtype (H1N1, H2N2, and H3N2), or the probability that someone born in that year remains naive to influenza and has not yet imprinted.
#' For cohorts >12 years old in the year of observation, the probability of remaining naive is 0, and the subtype-specific probabilities are normalized to sum to 1. For cohorts <=12 years old in the year of observation, the probability of remaining naive is non-zero. For cohorts not yet born at the time of observation, all output probabilities are 0.
#'
#' @export
get_imprinting_probabilities <- function(observation_years,
                                         countries,
                                         df_format = "long") {
  ## INPUT - a vector of countries, a vector of observation years
  ## OUTPUT - a list of matrices containing subtype-specific imprinting probabilities for each country-year of observation, and each birth year
  ## Input checks
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  if (!all(observation_years >= 1918 & observation_years <= current_year)) {
    stop("observation_years must be a numeric vector with values between 1918 and the current calendar year.")
  }
  if (!all(countries %in% pull(show_available_countries(), country))) {
    problem_inputs <- countries[!(countries %in% pull(show_available_countries()))]
    stop(sprintf("You input the following country names, which are invalid: \n\n%s\n\nRun `show_available_countries()` to see a list of valid countries.", paste(problem_inputs, collapse = ", ")))
  }
  max_year <- max(observation_years)
  stopifnot(max_year <= as.numeric(format(Sys.Date(), "%Y")))
  birth_years <- 1918:max_year
  infection_years <- birth_years
  nn_birth_years <- length(birth_years)

  # Initialize matrices to store imprinting probabilities for each country and year
  # Rows - which country and year are we doing the reconstruction from the persepctive of?
  # Cols - what birth year are we estimating imprinting probabilities for?
  H1N1_probs <- matrix(NA,
    nrow = length(countries) * length(observation_years),
    ncol = length(birth_years),
    dimnames = list(paste(rep(observation_years, length(countries)), rep(countries, each = length(observation_years)), sep = ""), rev(birth_years))
  )
  H2N2_probs <- naive_probs <- H3N2_probs <- H1N1_probs

  ## For each country, get imprinting probabilities
  for (this_country in countries) {
    who_region <- get_WHO_region(this_country)
    this_epi_data <- get_country_cocirculation_data(this_country, max_year, output_format = "matrix")
    this_intensity_data <- get_country_intensity_data(this_country, max_year, min_specimens = 50)
    stopifnot(!any(is.na(this_intensity_data$intensity)))

    # Extract and data from birth years of interest
    # These describe the fraction of circulating influenza viruses isolated in a given year that were of subtype H1N1 (type1), H2N2 (type2), or H3N2 (type3)
    H1.frac <- as.numeric(this_epi_data["A/H1N1", as.character(birth_years)])
    H2.frac <- as.numeric(this_epi_data["A/H2N2", as.character(birth_years)])
    H3.frac <- as.numeric(this_epi_data["A/H3N2", as.character(birth_years)])
    names(H1.frac) <- names(H2.frac) <- names(H3.frac) <- as.character(birth_years)

    ## Initialize master matrix with observation_years on rows and birth years on columns
    country_H1_mat <- matrix(0,
      nrow = length(observation_years),
      ncol = length(birth_years),
      dimnames = list((observation_years), (birth_years))
    )
    country_naive_mat <- country_H2_mat <- country_H3_mat <- country_H1_mat

    ## Loop across observation years
    for (jj in 1:length(observation_years)) {
      ## Loop across birth years
      n_valid_birth_years <- observation_years[jj] - 1918 + 1
      for (ii in 1:n_valid_birth_years) { # for all birth years elapsed up to the observation year
        n_infection_years <- min(12, observation_years[jj] - birth_years[ii]) # first infections can occur up to age 12, or up until the current year, whichever comes first
        inf.probs <- get_p_infection_year(
          birth_year = birth_years[ii],
          observation_year = observation_years[jj],
          baseline_annual_p_infection = 0.28,
          max_year = max_year,
          intensity_df = this_intensity_data
        ) # Get vector of year-specific probs of first infection
        # If all 13 possible years of infection have passed, normalize so that the probability of imprinting from age 0-12 sums to 1
        if (length(inf.probs) == 13) inf.probs <- inf.probs / sum(inf.probs)
        # Else, don't normalize and extract the probability of remaiing naive below.

        # Fill in the appropriate row (observation year) and column (birth year) of the output matrix
        # The overall probabilty of imprinting to a specific subtype for a given birth year is the dot product of year-specific probabilities of any imprinting, and the year-specific fraction of seasonal circulation caused by the subtype of interest
        valid_infection_years <- as.character(birth_years[ii:(ii + n_infection_years)])
        country_H1_mat[jj, ii] <- sum(inf.probs * H1.frac[valid_infection_years])
        country_H2_mat[jj, ii] <- sum(inf.probs * H2.frac[valid_infection_years])
        country_H3_mat[jj, ii] <- sum(inf.probs * H3.frac[valid_infection_years])
        country_naive_mat[jj, ii] <- round(1 - sum(inf.probs), digits = 8) # Rounds to the nearest 8 to avoid machine 0 errors
      } ## Close loop over valid birth years
    } ## Close loop over observation years

    # return the output in order of current_year:1918
    descending_chronological_order <- as.character(max(birth_years):min(birth_years))
    country_H1_mat <- country_H1_mat[, descending_chronological_order]
    country_H2_mat <- country_H2_mat[, descending_chronological_order]
    country_H3_mat <- country_H3_mat[, descending_chronological_order]
    country_naive_mat <- country_naive_mat[, descending_chronological_order]

    ##  Fill in the master matrix with country-specific outputs
    cc <- which(countries == this_country)
    rows_for_this_country <- ((cc - 1) * length(observation_years)) + 1:length(observation_years)
    H1N1_probs[rows_for_this_country, ] <- country_H1_mat
    H2N2_probs[rows_for_this_country, ] <- country_H2_mat
    H3N2_probs[rows_for_this_country, ] <- country_H3_mat
    naive_probs[rows_for_this_country, ] <- country_naive_mat
  } ## Close loop across countries

  ## Check that the total for each birth year is 1 when rounded to 4 decimals
  total <- H1N1_probs + H2N2_probs + H3N2_probs + naive_probs
  if (any(!(round(total, 4) %in% c(0, 1, NA)))) {
    warning("Weights do not sum to 1")
  }

  ## Normalize so that sum of weights is exactly 1 in each birth year
  total[which(total == 0)] <- 1 # Reset 0 values so as not to divide by 0
  H1N1_probs <- H1N1_probs / (total)
  H2N2_probs <- H2N2_probs / (total)
  H3N2_probs <- H3N2_probs / (total)
  naive_probs <- naive_probs / (total)

  outlist <- list(
    H1N1_probs = H1N1_probs,
    H2N2_probs = H2N2_probs,
    H3N2_probs = H3N2_probs,
    naive_probs = naive_probs
  )

  if (df_format == "wide") {
    return(to_long_df(outlist) %>%
      pivot_wider(id_cols = c(year, country, birth_year), names_from = subtype, values_from = imprinting_prob))
  } else {
    stopifnot(df_format == "long")
    return(to_long_df(outlist))
  }
}
