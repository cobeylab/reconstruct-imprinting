# Read data from CSVs into COUNTRY_NAMES, THOMPSON_DATA, and REGION_DATA
COUNTRY_NAMES <- readRDS("data/COUNTRY_NAMES.rds")
THOMPSON_DATA <- readRDS("data/THOMPSON_DATA.rds")
INTENSITY_DATA <- readRDS("data/INTENSITY_DATA.rds")

parse_region_names <- function(region) {
  ## Convert two-word region names for file import
  if (tolower(region) == "eastern mediterranean") {
    return("eastern_mediterranean")
  }
  if (tolower(region) == "western pacific") {
    return("western_pacific")
  }
  if (tolower(region) == "southeast asia") {
    return("southeast_asia")
  }
  ## else...
  return(region)
}

parse_country_names <- function(this.country) {
  ## check that this.country is a valid input
  if (!this.country %in% unlist(show_available_countries())) {
    stop(sprintf("%s is not valid. Run show_available_countries() for a list of valid inputs.", this.country))
  }
  ## Convert short country names to country names in the Flunet data files
  COUNTRY_NAMES %>%
    dplyr::filter(country == this.country) %>%
    pull(flunet_country) %>%
    return()
}

test_rowsums_subtype <- function(H1N1, H2N2, H3N2) {
  stopifnot(all(H1N1 + H3N2 + H2N2 == 1 | is.na(H1N1 + H3N2 + H2N2)))
}

test_rowsums_group <- function(group1, group2) {
  stopifnot(all(group1 + group2 == 1 | is.na(group1 + group2)))
}

check_years <- function(years, max_year) {
  check_max_year(max_year)
  stopifnot(1997:max_year %in% years)
}

check_max_year <- function(max_year) {
  if (!(max_year >= 1918 & max_year <= as.integer(format(Sys.Date(), "%Y")))) {
    stop(sprintf("max_year is %i. max_year must fall between 1918 and the current calendar year, %s.", max_year, as.integer(format(Sys.Date(), "%Y"))))
  }
}



#' Show a list of all available countries
#'
#' Lists all available countries, with valid spelling and formatting. Each country in the list matches or can be mapped to a country with data in [WHO Flu Mart](https://apps.who.int/flumart/Default?ReportNo=12). (Note: for convenience, this package sometimes uses different country names or spellings than Flu Mart.)
#' @return A data frame of valid country names.
#' @examples show_available_countries()
#' @export
show_available_countries <- function() {
  COUNTRY_NAMES %>%
    select(country)
}


#' Show all WHO regions
#'
#' Lists all WHO regions, with valid spelling and formatting.
#' @return A data frame of valid region names.
#' @examples show_available_regions()
#' @export
show_available_regions <- function() {
  COUNTRY_NAMES %>%
    select(region) %>%
    distinct()
}



#' Look up a country's WHO region
#'
#' @param this.country name of the input country (a string)
#'
#' @return Name of the corresponding WHO region
#'
#' @examples get_WHO_region("Germany")
#' @examples get_WHO_region("China")
#' @export
get_WHO_region <- function(this.country) {
  if (!this.country %in% unlist(show_available_countries())) {
    stop(sprintf("%s is not valid. Run show_available_countries() for a list of valid inputs.", this.country))
  }
  who_region <- COUNTRY_NAMES %>%
    dplyr::filter(tolower(country) == tolower(this.country)) %>%
    pull(region) %>%
    parse_region_names()
  ## If country not found, thorow an error and a help message
  if (length(who_region) < 1) {
    stop(sprintf("No country matching %s in database\n
                 see for a list of valid country names and WHO regions", country))
  } else if (is.na(who_region)) {
    stop("who_region is NA. See processed-data/country_names_long_short.csv for raw reference.")
  }
  who_region
}



#' Get country-independent flu circulation data for 1918-1996
#'
#' @description
#' `get_template_data()` returns a tibble showing the fraction of influenza A cases caused by subtype H1N1, H2N2, or H3N2 in each year from 1918-1996. These data are country-independent. Country-specific data are only available from 1997 on.
#'
#' * For years 1918-1976 only one influenza A subtype circulated, so all fractions are 0 or 1.
#' * From 1977-1996 H1N1 and H3N2 both circulated. `get_template_data()` reports the fraction of influenza A-positive specimens of each subtype observed in US flu surveillance. See [Thompson et al. JAMA, 2003](https://jamanetwork.com/journals/jama/fullarticle/195750), Table 1.
#' * Country-specific data from [WHO Flu Mart](https://apps.who.int/flumart/Default?ReportNo=12) will be appended to this template in later steps.
#'
#' @return A tibble with the following columns:
#'
#' * year
#' * `A/H1N1`, `A/H2N2`, and `A/H3N2` show the fraction of influenza cases caused by each subtype.
#' *  `A` = `A/H1N1` + `A/H2N2` + `A/H3N2`
#' * `B` is a placeholder for future calculate of influenza B imprinting probabilities, which currently contains `NA`.
#' * `group1` and `group2` show the fraction of cases caused by group 1 subtypes (H1N1 and H2N2), or group 2 (H3N2).
#' * `data_from` notes the data source.
#'
#' @seealso [Gostic et al. Science, 2016](https://www.science.org/doi/10.1126/science.aag1322) for detailed methods.
#'
#' @examples `get_template_data()`
#'
#' @export
get_template_data <- function() {
  Thompson_df <- THOMPSON_DATA %>%
    mutate(
      `A/H1N1` = n_H1N1 / n_A,
      `A/H2N2` = 0,
      `A/H3N2` = n_H3N2 / n_A,
      B = NA
    ) %>%
    select(year, `A/H1N1`, `A/H2N2`, `A/H3N2`, B) %>%
    mutate(data_from = "Thompsonetal_JAMA_2003")
  ## Set up template and fill in years of single-subtype circulation
  ## Then return
  template <- bind_rows(
    ## H1N1 era
    tibble(
      year = 1918:1956,
      `A/H1N1` = 1,
      `A/H2N2` = 0,
      `A/H3N2` = 0,
      B = NA,
      data_from = "Historical_assumption"
    ),
    ## H2N2 era
    tibble(
      year = 1957:1967,
      `A/H1N1` = 0,
      `A/H2N2` = 1,
      `A/H3N2` = 0,
      B = NA,
      data_from = "Historical_assumption"
    ),
    ## H3N2 era
    tibble(
      year = 1968:1976,
      `A/H1N1` = 0,
      `A/H2N2` = 0,
      `A/H3N2` = 1,
      B = NA,
      data_from = "Historical_assumption"
    ),
    ## cocirculation 1977-1996
    Thompson_df
  ) %>%
    mutate(
      group1 = `A/H1N1` + `A/H2N2`,
      group2 = `A/H3N2`,
      A = `A/H1N1` + `A/H2N2` + `A/H3N2`
    ) %>%
    select(year, starts_with("A"), starts_with("B"), starts_with("group"), data_from)
  ## Test
  test_rowsums_group(template$group1, template$group2)
  test_rowsums_subtype(template$`A/H1N1`, template$`A/H2N2`, template$`A/H3N2`)
  return(template)
}



#' Return raw flu surveillance data for a specific WHO region
#'
#' Load and return a tibble containing raw influenza surveillance data, aggregated across all countries in the WHO region of interest. Data are from [WHO Flu Mart](https://apps.who.int/flumart/Default?ReportNo=12).
#'
#' @param region name of WHO region. Run `show_available_regions()` for a list of options.
#' @param max_year results will be output for all available years up to `max_year`.
#'
#' @returns A tibble with the following columns:
#'
#' * `WHOREGION`: name of WHO region.
#' * `Year`: calendar year .
#' * `n_H1N1`, `n_H2N2`, `n_H3N2`: number of influenza specimens that tested positive for each influenza A subtype.
#' * `n_A`: total specimens positive for influenza A (= `n_H1N1` + `n_H2N2` + `n_H3N2`).
#' * `n_BYam`, `n_BVic`: number of specimens positive for each lineage of influenza B: Victoria or Yamagata.
#' * `n_B`: total specimens positive for influenza B.
#' * `n_processed`: total specimens processed.
#'
#' @examples get_regional_inputs_1997_to_present("americas", 2017)
#'
#' @export
get_regional_inputs_1997_to_present <- function(region,
                                                max_year) {
  check_max_year(max_year)
  valid_regions <- readRDS("data/valid_regions.rds")
  ## Throw an error and a help message if region doesn't exist
  if (!region %in% valid_regions) {
    stop(sprintf("%s is not valid. \n Run show_available_regions() for a list of valid region inputs.\n Run get_WHO_region('%s') to look up %s's WHO region", region, region, region))
  }

  ## Load data from valid files
  region_data <- readRDS("data/region_data.rds")
  current_region_data <- region_data[[region]]
  check_years(current_region_data$Year, max_year)
  return(current_region_data %>% arrange(Year))
}


#' Return raw flu surveillance data for a specific country
#'
#' Load and return a tibble containing raw influenza surveillance data for the country of interest. Data are from [WHO Flu Mart](https://apps.who.int/flumart/Default?ReportNo=12).
#'
#' @param country name of country. Run `show_available_countries()` for a list of options.
#' @param max_year results will be output for all available years up to `max_year`.
#'
#' @returns A tibble with the following columns:
#'
#' * `Country`: name of WHO region
#' * `Year`: calendar year of surveillance
#' * `n_x`: number of influenza specimens that tested positive for a specific subtype of influenza A (H1N1 or H3N2), lineage of influenza B (Yamagata or Victoria), or type of influenza (A or B). Note that `n_A` should be equal to `n_H1N1+n_H3N2` and `n_B` should be equal to `n_BYam+n_BVic
#' * `n_processed`: total specimens processed
#'
#' @examples
#' get_country_inputs_1997_to_present("Aruba", 1998)
#' get_country_inputs_1997_to_present("Honduras", 2022)
#'
#' @export
get_country_inputs_1997_to_present <- function(country,
                                               max_year) { ## usually the current year
  check_max_year(max_year)
  who_region <- get_WHO_region(country)
  ## Throw an error and a help message if region doesn't exist
  valid_regions <- readRDS("data/valid_regions.rds")
  if (!who_region %in% valid_regions) {
    stop(sprintf('No files found for region: "%s" \nValid regions are: %s\nSee https://en.wikipedia.org/wiki/List_of_WHO_regions for a list of WHO regions.\nNew data files can be obtained at WHO FluMart - https://apps.who.int/flumart/Default?ReportNo=12', who_region, paste(valid_regions, sep = ", ")))
  }
  if (max_year <= 1996) {
    warning("Country-specific data are not available prior to 1996. Returning a NULL result.")
    return(NULL)
  }
  country_data <- readRDS("data/country_data.rds")
  current_country_data <- country_data[[country]] %>%
    dplyr::filter(Year <= max_year)

  if (nrow(current_country_data) == 0) {
    stop(sprintf("No data in region %s for country %s\n Run show_available_countries() for a list of valid countries.\n Run get_WHO_region('%s') to look up %s's WHO region", who_region, country, country))
  }
  check_years(current_country_data$Year, max_year)
  return(current_country_data %>% arrange(Year))
}


#' Get data on the relative circulation of each influenza A subtype
#'
#' @description
#' `get_country_cocirculation_data()` imports data on the fraction of influenza A cases in a specific country and year that were caused by each influenza A subtype (H1N1, H2N2, or H3N2), or group (group 1 or group 2). Group 1 contains H1N1 and H2N2, and group 2 contains H2N2.
#'
#' @details
#' The data come from three sources:
#'
#' * Historical assumptions: From 1918-1956, we assume only H1N1 circulated. From 1957-1967, we assume only H2N2 circulated. From 1968-1976, we assume only H3N2 circulated.
#' * [Thompson et al. JAMA, 2003](https://jamanetwork.com/journals/jama/fullarticle/195750): From 1977-1996 we pull data on the relative dominance of H1N1 and H3N2 from Table 1 of Thompson et al. 2003, which reports surveillance data collected in the United States.
#' * From 1997-present, we pull in country or region-specific data from [WHO Flu Mart](https://apps.who.int/flumart/Default?ReportNo=12) on the fraction of specimens collected in routine influenza surveillance that test positive for each subtype. Country-specific data are the default. Regional data are if the number of country-specific specimens is insufficient. [get_template_data()] imports the data for 1918-1996. [get_country_inputs_1997_to_present()] and [get_regional_inputs_1997_to_present()] import the data for 1997 on.
#'
#' @param country country of interest. Run `show_available_countries()` for a list of valid inputs.
#' @param max_year last year of interest. Results will be generated from 1918:max_year.
#' @param min_samples if fewer than `min_samples` (default 30) are reported in the country and year of interest, the function will substitute data from the corresponding WHO region.
#'
#' @return A matrix with rows showing the calendar year, the fraction of influenza A-positive specimens of each subtype (rows `A/H1N1`, `A/H2N2`, and `A/H3N2`), and of each HA group (rows `group 1`, and `group 2`). Row `A` should always be 1, as it shows the sum of subtype-specific fractions. Row `B` is a placeholder whose values are all `NA`.
#'
#' @examples
#' get_country_cocirculation_data("United States", "2019")
#' get_country_cocirculation_data("Laos", "2022", min_samples = 40)
#'
#' @seealso See [Gostic et al. Science, 2016]() for details.
#'
#' @export
get_country_cocirculation_data <- function(country,
                                           max_year,
                                           min_samples = 30) {
  check_max_year(max_year)
  template <- get_template_data()
  ## Get country data, and only keep years in which there are enough samples to meet the threshold
  country_data <- get_country_inputs_1997_to_present(country, max_year) %>%
    dplyr::filter(n_A >= min_samples) %>%
    mutate(data_from = paste0("country: ", country))
  ## Get regional data for years that don't meet the threshold
  region_data <- get_regional_inputs_1997_to_present(get_WHO_region(country), max_year) %>%
    dplyr::filter(!(Year %in% country_data$Year))

  ## Calculate the proportions of each subtype from counts,
  ## And reformat to match the template columns
  formatted_data <- bind_rows(
    region_data,
    country_data
  ) %>%
    mutate(
      `A/H1N1` = n_H1N1 / n_A,
      `A/H2N2` = 0,
      `A/H3N2` = n_H3N2 / n_A,
      B = NA
    ) %>%
    mutate(
      group1 = `A/H1N1` + `A/H2N2`,
      group2 = `A/H3N2`,
      A = `A/H1N1` + `A/H2N2` + `A/H3N2`
    ) %>%
    rename(year = Year) %>%
    select(year, starts_with("A"), starts_with("B"), starts_with("group"), data_from)

  ## Combine with the template data for pre-1977 years
  full_outputs <- bind_rows(
    template,
    formatted_data
  )
  check_years(years = full_outputs$year, max_year = max_year)
  test_rowsums_group(full_outputs$group1, group2 = full_outputs$group2)
  test_rowsums_subtype(full_outputs$`A/H1N1`, full_outputs$`A/H2N2`, full_outputs$`A/H3N2`)
  ## Format as a matrix whose column names are years
  output_matrix <- full_outputs %>%
    as.matrix() %>%
    t()
  colnames(output_matrix) <- output_matrix["year", ]
  return(output_matrix)
}


#' Get the relative intensity of influenza A circulation
#'
#' @description
#' `get_country_intensity data()` returns data on the annual intensity of influenza circulation in each calendar year. Following [Gostic et al. Science, 2016](https://www.science.org/doi/10.1126/science.aag1322), we define 1 as the average intensity. Seasons with intensities greater than 1 have more flu A circulation than average, and seasons with intensities less than 1 are mild.
#'
#' @details
#' For 1918-1996, we use annual intensities from Gostic et al., Science, 2016. For 1997-present, we calculate country or region-specific intensities using surveillance data from [WHO Flu Mart](). Intensity is calculated as:
#'  \[fraction of processed samples positive for flu A\]/\[mean fraction of processed samples positive for flu A\].
#'  Country-specific data are used by default. Regional data are substituted when there are an insufficient number of country-specific specimens.
#'
#' @param country country of interest. Run `show_available_countries()` for valid inputs.
#' @param max_year last year of interest. Results will be generated from 1918:max_year.
#' @param min_specimens if fewer than `min_specimens` (default 50) were tested in the country and year of interest, the function will substitute data from the corresponding WHO region.
#'
#' @return A tibble showing the year and intensity score.
#' @export
get_country_intensity_data <- function(country,
                                       max_year,
                                       min_specimens = 50 ## If not enough observations available, default to regional data
) {
  check_max_year(max_year)
  pre_1997_intensity <- INTENSITY_DATA %>% dplyr::filter(year <= 1997)
  ## Get country data, and only keep years in which there are enough samples to meet the threshold
  country_data <- get_country_inputs_1997_to_present(country, max_year) %>%
    dplyr::filter(n_processed >= min_specimens) %>% ## Exclude country-years that don't meet the minimum sample size
    mutate(quality_check = n_processed >= (n_A + n_B)) %>%
    dplyr::filter(quality_check == TRUE) %>% ## Exclude county-years that don't meet the quality check
    mutate(data_from = paste0("country: ", country)) %>%
    mutate(
      raw_intensity = n_A / n_processed,
      intensity = ifelse(quality_check == FALSE, 1, raw_intensity / mean(raw_intensity[quality_check == TRUE])), ## Define intensity relative to the mean
      intensity = pmin(intensity, 2.5)
    )

  ## Get regional data for years that don't meet the quality check and sample size requirements
  region_data <- get_regional_inputs_1997_to_present(get_WHO_region(country), max_year) %>%
    dplyr::filter(!(Year %in% country_data$Year)) %>%
    mutate(
      data_from = paste0("region: ", get_WHO_region(country)),
      quality_check = n_processed >= (n_A + n_B)
    ) %>%
    mutate(
      raw_intensity = n_A / n_processed,
      intensity = ifelse(quality_check == FALSE, 1, raw_intensity / mean(raw_intensity[quality_check == TRUE])), ## Define intensity relative to the mean
      intensity = pmin(intensity, 2.5)
    )

  ## Calculate the proportions of each subtype from counts,
  ## And reformat to match the template columns
  formatted_data <- bind_rows(
    region_data,
    country_data
  ) %>%
    rename(year = Year) %>%
    arrange(year) %>%
    select(year, intensity)


  ## Combine with the template data for pre-1977 years
  full_outputs <- bind_rows(
    pre_1997_intensity,
    formatted_data
  )
  # ggplot(full_outputs) + geom_point(aes(x = year, y = intensity))
  check_years(years = full_outputs$year, max_year = max_year)
  ## Format as a matrix whose column names are years
  return(full_outputs)
}
