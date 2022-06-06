# Read data from CSVs into COUNTRY_NAMES, THOMPSON_DATA, and REGION_DATA
source('load_data.R')

test_rowsums_subtype <- function(H1N1, H2N2, H3N2){
 stopifnot(all(H1N1+H3N2+H2N2 == 1 | is.na(H1N1+H3N2+H2N2)))
}

test_rowsums_group <- function(group1, group2){
  stopifnot(all(group1+group2 == 1 | is.na(group1+group2)))
}

check_years <- function(years, max_year){
  stopifnot(1997:max_year %in% years)
}

show_available_countries <- function(){
  COUNTRY_NAMES %>%
    select(country)
}


show_available_regions <- function(){
  COUNTRY_NAMES %>%
    select(region) %>%
    distinct() 
}


parse_country_names <- function(this.country){
  ## Convert short country names to country names in the Flunet data files
  COUNTRY_NAMES %>%
    dplyr::filter(country == this.country) %>%
    pull(flunet_country) %>%
    return()
}


get_WHO_region <- function(this.country){
  who_region = COUNTRY_NAMES %>%
    dplyr::filter(tolower(country) == tolower(this.country)) %>%
    pull(region) %>%
    parse_region_names()
  ## If country not found, thorow an error and a help message
  if(length(who_region)<1){
    stop(sprintf('No country matching %s in database\n
                 see for a list of valid country names and WHO regions', country))
  }else if(is.na(who_region)){
    stop('who_region is NA. See ../processed-data/country_names_long_short.csv for raw reference.')
  }
  who_region
}


get_template_data <- function(){
  ## From 1918-1976, only 1 flu A subtype circulated
  ## From 1977-1996, country-specific data is not available
  ## For this time period, output a template of country and region-independent data
  ## Columns:
  ##  * year: 1918:1996
  ##  * `A/H1N1`: fraction of flu A positive specimens of subytpe H1N1
  ##  * `A/H2N2`: fraction of flu A positive specimens of subytpe H2N2
  ##  * `A/H3N2`: fraction of flu A positive specimens of subytpe H3N2
  ##  * `B`: placeholder -- all NA
  ##  * group1: fraction of flu A positive specimens of group1 (H1N1+H2N2)
  ##  * group2: fraction of flu A positive specimens of group2 (H3N2)
  ## Load epidemiologica data from 1977-1996
  ## From Thompson et al. JAMA, 2003 (JAMA. 2003;289(2):179-186. doi:10.1001/jama.289.2.179)
  Thompson_df = THOMPSON_DATA %>%
    mutate(`A/H1N1` = n_H1N1/n_A,
           `A/H2N2` = 0,
           `A/H3N2` = n_H3N2/n_A,
            B = NA) %>%
    select(year, `A/H1N1`, `A/H2N2`, `A/H3N2`, B) %>%
    mutate(data_from = 'Thompsonetal_JAMA_2003')
  ## Set up template and fill in years of single-subtype circulation
  ## Then return
  template = bind_rows(
    ## H1N1 era
    tibble(year = 1918:1956, 
           `A/H1N1` = 1,
           `A/H2N2` = 0,
           `A/H3N2` = 0,
           B = NA,
           data_from = 'Historical_assumption'),
    ## H2N2 era
    tibble(year = 1957:1967, 
           `A/H1N1` = 0,
           `A/H2N2` = 1,
           `A/H3N2` = 0,
           B = NA,
           data_from = 'Historical_assumption'),
    ## H3N2 era
    tibble(year = 1968:1976, 
           `A/H1N1` = 0,
           `A/H2N2` = 0,
           `A/H3N2` = 1,
           B = NA,
           data_from = 'Historical_assumption'),
    ## cocirculation 1977-1996
    Thompson_df
    ) %>%
    mutate(group1 = `A/H1N1` + `A/H2N2`,
           group2 = `A/H3N2`,
           A = `A/H1N1`+`A/H2N2`+`A/H3N2`) %>%
    select(year, starts_with('A'), starts_with('B'), starts_with('group'), data_from)
  ## Test
  test_rowsums_group(template$group1, template$group2)
  test_rowsums_subtype(template$`A/H1N1`, template$`A/H2N2`, template$`A/H3N2`)
  return(template)
}


get_regional_inputs_1997_to_present <- function(region, ## 'Americas', 'Europe', 'Asia' are current options
                                              max_year){ ## usually the current year 
   ## Get a list of the raw data files
  file_list = list.files(sprintf('../raw-data/%s/', parse_region_names(region)))
   ## Throw an error and a help message if region doesn't exist
  if(length(file_list)<1){
    valid_regions = paste(list.files('../raw-data/'), collapse = ', ')
    stop(sprintf('No files found for region:%s \nValid regions are: {%s}\nSee https://en.wikipedia.org/wiki/List_of_WHO_regions for a list of WHO regions.\nNew data files can be obtained at WHO FluMart - https://apps.who.int/flumart/Default?ReportNo=12', parse_region_names(region), valid_regions))
  }
  
  ## Load data from valid files
  region_data = lapply(file_list, function(this_file){ ## For each data file...
    this_filepath = sprintf('../raw-data/%s/%s', tolower(region), this_file)
    REGION_DATA[[this_filepath]] %>% ## Read in the file
      group_by(`WHOREGION`, Year) %>% ## Summarize the number of positive samples of each subtype observed in each year
      summarise(n_H1N1 = sum(AH1, na.rm = T)+sum(AH1N12009, na.rm = T),
                n_H3N2 = sum(AH3, na.rm = T),
                n_A = n_H1N1+n_H3N2,
                n_BYam = sum(BYAMAGATA, na.rm = T),
                n_BVic = sum(BVICTORIA, na.rm = T),
                n_B = n_BYam+n_BVic,
                n_processed = sum(SPEC_PROCESSED_NB, na.rm = T)) %>% ungroup()
  }) %>%
    bind_rows() ## Combine into a single data frame
  check_years(region_data$Year, max_year)
  return(region_data)
}


get_country_inputs_1997_to_present <- function(country, 
                                             max_year){ ## usually the current year 
  who_region = get_WHO_region(country) 
  ## Get a list of the raw data files
  file_list = list.files(sprintf('../raw-data/%s/', who_region))
  ## Throw an error and a help message if region doesn't exist
  if(length(file_list)<1){
    valid_regions = paste(list.files('../raw-data/'), collapse = ', ')
    stop(sprintf('No files found for region:%s \nValid regions are: {%s}\nSee https://en.wikipedia.org/wiki/List_of_WHO_regions for a list of WHO regions.\nNew data files can be obtained at WHO FluMart - https://apps.who.int/flumart/Default?ReportNo=12',  who_region, valid_regions))
  }
  
  ## Load data from valid files
  country_data = lapply(file_list, function(this_file){ ## For each data file...
    this_filepath = sprintf('../raw-data/%s/%s', tolower(who_region), this_file)
    REGION_DATA[[this_filepath]] %>% ## Read in the file
      dplyr::filter(tolower(Country) == tolower(parse_country_names(country))) %>% ## Keep only the country of interest
      group_by(Country, Year) %>% ## Summarize the number of positive samples of each subtype observed in each year
      summarise(n_H1N1 = sum(AH1, na.rm = T)+sum(AH1N12009, na.rm = T),
                n_H3N2 = sum(AH3, na.rm = T),
                n_A = n_H1N1+n_H3N2,
                n_BYam = sum(BYAMAGATA, na.rm = T),
                n_BVic = sum(BVICTORIA, na.rm = T),
                n_B = n_BYam+n_BVic,
                n_processed = sum(SPEC_PROCESSED_NB, na.rm = T)) %>%
      ungroup()
  }) %>%
    bind_rows() ## Combine into a single data frame
  if(nrow(country_data) == 0){
    stop(sprintf('No data in region %s for country %s\n See raw data files in ../raw-data/%s/ to check spelling.', who_region, country, who_region))
  }
  check_years(country_data$Year, max_year)
  return(country_data)
}


get_country_cocirculation_data <- function(country,
                             max_year,
                             min_fluA_positive_samples_per_year = 30 ## If not enough observations available, default to regional data
                             ){
  ## Input - country name
  ## Output - Data frame containing:
  ##  * year: 1918:max_year
  ##  * `A/H1N1`: fraction of flu A positive specimens of subytpe H1N1
  ##  * `A/H2N2`: fraction of flu A positive specimens of subytpe H2N2
  ##  * `A/H3N2`: fraction of flu A positive specimens of subytpe H3N2
  ##  * `B`: placeholder -- all NA
  ##  * group1: fraction of flu A positive specimens of group1 (H1N1+H2N2)
  ##  * group2: fraction of flu A positive specimens of group2 (H3N2)
  
  template = get_template_data()
  ## Get country data, and only keep years in which there are enough samples to meet the threshold
  country_data = get_country_inputs_1997_to_present(country, max_year) %>%
    dplyr::filter(n_A >= min_fluA_positive_samples_per_year) %>%
    mutate(data_from = paste0('country: ', country))
  ## Get regional data for years that don't meet the threshold
  region_data = get_regional_inputs_1997_to_present(get_WHO_region(country), max_year) %>%
    dplyr::filter(!(Year %in% country_data$Year))
  
  ## Calculate the proportions of each subtype from counts,
  ## And reformat to match the template columns
  formatted_data = bind_rows(region_data,
            country_data) %>%
    mutate(`A/H1N1` = n_H1N1/n_A,
           `A/H2N2` = 0,
           `A/H3N2` = n_H3N2/n_A,
           B = NA) %>%
    mutate(group1 = `A/H1N1`+`A/H2N2`,
           group2 = `A/H3N2`,
           A = `A/H1N1` + `A/H2N2` + `A/H3N2`) %>%
           rename(year = Year)%>%
    select(year, starts_with('A'), starts_with('B'), starts_with('group'), data_from)
  
  ## Combine with the template data for pre-1977 years
  full_outputs = bind_rows(template,
                           formatted_data)
  check_years(years = full_outputs$year, max_year = max_year)
  test_rowsums_group(full_outputs$group1, group2 = full_outputs$group2)
  test_rowsums_subtype(full_outputs$`A/H1N1`, full_outputs$`A/H2N2`, full_outputs$`A/H3N2`)
  ## Format as a matrix whose column names are years
  output_matrix = full_outputs %>%
    as.matrix() %>%
    t()
  colnames(output_matrix) = output_matrix['year',]
  return(output_matrix)    
}


get_country_intensity_data <- function(country,
                                       max_year,
                                       min_samples_processed_per_year = 30 ## If not enough observations available, default to regional data
){
  ## Input - country name
  ## Output - Data frame containing:
  ##  * year: 1918:max_year
  ##  * intensity: [fraction of processed samples positive for flu A]/[mean fraction of processed samples positive for flu A]
  
  pre_1997_intensity = INTENSITY_MASTER %>% dplyr::filter(year <= 1997)
  ## Get country data, and only keep years in which there are enough samples to meet the threshold
  country_data = get_country_inputs_1997_to_present(country, max_year) %>%
    dplyr::filter(n_A >= min_samples_processed_per_year) %>%
    mutate(data_from = paste0('country: ', country))
  ## Get regional data for years that don't meet the threshold
  region_data = get_regional_inputs_1997_to_present(get_WHO_region(country), max_year) %>%
    dplyr::filter(!(Year %in% country_data$Year))
  
  ## Calculate the proportions of each subtype from counts,
  ## And reformat to match the template columns
  formatted_data = bind_rows(region_data,
                             country_data) %>%
    mutate(raw_intensity = n_A/n_processed,
           intensity = raw_intensity/mean(raw_intensity, na.rm = T), ## Define intensity relative to the mean
           intensity = pmin(intensity, 2.5)) %>% ## Max intensity is 2.5
    rename(year = Year)%>%
    select(year, intensity)

  
  ## Combine with the template data for pre-1977 years
  full_outputs = bind_rows(pre_1997_intensity,
                           formatted_data)
  # ggplot(full_outputs) + geom_point(aes(x = year, y = intensity))
  check_years(years = full_outputs$year, max_year = max_year)
  ## Format as a matrix whose column names are years
  return(full_outputs)    
}



