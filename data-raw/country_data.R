library(tidyverse)
options(dplyr.summarise.inform = FALSE)

source("data-raw/load_data.R")
source("R/data_import_funs.R")

country_data = lapply(COUNTRY_NAMES$country, function(country) {
  who_region = get_WHO_region(country)
  file_list = list.files(sprintf('data-raw/who/%s/', who_region))

  country_data_current = lapply(file_list, function(this_file){
    this_filepath = sprintf('data-raw/who/%s/%s', tolower(who_region), this_file)
    read_csv(this_filepath, skip = 3, show_col_types = FALSE) %>% ## Read in the file
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
})

names(country_data) <- COUNTRY_NAMES$country

saveRDS(country_data, 'data/country_data.rds')
