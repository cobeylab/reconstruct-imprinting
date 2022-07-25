library(tidyverse)
options(dplyr.summarise.inform = FALSE)

source("data-raw/load_data.R")
source("R/data_import_funs.R")

regions <- list.files("data-raw/who/")

region_data <- lapply(regions, function(region) {
  ## Get a list of the raw data files
  file_list <- list.files(sprintf("data-raw/who/%s/", parse_region_names(region)))
  ## Throw an error and a help message if region doesn't exist
  if (length(file_list) < 1) {
    valid_regions <- paste(list.files("data-raw/who/"), collapse = ", ")
    stop(sprintf("No files found for region:%s \nValid regions are: {%s}\nSee https://en.wikipedia.org/wiki/List_of_WHO_regions for a list of WHO regions.\nNew data files can be obtained at WHO FluMart - https://apps.who.int/flumart/Default?ReportNo=12", parse_region_names(region), valid_regions))
  }

  ## Load data from valid files
  region_data <- lapply(file_list, function(this_file) { ## For each data file...
    this_filepath <- sprintf("data-raw/who/%s/%s", tolower(region), this_file)
    read_csv(this_filepath, skip = 3, show_col_types = FALSE) %>% ## Read in the file
      group_by(`WHOREGION`, Year) %>% ## Summarize the number of positive samples of each subtype observed in each year
      summarise(
        n_H1N1 = sum(AH1, na.rm = T) + sum(AH1N12009, na.rm = T),
        n_H3N2 = sum(AH3, na.rm = T),
        n_A = n_H1N1 + n_H3N2,
        n_BYam = sum(BYAMAGATA, na.rm = T),
        n_BVic = sum(BVICTORIA, na.rm = T),
        n_B = n_BYam + n_BVic,
        n_processed = sum(SPEC_PROCESSED_NB, na.rm = T)
      ) %>%
      ungroup()
  }) %>%
    bind_rows() ## Combine into a single data frame
})

names(region_data) <- regions

saveRDS(region_data, "data/region_data.rds")
