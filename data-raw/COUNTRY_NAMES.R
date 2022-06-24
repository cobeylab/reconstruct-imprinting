COUNTRY_NAMES = read_csv('data-raw/processed-data/country_names_long_short.csv', show_col_types = FALSE)
usethis::use_data(COUNTRY_NAMES, overwrite = TRUE)
