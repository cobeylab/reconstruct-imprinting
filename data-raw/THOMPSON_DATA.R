THOMPSON_DATA = read_csv('data-raw/processed-data/Thompson_data.csv', show_col_types = FALSE)

usethis::use_data(THOMPSON_DATA, overwrite = TRUE)
