THOMPSON_DATA <- read_csv("data-raw/processed-data/Thompson_data.csv", show_col_types = FALSE)

saveRDS(THOMPSON_DATA, "inst/extdata/THOMPSON_DATA.rds")
