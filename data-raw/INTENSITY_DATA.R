
INTENSITY_DATA <- read_csv("data-raw/processed-data/Intensitymaster.csv", show_col_types = F)
saveRDS(INTENSITY_DATA, "inst/extdata/INTENSITY_DATA.rds")
