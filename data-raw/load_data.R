COUNTRY_NAMES <- read_csv("data-raw/processed-data/country_names_long_short.csv", show_col_types = FALSE)
THOMPSON_DATA <- read_csv("data-raw/processed-data/Thompson_data.csv", show_col_types = FALSE)
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

get_region_data <- function() {
  paths <- list.files("raw-data/") %>%
    lapply(function(region) tolower(parse_region_names(region))) %>%
    lapply(function(region) list.files(sprintf("raw-data/%s", region), full.names = TRUE)) %>%
    flatten()
  dfs <- lapply(paths, function(path) read_csv(path, skip = 3, col_types = "ccciicciiiiiiiiiiiiiic"))
  names(dfs) <- paths
  dfs
}

REGION_DATA <- get_region_data()
