valid_regions <- list.files("data-raw/who/")
saveRDS(valid_regions, "inst/extdata/valid_regions.rds")
