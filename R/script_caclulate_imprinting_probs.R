## Generate imprinting probabilities
rm(list = ls())
source('calculation_funs.R')
source('data_import_funs.R')

get_imprinting_probabilities(max_year = 2022, 
                             observation_years = 2010, 
                             countries = c('United States'))

get_imprinting_probabilities(max_year = 2022, 
                             observation_years = 1997:2010, 
                             countries = c('United States', 'Mexico', 'Germany'))
