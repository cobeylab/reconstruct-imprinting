library(tidyverse)
options(dplyr.summarise.inform = FALSE)

## Generate imprinting probabilities
rm(list = ls())
source('calculation_funs.R')
source('data_import_funs.R')

## NOTES:
## Currently, the maximum allowed observation_years value is 2017. I need to update the data to fix this.
## We may want to format these outputs as a long data frame instead of a list of matrices. The current format is a holdover from my previous work.

## Get probabilities for the United States in a single observation year
get_imprinting_probabilities(observation_years = 2010, 
                             countries = c('United States'))

## Get probabilities for the United States in a many observation years
get_imprinting_probabilities(observation_years = 2010:2017, 
                             countries = c('United States'))

## Get probabilities for the other countries in one observation year
get_imprinting_probabilities(observation_years = 2015, 
                             countries = c('Mexico'))

get_imprinting_probabilities(observation_years = 2015, 
                             countries = c('Germany'))

get_imprinting_probabilities(observation_years = 2015, 
                             countries = c('Egypt'))

get_imprinting_probabilities(observation_years = 2015, 
                             countries = c('Kenya'))

get_imprinting_probabilities(observation_years = 2015, 
                             countries = c('Australia'))

get_imprinting_probabilities(observation_years = 2015, 
                             countries = c('Cambodia'))

## Get probabilities for the many countries in one observation year
get_imprinting_probabilities(observation_years = 2015, 
                             countries = c('Bolivia', 'France', 'Iran', 'South Africa', 'New Zealand', 'Vietnam', 'United States'))


## Get probabilities for many countries in several observation year
get_imprinting_probabilities(observation_years = 2015:2017, 
                             countries = c('Argentina', 'United Kingdom', 'Canada', 'Israel'))

