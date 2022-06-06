## Generate imprinting probabilities
rm(list = ls())
library(tidyverse)
options(dplyr.summarise.inform = FALSE)
source('calculation_funs.R')
source('data_import_funs.R')

## Get probabilities for the United States in a single observation year
get_imprinting_probabilities(observation_years = 2010, 
                             countries = c('United States'))

## Get probabilities for the United States in a many observation years
get_imprinting_probabilities(observation_years = 2010:2022, 
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

get_imprinting_probabilities(observation_years = c(2007, 2010, 2022), 
                             countries = c('Venezuela', 'Laos', 'Lithuania'))

