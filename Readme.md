This repository contains files that can be used to reconstruct birth-year specific probabilities of imprinting to influenza A, but subtype or by group, following the methods of [(Gostic et al. 2016)](https://www.science.org/doi/10.1126/science.aag1322).

Imprinting probabilities are specific to the country and year of observation:

* COUNTRY - For years 1997-present, we pull data from [WHO Flu Mart](https://apps.who.int/flumart/Default?ReportNo=12) to estimate the fraction of influenza A infections caused by subyptes H1N1 and H3N2. We use country-specific data whenever there are 30 or more influenza A samples available in the country and year of interest. If sample sizes are insufficient, we pull in data from the country's [WHO region](https://en.wikipedia.org/wiki/List_of_WHO_regions).
*  YEAR OF OBSERVATION - We asume children <13 years of age can be naive to influenza. The year of observation affects which birth years can be naive. In birth years >13 years of age, we assume that everyone has been infected at least one, and normalize so that subtype-specific imprinting probabilities sum to one [(Gostic et al. 2016)](https://www.science.org/doi/10.1126/science.aag1322).


# Data

### Fraction of influenza A infections caused by A/H1N1, A/H2N2, and A/H3N2 over time

* From 1918-1976, only one influenza A subtype circulated at a time, and pandemic years mark the transitions between subtypes. We assume that H1N1 caused 100% of influenza A cases from 1918-1956, that H2N2 causes 100% of influenza A cases from 1957-1967, and that H3N2 caused 100% of influenza A cases from 1968-1976.
* From 1977-present, A/H3N2 and A/H1N1 have both caused seasonal epidemics. The fraction of influenza cases caused by either subtype varies from year to year. Data avilability has improved over time:

  * From **1977-1996**, data on the relative dominance of A/H1N1 and A/H3N2 are limited. We use United States-specific data from Table 1 of [Thompson et al. *JAMA*, 2003](https://jamanetwork.com/journals/jama/fullarticle/195750) to estimate the fraction of flu A cases caused by each subtype. These data are stored in `processed-data/Thompson_data.csv`.
  * From **1997-present**, we use country or region-specific surveillance data from [WHO Flu Mart](https://apps.who.int/flumart/Default?ReportNo=12). We use country-specific data when more than 30 flu A specimens were reported in the year of interest, and region-specific data otherwise.
        
### Relative intensity of influenza circulation over time

* TO WRITE


# Workflow

All code runs in the /R/ directory.

* `get_imprinting_probabilities()` is the main function. It inputs a vector of observation years, a vector of country names, and the maximum year it outputs a matrix of birth year-specific imprinting probabilities for each country and observation year.  `script_calculate_imprinting_probs.R` is a script that shows example calls to `get_imprinting_probabilities()`

# Code

* `calculation_funs.R` contains functions used to calculate imprinting probabilities.
* `data_import_funs.R` contains functions used to import country or region-specific data on the fraction of infections caused by difference influenza A subtypes over time.

# Developer notes

* `raw-data/` contains influenza surveillance data for each [WHO region](https://en.wikipedia.org/wiki/List_of_WHO_regions), downloaded from [WHO Flu Mart](https://apps.who.int/flumart/Default?ReportNo=12).
* `processed-data/` contains data on the scaled annual intensity of influenza circulation [(Gostic et al. 2016)](https://www.science.org/doi/10.1126/science.aag1322), data from Table 1 of [Thompson et al. *JAMA*, 2003](https://jamanetwork.com/journals/jama/fullarticle/195750), and a table of WHO regions and countries obtained [here](https://en.wikipedia.org/wiki/List_of_WHO_regions).

# To do

[x] Update readme 

[x] Finish downloading data for all WHO regions

[x] Check country and region names

[] Unit tests

[] System tests

[] Plan for systematic data updates (can we automate this?)

[] Plan output filestructure and format.

[] Update Intensities in data and readme.

[] Reformat outputs to a long data frame.
