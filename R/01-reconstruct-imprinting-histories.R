#rm(list = ls())
source('0func-country_data_import.R')

## Function to calculate probs of first exposure in year x, given birth in year y
## INPUTS
##    - year in which an individual was born (birth.year)
##    - year in which the individual became infected with bird flu (infection year)
## OUTPUTS
##    - vector of 13 probabilities, the first representing the probability of first flu infection in the first year of life (age 0), the second representing the probability of first flu infection in the second year of life (age 1), and so on up to the 13th year of life (age 12)
get_p_infection_year = function(birth_year, 
                    infection_year,
                    baseline_annual_p_infection = 0.28,
                    max_year){
  ################# Inputs ---------------
  
  # Weighted attack rate = annual prob infection weighted by circulation intensity
  weighted.attack.rate = baseline_annual_p_infection*(intensities$Intensity)
  names(weighted.attack.rate) = 1911:max_year
  
  ################# Calculations ---------------
  jjs = birth_year:min(birth_year+12, infection_year) #Possible years of first infection (ages 0-12)
  nn = length(jjs) # How many possible years of infection?
  ajs = weighted.attack.rate[as.character(jjs)] #Get weighted attack rates corresponding to possible years of first infection
  ## Create matrices of 0s and 1s, which will be used below to vectorize and speed calculations
  ii = not_ii = matrix(0, nn, nn) #create two square matrices of dim nn
  diag(ii) = 1   #Fill in diagonal of one with 1s. Use this below as a binary indicator that marks the year of first infection. 1s will be multiplied by aj, the attack rate in year of first infection j
  not_ii[lower.tri(not_ii)] = 1  #Fill in sub-diagonal for all the years since birth in which the individual escaped infection. 1s will be multiplied by 1-ak, for all years in which a child remained naive.
  # Below, multiplying across rows of this matrix will calculate overall probability of first infection at different ages, given by prod{j = birth year, j = 1-j_first_infection}(1-aj)*a_yr_first_infection
  
  #Create a matrix that takes (1-aj) for all non-infection years and aj for all infection years
  prod.mat = ajs*ii+matrix(rep(1-ajs, nn), nn, nn, byrow = T)*not_ii
  ## Now prod.mat has probabilities of infection in a given year on the diagonal and probabilities of no infection in the sub-diagonal triangle.
  #Fill in upper triangle with 1s to make multiplication possible
  prod.mat[upper.tri(prod.mat)] = 1
  
  #Take product across rows
  p_ij = apply(prod.mat, 1, prod)
  p_ij # Output probability of first infection in year j given birth in year i.
}









## INPUTS - years.out: numeric vector of years in which data was observed.
##        - Countries.out: character vector of Countires in which data was observed
##        - region.in: if country-specific virological surveillance data is not available, should we substitute data from 'Asia' or 'Euro'?

## OUTPUTS - weights.master matrices with the country-year of case observation listed on rows, and birth year on the columns. We return four matrices: H1N1 imprinting probabilities, H3N2, H2N2 and naive
get.weights.master = function(years.out, Countries.out, region.in = 'default'){
  
  if(length(region.in) == 1) {region.in = rep('default', length(Countries.out))}
  
  # This script imports a .csv that holds data on which subtypes (H1N1, H3N2 and influenza B) were observed in specific countries, and in specific years.
  # This script also sources two functions, get.cocirculation.ref and get.country.data, which we will use below to format a matrix that tells what fraction of circulation was driven by H1N1 vs. H3N2 in a given season

  #head(cocirculation) # Data frame that records which subtypes circulated in each country and year
  
  # Set years of birth (we will estimate probabilities of imprinting to H1N1, H2N2 and H3N2 for each of these birth years)
  birth.years = 1918:2017
  infection.years = birth.years
  

  #Initialize master weight matrix
  #Rows - which country and year are we doing the reconstruction from the persepctive of?
  #Cols - what birth year are we estimating imprinting probabilities for?
  weights.master.1 = matrix(NA, nrow = length(Countries.out)*length(years.out), ncol = length(birth.years), dimnames = list(paste(rep(years.out, length(Countries.out)), rep(Countries.out, each = length(years.out)), sep = ''), rev(birth.years))) # Initialize a matrix in which to store probs of H1N1 imprinting
  weights.master.2 = weights.master.naiive = weights.master.3 = weights.master.1
  # weights.master.1 stores probs of H1N1 imprinting
  #             ...2 stores probs of H2N2 imrpinting
  #             ...3 stores probs of H3N2 imprinting
  #             ...naiive stores probs of no influenza exposure (no imprintng). This will only take nonzero values for very young birth cohorts.
  
  # The code below fills in weights.master.1, etc.
  #   Repeat the loop for each country of interest
  for(cc in 1:length(Countries.out)){ 
    # Get a table of the fraction of seasonal circulation caused by H1, H2 and H3 in the country of interest
    cocirculation.dat = import.country.dat(Countries.out[cc])

    #Extract and data from birth years of interest
    #These describe the fraction of circulating influenza viruses isolated in a given year that were of subtype H1N1 (type1), H2N2 (type2), or H3N2 (type3)
    H1.frac = (cocirculation.dat['H1', as.character(birth.years)]) 
    H2.frac = (cocirculation.dat['H2', as.character(birth.years)]) 
    H3.frac = (cocirculation.dat['H3', as.character(birth.years)]) 


## Initialize master matrix with years.out on rows and birth years on columns
H1.mat = matrix(0, nrow = length(years.out), ncol = length(birth.years), dimnames = list((years.out), (birth.years)))
naiive.mat = H2.mat = H3.mat = H1.mat
for(jj in 1:length(years.out)){
  for(ii in 1:(years.out[jj]-1918+1)){ #for all relevant birth years
    n.inf.years = min(12, years.out[jj]-birth.years[ii]) # Assume first infections can occur up to age 12, or up until the current year, whichever comes first
    inf.probs = get.e_ij(birth.years[ii], years.out[jj]) # For each birth year/infection year combination, get the imprinting probability
    
    #If all 13 possible years of infection have passed, normalize so that the probability of imprinting from age 0-12 sums to 1
    if(length(inf.probs) == 13) inf.probs = inf.probs/sum(inf.probs)
    # Else, don't normalize and extract the probability of remaiing naive below.
    
    #Fill in the appropriate row (observation year) and column (birth year) of the output matrix
    #The overall probabilty of imprinting to a specific subtype for a given birth year is the dot product of year-specific probabilities of any imprinting, and the year-specific fraction of seasonal circulation caused by the subtype of interest
    H1.mat[jj, ii] = sum(inf.probs*H1.frac[as.character(birth.years[ii:(ii+n.inf.years)])])
    H2.mat[jj, ii] = sum(inf.probs*H2.frac[as.character(birth.years[ii:(ii+n.inf.years)])])
    H3.mat[jj, ii] = sum(inf.probs*H3.frac[as.character(birth.years[ii:(ii+n.inf.years)])])
    naiive.mat[jj, ii] = round(1-sum(inf.probs), digits = 8) #Rounds to the nearest 8 to avoid machine 0 errors
  }
}


#return the output in order of current_year:1918
H1.mat = H1.mat[,as.character(max(birth.years):min(birth.years))]
H2.mat = H2.mat[,as.character(max(birth.years):min(birth.years))]
H3.mat = H3.mat[,as.character(max(birth.years):min(birth.years))]
naiive.mat = naiive.mat[,as.character(max(birth.years):min(birth.years))]


## Hx.mat describes the chunk of the master output matrix specific to country cc.
##  Fill in the appropriate chunk and then repeat for other countries of interest
weights.master.1[((cc-1)*length(years.out))+1:length(years.out), ] = H1.mat #Fill in the appropriate country-year row of the weights.master.## matrix
weights.master.2[((cc-1)*length(years.out))+1:length(years.out), ] = H2.mat
weights.master.3[((cc-1)*length(years.out))+1:length(years.out), ] = H3.mat
weights.master.naiive[((cc-1)*length(years.out))+1:length(years.out), ] = naiive.mat
  }
  
  ## Check that the total for each birth year is 1 when rounded to 4 decimals
  total = weights.master.1+weights.master.2+weights.master.3+weights.master.naiive
  if(any(! (round(total, 4)%in%c(0,1)) )){warning('Weights do not sum to 1')}
  
  ## Normalize so that sum of weights is exactly 1 in each birth year
  total[which(total==0)] = 1 # Reset 0 values so as not to divide by 0
  weights.master.1 = weights.master.1/(total)
  weights.master.2 = weights.master.2/(total)
  weights.master.3 = weights.master.3/(total)
  weights.master.naiive = weights.master.naiive/(total)

return(list(weights.master.1 = weights.master.1, weights.master.2=weights.master.2, weights.master.3=weights.master.3, weights.master.naiive=weights.master.naiive))
}


# # Test Outputs
# yo = c(1997, 2003:2017)
# co = c('China', 'Cambodia', 'Egypt', 'Indonesia', 'Vietnam', 'Thailand')
# wts = get.weights.master(years.out = yo, Countries.out = co, region.in = rep('Asia', 6))
