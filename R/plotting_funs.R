## Functions to plot the outputs of `get_imprinting_probabilities`

#' Plot imprinting probabilities for a single country and year
#' 
#' Generate a stacked barplot, where each bar represents a birth cohort, and the colors within the bar show the probabilities that someone born in that cohort has a particular imprinting status.
#' 
#' @param imprinting_df A long data frame of imprinted probabilities output by [get_imprinting_probabilities()]. If the data frame contains more than one country and year, on the first will be plotted.
#' 
#' @examples 
#' plot_one_country_year(imprinting_df = get_imprinting_probabilities(observation_years = 2022, countries = 'Aruba'))
#' 
#' imprinting_df = get_imprinting_probabilities(observation_years = 1997, countries = c('Algeria', 'South Africa'))
#' plot_one_country_year(imprinting_df)
#' @export
plot_one_country_year <- function(imprinting_df){
  ## This function plots imprinting patterns for a single country-year
  ## If the data frame contains more than one country-year, it plots the first listed
  countries = unique(imprinting_df$country)
  years = unique(imprinting_df$year)
  obs_year = years[1]
  axis_ticks = seq(1920, obs_year, by = 10)
  replace_these = which(axis_ticks %in% c(1960, 1970, 1980))
  axis_ticks[replace_these] = c(1957, 1968, 1977) ## Replace 3 axis ticks with pandemic years
  axis_tick_labs = sapply(axis_ticks, function(yr){sprintf('%i\n%i', yr, obs_year-yr)}) # Label each tick with the birth year/current age of that cohort
  x_axis_text = sprintf('birth year\nage in %s', obs_year) ## Axis label is birth year[newline]age now
  colors = c('dodgerblue1', 'lightblue', 'firebrick2', 'gray')
  imprinting_df %>%
    dplyr::filter(country == countries[1]) %>%
    dplyr::filter(year == obs_year) %>%
    ggplot() +
    geom_bar(aes(x = birth_year, y = imprinting_prob, fill = subtype), stat = 'identity') +
    xlab(x_axis_text)+
    ylab('imprinting fraction')+
    scale_color_manual(values = colors, aesthetics = c('color', 'fill')) +
    scale_x_continuous(breaks = axis_ticks, labels = axis_tick_labs) +
    ggtitle(sprintf('Probabilities for %s in %i', countries[1], obs_year))
}


#' Plot imprinting probabilities for up to five country-years
#' 
#' For each country and year, generate two plots:
#' * A stacked barplot, where each bar represents a birth cohort, and the colors within the bar show the probabilities that someone born in that cohort has a particular imprinting status, for the first observation year.
#' * A lineplot showing the age-specific probability of imprinting to H3N2 in the first and last observation year. When the data contain more than one observation year, this plot shows how cohorts age over time.
#' 
#' @param imprinting_df A long data frame of imprinted probabilities output by [get_imprinting_probabilities()]. Up to five countries and an arbitrary span of years can be plotted.
#' 
#' @examples 
#' plot_many_country_years(imprinting_df = get_imprinting_probabilities(observation_years = 2010:2022, countries = c('Spain', 'Vietnam', 'France')))
#' 
#' imprinting_df = get_imprinting_probabilities(observation_years = 1997:2000, countries = c('Oman', 'Indonesia'))
#' plot_many_country_years(imprinting_df)
#' @export
plot_many_country_years <- function(imprinting_df){
  countries = unique(imprinting_df$country)
  if(length(countries)>5){warning('Plotting only the first 5 countires.')}
  years = unique(imprinting_df$year)
  max_obs_year = max(years)
  min_obs_year = min(years)
  axis_ticks = seq(1920, max_obs_year, by = 10)
  replace_these = which(axis_ticks %in% c(1960, 1970, 1980))
  axis_ticks[replace_these] = c(1957, 1968, 1977) ## Replace 3 axis ticks with pandemic years
  axis_tick_labs = sapply(axis_ticks, function(yr){sprintf('%i\n%i', yr, max_obs_year-yr)}) # Label each tick with the birth year/current age of that cohort
  x_axis_text = sprintf('birth year\nage in %s', max_obs_year) ## Axis label is birth year[newline]age now
  colors = c('dodgerblue1', 'lightblue', 'firebrick2', 'gray')
  ## Print a max of 5 countries
  imprinting_df <- mutate(imprinting_df, plot_number = ceiling(as.numeric(as.factor(country))/5))
  ## For each country, plot barplots in the max year of observation
  bar_plots = imprinting_df %>%
      dplyr::filter(plot_number == 1) %>% ## Only include up to 5 countries
      dplyr::filter(year == max_obs_year) %>%
      ggplot() +
      geom_bar(aes(x = birth_year, y = imprinting_prob, fill = subtype), stat = 'identity') +
      xlab(x_axis_text)+
      ylab('imprinting fraction')+
      scale_color_manual(values = colors, aesthetics = c('color', 'fill')) +
      scale_x_continuous(breaks = axis_ticks, labels = axis_tick_labs) +
      ggtitle(sprintf('Probabilities in %i', max_obs_year)) +
      facet_grid(country~.)+
      theme(legend.position = 'bottom')+
      guides(fill = guide_legend(nrow=2))
  ## For each country, plot lineplots of the H1N1 imprinting probs, in the min and max years of observation
  line_plots = imprinting_df %>%
      dplyr::filter(plot_number == 1) %>%
      dplyr::filter(year %in% c(max_obs_year, min_obs_year)) %>%
      dplyr::filter(subtype %in% c('H3N2')) %>%
      mutate(age_at_observation = year-birth_year,
             pandemic_1968 = year-1968) %>%
      dplyr::filter(age_at_observation >= 0) %>%
      ggplot() +
      geom_vline(aes(xintercept = pandemic_1968, lty = as.factor(year)), show.legend = F)+
      geom_line(aes(x = age_at_observation, y = imprinting_prob, lty = as.factor(year)), color = 'firebrick2') +
      xlab('Age at time of observation')+
      ylab('H3N2\nimprinting fraction')+
      geom_text(aes(x=min_obs_year-1968-2, y=0.15, label = '1968 birth yr'), angle = 90, size = 3)+
      scale_linetype(name = 'Year of observation\n  \n \n \n ')+
      ggtitle('Aging of imprinted birth cohorts') +
      facet_grid(country~.) +
      theme(legend.position = 'bottom')
  if(length(years)>1){
    line_plots = line_plots+geom_segment(aes(x = min_obs_year-1968, xend = max_obs_year-1968, y = .25, yend = 0.25), arrow = arrow(length = unit(.05, 'in')))
  }
  ## return an image with barplots on the left and lineplots on the right
  cowplot::plot_grid(bar_plots, line_plots, ncol = 2)
}
