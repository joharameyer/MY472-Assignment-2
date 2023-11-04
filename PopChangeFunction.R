## Function pop.change()
# Calculates population change and inserts outcome into text chunk
# load packages
library(tidyr)
library(tidyverse)
library(purrr)
library(dplyr)

# Load dataset 'population' from tidyr
data('population')

# Create function which calculates population change between two years and prints text
# Input: population dataset and country of interest
# Output: calculated percentage change in population and country rank inside text snippet

# !Warning!
# This function was written using column names from the 'population' dataset
# found in the 'tidyr' package. In order to use this function with a different dataset some
# columns may need to be renamed. The code below shows how to rename columns to match the
# names used in the function below
# names(data)[names(data) == 'old.country.var.name'] <- 'country'
# names(data)[names(data) == 'old.year.var.name'] <- 'year'
# names(data)[names(data) == 'old.year.var.name'] <- 'population'

pop.change <- function(data, country) {
  
  ## Preparation
  # Define helper function
  # Code for helper function was taken from (https://stackoverflow.com/questions/5831794/opposite-of-in-exclude-rows-with-values-specified-in-a-vector)
  
  `%not_in%` <- purrr::negate(`%in%`)
  
  ## Check Assumption
  # Assumption 1: population data is available for all countries across the years of interest.
  # In this case, we are interested in the first and last year for which data is available
  # in the dataset.
  
  # define first and last year 
  firstyear <- min(data$year)
  lastyear <- max(data$year)
  
  # check if data is missing for years of interest
  check.df <- data %>%
    arrange(country, year) %>% 
    group_by(country) %>%
    summarise(check.year = if_else((firstyear %in% year && lastyear %in% year), 'Complete','Missing'),
              check.data = if_else(is.na(population[year == first(year)])|
                                     is.na(population[year == last(year)]), 'Missing','Complete' ))
  
  no.data <- check.df %>% filter(check.year == "Missing" | check.data == "Missing") 
  
  ## removes countries for which data is missing 
  data <- subset(data, data$country %not_in% no.data$country)
  
  ## Calculation   
  # calculate population change in percent
  change.df <- data  %>%
    arrange(country, year) %>% # arrange ensures that the order is correct (earlier to later years)
    group_by(country) %>%
    summarise(perct_change = (last(population) - first(population))/first(population)*100) %>%
    ungroup()
  
  # calculate mean percentage change across all countries 
  mean.change <- mean(change.df$perct_change)
  
  # calculate percentage change for country defined
  country.change <- change.df$perct_change[change.df$country == country]
  
  # calculate rank of each country 
  change.df$low2high <- rank(change.df$perct_change)
  change.df$high2low <- rank(- change.df$perct_change)
  
  
  ##Output
  
  if (country.change > mean.change) {
    
    # calculate rank of country defined high to low
    country.rank <- change.df$high2low[change.df$country == country]
    
    # print text
    print(paste0("On average population size changed by ", round(mean(change.df$perct_change), 2),
                 " percent between ", firstyear," and ", lastyear, " across the countries in the dataset.", " At ", round(country.change,2)," percent population change ", country ," was ", round(abs((country.change - mean.change)),2) ," percent above the average. This places ", country," as the country with the ", country.rank, ". largest increase in population numbers."))
    
    print(ifelse(length(no.data)>0,
                 paste0("Please note that the following countries were removed due to missing data: ",
                        paste(no.data$country, collapse = ", "), "."),
                 paste0("")))
    
  } else {
    
    # calculate rank of country defined low to high
    country.rank <- change.df$low2high[change.df$country == country]
    
    print(paste0("On average population size changed by ", round(mean(change.df$perct_change), 2),
                 " percent between ", firstyear," and ", lastyear, " across the countries in the dataset.", " At ", round(country.change,2)," percent population change ", country ," was ", round(abs((country.change - mean.change)),2) ," percent below the average. This places ", country," as the country with the ", country.rank, ". lowest increase in population numbers."))
    
    print(ifelse(length(no.data)>0,
                 paste0("Please note that the following countries were removed due to missing data: ",
                        paste(no.data$country, collapse = ", "), "."),
                 paste0("")))
    
  }
}