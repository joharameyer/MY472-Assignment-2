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
# Input: population dataset 
# Output: calculated percentage change in population inside text snippet

# !Warning!
# This function was written using column names from the 'population' dataset
# found in the 'tidyr' package. In order to use this function with a different dataset some
# columns may need to be renamed. The code below shows how to rename columns to match the
# names used in the function below
# names(data)[names(data) == 'old.country.var.name'] <- 'country'
# names(data)[names(data) == 'old.year.var.name'] <- 'year'
# names(data)[names(data) == 'old.year.var.name'] <- 'population'

pop.change <- function(data) {
  
  ## Preparation
  # Define helper function
  
  `%not_in%` <- purrr::negate(`%in%`)
  
  ## Check Assumption
  # Assumption 1: population data is available for all countries across the years of interest
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
  
  #Output
  print(paste0("On average population size changed by ", round(mean(change.df$perct_change), 2),
               " percent between ", firstyear," and ", lastyear, " across the countries in the dataset."))
  print(ifelse(length(no.data)>0,
               paste0("Please note that the following countries were removed due to missing data: ",
                      paste(no.data$country, collapse = ", "), "."),
               paste0("")))
}
