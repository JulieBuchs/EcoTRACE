library(tidyverse)

FUNStormEventsData_filterData <- function(
  myData,
  myYears,
  myMonths,
  myEventTypes
) {
  
  myData <- myData %>% 
    mutate(fips = state_county_fips)
  
  # Create data for US plot
  dataForUsPlot <- 
    myData %>% 
    filter(year %in% myYears) %>% 
    filter(month_name %in% myMonths) %>% 
    filter(event_type %in% myEventTypes) %>% 
    distinct(episode_id, fips, .keep_all = TRUE) %>% 
    group_by(fips, year) %>% 
    summarise(
      nEpisodes = n(),
      .groups = 'drop'
    ) %>% 
    mutate(episodes_bin = if_else(nEpisodes > 0, TRUE, FALSE)) %>% 
    left_join(
      x = distinct(myData, fips, year) %>% 
        complete(fips, year) %>% 
        filter(year %in% myYears),
      by = c("year", "fips")
    ) %>% 
    mutate(
      nEpisodes = replace_na(nEpisodes, 0),
      episodes_bin = replace_na(episodes_bin, FALSE)
    )
  
  # Create data for histogram
  dataForHist <- 
    myData %>% 
    filter(year %in% myYears) %>% 
    filter(event_type %in% myEventTypes) %>% 
    distinct(episode_id, fips, .keep_all = TRUE) %>% 
    group_by(fips, year, month_name) %>% 
    summarise(
      nEpisodes = n(),
      .groups = 'drop'
    ) %>% 
    mutate(episodes_bin = if_else(nEpisodes > 0, TRUE, FALSE)) %>% 
    left_join(
      x = distinct(myData, fips, year, month_name) %>% 
        complete(fips, year, month_name) %>% 
        filter(year %in% myYears),
      by = c("year", "fips", "month_name")
    ) %>% 
    mutate(
      nEpisodes = replace_na(nEpisodes, 0),
      episodes_bin = replace_na(episodes_bin, FALSE)
    ) %>% 
    group_by(year, month_name) %>% 
    summarise(
      nEpisodes = sum(nEpisodes),
      .groups = 'drop'
    )
  
  return(list(
    dataForUsPlot = dataForUsPlot,
    dataForHist = dataForHist
  ))
}