library(tidyverse)

FUNStormEventsData_filterData_day <- function(
    myData,
    myDate,
    myEventTypes
) {
  
  # Convert reference date to Date format
  myDate <- as.Date(myDate)
  
  # Prepare dataset and compute temporal distance to reference date
  data_prepared <- myData %>% 
    mutate(
      fips = state_county_fips,
      event_type = as.character(event_type),
      event_date = as.Date(end_date_time),
      daysBeforeMyDate = as.numeric(myDate - event_date)
    ) %>% 
    # Keep only selected event types
    filter(event_type %in% myEventTypes) %>% 
    # Keep only events that occurred before or on the reference date
    filter(!is.na(daysBeforeMyDate)) %>% 
    filter(daysBeforeMyDate >= 0)
  
  
  # ------------------------------------------------------------------
  # Create datasets for the two time windows
  # ------------------------------------------------------------------
  
  # Events within the last 30 days
  data_30 <- data_prepared %>% 
    filter(daysBeforeMyDate <= 30) %>% 
    mutate(time_window = "last30days")
  
  # Events within the last 360 days
  data_360 <- data_prepared %>% 
    filter(daysBeforeMyDate <= 360) %>% 
    mutate(time_window = "last360days")
  
  
  # Combine both time windows
  data_combined <- bind_rows(data_30, data_360)
  
  
  # ------------------------------------------------------------------
  # Create dataset for US plot with event types
  # Counts number of unique episodes per county, event type, and time window
  # ------------------------------------------------------------------
  
  dataForUsPlot_neu <- data_combined %>% 
    distinct(episode_id, fips, event_type, time_window, .keep_all = TRUE) %>% 
    group_by(fips, event_type, time_window) %>% 
    summarise(
      nEpisodes = n(),
      .groups = "drop"
    ) %>% 
    mutate(
      episodes_bin = if_else(nEpisodes > 0, TRUE, FALSE)
    ) %>% 
    right_join(
      y = data_combined %>% 
        distinct(fips, event_type, time_window) %>% 
        complete(fips, event_type, time_window),
      by = c("fips", "event_type", "time_window")
    ) %>% 
    mutate(
      nEpisodes = replace_na(nEpisodes, 0),
      episodes_bin = replace_na(episodes_bin, FALSE)
    )
  
  
  return(dataForUsPlot_neu)
}
