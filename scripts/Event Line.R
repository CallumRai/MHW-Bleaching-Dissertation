# The packages we will use
library(dplyr) # A staple for modern data management in R
library(lubridate) # Useful functions for dealing with dates
library(ggplot2) # The preferred library for data visualisation
library(tidync) # For easily dealing with NetCDF data
library(rerddap) # For easily downloading subsets of data
library(doParallel) # For parallel processing
library(heatwaveR)

# This function downloads and prepares data based on user provided start and end dates
# Lat. and long. correspond to a specific point in the Caribbean
OISST_sub_dl <- function(time_df){
  OISST_dat <- griddap(x = "ncdcOisst21Agg_LonPM180", 
                       url = "https://coastwatch.pfeg.noaa.gov/erddap/", 
                       time = c(time_df$start, time_df$end), 
                       zlev = c(0, 0),
                       latitude = c(20.624, 20.626),
                       longitude = c(-77.626, -77.624),
                       fields = "sst")$data %>% 
    mutate(time = as.Date(stringr::str_remove(time, "T00:00:00Z"))) %>% 
    dplyr::rename(t = time, temp = sst) %>% 
    select(lon, lat, t, temp) %>% 
    na.omit()
}

# Date download range by start and end dates per year
# Dates are 1982-2020, code can only process 9 years at a time
dl_years <- data.frame(date_index = 1:5,
                       start = as.Date(c("1982-01-01", "1990-01-01", 
                                         "1998-01-01", "2006-01-01", "2014-01-01")),
                       end = as.Date(c("1989-12-31", "1997-12-31", 
                                       "2005-12-31", "2013-12-31", "2019-12-31")))

# Download all of the data with one nested request
# The time this takes will vary greatly based on connection speed
OISST_data <- dl_years %>% 
  group_by(date_index) %>% 
  group_modify(~OISST_sub_dl(.x)) %>% 
  ungroup() %>% 
  select(lon, lat, t, temp)

#Select only time and temp. columns
OISST_data <- OISST_data[c("t","temp")]

# ts2clm creates a daily climatology from daily temps. using sliding window for mean and threshold 
ts <- ts2clm(OISST_data, climatologyPeriod = c("1982-01-01", "2019-12-31"))

# detect_events applied Hobday et al. (2016) MHW definition
mhw <- detect_event(ts)

# event_line creates a graph of warm events
plot(event_line(mhw, metric = "intensity_max", 
           start_date = "2014-01-01", end_date = "2019-12-31"))



