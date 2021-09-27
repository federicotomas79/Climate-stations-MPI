# Climate stations NOAA list (https://www.ncei.noaa.gov/products/land-based-station/global-historical-climatology-network-daily)
# See https://www.ncei.noaa.gov/data/global-historical-climatology-network-daily/doc/GHCND_documentation.pdf

rm(list = ls())
library(tidyverse)

#-------------------------------------------------------
# Station locations
stations <- readRDS('./noaa-ghcnd-stations.rds') # I saved a local copy

#stations <- read_table("https://www.ncei.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt", col_names = FALSE)

# Website says columns contain Station ID, latitude, longitude, elevation, State (if applicable) and Station name (6 columns)
glimpse(stations) # but we get 8 columns

# unique(stations$X5) # probably State

# names(stations) <- c('id', 'lat', 'lon', 'elev', 'state', 'station_nm', 'X7', 'X8')

# saveRDS(stations, file = './noaa-ghcnd-stations.rds') # to save downloading again

# DT::datatable(stations, options = list(pageLength = 20))

#-------------------------------------------------------
# Begin/end dates
inventory <- readRDS('./noaa-ghcnd-inventory.rds') # I saved a local copy

# inventory <- read_table("https://www.ncei.noaa.gov/pub/data/ghcn/daily/ghcnd-inventory.txt", col_names = FALSE)

# Website says columns contain Station ID, latitude, longitude, element type, and begin/end date
glimpse(inventory)

# names(inventory) <- c('id', 'lat', 'lon', 'type', 'start_yr', 'end_yr')

# saveRDS(inventory, file = './noaa-ghcnd-inventory.rds') # to save downloading again

inventory$duration <- inventory$end_yr - inventory$start_yr

summary(inventory$duration)

unique(inventory$type) # presumably we want TMAX, TMIN & PRCP 

# Assume we want at least 20 years of data that finished no later than the year 2000
inventory2 <- 
	filter(inventory, end_yr >= 2000 & duration >= 20 & type %in% c('TMAX', 'TMIN', 'PRCP'))

summary(inventory2$duration)

inventory2 %>% # double check all stations have TMAX, TMIN & PRCP
	select(id, type) %>%
	group_by(id, type) %>%
	summarise(n = n(), .groups = 'drop') %>%
	filter(n < 1) # all stations have all 3 variables 

# Quickly look where the stations are
inventory2 %>%
	distinct(id, .keep_all = TRUE) %>%
	ggplot(aes(x = lon, y = lat)) +
	geom_point() +
	coord_fixed()

#Next step (https://github.com/bczernecki/climate)
#if (!require('climate')) install.packages('climate'); library('climate')
#NZL = stations_ogimet(country = "NZWD", add_map = TRUE)


