# 20211005 Try package rnoaa for downloading climate station data. My initial impression is his could be inferior to using package 'climate'. Related information is at these links:
# NOAA GHCND = global historical climatology network daily
# https://docs.opendata.aws/noaa-ghcn-pds/readme.html
# https://recology.info/2015/07/weather-data-with-rnoaa/ # July 2015
# https://rdrr.io/cran/rnoaa/#vignettes
# https://rdrr.io/github/UBC-MDS/noaastnr/ 
#https://www.ncei.noaa.gov/products/land-based-station/global-historical-climatology-network-daily)
#
#https://www.ncei.noaa.gov/data/global-historical-climatology-network-daily/doc/GHCND_documentation.pdf
#
# NOAA ISD = integrated surface dataset (global) includes air quality, atmospheric pressure, atmospheric temperature/dew point, atmospheric winds, clouds, precipitation, ocean waves, tides and more. https://www.ncei.noaa.gov/access/metadata/landing-page/bin/iso?id=gov.noaa.ncdc:C00532
#
# NOAA NCDC = national climatic data centre (now called national centers for environmental information) https://www.ncdc.noaa.gov/cdo-web/#t=secondTabLink

rm(list = ls())
library(tidyverse)

# if (!require("rnoaa")) install.packages("rnoaa", repos = 'https://cran.stat.auckland.ac.nz/') 
library(rnoaa)
library(maps)

#-------------------------------------------------------
# Station locations
#-------------------------------------------------------
#stations <- read_table("https://www.ncei.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt", col_names = FALSE)
# unique(stations$X5) # probably State
# names(stations) <- c('id', 'lat', 'lon', 'elev', 'state', 'station_nm', 'X7', 'X8')
# saveRDS(stations, file = './noaa-ghcnd-stations.rds') # to save downloading again
# DT::datatable(stations, options = list(pageLength = 20))

stations <- readRDS('./noaa-ghcnd-stations.rds') # I saved a local copy

# Website says columns contain Station ID, latitude, longitude, elevation, State (if applicable) and Station name (6 columns)
glimpse(stations) # but we get 8 columns

#-------------------------------------------------------
# Begin/end dates
#-------------------------------------------------------
# inventory <- read_table("https://www.ncei.noaa.gov/pub/data/ghcn/daily/ghcnd-inventory.txt", col_names = FALSE)
# names(inventory) <- c('id', 'lat', 'lon', 'type', 'start_yr', 'end_yr')
# saveRDS(inventory, file = './noaa-ghcnd-inventory.rds') # to save downloading again
inventory <- readRDS('./noaa-ghcnd-inventory.rds') # I saved a local copy

# Website says columns contain Station ID, latitude, longitude, element type, and begin/end date
glimpse(inventory)

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

# Quickly look where some stations are
nz_inventory <-
	inventory %>%
		filter(lon >= 166 & lon <= 179 & lat >= -48 & lat <= -34) %>% # NZ
		distinct(id, .keep_all = TRUE)

nz <- map_data('nz')

# Look where NZ stations are & see their IDs. Best viewed in full screen. Shows fewer stations than package climate version.		
ggplot() +
	geom_polygon(data = nz, aes(x = long, y = lat, group = group), colour = 'grey50', fill = 'white') +
	geom_text(data = nz_inventory, aes(x = lon, y = lat, label = id), nudge_x = 1) +
	geom_point(data = nz_inventory, aes(x = lon, y = lat), pch = 19, colour = 'blue') +
	coord_quickmap()

# ggsave('nz_stns_rnoaa.png')

#-------------------------------------------------------
# Download data for particular locations
#-------------------------------------------------------
# Get an API key (aka, token) at https://www.ncdc.noaa.gov/cdo-web/token
# Sys.getenv()
# Sys.setenv(NOAA_KEY  = "OTACdtTIHastAfxhIelgHJjqrtQGfotX")
# Sys.unsetenv("NOAA_KEY")
# Sys.getenv("NOAA_KEY")
tkn <- "OTACdtTIHastAfxhIelgHJjqrtQGfotX"

# Example
# ncdc_stations()
# ncdc_stations(limit=5, token=tkn)

# New Zealand
ncdc_stations(extent = c(-46.641235447, 166.509144322, -34.4506617165, 178.517093541), token = tkn)

ncdc_datasets(stationid='GHCND:NZM00093781', token=tkn)

# GHCNDMS Monthly Summaries
# Dates are YMD. Always returns many fewer days of data than requested
l <- ncdc(datasetid='GHCND', stationid='GHCND:NZM00093781', startdate = '2018-05-01', enddate = '2018-07-01', add_units=TRUE, token=tkn)
str(l) # list of 3
d <- l$data
glimpse(d)
table(d$datatype) # not a nice data format. Note weird units!
d$date <- as.Date(d$date)
max(d$date) - min(d$date) # only get 6 days of data, despite requesting 2 months

# reformat data to make them easier to work with
d$fl_m <- 'H' # fill in blanks to help pivot_wider work better
d2 <- pivot_wider(d, names_from = datatype, values_from = value)

ggplot(d2) +
	theme_bw() +
	geom_point(aes(x = date, y = .1 * TMIN), color = 'green', size = 4) +
	geom_point(aes(x = date, y = .1 * TMAX), color = 'pink', size = 4)

#-------------------------------------------------------
# Download all data for a location
#-------------------------------------------------------
dall <- ghcnd(stationid='GHCND:NZM00093781', token=tkn) # doesn't always run. Server issue?
glimpse(dall) # 31 columns of values. Perhaps 1 for each day of month?
summary(dall$year)

dall %>%
	filter(element!='TAVG' & month==1) %>%
	select(id, year, month, element, VALUE1) %>%
	pivot_wider(names_from = element, values_from = VALUE1) %>%
	arrange(year, month) %>%
	print(n = nrow(.))

# So far I've found downloading data via rnoaa rather inefficient

#-------------------------------------------------------
# Brute-force data download!
# Manually download annual data for whole world from https://www.ncei.noaa.gov/pub/data/ghcn/daily/by_year/
# Big data files slow to download & take a lot of memory
#-------------------------------------------------------
# download/save to local dir, then read into R
#y21z <- gzfile('./2021.csv.gz', open = 'rt')
#y21 <- read.csv(y21z, header=FALSE)
# Convert to RDS to save memory & delete other versions
#saveRDS(y21, file='ghcn2021.rds')

y21 <- readRDS('./ghcn2021.rds')
glimpse(y21)

sort(unique(y21$V1[str_which(y21$V1, '^NZ')]))

y21 %>%
	select(id = V1, obsdate = V2, varname = V3, val = V4) %>%
	filter(id == 'NZM00093781') %>% # Christchurch
	pivot_wider(names_from = varname, values_from = val) %>%
	print(n = 30)

# So we could get the data this manual way if necessary...
