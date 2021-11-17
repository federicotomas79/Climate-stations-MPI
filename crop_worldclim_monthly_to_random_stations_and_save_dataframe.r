# 20201028 Craig Phillips AGR Demo script for extracting monthly climate records from worldclim rasters and interpolating them to weekly

rm(list = ls())

library(tidyverse)
library(raster)

getwd()

#-----------------------------------------------------
# Constants
#-----------------------------------------------------
dat_dir <- './' # dir of worldclim data ('./' assumes data are in current working dir)

#-----------------------------------------------------
# Read from disc 2.5 minute wordclim data for min temp, max temp, precip, & water vapor pressure. 20201028 - Jan 2020 versions were downloaded data from https://www.worldclim.org/data/worldclim21.html
#-----------------------------------------------------
# wcd = worldclim directories. There are different directories for precip, tmax, tmin & water vapor
(wcd <- dir(dat_dir, pattern = 'wc2.1_2.5m_', recursive = FALSE)) 

# Get names of monthly files in subdirs. If writing a proper script, would check for 'readme.txt' in each wcd and also check that there are exactly 12 filenames in 'prec_fn' etc.
(prec_fn <- dir(paste(dat_dir, wcd[1], sep = '/'))[-1]) # -1 omits 'readme.txt'
(tmax_fn <- dir(paste(dat_dir, wcd[2], sep = '/'))) # no readme in other wcd
(tmin_fn <- dir(paste(dat_dir, wcd[3], sep = '/')))
(vapr_fn <- dir(paste(dat_dir, wcd[4], sep = '/')))

# Put data for each month in a stack
prec_stack <- stack(paste(dat_dir, wcd[1], prec_fn, sep = '/'))
tmax_stack <- stack(paste(dat_dir, wcd[2], tmax_fn, sep = '/'))
tmin_stack <- stack(paste(dat_dir, wcd[3], tmin_fn, sep = '/'))
vapr_stack <- stack(paste(dat_dir, wcd[4], vapr_fn, sep = '/'))

# Simplify names to 'm1' (month 1 = Jan) to 'm12' (Dec)
names(prec_stack)
(mo <- paste0('m', 1: 12))
names(prec_stack) <- mo
names(tmax_stack) <- mo
names(tmin_stack) <- mo
names(vapr_stack) <- mo

#-----------------------------------------------------
# Functions for interpolating monthly records to weekly. Adapted from function 'weekly.climate.data' in Documents\gis\Climate\Federico\RClimexFromCraigsCDrive\climex_v2.r
#-----------------------------------------------------
mid_month <- c (-15, 15, 46, 74, 105, 135, 166, 196, 227, 258, 288, 319, 349, 380)
mid_day <- seq(4, 361, 7)

# Function to interpolate precipitation/rain. y is a monthly climate variable split by location with 12 records per location
f_interpolate_prec <- function(y) {
	intp <- approx(mid_month, c(y[12], y, y[1]), xout = mid_day)
	scalar <- sum(y) / sum(intp$y) # scale weeky prec to equal monthly total
	z <- intp$y * scalar
	return(z)
}

# Function to interpolate temperature (also used for vapor pressure).
f_interpolate_temp <- function(y) { 
	intp <- approx(mid_month, c(y[12], y, y[1]), xout = mid_day)
	return(intp$y)
}

#-----------------------------------------------------
# Crop stacks by random points
#-----------------------------------------------------

# Previous code: 
#nz_e <- extent(166, 179, -48, -34) # approx extent of NZ
# New code (use extract (https://rdrr.io/cran/raster/man/extract.html))

#Import climate stations used in WorldClim with Precipitation, Tmin & Tmax
wc2 <- read.csv('./wc2.csv')
class(wc2)

# Select inland stations using "WCID"
wc2rnd <- filter (wc2, WCID %in% c("33111", "55020", "48521", "17110", "11867", "40431", "18153", "29650", "54050", "20626"))

# Selecting locations with locator (not ideal right now)
#rndmp1 <- locator(10, type="p", col="red", pch=20)
#sp <- SpatialPoints(rndmp1)

# Selecting randomly
#rndpts <- spsample(wcs2, n = 10, "random")
#summary(rndpts)

#Plot climate stations and randomly selected one (n=10)
#x11()
w <- map_data('world')
ggplot() +
  geom_polygon(data = w, aes(x = long, y = lat, group = group), colour = 'grey50', fill = 'white') +
  #	geom_point(data = wc, aes(x = LONG, y = LAT), colour = 'grey50', size = 1) +
  geom_point(data = wc2,
             aes(x = LONG, y = LAT), pch = 21, colour = 'white', fill = 'black', size = 2) +
  geom_point(data = as.data.frame(wc2rnd), aes(x = LONG, y = LAT), pch = 21, colour = 'red', fill = 'red', size = 2) +
  ggtitle('Random stations & climate stations from WordClim with TMAX, TMIN & PREC') +
  theme_bw() +
  coord_quickmap()

#Convert random points into SpatialPoints
wcs2 <- wc2rnd
coordinates(wcs2) <- ~LONG+LAT
summary(wcs2)

#Doing exercise with precipitation only
#prec_stack_crop <- raster::crop(prec_stack, nz_e, snap = 'near') 
prec_stack_extr <- raster::extract(prec_stack, wcs2, method="simple")
class(prec_stack_extr)
prec_stack_extr.df <- as.data.frame(prec_stack_extr)
prec_stack_extr.df
class(prec_stack_extr.df)

# tmax_stack_crop <- raster::crop(tmax_stack, nz_e, snap = 'near') 
# tmin_stack_crop <- raster::crop(tmin_stack, nz_e, snap = 'near') 
# vapr_stack_crop <- raster::crop(vapr_stack, nz_e, snap = 'near') 
# 
# plot(prec_stack_crop$m1) # Westland is wetland

#-----------------------------------------------------
# Convert spatially cropped stacks to dataframes
#-----------------------------------------------------
ncell(prec_stack_crop) # 104,832 is too many for NZ because we also have many NAs from ocean. Will remove NAs once data in dataframes. Would it be tidier to use a mask rather than an extent to crop to a spatial area?

# Function to convert stacks to dataframes - 'a_stack' is the stack name & 'val_name' is the value recorded in the stack (eg, 'tmax')
f_stack_to_df <- function(a_stack, val_name) {
	stack_df <- as.data.frame(a_stack, xy = TRUE, na.rm = TRUE) # na.rm = TRUE excludes NA 
	stack_df$Key <- 1: nrow(stack_df) # assign unique ID to each location
	stack_df <- pivot_longer(stack_df, # convert to long format
		cols = m1: m12,
		names_to = 'Mo',
		values_to = val_name)
	stack_df <- rename(stack_df, Lat = y, Long = x) 
	return(stack_df)
}

prec_crop_df <- f_stack_to_df(prec_stack_crop, 'Rain')

# Check output
head(prec_crop_df)
length(unique(prec_crop_df$Key)) # 17848 locations is what I expect for NZ
nrow(prec_crop_df)/12 # 12 months of data for each location

# Plot 1 month of data
prec_crop_df %>% 
	filter(Mo == 'm1') %>% 
	ggplot(aes(x = Long, y = Lat, color = Rain)) + geom_point(size = .5)

tmax_crop_df <- f_stack_to_df(tmax_stack_crop, 'Tmax')
tmin_crop_df <- f_stack_to_df(tmin_stack_crop, 'Tmin')
vapr_crop_df <- f_stack_to_df(vapr_stack_crop, 'Vapr')

#-----------------------------------------------------
# Create one set of spatially cropped monthly data & save
#-----------------------------------------------------
m <- # m = monthly
	bind_cols(tmax_crop_df, tmin_crop_df, prec_crop_df, vapr_crop_df) %>%
	mutate(Continent = 'New Zealand', Mnth = as.integer(str_replace(Mo, 'm', ''))) %>%
	dplyr::select(Key, Lat, Long, Continent, Mnth, Tmax, Tmin, Rain, Vapr)

head(m)
nrow(m)/12

# Plot 1 month of Rain
m %>% 
	filter(Mnth == 1) %>% 
	ggplot(aes(x = Long, y = Lat, color = Rain)) + 
		geom_point(size = .5)

# Write to disc
# write.csv(m, file = 'nz_monthly_worldclim_1970_2000_version_202001.csv', row.names = FALSE) # sent to JK 20201029

#-----------------------------------------------------
# Interpolate spatially cropped data from monthly to weekly
#-----------------------------------------------------
# Interpolate Rain
prec_i <- 
	m %>%
		dplyr::select(Key, Rain) %>%
		split(.$Key) %>%
		map_dfr(~f_interpolate_prec(.$Rain)) %>%
		gather(., key = 'Key', value = 'Rain', names(.)) 

nrow(filter(prec_i, Key == 1)) # 52 weeks of data for each location
	
# Interpolate Tmax
tmax_i <- 
	m %>%
		dplyr::select(Key, Tmax) %>%
		split(.$Key) %>%
		map_dfr(~f_interpolate_temp(.$Tmax)) %>%
		gather(., key = 'Key', value = 'Tmax', names(.)) 

# Interpolate Tmin
tmin_i <- 
	m %>%
		dplyr::select(Key, Tmin) %>%
		split(.$Key) %>%
		map_dfr(~f_interpolate_temp(.$Tmin)) %>%
		gather(., key = 'Key', value = 'Tmin', names(.)) 

# Interpolate Vapr
vapr_i <- 
	m %>%
		dplyr::select(Key, Vapr) %>%
		split(.$Key) %>%
		map_dfr(~f_interpolate_temp(.$Vapr)) %>% 
		gather(., key = 'Key', value = 'Vapr', names(.)) 

#-----------------------------------------------------
# Create one set of spatially cropped interpolated weekly data, w
#-----------------------------------------------------
head(vapr_i)
nrow(vapr_i)

# Reclaim xy from prec_df to add to w
xy <- 
	prec_crop_df %>%	
		distinct(Key, .keep_all = TRUE) %>%
		dplyr::select(Key, Lat, Long) %>%
		print(n = 5)

w <- # m = monthly
	bind_cols(prec_i, tmax_i, tmin_i, vapr_i) %>%
	mutate(Continent = 'New Zealand', Week = rep(1: 52, nrow(.)/52), Key = as.integer(Key)) %>%
	full_join(xy, ., by = 'Key') %>%
	dplyr::select(Key, Lat, Long, Continent, Week, Tmax, Tmin, Rain, Vapr) %>%
	print(n = 5)

nrow(w)
nrow(w) == nrow(xy) * 52 # should be TRUE
nrow(w)%%52 == 0 # should be TRUE

# Plot 1 week of Rain
w %>% filter(Week == 1) %>% ggplot(., aes(x = Long, y = Lat, color = Rain)) + geom_point(size = .5)

#-----------------------------------------------------
# Write to spatially cropped interpolated weekly data to disc
#-----------------------------------------------------
list.dirs(recursive = FALSE)

# saveRDS(w, file = 'nz_weekly_worldclim_1970_2000_version_202001.rds') # saved 20201029
