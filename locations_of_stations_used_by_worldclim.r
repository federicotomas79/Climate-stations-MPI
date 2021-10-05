# 20211005 This script doesn't actually get or provide access to climate data. Rather it shows the locations of climate stations that contributed data to WorldClim (WC). MPI has specified that--for the pairwise location vs location climate match part of the work--it wants us to use actual climate station data rather than WC data. I think this is because WC data are gridded, thus data for some WC locations without climate stations must be spatially interpolated (& perhaps inaccurate). However, we would avoid any spatial interpolation if we used the subset of WC data that came directly from climate station locations. This would have 2 main benefits: 1) The same WC data could be used in both the climate station vs climate station pairwise comparisons & the NZ vs world comparisions; 2) The WC data are already nicely formatted for our needs (unlike raw NOAA data). I will ask MPI about this idea.

rm(list = ls())
library(tidyverse)
library(maps)

#------------------------------------------------------------------
# Get data from (https://databasin.org/datasets/15a31dec689b4c958ee491ff30fcce75/)
# wc <- foreign::read.dbf("stations1.dbf", as.is = TRUE) # don't need factors
# saveRDS(wc, './wc_stations1.rds')
wc <- readRDS('./wc_stations1.rds') # I saved a local copy
glimpse(wc)

#Summarize data per country
cntry <- wc %>% 
  group_by(COUNTRY) %>%
  summarise(count = n(), .groups = 'drop')

glimpse(cntry)
sum(cntry$count) # 55879

#------------------------------------------------------------------
# Extract only climate stations data with Precipitation, Tmin & Tmax
any(is.na(wc$RAINID)) # check if contains both NA & "NA"
any(is.na(wc$TMINID)) 
any(is.na(wc$TMAXID)) 

# wc %>% filter(RAINID!="NA", TMINID!="NA", TMAXID!="NA") # this line did not write the filtered data to wc, & the filter should be "&" rather than ","

# Summarize data with 3 vars per country
vars3 <- wc %>% 
	filter(RAINID != "NA" & TMINID != "NA" & TMAXID != "NA") %>%
  group_by(COUNTRY) %>%
  summarise(with3vars = n(), .groups = 'drop')

glimpse(vars3)

sum(vars3$with3vars) # 10531

#------------------------------------------------------------------
# Look at summarised data
summ <- left_join(cntry, vars3, by = 'COUNTRY')

mycap <- paste('Total climate stations is', sum(cntry$count), 'and stations with TMAX, TMIN and PREC is',  sum(vars3$with3vars))

summ %>%
	arrange(desc(with3vars)) %>%
  DT::datatable(caption = mycap, options = list(pageLength = 20))

#------------------------------------------------------------------
# Plot locations

w <- map_data('world')

ggplot() +
	geom_polygon(data = w, aes(x = long, y = lat, group = group), colour = 'grey50', fill = 'white') +
#	geom_point(data = wc, aes(x = LONG, y = LAT), colour = 'grey50', size = 1) +
	geom_point(data = filter(wc, RAINID != "NA" & TMINID != "NA" & TMAXID != "NA"),
		aes(x = LONG, y = LAT), pch = 21, colour = 'white', fill = 'black', size = 2) +
	ggtitle('Climate stations used to compile WordClim data that have TMAX, TMIN & PREC') +
	theme_bw() +
	coord_quickmap()




