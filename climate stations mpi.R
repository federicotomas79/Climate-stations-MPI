rm(list = ls())
library(tidyverse)

#------------------------------------------------------------------
# Get data from (https://databasin.org/datasets/15a31dec689b4c958ee491ff30fcce75/)

wc <- readRDS('./wc_stations1.rds') # I save a local copy
# wc <- foreign::read.dbf("stations1.dbf", as.is = TRUE) # don't need factors
# saveRDS(wc, './wc_stations1.rds')

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

mycap <- paste('Total climate stations is', sum(cntry$count), 'and stations with our 3 vars is',  sum(vars3$with3vars))

summ %>%
	arrange(desc(with3vars)) %>%
  DT::datatable(caption = mycap, options = list(pageLength = 20))

#------------------------------------------------------------------
# Plot locations
ggplot() +
	geom_point(data = wc, aes(x = LONG, y = LAT), colour = 'grey50', size = 1) +
	geom_point(data = filter(wc, RAINID != "NA" & TMINID != "NA" & TMAXID != "NA"),
		aes(x = LONG, y = LAT), colour = 'red', size = .5) +
	ggtitle('All climate stations (grey) & with our 3 variables (red)') +
	theme_bw() +
	coord_equal()



