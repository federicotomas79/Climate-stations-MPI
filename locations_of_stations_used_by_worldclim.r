# Craig Phillips 20211005: This script doesn't actually get or provide access to climate data. Rather it shows the locations of climate stations that contributed data to WorldClim (WC). MPI has specified that--for the pairwise location vs location climate match part of the work--it wants us to use actual climate station data rather than WC data. I think this is because WC data are gridded, thus data for some WC locations without climate stations must be spatially interpolated (& perhaps inaccurate). However, we would avoid any spatial interpolation if we used the subset of WC data that came directly from climate station locations. This would have 2 main benefits: 1) The same WC data could be used in both the climate station vs climate station pairwise comparisons & the NZ vs world comparisions; 2) The WC data are already nicely formatted for our needs (unlike raw NOAA data). I will ask MPI about this idea.
# Craig Phillips 20211101: Discussed using WC data from climate station locations with MPI on 20211006 & they agreed. On the same day I emailed a leaflet map of WC climate station locations (produced by this script) to Stephan Halloy & Ursula Torres (cc, FT & JK) & asked them to choose the locations they would like to use. (I also pushed the leaflet map to this current "Worldclim-stations" repo to.) SH & UT agreed, but none sent to us, so at our next meeting I asked if they could at least choose a few for us to start developing a workflow with: However, as of today still no response.
# We should go ahead & choose 10 or so stations ourselves to start developing the workflow. Today I asked FT to choose 10 locations then write a script for extracting their climate variables from WorldClim rasters. Suggested as a starting point for that script he could copy "crop_worldclim_monthly_to_nz_and_save_dataframe.r" from my "world_clim" GitHub repo. 
# After we have the data, we can calculate CMIs between each pair. It might be good to do this with our R version of MCR because it won't be computationally intensive, & using the R version will get me started on refining it for NCSU.

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
#------------------------------------------------------------------
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

# Look at summarised data
summ <- left_join(cntry, vars3, by = 'COUNTRY')

mycap <- paste('Total climate stations is', sum(cntry$count), 'and stations with TMAX, TMIN and PREC is',  sum(vars3$with3vars))

summ %>%
	arrange(desc(with3vars)) %>%
  DT::datatable(caption = mycap, options = list(pageLength = 20))

#------------------------------------------------------------------
# Static plot of locations
#------------------------------------------------------------------
w <- map_data('world')

#x11()
ggplot() +
	geom_polygon(data = w, aes(x = long, y = lat, group = group), colour = 'grey50', fill = 'white') +
#	geom_point(data = wc, aes(x = LONG, y = LAT), colour = 'grey50', size = 1) +
	geom_point(data = filter(wc, RAINID != "NA" & TMINID != "NA" & TMAXID != "NA"),
		aes(x = LONG, y = LAT), pch = 21, colour = 'white', fill = 'black', size = 2) +
	ggtitle('Climate stations used to compile WordClim data that have TMAX, TMIN & PREC') +
	theme_bw() +
	coord_quickmap()

#------------------------------------------------------------------
# Leaflet plot of locations with TMIN, TMAX & RAIN
#------------------------------------------------------------------
wc2 <- wc %>% 
	filter(RAINID != "NA" & TMINID != "NA" & TMAXID != "NA") %>%
  dplyr::select(-c(RAINID: TMEANID))

length(which(is.na(wc2$COUNTRY)))	
length(unique(wc2$NAME)) # 10105
# tail(sort(unique(wc2$NAME)), n = 100)

glimpse(wc2)

#Export locations for next exercise
#write.csv(wc2,"C://Users//TOMASETTOF//OneDrive - AgResearch//Documents//GitHub//world_clim//wc2.csv", row.names = FALSE)
  
library(leaflet)

map <-
	leaflet(data = wc2, options = leafletOptions(preferCanvas = TRUE)) %>% 
		addProviderTiles(providers$Esri.NatGeoWorldMap,
			options = providerTileOptions(
				updateWhenZooming = FALSE, # map won't update tiles until zoom is done
				updateWhenIdle = TRUE)) %>% # map won't load new tiles when panning
			leaflet.extras::addSearchOSM() %>% # search for names within tiles
			leaflet.extras::addResetMapButton() %>% # go back to default zoom
			addControl('Hover for climate station ID, click for ID, coordinates & name (AGR 20211006)', position  =  'topright') %>%
			setView(0, 0, zoom = 2) %>% # initialise view extent
			setMaxBounds(lng1 = -200,
									 lat1 = 100,
									 lng2 = 200,
									 lat2 = -100) %>%
    clearMarkers() %>%
    addCircleMarkers(lng = wc2$LONG, lat = wc2$LAT, weight = 1.5,
			color = "#FFFFFF",
			opacity = 1,
			fillColor = "#000000",
			fillOpacity = 1,
			radius = 4,
      label = ~as.character(WCID), 
      popup = ~as.character(paste0('Id=', WCID, '; Lon=', LONG, '; Lat=', LAT, '; Name=', NAME)))

# map

htmlwidgets::saveWidget(widget = map, file = 'climate_stations_with_tmin_tmax_prec_used_by_wordclim.html', selfcontained = TRUE)

