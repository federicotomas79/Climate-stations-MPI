# 20211005 Try package 'climate' for downloading climate station data. My first impression is this could be better than using package 'rnoaa'. Related information is at these links:
# https://github.com/bczernecki/climate
# https://www.ogimet.com/gsynres.phtml.en
# https://ogimet.com/display_stations.php?lang=en&tipo=AND&isyn=&oaci=&nombre=&estado=&Send=Send>

rm(list = ls())
library(tidyverse)
# if (!require('climate')) install.packages('climate', repos = 'https://cran.stat.auckland.ac.nz/')
library('climate')
library('maps')
# script also requires package lubridate

# nz = stations_ogimet(country = "NZWD", add_map = TRUE) # Fede, your code here can't run because country isn't a parameter of stations_ogimet (plus NZWD is "Williams Field, Antarctic")

stn_nz <- nearest_stations_ogimet(
	country = "New+Zealand",
	date = Sys.Date()-1,
	add_map = FALSE,
	point = c(174.8, -41.3), # Wellington
	no_of_stations = 50)

glimpse(stn_nz)

nz <- map_data('nz') # NZ outline for plotting

# Look where NZ stations are & see their IDs. Best viewed in full screen. Shows more stations than rnoaa version.
ggplot() + 
	theme_bw() +
	geom_polygon(data = nz, aes(x = long, y = lat, group = group), colour = 'grey50', fill = 'white') +
	geom_text(data = stn_nz, aes(x = lon, y = lat, label = wmo_id), nudge_x = .5, check_overlap = FALSE) +
	geom_point(data = stn_nz, aes(x = lon, y = lat), pch = 19, colour = 'blue') +
	coord_quickmap()

# ggsave('nz_stns_r_climate.png')

# Get data for a station. Slow!
cli <- meteo_ogimet(interval = "daily",
 date = c("2018-05-01", "2018-07-01"), # dates are YMD
 station = 93439, # Wellington
 coords = TRUE)

glimpse(cli) # Returns data in nice format

max(cli$Date) - min(cli$Date)

ggplot(cli) +
	theme_bw() +
	geom_smooth(aes(x = Date, y = TemperatureCMin), fill = 'green') +
	geom_smooth(aes(x = Date, y = TemperatureCMax), fill = 'pink')

# Daily data would be easy to generate monthly data from to use for climate matching. (Or should we generate weekly records, rather than interpolating from monthly as is the usual process?)
cli %>%
	select(Date, TemperatureCMax, TemperatureCMin, Precmm) %>%
	mutate(mnth = lubridate::month(Date)) %>%
	group_by(mnth) %>%
	summarise(tmax = mean(TemperatureCMax, na.rm = TRUE),
		tmin = mean(TemperatureCMin, na.rm = TRUE),
		prec = sum(Precmm),
		.groups = 'drop')

# Can we get enough data for other parts of the world? Try Italy...
stn_it <- nearest_stations_ogimet(
	country = "Italy",
	date = Sys.Date()-1,
	add_map = FALSE,
	point = c(12.5, 41.9), # Rome
	no_of_stations = 50)

glimpse(stn_it)

it <- map_data('italy') 

# Look where stations near Rome are & see their IDs. Best viewed in full screen.
ggplot() + 
	theme_bw() +
	geom_polygon(data = it, aes(x = long, y = lat, group = group), colour = 'grey50', fill = 'white') +
	geom_text(data = stn_it, aes(x = lon, y = lat, label = wmo_id), nudge_x = .5, check_overlap = FALSE) +
	geom_point(data = stn_it, aes(x = lon, y = lat), pch = 19, colour = 'blue') +
	coord_quickmap()


