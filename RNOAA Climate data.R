# R NOAA 
# Getting climate data from NOAA Stations (https://cran.r-project.org/web/packages/rnoaa/rnoaa.pdf)

#Read (https://github.com/ropensci/rnoaa)

if (!require("rnoaa")) install.packages("rnoaa"); library(rnoaa)

#Get an API key (aka, token) at https://www.ncdc.noaa.gov/cdo-web/token
Sys.setenv(NOAA_KEY  = "OTACdtTIHastAfxhIelgHJjqrtQGfotX")
Sys.getenv("NOAA_KEY")

#Example
ncdc_stations()
ncdc_stations(limit=5)

#Get info on a station by specifying a datasetid, locationid, and stationid
ncdc_stations(datasetid='GHCND', locationid='FIPS:12017', stationid='GHCND:USC00084289')

#Search for data and get a data.frame
out <- ncdc(datasetid='NORMAL_DLY', datatypeid='dly-tmax-normal', startdate = '2010-05-01', enddate = '2010-05-10')
out$data
