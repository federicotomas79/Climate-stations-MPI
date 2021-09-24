#Climate stations NOAA list
#(https://www.ncei.noaa.gov/products/land-based-station/global-historical-climatology-network-daily)


if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')
if (!require('readr')) install.packages('foreign'); library('foreign')

ghcnd_stations_names <- read_table("https://www.ncei.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt", col_names = FALSE)

ghcnd_stations_names <- plyr::rename(ghcnd_stations_names, c("X1" = "WScode", "X2" = "Lat", "X3"="Long", "X4" = "Alt"))
ghcnd_stations_names2 <- select(ghcnd_stations_names, -c(8,9)) 

glimpse(ghcnd_stations_names2)
View(ghcnd_stations_names2)


ghcnd_stations_spec <- read_table("https://www.ncei.noaa.gov/pub/data/ghcn/daily/ghcnd-inventory.txt", col_names = FALSE)
ghcnd_stations_spec  <- plyr::rename(ghcnd_stations_spec, c("X1" = "WScode", "X2" = "Lat", "X3"="Long", "X4" = "Element_rec", 
                                                            "X5" = "Begin_date", "X6" = "End_date"))

glimpse(ghcnd_stations_spec)
View(ghcnd_stations_spec)

#Next step (https://github.com/bczernecki/climate)
#if (!require('climate')) install.packages('climate'); library('climate')
#NZL = stations_ogimet(country = "NZWD", add_map = TRUE)


