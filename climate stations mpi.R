#Federico Tomasetto
#9th September 2021
#MPI-R Shiny project

getwd()
setwd(choose.dir(default = "", caption = "Select folder"))

if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')
if (!require('foreign')) install.packages('foreign'); library('foreign')
if (!require('gt')) install.packages('gt'); library('gt')

#Get data from (https://databasin.org/datasets/15a31dec689b4c958ee491ff30fcce75/)
clim.stat.wc1 <- read.dbf("C:/Users/TOMASETTOF/OneDrive - AgResearch/Documents/GitHub/Worldclim-stations/climate stations/Data0/stations1.dbf", as.is = FALSE)

#Summarize data per country
clim.stat.country1 <- clim.stat.wc1 %>% 
  group_by(COUNTRY) %>%
  summarise(count = n_distinct(WCID))
clim.stat.country1                 

#Export data as table
write.table(clim.stat.country1, file = "Worldclim stations per country.txt", sep = ",")

#Export data for presentation
wclim.table1 <- 
  clim.stat.country1 %>% 
  gt()%>%
    tab_header(title = md("Worldclim stations = 56,000"))

wclim.table1 %>%
  gtsave("Worldclim stations per country.html", inline_css = TRUE)

#------------------------------------------------------------------#
#Extract only climate stations data with Precipitation, Tmin & Tmax
glimpse(clim.stat.wc1)

clim.stat.wc1 %>% filter(RAINID!="NA", TMINID!="NA", TMAXID!="NA")

#Summarize data per country
clim.stat.country2 <- clim.stat.wc1 %>% 
  group_by(COUNTRY) %>%
  summarise(count = n_distinct(WCID))
clim.stat.country2                 

#Export data as table
write.table(clim.stat.country2, file = "Worldclim stations per country no NAs.txt", sep = ",")

#Export data for presentation
wclim.table2 <- 
  clim.stat.country2 %>% 
  gt()%>%
  tab_header(title = md("Worldclim stations no Ptt NAs = 10,400"))

wclim.table2 %>%
  gtsave("Worldclim stations without Ptt NAs.html", inline_css = TRUE)





