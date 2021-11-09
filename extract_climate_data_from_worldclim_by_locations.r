library(raster)
library(sp)

#Example for bioclimatic variables 
r <- getData("worldclim", var="bio", res=10)

r <- r[[c(1,12)]]
names(r) <- c("Temp","Prec")
r

points <- spsample(as(r@extent, 'SpatialPolygons'), n=10, type="random")  

values <- extract(r,points)
values

df <- cbind.data.frame(coordinates(points),values)
head(df)

#Plot annual precipitation
plot(r[[2]])
plot(points,add=T)


#Our example

r_tim <- getData("worldclim", var='tmin', res=2.5)

r <- r[[c(1,12)]]
names(r) <- c("Temp","Prec")
r

points <- spsample(as(r@extent, 'SpatialPolygons'), n=10, type="random")  

values <- extract(r,points)
values

df <- cbind.data.frame(coordinates(points),values)
head(df)

plot(r[[2]])
plot(points,add=T)
