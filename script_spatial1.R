
# Spatial Data Analysis - getting started

# packages
library(rgdal)
library(sp)
library(raster)
library(mapview)
library(spdep)
library(gstat)

# install.packages('gstat')

# load the data
vistula = readOGR('data_vistula/vistula_islands.shp', verbose = F)
vistula
plot(vistula, axes = T)

abund = readOGR('data_vistula/abundance_n.shp', verbose = F)
abund
colnames(attr(abund, 'coords')) = c('x', 'y')
spplot(abund, zcol = 'abundance')

mapview(vistula) + mapview(abund)

plot(vistula, axes = TRUE, col = 'lightgreen')
points(abund, cex = 0.5, pch = 3)

coords = coordinates(abund)
gd = pointDistance(coords, lonlat = FALSE) # this calculates the distance matrix
dim(gd)
gd[1:5, 1:5]    # this is the distances for the first 5 obs

# define weighting matrix
W = gd < 30
W[1:5, 1:5]
wl = mat2listw(W)

moran.test(abund$abundance, wl, zero.policy = TRUE)  # Moran I

locI = localmoran(abund$abundance, wl)
head(locI)
tail(locI)

abund$Ii = locI[, 1]
abund
spplot(abund, zcol = 'Ii')

# spatial prediction
plot(vistula)
vistula
north = subset(vistula, Name == 'northern_island')
plot(north)
points(abund)

# prediction grid
gr = spsample(north, type ='regular', n = 9000)
gr
points(gr, col = 'red', cex = 0.5)

colnames(attr(gr, 'coords')) = c('x', 'y')
gridded(gr) = TRUE
crs(gr) = crs(north)

pred_idw = idw(abundance ~ 1, abund, newdata = gr, idp = 3)
pred_idw

r = raster(pred_idw)
r
plot(r)

mapview(r)

writeRaster(r, filename = 'raster_north.tif')  # geoTIFF

rr = raster('raster_north.tif')
rr
plot(rr)