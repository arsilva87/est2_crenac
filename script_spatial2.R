
# Spatial Trends

# packages
library(rgdal)
library(sp)
library(raster)
library(mapview)
library(spdep)
library(gstat)
library(nlme)
library(AICcmodavg)

# load the data
vistula = readOGR('data_vistula/vistula_islands.shp', verbose = F)
vistula
plot(vistula)

north = subset(vistula, Name == 'northern_island')
north
plot(north)

abund = readOGR('data_vistula/abundance_n.shp', verbose = F)
abund
colnames(attr(abund, 'coords')) = c('x', 'y')
spplot(abund, zcol = 'abundance')

plot(north, axes = TRUE, col = 'lightgreen')
points(abund, cex = 0.5, pch = 3)

# --------------------------------------------------
# trend across x or y

plot(abundance ~ x, data = abund)
smoo = loess(abundance ~ x, data = abund)
lines(predict(smoo) ~ x, abund, col = 'red')

# -----------------------------------------
# Fitting regression models

model_1 = lm(abundance ~ 1, data = abund)
coef(model_1)
model_2 = lm(abundance ~ I(x^2) + I(y^2) + I(x*y) + x + y, data = abund)
coef(model_2)

# compare the models (lower AIC is better)
AIC(model_1, model_2)

# H0: model_1 = model_2, mu = b0 (is the data stationary?)
anova(model_1, model_2, test = 'LR')  # likelihood ratio test 

# --------------------------------------------------
# prediction grid
ext = extent(north)[]
gr = expand.grid(x = seq(ext[1], ext[2], length = 140), 
                 y = seq(ext[3], ext[4], length = 40))
head(gr)
coordinates(gr) = c('x', 'y')  # SpatialPoints
gridded(gr) = TRUE             # SpatialPixel object
crs(gr) = crs(north)

plot(abund)
points(gr, cex = 0.4, col  ='blue')

# --------------------------------------------------
# predictions
pred_1 = predict(model_1, newdata = gr, se.fit = TRUE)
pred_2 = predict(model_2, newdata = gr, se.fit = TRUE)

names(pred_2)   # check what is inside 'pred_1' or pred_2'

# put the predictions (fit) into the raster of the grid
r = raster(gr)  
r$pred = pred_2$se.fit
plot(r)
lines(north)

# check spatial autocorrelation on residuals with Moran's Index
gd = pointDistance(coordinates(abund), lonlat = FALSE)
wl = mat2listw(gd < 50)
moran.test(residuals(model_2, type = 'pearson'), listw = wl)

# --------------------------------------------------
# modelling the trend and the spatial autocorrelation
model_3 = gls(abundance ~ x+y,                      # trend
              data = abund,
              correlation = corGaus(form = ~ x+y,   # corr
                                   nugget = TRUE))
model_3

model_4 = gls(abundance ~ x+y,                      # trend
              data = abund)
model_4

# compare the models again
AIC(model_1, model_2, model_3, model_4)

# visualize the correlation function for model corExp()
curve((1 - 4.409231e-11)*exp(-x/3.680545e+01), from = 0, to = 200)

# visualize the correlation function for model corGaus()
curve((1 - 0.1443988)*exp(-(x/25.1052582)^2), 
      from = 0, to = 200, add = TRUE, col = 2)

# Check whether there is still spatial autocorrelation on residuals
res_3 = residuals(model_3, type = 'normalized')
moran.test(res_3, listw = wl)

