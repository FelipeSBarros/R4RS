library(raster)
library(rgdal)

# loading landsSat image
rj_2016 <- stack('./Landsat_imagery/rj_2016_pixel.tif')

# Plotting
# ?plotRGB
plotRGB(rj_2016, r=4, g=3, b=2, scale = maxValue(rj_2016), stretch = 'hist')
#plotRGB(rj_2016, r=4, g=2, b=1, scale = maxValue(rj_2016), stretch = 'lin')

# Loading county boundary
rj <- readOGR(dsn='./shape/', layer='33MUE250GC_SIR')
# unique(rj@data$NM_MUNICIP)
rj <- rj[rj@data$NM_MUNICIP=='RIO DE JANEIRO',]
rj <- spTransform(rj, CRS=CRS(proj4string(rj_2016)))

# Adding to map
plot(rj, add=TRUE)

# Changing LandSat extent
rj_2016 <- crop(rj_2016, rj)

# Plotting resoult
plotRGB(rj_2016, r=4, g=3, b=2, stretch='hist')
plot(rj, add=TRUE)

# Calculatin NDVI
ndvi <- overlay(rj_2016[[4]], rj_2016[[5]], fun=function(x,y){(x-y)/(x+y)})
rj_2016_2 <- addLayer(rj_2016,ndvi)
plotRGB(rj_2016_2, r=7,g=3, b=2, stretch='hist')

# Running cluster analysis (Automatic classification)
source('~/Projetos/SegmentationFCT/segmentation.R')
args(segmentation)
segmentation(envLayer = rj_2016_2, studyArea = rj, projName = "RJ_2016", 
          randomforest = FALSE, random.pt = NULL, Kmeans = TRUE, ngroup = 6, 
          polygonize = FALSE, seed = 123) 

# Result
RJ_2016_seg <- raster('./km_segmentation_RJ_2016.tif')
plot(RJ_2016_seg)

# Analysing band relation with category
graph <- addLayer(rj_2016, RJ_2016_seg)
class.df <- (as.data.frame(graph))
head(class.df)
colnames(class.df)<-c('um','dois','tres','quatro','cinco','seis','class')

#Plot
library(ggplot2)
ggplot(class.df, aes(x=um, y=dois, colour=as.factor(class))) + geom_point()