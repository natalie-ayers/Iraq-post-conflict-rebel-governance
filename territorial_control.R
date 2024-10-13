library(ggplot2)
library(gridExtra)
library(grid)
library(broom)
library(sp)
library(dplyr)
library(lubridate)
library(geojsonio)
library(data.table)
library(sf)

setwd("~/Research/Iraq-post-conflict-rebel-governance")


isis_jan2016 <- read_sf("./data/liveuamap/liveuamap_2016-01-01-isis.geojson")


plot(select(isis_jan2016,c('Name','geometry')))

isis_jan2016 <- read_sf("./data/liveuamap/Polygons.shp")
