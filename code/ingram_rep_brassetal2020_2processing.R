################################################################################
#
# Ingram, M. 
# Reproduction of Brass et al. (2020) in Political Geography
# last update: 2023-04-23
# steps here: load data and process
#
##################################################################

### Load Data Files

##################################################################

# Note: 'shapefile' is common format for spatial data
# other formats: geojson (.geojson), geopackage (.gpkg)

# Load shapefile with district data
# note structure of shapefile (minimum of 3 files: .shp, .dbf, .shx)
# don't need to call all three separately; all will load automatically
shp <- readOGR(dsn="./data/original", 
               layer="20170226_Districts")

# can make quick plot to check that loaded correctly
plot(shp)

#inspect
names(shp)
str(shp)
str(shp@data)
summary(shp)

# Variable descriptions from authors

###Districts 2008
#Count_ is Total Solar
#pov_p_2008 is Percent in poverty
#gini_2008 is Gini Index
#ferat_2008 is Female Ratio
#p_share is NDC 2008 (MI note: NDC vote share)
#p_shvol is NDC vote share volatility
#p_turn is Turnout 2008
#p_tuvol is Turnout volatility
#p_ethfr is Ethnic Fractionalization
#Count_3 is World Bank projects
#Density_RD is Road density
#Pop_Densit is population density
#Count_4 is health facilities
#literacy is literacy
#grid_densi is grid density
#grid_perCa is grid per capita
#Count_5 is Lake Volta dummy

###########################################################################
# note from authors:
#In ArcMap, I used Intersect between districts and electric grid.
#Then, I used Dissolve for grid based on districts. I projected to 1984 WCS Mercator projection system.
#Then, I used "Calculate Geometry" to calculate line lengths.
#Read in the resultant data below.

# NOTE MI 2023: do not need to do this because grid per capita is already in 
# data from authors
# however, have code here for how to merge in non-spatial data and combine with
# spatial data

#newer.grid.length<- read.csv("./data/original/Grid_line_lengths_2.csv")
#shp.merged <- merge(shp,newer.grid.length,by.x = "DIST_2008",
#                    by.y= "DIST_2008",all.x=T)
#shp.merged@data$length[is.na(shp.merged@data$length)] <- 0
#shp.merged@data$gpc <- shp.merged@data$length/shp.merged@data$pop_2008

###################################################################

# other data not from authors

# Ghana Lakes shapefile from World Bank
# source: https://wbwaterdata.org/dataset/ghana-lakes
lakes <- readOGR(dsn="./shapefiles/lakes_worldbank", 
                 layer="Lakes")
plot(lakes)

roads <- readOGR(dsn="./shapefiles/ghana_roads_worldbank", 
                 layer="GHA_roads")
electricity <- readOGR(
  dsn="./shapefiles/ghana-electricity-transmission-network_worldbank", 
  layer="Ghana Electricity Transmission Network")

# convert spatial poly objects to sf objects
shp.sf <- st_read(dsn="./data/original", 
                  layer="20170226_Districts")
lakes.sf <- st_read(dsn="./shapefiles/lakes_worldbank", 
                    layer="Lakes")
roads.sf <- st_read(dsn="./shapefiles/ghana_roads_worldbank", 
                    layer="GHA_roads")
electricity.sf <- st_read(
  dsn="./shapefiles/ghana-electricity-transmission-network_worldbank", 
  layer="Ghana Electricity Transmission Network")

# with sf objects, can plot vars quickly to inspect
plot(shp.sf)

#####################################
# BASIC THEMATIC MAPS
# choropleth maps (no "L")
#####################################

# make sure CRS match across objects

st_crs(shp) == st_crs(lakes) 
st_crs(shp) == st_crs(roads)
st_crs(shp) == st_crs(electricity)
st_crs(lakes) == st_crs(roads)
st_crs(lakes) == st_crs(electricity)
st_crs(roads) == st_crs(electricity)
# only roads and electricity are the same

proj4string(shp)
st_crs(shp)
#NA

proj4string(lakes)
st_crs(lakes)

proj4string(roads)
st_crs(roads)

proj4string(electricity)
st_crs(electricity)

# make all the same as roads or electricity
proj4string(shp) <- proj4string(roads)
# or
#proj4string(shp) <-CRS("+init=epsg:24383")

# lakes already has a CRS, but unmatched one
# cannot use proj4string; need to change one CRS to another
# check here to find correct projection string for WGS 84, 
# which is what shp, roads, and electricity now have:
# https://spatialreference.org/ref/epsg/4326/
lakes <- spTransform(lakes, CRS=
                       CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

# convert all just to be safe
shp <- spTransform(shp, CRS=
                     CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
roads <- spTransform(roads, CRS=
                       CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
electricity <- spTransform(electricity, CRS=
                             CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

# or
#proj4string(lakes) <-CRS("+init=epsg:24383")

# re-check CRS
st_crs(shp) == st_crs(lakes) 
st_crs(shp) == st_crs(roads)
st_crs(shp) == st_crs(electricity)
st_crs(lakes) == st_crs(roads)
st_crs(lakes) == st_crs(electricity)
st_crs(roads) == st_crs(electricity)
# all TRUE

# convert to sf objects and re-transform and re-check

shp.sf <- st_as_sf(shp)
lakes.sf <- st_as_sf(lakes)
roads.sf <- st_as_sf(roads)
electricity.sf <- st_as_sf(electricity)

# check
st_crs(shp.sf) == st_crs(lakes.sf) 
st_crs(shp.sf) == st_crs(roads.sf) 
st_crs(shp.sf) == st_crs(electricity.sf) 
st_crs(lakes.sf) == st_crs(roads.sf) 
st_crs(lakes.sf) == st_crs(electricity.sf) 
st_crs(roads.sf) == st_crs(electricity.sf) 
# all TRUE

# if needed to transform:

# can set to crs of another sf object
#shp.sf <- st_transform(shp.sf, st_crs(roads.sf))
#st_crs(shp.sf)
#lakes.sf <- st_transform(lakes.sf, 4326)
#lakes.sf <- st_transform(lakes.sf, st_crs(shp.sf))
#st_crs(lakes.sf)

# or can specify crs
#shp.sf <- st_set_crs(shp.sf, 4326)  # if want to change, use st_transform()
#st_crs(shp.sf)

# check
st_crs(shp.sf) == st_crs(lakes.sf) 
st_crs(shp.sf) == st_crs(roads.sf) 
st_crs(shp.sf) == st_crs(electricity.sf) 
st_crs(lakes.sf) == st_crs(roads.sf) 
st_crs(lakes.sf) == st_crs(electricity.sf) 
st_crs(roads.sf) == st_crs(electricity.sf) 
# all TRUE

# can make quick plot to check that loaded correctly
plot(shp)
plot(lakes, add=TRUE)
plot(roads, add=TRUE)
plot(electricity, add=TRUE)


# subset within lakes
plot(lakes.sf)  # lake 4 is Volta
plot(lakes.sf[4,])

# if needed, can crop roads to fit country shp; different form of spatial subset
#roads.sf.crop <- roads.sf[shp.sf,]


# save new shapefiles with projection file

#shp <- as_Spatial(shp.sf)
writeOGR(shp, dsn="./data/working", layer="ghana_working",
         driver = "ESRI Shapefile", overwrite_layer = TRUE)

#lakes <- as_Spatial(lakes.sf)
writeOGR(shp, dsn="./data/working", layer="lakes_working",
         driver = "ESRI Shapefile", overwrite_layer = TRUE)

#roads <- as_Spatial(roads.sf)
writeOGR(shp, dsn="./data/working", layer="roads_working",
         driver = "ESRI Shapefile", overwrite_layer = TRUE)

#electricity <- as_Spatial(electricity.sf)
writeOGR(shp, dsn="./data/working", layer="electricity_working",
         driver = "ESRI Shapefile", overwrite_layer = TRUE)

# save working data

save.image("./data/working/working20230525")

#end
