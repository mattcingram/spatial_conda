################################################################################
#
# Ingram, Matt 
# Reproduction of Brass et al. (2020) in Political Geography
# created: 2023-04-23
# last update: 2023-06-26
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

# two main packages to load shapefiles: rgdal and sf
# rgdal used to be common but is retiring
# sf is dominant package now
# however, good to know both in case you come across other projects using rgdal

# using rgdal:
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

# reverse % turnout (p_turn) to get % nonvoters
shp$turninv<- (1-shp$p_turn)


###########################################################################
# note from original authors' code:
# "In ArcMap, I used Intersect between districts and electric grid.
# Then, I used Dissolve for grid based on districts. I projected to 1984 WCS Mercator projection system.
# Then, I used "Calculate Geometry" to calculate line lengths.
# Read in the resultant data below.

# NOTE MI 2023-04: do not need to do this because grid per capita is already in 
# data from authors
# however, have code here for how to merge in non-spatial data and combine with
# spatial data
# as long as unit labels are the same in both data objects, can merge;
# variables do not have to have same name:

# Example
newer.grid.length<- read.csv("./data/original/Grid_line_lengths_2.csv")
str(newer.grid.length)  
# note: only have data for 147 of 170 districts
summary(newer.grid.length)
shp.merged <- merge(shp, newer.grid.length,by.x = "DIST_2008",
                    by.y= "DIST_2008", all.x=TRUE)
summary(shp.merged)
# have 23 missing values (NAs)
# replace NA with zero
shp.merged@data$length[is.na(shp.merged@data$length)] <- 0
summary(shp.merged)

# now have new variable merged with geographic features of shapefile

# remove objects since don't need them here
rm(shp.merged, newer.grid.length)

###############################################
# note on merging:
# when merging data from other sources, e.g., Afrobarometer round 4 (2008),
# could encounter problems with how districts are identified
afrobar4 <- read_sav("./data/original/afrobarometer/gha_r4_data.sav") 

# district variable:
table(afrobar4$DISTRICT)
str(table(afrobar4$DISTRICT)) # only 105 of 107 districts

str(shp$DIST_2008)
table(shp$DIST_2008)

table(shp$DIST_2008)[1:20]
table(afrobar4$DISTRICT)[1:20]

# e.g., Accra Metropolis in shp is "A M A" in afrobar4, or there are some units
# that might be hard to reconcile, e.g., ADANSI NORTH and ADANSI SOUTH in shp,
# but ADANSI EAST and ADANSI WEST in afrobar4

# these are common issues in merging any two data files, and are not unique to 
# spatial analysis

###################################################################
###################################################################

# other data 
# note: these files are NOT from authors

# Ghana Lakes shapefile from World Bank
# source: https://wbwaterdata.org/dataset/ghana-lakes
lakes <- readOGR(dsn="./shapefiles/lakes_worldbank", 
                 layer="Lakes")
plot(lakes)

# Ghana roads file from World Bank
# source: https://datacatalog.worldbank.org/search/dataset/0039327
roads <- readOGR(dsn="./shapefiles/ghana_roads_worldbank", 
                 layer="GHA_roads")

# Ghana electricity grid from World Bank
# source: https://datacatalog.worldbank.org/search/dataset/0041733
electricity <- readOGR(
  dsn="./shapefiles/ghana-electricity-transmission-network_worldbank", 
  layer="Ghana Electricity Transmission Network")

# Ghana district capitals from World Bank
# source: https://datacatalog.worldbank.org/search/dataset/0039930
capitals <- readOGR(
  dsn="./shapefiles/district_capitals_worldbank", 
  layer="GHA_District_Capitals")

plot(capitals)

################################
# alternate: use sf package
# read shapefiles as sf objects

shp.sf          <- st_read(dsn="./data/original", 
                  layer="20170226_Districts")
lakes.sf        <- st_read(dsn="./shapefiles/lakes_worldbank", 
                    layer="Lakes")
roads.sf        <- st_read(dsn="./shapefiles/ghana_roads_worldbank", 
                    layer="GHA_roads")
electricity.sf  <- st_read(
  dsn="./shapefiles/ghana-electricity-transmission-network_worldbank", 
  layer="Ghana Electricity Transmission Network")
capitals.sf     <- st_read(
  dsn="./shapefiles/district_capitals_worldbank", 
  layer="GHA_District_Capitals")

# with sf objects, can plot vars quickly to inspect
plot(shp.sf)
# or
plot(shp.sf[3])


#####################################
# COORDINATE REFERENCE SYSTEMS (CRS)
# To work with all files together: 
# need to make sure make sure COORDINATE REFERENCE SYSTEM (CRS) match 
# across objects
# CRS accounts for the way the data from a 3-D spherical surface (globe) are
# projected (represented on a flat, 2-D surface)

# check if crs matches across objects
st_crs(shp) == st_crs(lakes) 
st_crs(shp) == st_crs(roads)
st_crs(shp) == st_crs(electricity)
st_crs(shp) == st_crs(capitals)
st_crs(lakes) == st_crs(roads)
st_crs(lakes) == st_crs(electricity)
st_crs(lakes) == st_crs(capitals)
st_crs(roads) == st_crs(electricity)
st_crs(roads) == st_crs(capitals)
st_crs(electricity) == st_crs(capitals)
# only roads, electricity, and capitals are the same

# check to see what crs is
proj4string(shp)
st_crs(shp)
#NA, so core shapefile does not have a CRS

proj4string(lakes)
st_crs(lakes)

proj4string(roads)
st_crs(roads)

proj4string(electricity)
st_crs(electricity)

proj4string(capitals)
st_crs(capitals)

# make all the same as roads, capitals, or electricity
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


# just to be safe, convert all to same CRS
shp <- spTransform(shp, CRS=
                     CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
roads <- spTransform(roads, CRS=
                       CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
electricity <- spTransform(electricity, CRS=
                             CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
capitals <- spTransform(capitals, CRS=
                             CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))


# re-check CRS
st_crs(shp) == st_crs(lakes) 
st_crs(shp) == st_crs(roads)
st_crs(shp) == st_crs(electricity)
st_crs(shp) == st_crs(capitals)
st_crs(lakes) == st_crs(roads)
st_crs(lakes) == st_crs(electricity)
st_crs(lakes) == st_crs(capitals)
st_crs(roads) == st_crs(electricity)
st_crs(roads) == st_crs(capitals)
st_crs(electricity) == st_crs(capitals)
# all TRUE

# sf objects
# could convert to sf objects and re-transform and re-check
# or could simply check the previously loaded sf objects 

# first, just check crs of sf objects
st_crs(shp.sf) == st_crs(lakes.sf) 
st_crs(shp.sf) == st_crs(roads.sf) 
st_crs(shp.sf) == st_crs(electricity.sf) 
st_crs(shp.sf) == st_crs(capitals.sf) 
st_crs(lakes.sf) == st_crs(roads.sf) 
st_crs(lakes.sf) == st_crs(electricity.sf) 
st_crs(lakes.sf) == st_crs(capitals.sf) 
st_crs(roads.sf) == st_crs(electricity.sf) 
st_crs(roads.sf) == st_crs(capitals.sf) 
st_crs(electricity.sf) == st_crs(capitals.sf) 
# as was case initially with sp objects, only roads, electricity,
# and capitals match

# need to transform:

# can set to crs of another sf object
# however, cannot do this if the crs is missing (NA)
# check this with shp.sf, which had NA for crs
# shp.sf <- st_transform(shp.sf, st_crs(roads.sf))
# st_crs(shp.sf)

# if NA, can specify crs
shp.sf <- st_set_crs(shp.sf, 4326)  # if want to change, use st_transform()
st_crs(shp.sf)

lakes.sf <- st_transform(lakes.sf, 4326)
lakes.sf <- st_transform(lakes.sf, st_crs(shp.sf))
st_crs(lakes.sf)

# check
st_crs(shp.sf) == st_crs(lakes.sf) 
st_crs(shp.sf) == st_crs(roads.sf) 
st_crs(shp.sf) == st_crs(electricity.sf) 
st_crs(shp.sf) == st_crs(capitals.sf) 
st_crs(lakes.sf) == st_crs(roads.sf) 
st_crs(lakes.sf) == st_crs(electricity.sf) 
st_crs(lakes.sf) == st_crs(capitals.sf) 
st_crs(roads.sf) == st_crs(electricity.sf) 
st_crs(roads.sf) == st_crs(capitals.sf) 
st_crs(electricity.sf) == st_crs(capitals.sf) 
# all TRUE

# could have also transformed all sp objects to sf objects
# and then checked crs
shp.sf <- st_as_sf(shp)
lakes.sf <- st_as_sf(lakes)
roads.sf <- st_as_sf(roads)
electricity.sf <- st_as_sf(electricity)
capitals.sf <- st_as_sf(capitals)

# check
st_crs(shp.sf) == st_crs(lakes.sf) 
st_crs(shp.sf) == st_crs(roads.sf) 
st_crs(shp.sf) == st_crs(electricity.sf) 
st_crs(shp.sf) == st_crs(capitals.sf) 
st_crs(lakes.sf) == st_crs(roads.sf) 
st_crs(lakes.sf) == st_crs(electricity.sf) 
st_crs(lakes.sf) == st_crs(capitals.sf) 
st_crs(roads.sf) == st_crs(electricity.sf) 
st_crs(roads.sf) == st_crs(capitals.sf) 
st_crs(electricity.sf) == st_crs(capitals.sf) 
# all TRUE

# either way, now all sp and sf objects have same CRS

# can make quick plot to check that loaded correctly
plot(shp)
plot(lakes, add=TRUE, col="black")
plot(roads, add=TRUE, col="red")
plot(electricity, add=TRUE, col="yellow")


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
writeOGR(lakes, dsn="./data/working", layer="lakes_working",
         driver = "ESRI Shapefile", overwrite_layer = TRUE)

#roads <- as_Spatial(roads.sf)
writeOGR(roads, dsn="./data/working", layer="roads_working",
         driver = "ESRI Shapefile", overwrite_layer = TRUE)

#electricity <- as_Spatial(electricity.sf)
writeOGR(electricity, dsn="./data/working", layer="electricity_working",
         driver = "ESRI Shapefile", overwrite_layer = TRUE)

writeOGR(capitals, dsn="./data/working", layer="capitals_working",
         driver = "ESRI Shapefile", overwrite_layer = TRUE)

# same writing of objects, 
# but now with sf package

# e.g.
st_write(shp.sf, dsn="./data/working", layer="ghana_working",
         driver = "ESRI Shapefile", append=FALSE) # append=FALSE will overwrite layer

# save working data

save.image("./data/working/working20230626_processing.RData")

#end
