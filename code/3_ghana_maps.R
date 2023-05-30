################################################################################
#
# Ingram, M. 
# Reproduction of Brass et al. (2020) in Political Geography
# last update: 2023-04-23
# steps here: basic mapping of different data types; retrieving background maps 
# from web
#
################################################################################

############################
# maps
############################

# with geom_sf()

# basic choropleth of ghana with vote share measure (p_share)

g <- ggplot(data=shp.sf) +
  geom_sf(aes(fill=p_share))
g

# next steps polish and add data to this basic map

# reverse coding of mapped variable so that darker shades indicate higher values
# and clean up background

g <- ggplot(data=shp.sf) +
  geom_sf(aes(fill=p_share)) +
  scale_fill_continuous(trans = 'reverse') +
  theme_minimal()
g


############################
# nicer maps
############################

# with geom_sf(), can re-check which lake is Lake volta
# should be object 4 within lakes object
ggplot(data=lakes.sf[4,]) +
  geom_sf() 
# or
plot(lakes.sf)
plot(lakes.sf[4,])

g <- ggplot(data=shp.sf) +
  geom_sf(aes(fill=p_share)) +
  scale_fill_continuous(trans = 'reverse') +
  geom_sf(data=lakes.sf[4,], fill="black", inherit.aes = FALSE) +
  geom_sf(data=roads.sf, color="red", inherit.aes = FALSE) +
  geom_sf(data=electricity.sf, color="yellow", inherit.aes = FALSE) +
  #coord_sf(default_crs=sf::st_crs(4326)) +     # set a crs that applies to all layers
  #xlim(-3.5, 1.5) +
  #ylim(4.5, 11.5) +
  theme_minimal()
g

png(file="./figures/map_p_share.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()



#########################################################################
# add layer for lakes and then crop to focus on Ghana
# add layer
g <- ggplot(data=shp.sf) +
  geom_sf(aes(fill=p_share)) +
  scale_fill_continuous(trans = 'reverse') +
  geom_sf(data=lakes.sf, fill="black") +
  coord_sf(crs=4326) +     # applies to all layers
  theme_minimal()
g

# crop
g <- ggplot(data=shp.sf) +
  geom_sf(aes(fill=p_share)) +
  scale_fill_continuous(trans = 'reverse') +
  geom_sf(data=lakes.sf, fill="black") +
  coord_sf(crs=4326) +     # applies to all layers
  xlim(-3.25, 1.1) +
  ylim(4.5, 11.5) +
  theme_minimal()
g
########################################################################

# better alternative: subset lakes.sf to add only Lake Volta; no need to crop

g <- ggplot(data=shp.sf) +
  geom_sf(aes(fill=p_share)) +
  scale_fill_continuous(trans = 'reverse') +
  geom_sf(data=lakes.sf[4,], fill="black") +   # subsets Lake Volta from lakes.sf
  coord_sf(crs=4326) +     # applies to all layers; ensures same CRS
  theme_minimal()
g

# add district capitals (points)

g <- ggplot(data=shp.sf) +
  geom_sf(aes(fill=p_share)) +
  scale_fill_continuous(trans = 'reverse') +
  geom_sf(data=lakes.sf[4,], fill="black") +
  geom_sf(data=capitals.sf, color="red") +
  coord_sf(crs=4326) +     # applies to all layers
  xlim(-3.25, 1.1) +
  ylim(4.5, 11.5) +
  theme_minimal()
g


# add roads (lines)
g <- ggplot(data=shp.sf) +
  geom_sf(aes(fill=p_share)) +
  scale_fill_continuous(trans = 'reverse') +
  geom_sf(data=lakes.sf[4,], fill="black") +
  geom_sf(data=roads.sf.crop, color="yellow") +
  geom_sf(data=capitals.sf, color="red") +  # add points after lines so they show
  coord_sf(crs=4326) +     # applies to all layers
  xlim(-3.5, 1.25) +
  ylim(4.5, 11.5) +
  theme_minimal()
g

# place on satellite map of region
register_google(key="AIzaSyBjzmzxEM8x1LMZMVg_MrnHAtyUWTwmBDg")
region <- get_map("ghana", maptype = "satellite", zoom = 6, source = "google")
region2 <- get_map("ghana", maptype = "satellite", zoom = 7, source = "google")
region3 <- get_map("ghana", maptype = "terrain", zoom = 7, source = "google")
region4 <- get_map("ghana", maptype = "hybrid", zoom = 7, source = "google")

# check
ggmap(region2)
ggmap(region3)

g <- ggmap(region4) +
  geom_sf(data=shp.sf, aes(fill=p_share), inherit.aes = FALSE) +
  scale_fill_continuous(trans = 'reverse') +
  geom_sf(data=lakes.sf[4,], fill="black", inherit.aes = FALSE) +
  geom_sf(data=cropped, color="yellow", inherit.aes = FALSE) +
  geom_sf(data=capitals.sf, color="red", inherit.aes = FALSE) +  # add points after lines so they show
  coord_sf(crs=st_crs(4326)) +     # applies to all layers
  xlim(-3.5, 1.25) +
  ylim(4.5, 11.5) +
  theme_minimal() 
g

# save image to file
png(file="./figures/map_p_share1.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()

# or
ggsave("./figures/map_p_share2.png", plot=g, 
       height=6, width=6, units="in", dpi=300)


# save region images to file; increase dpi to 600 to retain high-res images 
g <- ggmap(region)
ggsave(filename = "./data/original/ggmap_region.png", plot = g, 
       height=6, width=6, units="in", dpi=600)

g <- ggmap(region2)
ggsave(filename = "./data/original/ggmap_region2.png", plot = g, 
       height=6, width=6, units="in", dpi=600)

g <- ggmap(region3)
ggsave(filename = "./data/original/ggmap_region3.png", plot = g, 
       height=6, width=6, units="in", dpi=600)

g <- ggmap(region4)
ggsave(filename = "./data/original/ggmap_region4.png", plot = g, 
       height=6, width=6, units="in", dpi=600)

#end
