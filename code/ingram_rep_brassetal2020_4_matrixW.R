################################################################################
#
# Ingram, Matt 
# Reproduction of Brass et al. (2020) in Political Geography
# created: 2023-04-23
# last updated: 2023-06-26
# steps here: generate connectivity matrix (W) and graph
#
################################################################################

##########################################################
# if returning to project, load last working data file:
load("./data/working/working20230626_processing.RData")


################################################
# Generate measures of connectivity
################################################

# Two main approaches based on: (1) contiguity and (2) distance

# note: this is a KEY STEP
# caution: if going to be estimating direct, indirect, and total effects, then 
# note that row-standardizing W restricts estimates of average total effects
# this may not be a reasonable approach (see Whitten, Williams, and Wimpy 2021, p149)
# here, authors row-standardized so we also row-standardize;
# we also generate other Ws for illustration purposes, and for option to check
# robustness later across different Ws

# a lot of theory can go into constructing W
# will return to this later

#####################################
# contiguity-based 
#####################################

# generate neighbor list objects
nb.q1 <- poly2nb(shp)   # default is queen=TRUE
nb.r1 <- poly2nb(shp, queen=FALSE)   # default is queen=TRUE
# inspect and note structure
nb.q1
nb.r1
# same connectivity structure, i.e., no difference between them in this shp
# can confirm with diffnb
diffnb(nb.q1, nb.r1)
# generates nb object with unmatched connections
# here, 0, so all connections matched

str(nb.q1)
head(nb.q1)
# e.g., unit 1 has 3 neighbors: 125, 128, and 129


#################################
# distance-based nbs
#################################

# needs coordinates object
coords <- st_coordinates(     # st_coordinates() generates matrix with 2 columns
  st_centroid(                # could use st_centroid, but this generates sfc_POINT object
    st_geometry(shp.sf), of_largest_polygon=TRUE))
# if SpatialPolygons object:
#coords <- data.frame(coordinates(shp))  # note first entry has row.names=0
colnames(coords) <- c("x", "y")

# convert to data.frame
coords <- data.frame(coords)

str(coords)
head(coords)
tail(coords)

# k # of nearest neighbors (knn) connectivity
# single nearest neighbor
k1 <- knn2nb(knearneigh(coords))

# 5 nearest neighbors (k=5)
k5 <- knn2nb(knearneigh(coords, k=5))
# note this is a neighbor list (nb) object, just like nb.q1

# distance-based connectivity
all.linked <- max(unlist(nbdists(k5, coords))) # max distance between any 2 units
dist.nb.0.all <- dnearneigh(coords, 0, all.linked, row.names=row.names(shp.sf))


#####################################################
# basic visualization of connectivity

# using queen-1 W
plot(st_geometry(shp.sf), border="grey", reset=FALSE, 
     main="contiguity-based nb, queen-1")
plot(nb.q1, coords=coords, col="red", add=TRUE)

# using k1 W
plot(st_geometry(shp.sf), border="grey", reset=FALSE, 
     main=paste("distance-based nb, k=", attributes(k1)$`knn-k`, sep=""))
plot(k1, coords=coords, col="red", add=TRUE)

# using k5 W
plot(st_geometry(shp.sf), border="grey", reset=FALSE, 
     main=paste("distance-based nb, k=", attributes(k5)$`knn-k`, sep=""))
plot(k5, coords=coords, col="red", add=TRUE)

# using distance (dnn) W
plot(st_geometry(shp.sf), border="grey", reset=FALSE, 
     main=paste("distance-based nb, 0-", all.linked, sep=""))
plot(dist.nb.0.all, coords=coords, col="red", add=TRUE)


########################################################
#### Convert neighbor list to spatial weights matrix (W), listw objects

# many functions in R require listw objects

# queen-1
wq1 <- nb2listw(nb.q1, style="W", zero.policy=T)  # row-standardized
wq1b <- nb2listw(nb.q1, style="B", zero.policy=T)  # basic binary coding
# rook-1
wr1 <- nb2listw(nb.r1, style="W", zero.policy=T)  # row-standardized
wr1b <- nb2listw(nb.r1, style="B", zero.policy=T)  # basic binary coding

# check structure
wq1b
wr1b
# confirm same connections
str(wq1b)
str(wr1b)

# distance-based Ws
wk1 <- nb2listw(k1, style="B", zero.policy=T)  # basic binary coding
wk5 <- nb2listw(k5, style="B", zero.policy=T)  # basic binary coding
wdist <- nb2listw(dist.nb.0.all, style="B", zero.policy=T)  # basic binary coding

#################################
# nicer map of connectivity
# note: we'll return to some of these graph depictions of W later in discussion
# of connections between spatial and network analysis

# using queen-1

# create graph object from matrix of list object
g <- graph.adjacency(listw2mat(wq1b))
# generate edgelist
edges <- as.data.frame(get.edgelist(g))

# match coordinates of start and end nodes for each edge
head(coords)
tail(coords)
head(edges)
tail(edges)
colnames(edges) <- c("startnode", "endnode")
coords$id <- as.numeric(rownames(coords))
# if coords from spdep: coords$id <- as.numeric(rownames(coords))+1
str(coords)
summary(coords) # range of id is 1:170
str(edges)
summary(edges) # range of both startnode and endnode is 1:170
edgecoords1 <- merge(edges, coords, by.x="startnode", by.y="id")
str(edgecoords1)
head(edgecoords1)
edgecoords2 <- merge(edgecoords1, coords, by.x="endnode", by.y="id")
head(edgecoords2)

# because merged by "endnode", endnode now appears in column 1, 
# and columns 3-4 correspond to coords for endnode, and cols 5-6 to startnode
# need to sort and reorder
edgecoords2 <- edgecoords2[order(edgecoords2$startnode), ]
head(edgecoords2)
# reorder
edgecoords2 <- edgecoords2[,c(2,1,
                              3,4,
                              5,6)]
head(edgecoords2)

colnames(edgecoords2) <- c("startnode", "endnode", "xstart", "ystart",
                                "xend", "yend")
head(edgecoords2)

# graph q1 connectivity with ggplot

g1 <- ggplot(data=shp.sf) +
  geom_sf(aes(), colour="grey30", fill=NA) +
  geom_point(data=coords[,1:2], aes(x=x, y=y), inherit.aes=FALSE) +
  geom_segment(data=edgecoords2, aes(x=xstart, y=ystart, 
                                     xend=xend, yend=yend,
                                     colour="red"), 
               inherit.aes=FALSE, show.legend = FALSE) +
  theme(axis.text = element_text(family = 'Cairo')) +  # Cairo enable degree symbol
  theme_minimal()
g1

png(file="./figures/map_wq1.png", height=6, width=6, units="in", res=300)
print(g1)
dev.off()


# shorter, uncommented version with k1 W

g <- graph.adjacency(listw2mat(wk1))
edges <- as.data.frame(get.edgelist(g))
colnames(edges) <- c("startnode", "endnode")
coords$id <- as.numeric(rownames(coords))
edgecoords1 <- merge(edges, coords, by.x="startnode", by.y="id")
edgecoords2 <- merge(edgecoords1, coords, by.x="endnode", by.y="id")
edgecoords2 <- edgecoords2[order(edgecoords2$startnode), ]
edgecoords2 <- edgecoords2[,c(2,1,
                              3,4,
                              5,6)]
colnames(edgecoords2) <- c("startnode", "endnode", "xstart", "ystart",
                           "xend", "yend")

g2 <- ggplot(data=shp.sf) +
  geom_sf(aes(), colour="grey30", fill=NA) +
  geom_point(data=coords[,1:2], aes(x=x, y=y), inherit.aes=FALSE) +
  geom_segment(data=edgecoords2, aes(x=xstart, y=ystart, 
                                     xend=xend, yend=yend,
                                     colour="red"), 
               inherit.aes=FALSE, show.legend = FALSE) +
  theme(axis.text = element_text(family = 'Cairo')) +  # Cairo enable degree symbol
  theme_minimal()
g2

png(file="./figures/map_wk1.png", height=6, width=6, units="in", res=300)
print(g2)
dev.off()

# plot side by side

grid.arrange(g1, g2, ncol=2)

png(file="./figures/map_wq1-wk1.png", height=6, width=6, units="in", res=300)
grid.arrange(g1, g2, ncol=2)
dev.off()

# remove large graph objects
rm(g1, g2)

####################################
# save working data

save.image("./data/working/working20230626_W.RData")

# end
