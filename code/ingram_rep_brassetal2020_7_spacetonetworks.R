################################################################################
#
# Ingram, Matt 
# Reproduction of Brass et al. (2020) in Political Geography
# created: 2023-04-23
# last updated: 2023-06-23
# steps here: from space to networks
#
################################################################################

##########################################################
# if returning to project, load last working data file:
load("./data/working/working20230621.RData")

######################################
#
# From Spatial to Network Analysis
#
######################################

# core issues:
# (1) how weights matrix captures connectivity among units
# (2) network methods for analysis

# regarding network W:
# here, show how could move away from contiguity and distance approaches 
# typical of spatial analysis and move to other forms fo connectivity 

# Question: why might you want to do this? (for different, theory-based W)
# Question: when would you be able to do this? (when have data)

# regarding network methods:
# range of descriptive, exploratory, and explanatory tools

#######################################
# Network connectivity
# Illustration with fake edge weights

# recall discussion of W earlier using graph/network ideas

# create graph object from matrix of earlier queen-1 contiguity W
g <- graph_from_adjacency_matrix(
  listw2mat(wq1b),
  mode="undirected",
  weighted=NULL
  )
g

plot(g)
# replot several times (re-run same line)
plot(g, vertex.size=5, edge.width=3)
# can see better if highlight set of connected nodes/vertices
highlight.these=c(16,27,52,111)
vertex.attributes(g)$color=
  ifelse(vertex_attr(g)$name%in%highlight.these, 
         "yellow","white")
plot(g, vertex.size=5, edge.width=3)
# note changes
# as if picking up and dropping bottlecaps connected by string;
# shape might change (graph layout), but connectivity structure (W) remains the same

# now connect to Ghana geography

# simple igraph plot, adjusting layout to geo coordinates
plot(g, layout=as.matrix(coords[, 1:2]))

plot(g, layout=as.matrix(coords[, 1:2]),
     vertex.size=5,
     edge.width=3)

# ADJUST EDGE WEIGHT
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



# ADD EDGE ATTRIBUTE
# *** this could be anything
edges <- data.frame(get.edgelist(g))
head(edges)

g <- set_edge_attr(g, 
                   name="weight", 
                   value=runif(nrow(edges)/2, min=1, max=10)  # divide by 2 because undirected
                   )

# plot weighted graph
plot(g, layout=as.matrix(coords[, 1:2]),
     vertex.size=5,
     edge.width=E(g)$weight)

# recover W from g
gmat <- as_adjacency_matrix(g, type = "both",   # type=c("both", "upper", "lower"),
                            attr = "weight", 
                            #edges = FALSE, 
                            names = TRUE,
                            sparse = igraph_opt("sparsematrices"))

gmat[1:5, 1:5]

# convert to list
wgmat <- mat2listw(gmat)

# could now run exploratory and explanatory models with new network W

# global Moran's I
my1.net <- moran.test(shp$Count_, listw=wgmat)
my1.net

slm.net <- lagsarlm(Count_~ pov_p_2008 + gini_2008      # formula
                + ferat_2008 + p_share
                + p_shvol + turninv
                + p_tuvol + p_ethfr
                + Count_3 + Density_RD
                + Pop_Densit + Count_4
                + literacy + grid_perCa,
                data=shp,      # data
                listw=wgmat)   # W
summary(slm.net)

# Questions?



# could also extend further with (a) other data and (b) network methods,
# descriptive, exploratory, and explanatory



#############################################################
# Can try with actual data on roadnetwork
######################################
# road network

rnet <- shp2graph::readshpnw(roads)
str(rnet)
# see that rnet[[2]] and rnet[[3]] contain net structure
rnet[[2]]   # node list
rnet[[3]]   # edge list
rnet[[5]]   # df with edge attributes
rnet[[6]]   # x-coords of nodes
rnet[[7]]   # y-coord of nodes

# weight edge by surface type
table(rnet[[5]]$SURF_TYPE_)
#-99   2   3   4 
#15 183 577 468 
# invert and recode so that larger numbers identify better surface (e.g., asphalt)
rnet[[5]]$surftypeinv <- 6 - rnet[[5]]$SURF_TYPE_   # df with edge attributes
rnet[[5]]$surftypeinv[rnet[[5]]$SURF_TYPE_ == -99] <- 1   # df with edge attributes
table(rnet[[5]]$surftypeinv)
rnet[[5]]$surftypeinv <- rnet[[5]]$surftypeinv^1.5   # df with edge attributes
table(rnet[[5]]$surftypeinv)

# check plot coords
plot(rnet[[6]], rnet[[7]])

# convert net to igraph object
igr1 <- shp2graph::nel2igraph(rnet[[2]], rnet[[3]], eadf = rnet[[5]]) 
summary(igr1)
# network of 1382 vertices and 1243 edges

# simple plot
plot(igr1, vertex.label = NA, vertex.size = 2, vertex.size2 = 2, 
     mark.col = "green", main = "The converted igraph road net")

# plot with edges weighted by attribute (here, surface type)
plot(igr1, vertex.label = NA, vertex.size = 2, vertex.size2 = 2, 
     #edge.width = E(igr1)$SURF_TYPE_,
     edge.width = E(igr1)$surftypeinv,
     mark.col = "green", main = "The converted igraph road net, surface type")

mat.igr1 <- as_adjacency_matrix(igr1, type = "both",  #c("both", "upper", "lower"),
                                attr = surftypeinv, edges = TRUE, names = TRUE,
                                sparse = igraph_opt("sparsematrices"))

mat.igr1

########################################
# pick up here
# merge points and lines and calculate distances with riverdist package
# source: https://cran.r-project.org/web/packages/riverdist/vignettes/riverdist_vignette.html

roads.r <- line2network(path = "./shapefiles/ghana_roads_worldbank",
                      layer = "GHA_roads",
                      reproject = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

plot(roads)
roads_clean <- cleanup(roads)

# check connectedness
topologydots(rivers=roads_clean)



# using sfnetworks package
#rnet.sf <- as_sfnetwork(rnet[[3]])

# self-connected components or subgraphs
rnet.nt.con <- igraph::clusters(igr1)   # 150 subgraphs
# same as
rnet.nt.con <- shp2graph::nt.connect(roads) # 150 self-connected parts
# highly fragmented road network
plot(rnet.nt.con) # largest self-connected part



########################################################
# exercise:
# compare to London road net (data in shp2graph package) with 1 large connected component
# note: may take a while to process nt.connect
data(LNNT) # load data from package
LNnt.con <- nt.connect(LN.nt) # LN.nt is the London road shp
# last step may take a few minutes
# remove these London objects
rm(LNNT)
rm(LNnt.con)
########################################################

# get centroids of areal units (polygons) from shapefile
cen_points <- st_coordinates(shp.sf)
cen_points <- as_Spatial(cen_points)

# snap points to nearest point on lines from road net
tic()
rnet.vnp <- shp2graph::points2network(ntdata = roads, pointsxy = cen_points, 
                          ELComputed = TRUE, 
                          approach = 1,   # 1 = map ea point to closest point on line
                          Detailed = FALSE,
                          ea.prop = rep(0, 26))
toc()


str(rnet.vnp)
# unit centroids are at rnet.vnp[[3]]

igr2 <- shp2graph::nel2igraph(rnet.vnp[[1]], rnet.vnp[[2]])
plot(igr2)

mat.igr2 <- as_adjacency_matrix(igr2, type = "both", # c("both", "upper", "lower"),
                                attr = , edges = FALSE, names = TRUE,
                                sparse = igraph_opt("sparsematrices"))
# or
rnet.vnp2 <- maptools::snapPointsToLines(cen_points, 
                                         roads, 
                                         maxDist = 200  # in meters
                                         )

# pick up here with r-spatial exampel on distances

ptsinnt.view(ntdata = roads, nodelist = res.vnp[[1]], pointsxy = cen_points, 
             CoorespondIDs = res.vnp[[3]], VElist = res.vnp[[7]])

# convert road network to W
wnet <- network_li



################################

#r-spatial example

# Add data to GRASS spatial database

# read roads shapefile with terra so have SpatVector object
roads.vec <- terra::vect("./shapefiles/ghana_roads_worldbank/GHA_roads.shp")
rgrass::write_VECT(                           # rgrass7::writeVECT deprecated; use rgrass::write_VECT
  x = roads.vec, 
  vname = 'roads.vec', 
  flags = 'overwrite'
)

# left off here

####################################
# save working data

save.image("./data/working/working20230621.RData")

#end
