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
load("./data/working/working20230626_models.RData")

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

# note changes
# as if picking up and dropping bottlecaps connected by string;
# shape might change (graph layout), but connectivity structure (W) remains the same

# now connect to Ghana geography

# simple igraph plot, adjusting layout to geo coordinates
plot(g, layout=as.matrix(coords[, 1:2]))

plot(g, layout=as.matrix(coords[, 1:2]),
     vertex.size=5,
     edge.width=3)


# ADD EDGE ATTRIBUTE
# *** this could be anything
edges <- data.frame(get.edgelist(g))
head(edges)

g <- set_edge_attr(g, 
                   name="weight", 
                   value=runif(nrow(edges), min=1, max=10)
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



####################################
# save working data

save.image("./data/working/working20230621.RData")

#end
