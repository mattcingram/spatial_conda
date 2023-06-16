################################################################################
#
# Ingram, Matt 
# Reproduction of Brass et al. (2020) in Political Geography
# created: 2023-04-23
# last updated: 2023-06-15
# steps here: explanatory analysis
#
################################################################################

##########################################################
# if returning to project, load last working data file:
load("./data/working/working20230615.RData")

###############################################################
# EXPLANATORY ANALYSIS
###############################################################

################# SLDV models (Table 2) #######################
### Model local, spill-over, and total effects of change in IV
# local effects are the "Direct" effects
# spill-over is the "Indirect"
# Total is the sum of the "Direct" and "Indirect" effects
#############
# Interpret the Direct Effects like OLS coefficients.

shp$turninv<- (1-shp$p_turn)

#For the robustness check with newer electric grid data
#shp.merged$turninv<- (1-shp.merged$p_turn)

#Model 1
ols1 <- lm.cluster(Count_~ pov_p_2008 + gini_2008
                   + ferat_2008 + p_share
                   + p_shvol + turninv
                   + p_tuvol + p_ethfr
                   + Count_3 + Density_RD
                   + Pop_Densit + Count_4
                   + literacy + grid_perCa,
                   data=shp@data,cluster = "REGION")
summary(ols1)   # stargazer does not recognize object created by lm.cluster

#Model 2
ols2 <- lm.cluster(Count_~ pov_p_2008 + gini_2008
                   + ferat_2008 + p_share:turninv
                   + p_share + turninv
                   + p_shvol 
                   + p_tuvol + p_ethfr
                   + Count_3 + Density_RD
                   + Pop_Densit + Count_4
                   + literacy + grid_perCa,
                   data=shp,cluster = "REGION")

summary(ols2)

#Model 3
dist2008.sldv.3<- lagsarlm(Count_~ pov_p_2008 + gini_2008
                           + ferat_2008 + p_share
                           + p_shvol + turninv
                           + p_tuvol + p_ethfr
                           + Count_3 + Density_RD
                           + Pop_Densit + Count_4
                           + literacy + grid_perCa,
                           data=shp, listw=shp.w)
summary(dist2008.sldv.3)
#Robustness check with newer grid data
#ghana.dist2008.sldv.altgrid<- lagsarlm(Count_~ pov_p_2008 + gini_2008
#                                       + ferat_2008 + p_share
#                                       + p_shvol + turninv
#                                       + p_tuvol + p_ethfr
#                                       + Count_3 + Density_RD
#                                       + Pop_Densit + Count_4
#                                       + literacy + gpc,
#                                       data=dist2008.newer, ghana.dist2008.weights)


dist2008.impacts.3<- impacts(dist2008.sldv.3, 
                             listw=shp.w,
                             R=1000,zstats=TRUE#,useHESS = T  # Hessian matrix not available
)

#dist2008.impacts.altgrid<- impacts(ghana.dist2008.sldv.altgrid, 
#                                   listw=ghana.dist2008.weights,
#                                   R=1000,zstats=TRUE,useHESS = T)

summary(dist2008.impacts.3, zstats=TRUE)
#summary(dist2008.impacts.altgrid, zstats=TRUE)


###############################################
# GWR of model 3 above

tic("dm")
dMat <- gw.dist(dp.locat=coordinates(shp))
toc()

model <- formula(Count_~ pov_p_2008 + gini_2008
                 + ferat_2008 + p_share
                 + p_shvol + turninv
                 + p_tuvol + p_ethfr
                 + Count_3 + Density_RD
                 + Pop_Densit + Count_4
                 + literacy + grid_perCa)

bw.bsq <- bw.gwr(model,
                 data=shp, 
                 approach="CV",
                 kernel="bisquare",
                 adaptive=TRUE,
                 dMat=dMat)


tic("gwr1")
gwr1 <- gwr.basic(model, 
                  data=shp, 
                  bw=bw.bsq,     
                  kernel="bisquare",
                  adaptive=TRUE, 
                  dMat=dMat
)
toc()
# less than 1 sec

write.csv(gwr1$SDF, "./tables/gwr1.csv") 


# check with different bandwidth
bw.g <- bw.gwr(model,
               data=shp, 
               approach="CV",
               kernel="gaussian",
               adaptive=TRUE,
               dMat=dMat)


tic("gwr2")
gwr2 <- gwr.basic(model, 
                  data=shp, 
                  bw=bw.g,     
                  kernel="gaussian",
                  adaptive=TRUE, 
                  dMat=dMat
)
toc()
# less than 1 sec

write.csv(gwr2$SDF, "./tables/gwr2.csv") 

###############################################
# Generate local estimates at 95% confidence
###############################################

# note: authors only focus on road density and turnout volatility
# we do all to check

#############################################

# local estimates from gwr1
gwr1$SDF$p_share95 <- gwr1$SDF$p_share
gwr1$SDF$p_share95[abs(gwr1$SDF$p_share_TV)<=1.96] <- 0

gwr1$SDF$p_shvol95 <- gwr1$SDF$p_shvol
gwr1$SDF$p_shvol95[abs(gwr1$SDF$p_shvol_TV)<=1.96] <- 0

gwr1$SDF$p_tuvol95 <- gwr1$SDF$p_tuvol
gwr1$SDF$p_tuvol95[abs(gwr1$SDF$p_tuvol_TV)<=1.96] <- 0

gwr1$SDF$Density_RD95 <- gwr1$SDF$Density_RD
gwr1$SDF$Density_RD95[abs(gwr1$SDF$Density_RD_TV)<=1.96] <- 0

gwr1$SDF$grid_perCa95 <- gwr1$SDF$grid_perCa
gwr1$SDF$grid_perCa95[abs(gwr1$SDF$grid_perCa_TV)<=1.96] <- 0

# local estimates from gwr2
gwr2$SDF$p_share95 <- gwr2$SDF$p_share
gwr2$SDF$p_share95[abs(gwr2$SDF$p_share_TV)<=1.96] <- 0

gwr2$SDF$p_shvol95 <- gwr2$SDF$p_shvol
gwr2$SDF$p_shvol95[abs(gwr2$SDF$p_shvol_TV)<=1.96] <- 0

gwr2$SDF$p_tuvol95 <- gwr2$SDF$p_tuvol
gwr2$SDF$p_tuvol95[abs(gwr2$SDF$p_tuvol_TV)<=1.96] <- 0

gwr2$SDF$Density_RD95 <- gwr2$SDF$Density_RD
gwr2$SDF$Density_RD95[abs(gwr2$SDF$Density_RD_TV)<=1.96] <- 0

gwr2$SDF$grid_perCa95 <- gwr2$SDF$grid_perCa
gwr2$SDF$grid_perCa95[abs(gwr2$SDF$grid_perCa_TV)<=1.96] <- 0

#####################################
# add local estimates to shapefile
#####################################

# add estimates from new models

shpplot <- shp
# gwr1
shpplot$p_share.gwr1 <- gwr1$SDF$p_share95
shpplot$p_shvol.gwr1 <- gwr1$SDF$p_shvol95
shpplot$p_tuvol.gwr1 <- gwr1$SDF$p_tuvol95
shpplot$road.gwr1 <- gwr1$SDF$Density_RD95
shpplot$gpc.gwr1 <- gwr1$SDF$grid_perCa95

# gwr2
shpplot$p_share.gwr2 <- gwr2$SDF$p_share95
shpplot$p_shvol.gwr2 <- gwr2$SDF$p_shvol95
shpplot$p_tuvol.gwr2 <- gwr2$SDF$p_tuvol95
shpplot$road.gwr2 <- gwr2$SDF$Density_RD95
shpplot$gpc.gwr2 <- gwr2$SDF$grid_perCa95

#########################################################
# save data
save.image("./data/working/working20230417.RData")
#########################################################


########################################################
#
# MAPS OF GWR COEFFICIENTS
#
########################################################

#################################################
# restructure data for ggplot

shpplot@data$id = rownames(shpplot@data)
# tidy requires package 'broom' from tidyverse
shpplot.points <- tidy(shpplot, region="id")

shpplot.df = join(shpplot.points, shpplot@data, by="id")  # join is from package plyr

#####################################################


# GWR1:


g <- ggplot(shpplot.df) + 
  aes(long,lat,group=group,fill=p_shvol.gwr1) + 
  geom_polygon(colour="transparent", fill="white")

g <- g + 
  geom_polygon(aes(x = long, y = lat, group = group), data = shpplot.df) +            
  #             colour = 'white', fill = 'black', alpha = .4, size = .3) +
  #scale_fill_distiller(name=NULL, palette = "Greys", trans="reverse", breaks = pretty_breaks(n = 5), na.value="white", guide="colourbar") + 
  #scale_fill_manual(name="LISA cluster", values=c("white", "red", "blue","lightblue","pink"), breaks = c("0", "1", "2", "3", "4"), labels=c("n.s.", "high-high", "low-low","low-high","hgh-low"), guide="legend") + 
  scale_fill_gradient2(name="\u03B2", 
                       #values=rescale(c(0, .05, .2)), 
                       low="blue",
                       mid="white",
                       high="red",
                       midpoint=0,
                       guide="colourbar") + 
  labs(x="", y="", title="Vote Share volatility, local \u03B2 (GWR 1, bisquare)") +
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
        axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
        axis.line.x = element_blank(), axis.line.y = element_blank(),
        plot.title = element_text(lineheight=.8, face="bold", vjust=1), # make title bold and add space
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), # removes background grid
        panel.background = element_blank(), # removes grey background; could add black axis lines with #axis.line = element_line(colour = "black")) 
        panel.spacing=unit(c(0,0,0,0), "lines"),
        plot.margin=unit(c(0,0,0,0), "mm")) + # sets margin around full plot at top, right, bottom, and left; units can also be "lines" or "cm"
  coord_equal(ratio=1) +
  geom_polygon(data=shp, aes(long,lat, group=group), colour="grey50", fill=NA) 
g

png(file="./figures/gwr1_voteshare_volatility.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()

# turnout volatility

g <- ggplot(shpplot.df) + 
  aes(long,lat,group=group,fill=p_tuvol.gwr1) + 
  geom_polygon(colour="transparent", fill="white")

g <- g + 
  geom_polygon(aes(x = long, y = lat, group = group), data = shpplot.df) +            
  #             colour = 'white', fill = 'black', alpha = .4, size = .3) +
  #scale_fill_distiller(name=NULL, palette = "Greys", trans="reverse", breaks = pretty_breaks(n = 5), na.value="white", guide="colourbar") + 
  #scale_fill_manual(name="LISA cluster", values=c("white", "red", "blue","lightblue","pink"), breaks = c("0", "1", "2", "3", "4"), labels=c("n.s.", "high-high", "low-low","low-high","hgh-low"), guide="legend") + 
  scale_fill_gradient2(name="\u03B2", 
                       #values=rescale(c(0, .05, .2)), 
                       low="blue",
                       mid="white",
                       high="red",
                       midpoint=0,
                       guide="colourbar") + 
  labs(x="", y="", title="Turnout volatility, local \u03B2 (GWR 1, bisquare)") +
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
        axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
        axis.line.x = element_blank(), axis.line.y = element_blank(),
        plot.title = element_text(lineheight=.8, face="bold", vjust=1), # make title bold and add space
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), # removes background grid
        panel.background = element_blank(), # removes grey background; could add black axis lines with #axis.line = element_line(colour = "black")) 
        panel.spacing=unit(c(0,0,0,0), "lines"),
        plot.margin=unit(c(0,0,0,0), "mm")) + # sets margin around full plot at top, right, bottom, and left; units can also be "lines" or "cm"
  coord_equal(ratio=1) +
  geom_polygon(data=shp, aes(long,lat, group=group), colour="grey50", fill=NA) 
g

png(file="./figures/gwr1_turnout_volatility.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()

# road density

g <- ggplot(shpplot.df) + 
  aes(long,lat,group=group,fill=road.gwr1) + 
  geom_polygon(colour="transparent", fill="white")

g <- g + 
  geom_polygon(aes(x = long, y = lat, group = group), data = shpplot.df) +            
  #             colour = 'white', fill = 'black', alpha = .4, size = .3) +
  #scale_fill_distiller(name=NULL, palette = "Greys", trans="reverse", breaks = pretty_breaks(n = 5), na.value="white", guide="colourbar") + 
  #scale_fill_manual(name="LISA cluster", values=c("white", "red", "blue","lightblue","pink"), breaks = c("0", "1", "2", "3", "4"), labels=c("n.s.", "high-high", "low-low","low-high","hgh-low"), guide="legend") + 
  scale_fill_gradient2(name="\u03B2", 
                       #values=rescale(c(0, .05, .2)), 
                       low="blue",
                       mid="white",
                       high="red",
                       midpoint=0,
                       guide="colourbar") + 
  labs(x="", y="", title="Road density, local \u03B2 (GWR 1, bisquare)") +
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
        axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
        axis.line.x = element_blank(), axis.line.y = element_blank(),
        plot.title = element_text(lineheight=.8, face="bold", vjust=1), # make title bold and add space
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), # removes background grid
        panel.background = element_blank(), # removes grey background; could add black axis lines with #axis.line = element_line(colour = "black")) 
        panel.spacing=unit(c(0,0,0,0), "lines"),
        plot.margin=unit(c(0,0,0,0), "mm")) + # sets margin around full plot at top, right, bottom, and left; units can also be "lines" or "cm"
  coord_equal(ratio=1) +
  geom_polygon(data=shp, aes(long,lat, group=group), colour="grey50", fill=NA) 

png(file="./figures/gwr1_road_density.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()

# grid density per capita

g <- ggplot(shpplot.df) + 
  aes(long,lat,group=group,fill=gpc.gwr1) + 
  geom_polygon(colour="transparent", fill="white")

g <- g + 
  geom_polygon(aes(x = long, y = lat, group = group), data = shpplot.df) +            
  #             colour = 'white', fill = 'black', alpha = .4, size = .3) +
  #scale_fill_distiller(name=NULL, palette = "Greys", trans="reverse", breaks = pretty_breaks(n = 5), na.value="white", guide="colourbar") + 
  #scale_fill_manual(name="LISA cluster", values=c("white", "red", "blue","lightblue","pink"), breaks = c("0", "1", "2", "3", "4"), labels=c("n.s.", "high-high", "low-low","low-high","hgh-low"), guide="legend") + 
  scale_fill_gradient2(name="\u03B2", 
                       #values=rescale(c(0, .05, .2)), 
                       low="blue",
                       mid="white",
                       high="red",
                       midpoint=0,
                       guide="colourbar") + 
  labs(x="", y="", title="Grid density pc, local \u03B2 (GWR 1, bisquare)") +
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
        axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
        axis.line.x = element_blank(), axis.line.y = element_blank(),
        plot.title = element_text(lineheight=.8, face="bold", vjust=1), # make title bold and add space
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), # removes background grid
        panel.background = element_blank(), # removes grey background; could add black axis lines with #axis.line = element_line(colour = "black")) 
        panel.spacing=unit(c(0,0,0,0), "lines"),
        plot.margin=unit(c(0,0,0,0), "mm")) + # sets margin around full plot at top, right, bottom, and left; units can also be "lines" or "cm"
  coord_equal(ratio=1) +
  geom_polygon(data=shp, aes(long,lat, group=group), colour="grey50", fill=NA) 

png(file="./figures/gwr1_gpc.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()

#######################
# GWR2

# vote share volatility

g <- ggplot(shpplot.df) + 
  aes(long,lat,group=group,fill=p_shvol.gwr2) + 
  geom_polygon(colour="transparent", fill="white")

g <- g + 
  geom_polygon(aes(x = long, y = lat, group = group), data = shpplot.df) +            
  #             colour = 'white', fill = 'black', alpha = .4, size = .3) +
  #scale_fill_distiller(name=NULL, palette = "Greys", trans="reverse", breaks = pretty_breaks(n = 5), na.value="white", guide="colourbar") + 
  #scale_fill_manual(name="LISA cluster", values=c("white", "red", "blue","lightblue","pink"), breaks = c("0", "1", "2", "3", "4"), labels=c("n.s.", "high-high", "low-low","low-high","hgh-low"), guide="legend") + 
  scale_fill_gradient2(name="\u03B2", 
                       #values=rescale(c(0, .05, .2)), 
                       low="blue",
                       mid="white",
                       high="red",
                       midpoint=0,
                       guide="colourbar") + 
  labs(x="", y="", title="Vote Share volatility, local \u03B2 (GWR 2, gaussian)") +
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
        axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
        axis.line.x = element_blank(), axis.line.y = element_blank(),
        plot.title = element_text(lineheight=.8, face="bold", vjust=1), # make title bold and add space
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), # removes background grid
        panel.background = element_blank(), # removes grey background; could add black axis lines with #axis.line = element_line(colour = "black")) 
        panel.spacing=unit(c(0,0,0,0), "lines"),
        plot.margin=unit(c(0,0,0,0), "mm")) + # sets margin around full plot at top, right, bottom, and left; units can also be "lines" or "cm"
  coord_equal(ratio=1) +
  geom_polygon(data=shp, aes(long,lat, group=group), colour="grey50", fill=NA) 

png(file="./figures/gwr2_voteshare_volatility.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()


# turnout volatility

g <- ggplot(shpplot.df) + 
  aes(long,lat,group=group,fill=p_tuvol.gwr2) + 
  geom_polygon(colour="transparent", fill="white")

g <- g + 
  geom_polygon(aes(x = long, y = lat, group = group), data = shpplot.df) +            
  #             colour = 'white', fill = 'black', alpha = .4, size = .3) +
  #scale_fill_distiller(name=NULL, palette = "Greys", trans="reverse", breaks = pretty_breaks(n = 5), na.value="white", guide="colourbar") + 
  #scale_fill_manual(name="LISA cluster", values=c("white", "red", "blue","lightblue","pink"), breaks = c("0", "1", "2", "3", "4"), labels=c("n.s.", "high-high", "low-low","low-high","hgh-low"), guide="legend") + 
  scale_fill_gradient2(name="\u03B2", 
                       #values=rescale(c(0, .05, .2)), 
                       low="blue",
                       mid="white",
                       high="red",
                       midpoint=0,
                       guide="colourbar") + 
  labs(x="", y="", title="Turnout volatility, local \u03B2 (GWR 2, gaussian)") +
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
        axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
        axis.line.x = element_blank(), axis.line.y = element_blank(),
        plot.title = element_text(lineheight=.8, face="bold", vjust=1), # make title bold and add space
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), # removes background grid
        panel.background = element_blank(), # removes grey background; could add black axis lines with #axis.line = element_line(colour = "black")) 
        panel.spacing=unit(c(0,0,0,0), "lines"),
        plot.margin=unit(c(0,0,0,0), "mm")) + # sets margin around full plot at top, right, bottom, and left; units can also be "lines" or "cm"
  coord_equal(ratio=1) +
  geom_polygon(data=shp, aes(long,lat, group=group), colour="grey50", fill=NA) 

png(file="./figures/gwr2_turnout_volatility.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()

# road density

g <- ggplot(shpplot.df) + 
  aes(long,lat,group=group,fill=road.gwr2) + 
  geom_polygon(colour="transparent", fill="white")

g <- g + 
  geom_polygon(aes(x = long, y = lat, group = group), data = shpplot.df) +            
  #             colour = 'white', fill = 'black', alpha = .4, size = .3) +
  #scale_fill_distiller(name=NULL, palette = "Greys", trans="reverse", breaks = pretty_breaks(n = 5), na.value="white", guide="colourbar") + 
  #scale_fill_manual(name="LISA cluster", values=c("white", "red", "blue","lightblue","pink"), breaks = c("0", "1", "2", "3", "4"), labels=c("n.s.", "high-high", "low-low","low-high","hgh-low"), guide="legend") + 
  scale_fill_gradient2(name="\u03B2", 
                       #values=rescale(c(0, .05, .2)), 
                       low="blue",
                       mid="white",
                       high="red",
                       midpoint=0,
                       guide="colourbar") + 
  labs(x="", y="", title="Road density, local \u03B2 (GWR 2, gaussian)") +
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
        axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
        axis.line.x = element_blank(), axis.line.y = element_blank(),
        plot.title = element_text(lineheight=.8, face="bold", vjust=1), # make title bold and add space
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), # removes background grid
        panel.background = element_blank(), # removes grey background; could add black axis lines with #axis.line = element_line(colour = "black")) 
        panel.spacing=unit(c(0,0,0,0), "lines"),
        plot.margin=unit(c(0,0,0,0), "mm")) + # sets margin around full plot at top, right, bottom, and left; units can also be "lines" or "cm"
  coord_equal(ratio=1) +
  geom_polygon(data=shp, aes(long,lat, group=group), colour="grey50", fill=NA) 

png(file="./figures/gwr2_road_density.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()

# grid density per capita

g <- ggplot(shpplot.df) + 
  aes(long,lat,group=group,fill=gpc.gwr2) + 
  geom_polygon(colour="transparent", fill="white")

g <- g + 
  geom_polygon(aes(x = long, y = lat, group = group), data = shpplot.df) +            
  #             colour = 'white', fill = 'black', alpha = .4, size = .3) +
  #scale_fill_distiller(name=NULL, palette = "Greys", trans="reverse", breaks = pretty_breaks(n = 5), na.value="white", guide="colourbar") + 
  #scale_fill_manual(name="LISA cluster", values=c("white", "red", "blue","lightblue","pink"), breaks = c("0", "1", "2", "3", "4"), labels=c("n.s.", "high-high", "low-low","low-high","hgh-low"), guide="legend") + 
  scale_fill_gradient2(name="\u03B2", 
                       #values=rescale(c(0, .05, .2)), 
                       low="blue",
                       mid="white",
                       high="red",
                       midpoint=0,
                       guide="colourbar") + 
  labs(x="", y="", title="Grid density pc, local \u03B2 (GWR 2, gaussian)") +
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
        axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
        axis.line.x = element_blank(), axis.line.y = element_blank(),
        plot.title = element_text(lineheight=.8, face="bold", vjust=1), # make title bold and add space
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), # removes background grid
        panel.background = element_blank(), # removes grey background; could add black axis lines with #axis.line = element_line(colour = "black")) 
        panel.spacing=unit(c(0,0,0,0), "lines"),
        plot.margin=unit(c(0,0,0,0), "mm")) + # sets margin around full plot at top, right, bottom, and left; units can also be "lines" or "cm"
  coord_equal(ratio=1) +
  geom_polygon(data=shp, aes(long,lat, group=group), colour="grey50", fill=NA) 

png(file="./figures/gwr2_gpc.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()

####################################
# save working data

save.image("./data/working/working20230615.RData")

#end
