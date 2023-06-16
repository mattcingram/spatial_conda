################################################################################
#
# Ingram, Matt 
# Reproduction of Brass et al. (2020) in Political Geography
# created: 2023-04-23
# last updated: 2023-06-15
# steps here: exploratory analysis; global moran, local c, and LISA
#
################################################################################

##########################################################
# if returning to project, load last working data file:
load("./data/working/working20230606.RData")


#############################################
# Exploratory Analysis
#############################################

# Global Moran'S I 

# global moran of outcome
my1 <- moran.test(shp$Count_, listw=wq1b)
my1
# spatial dependence present: I = 0.304 and is sig at p<.001
# specifically, positive spatial dependence

# global moran of key predictors
mx1 <- moran.test(shp$p_share, listw=wq1b)
mx2 <- moran.test(shp$p_tuvol, listw=wq1b)
mx3 <- moran.test(shp$Density_RD, listw=wq1b)
mx4 <- moran.test(shp$grid_perCa, listw=wq1b)

# build table with global Is and p-values

globalIs <- as.data.frame(rbind(cbind(round(my1$estimate[[1]], 3), #"<.001"), #round(
                                      my1$p.value),
                                cbind(round(mx1$estimate[[1]], 3), #"<.001"), #round(
                                      mx1$p.value),
                                cbind(round(mx2$estimate[[1]], 3), #"<.001"), #round(
                                      mx2$p.value),
                                cbind(round(mx3$estimate[[1]], 3), #"<.001"), #round(
                                      mx3$p.value),
                                cbind(round(mx4$estimate[[1]], 3), #"<.001"), #round(
                                      mx4$p.value)))

globalIs$Variable <- c("solar", "vote share", "turnover volatility", 
                       "road density", "grid percap")
# reorder columns
globalIs <- globalIs[, c(3,1,2)]

# names
colnames(globalIs) <- c("Variable", "I", "p")

str(globalIs)

knitr::kable(globalIs, format="simple", digits=10)
# only shows digist as far as 10, even for sci notation

# output to file
xtable(globalIs)

print(xtable(globalIs), 
      file="./tables/table_MoransI.tex", 
      floating=FALSE,
      type="latex", digits=4,
      include.rownames=FALSE,
      display=c("s","f","f"))


##########################################################
#
# Local Indicators of Spatial Association (LISAs)
# Local Moran, Local G, and Local C
# note: for Local Moran, cover two estimation strategies: permutation and saddlepoint
# other estimation strategies available, but permutation is more intuitive and 
# and saddlepoint is more conservative
# Tiefelsdorf (2002, 204) calls saddlepoint "approximation method of first choice"
# Bivand and Wong (2018) also favor saddlepoint, and note that permutation 
# approach is less conservative (i.e., more likely to generate large Z-values, or
# false positive, i.e., "false discovery" of local clusters)
##########################################################

# y = solar panels per district ("Count_)

nb <- nb.q1
w <- nb2listw(nb)

#########################
# permutation approach
lisa_perm <- localmoran_perm(shp.sf$Count_, listw=w, nsim=5000)
summary(lisa_perm)

# create LISA cluster identifiers
DV <- shp$Count_
quadrant <- vector(mode="numeric",length=nrow(lisa_perm))
cDV <- DV - mean(DV) 
lagDV <- lag.listw(w, DV)
clagDV <- lagDV - mean(lagDV)

# LISA significance with permutation method
p <- lisa_perm[,5]
quadrant <- vector(mode="numeric",length=nrow(lisa_perm))
quadrant[cDV >0 & clagDV>0 & p<=.05] <- 1 
quadrant[cDV <0 & clagDV<0 & p<=.05] <- 2      
quadrant[cDV <0 & clagDV>0 & p<=.05] <- 3
quadrant[cDV >0 & clagDV<0 & p<=.05] <- 4
# non-significant obs will remain coded as zeroes (0)
table(quadrant)

# merge LISA of DV data into shapefile
# merge a single variable from table
shp$lisa_perm.dv <- lisa_perm[,1]
shp$lisa_perm.p.dv <- p
shp$lisa_perm.cl.dv <- as.factor(quadrant)
#names(shp)

shp.sf$lisa_perm.dv <- lisa_perm[,1]
shp.sf$lisa_perm.p.dv <- p
shp.sf$lisa_perm.cl.dv <- as.factor(quadrant)

#########################################################
#
# color version of moran scatterplot
#
#########################################################

df = data.frame(y = cDV, Wy = clagDV, cl=quadrant, sig=p, 
                meany=mean(cDV), meanWy=mean(clagDV))

g <- ggplot(df, aes(x = y, y = Wy))+
  geom_point(colour="black", pch=21, size=3,
             aes(fill = factor(cl))) +
  scale_fill_manual(name = "Cluster",
                    values = c("0" = "white",
                               "1" = "red",
                               "2" = "blue",
                               "3" = "lightblue",
                               "4" = "pink"),
                    labels = c("n.s.", "high-high", "low-low", "low-high", "high-low")) +
  labs(x="Solar panels (centered on mean)", 
       y="spatial lag of solar panels (centered on mean)", 
       title="LISA Clusters, Solar Panels (permutation)") +
  geom_smooth(method = "lm", se=FALSE, colour="black", linewidth=.7) + 
  geom_vline(xintercept=df$meany,colour="black",linetype="longdash") + 
  geom_hline(yintercept=df$meanWy,colour="black",linetype="longdash")+ 
  theme(axis.line=element_line(color="black"),
        axis.title.x=element_text(size=10,vjust=0.1),
        axis.title.y=element_text(size=10,vjust=0.1),
        axis.text= element_text(colour="black", size=10, angle=0,face = "plain"),
        #plot.title = element_text(size = 10, lineheight=.8, face="bold", vjust=1), # make title bold and add space
        legend.title = element_text(size = 8), # legend title size
        legend.text = element_text(size = 8), # legend label size
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), # removes background grid
        panel.background = element_blank(), # removes grey background; could add black axis lines with #axis.line = element_line(colour = "black")) 
        panel.spacing=unit(c(0,0,0,0), "lines"),
        plot.margin=unit(c(0,0,0,0), "mm"))  # sets margin around full plot at top, right, bottom, and left; units can also be "lines" or "cm"
g

png(file="./figures/moranplot_yavg_perm_col_cen.png", height=6, width=6, units="in", res=300) # could also do pdf, jpeg, bmp, tiff
print(g)
dev.off()

#########################
# LISA MAP
#########################

g <- ggplot(data=shp.sf) +
  geom_sf(aes(fill=lisa_perm.cl.dv)) +
  scale_fill_manual(name="cluster", 
                    values=c("white", "red", "blue","lightblue","pink"), 
                    breaks = c("0", "1", "2", "3", "4"), 
                    labels=c("n.s.", "high-high", "low-low","low-high","hgh-low"), guide="legend") + 
  labs(x="", y="", title="Local Moran Clusters, Solar Panels (permutation)") +
  geom_sf(data=lakes.sf[4,], fill="black") +
  #coord_sf(crs=4269) +     # applies to all layers
  #xlim(-3.5, 1.5) +
  #ylim(4.5, 11.5) +
  theme_minimal()
g

png(file="./figures/lisamap_yavg_perm_color.png", height=6, width=6, units="in", res=300) # could also do pdf, jpeg, bmp, tiff
print(g)
dev.off()


##################################
# saddlepoint approach
##################################
lisa_s <- as.data.frame(
  summary(localmoran.sad(lm(Count_ ~ 1, shp), 
                         nb=nb, style="C"))) # this works; 
# need summary for saddlepoint syntax

# create LISA cluster identifiers
DV <- shp$Count_
quadrant <- vector(mode="numeric",length=nrow(lisa_s))
cDV <- DV - mean(DV) 
lagDV <- lag.listw(w, DV)
clagDV <- lagDV - mean(lagDV)

# LISA significance with saddlepoint method
p <- lisa_s[,5]
quadrant <- vector(mode="numeric",length=nrow(lisa_s))
quadrant[cDV >0 & clagDV>0 & p<=.05] <- 1 
quadrant[cDV <0 & clagDV<0 & p<=.05] <- 2      
quadrant[cDV <0 & clagDV>0 & p<=.05] <- 3
quadrant[cDV >0 & clagDV<0 & p<=.05] <- 4
# non-significant obs will remain coded as zeroes (0)
table(quadrant)

# merge LISA of DV data into shapefile
# merge a single variable from table
shp$lisa_s.dv <- lisa_s[,4]
shp$lisa_s.p.dv <- p
shp$lisa_s.cl.dv <- as.factor(quadrant)
#names(shp)

shp.sf$lisa_s.dv <- lisa_s[,4]
shp.sf$lisa_s.p.dv <- p
shp.sf$lisa_s.cl.dv <- as.factor(quadrant)


#########################################################
#
# color version of moran scatterplot
#
#########################################################

df = data.frame(y = cDV, Wy = clagDV, cl=quadrant, sig=p, meany=mean(cDV), meanWy=mean(clagDV))

g <- ggplot(df, aes(x = y, y = Wy))+
  geom_point(colour="black", pch=21, size=3,
             aes(fill = factor(cl))) +
  scale_fill_manual(name = "Cluster",
                    values = c("0" = "white",
                               "1" = "red",
                               "2" = "blue",
                               "3" = "lightblue",
                               "4" = "pink"),
                    labels = c("n.s.", "high-high", "low-low", "low-high", "high-low")) +
  #labs(x="", y="", title="LISA Clusters, HR (all)") +
  geom_smooth(method = "lm", se=F, colour="black", linewidth=.7) + 
  geom_vline(xintercept=df$meany,colour="black",linetype="longdash") + 
  geom_hline(yintercept=df$meanWy,colour="black",linetype="longdash")+ 
  xlab("Solar panels (centered on mean)") + ylab("spatial lag of solar panels (centered on mean)")+
  theme(axis.line=element_line(color="black"),
        axis.title.x=element_text(size=10,vjust=0.1),
        axis.title.y=element_text(size=10,vjust=0.1),
        axis.text= element_text(colour="black", size=10, angle=0,face = "plain"),
        #plot.title = element_text(size = 10, lineheight=.8, face="bold", vjust=1), # make title bold and add space
        legend.title = element_text(size = 8), # legend title size
        legend.text = element_text(size = 8), # legend label size
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), # removes background grid
        panel.background = element_blank(), # removes grey background; could add black axis lines with #axis.line = element_line(colour = "black")) 
        panel.spacing=unit(c(0,0,0,0), "lines"),
        plot.margin=unit(c(0,0,0,0), "mm"))  # sets margin around full plot at top, right, bottom, and left; units can also be "lines" or "cm"
g

png(file="./figures/moranplot_yavg_sad_col_cen.png", height=6, width=6, units="in", res=300) # could also do pdf, jpeg, bmp, tiff
print(g)
dev.off()

#########################
# LISA MAP
#########################

g <- ggplot(data=shp.sf) +
  geom_sf(aes(fill=lisa_s.cl.dv)) +
  scale_fill_manual(name="cluster", 
                    values=c("white", "red", "blue","lightblue","pink"), 
                    breaks = c("0", "1", "2", "3", "4"), 
                    labels=c("n.s.", "high-high", "low-low","low-high","hgh-low"), guide="legend") + 
  labs(x="", y="", title="Local Moran Clusters, Solar Panels (saddlepoint)") +
  geom_sf(data=lakes.sf[4,], fill="black") +
  #coord_sf(crs=4269) +     # applies to all layers
  #xlim(-3.5, 1.5) +
  #ylim(4.5, 11.5) +
  theme_minimal()
g

png(file="./figures/lisamap_yavg_sad_color.png", height=6, width=6, units="in", res=300) # could also do pdf, jpeg, bmp, tiff
print(g)
dev.off()



##############################################
# could use same process, but now for key X, p_share

# LISAs for key X, p_share

##############################
# skip permutation approach; can do on your own based on template above


##############################
# saddlepoint approach

lisa_s <- as.data.frame(
  summary(localmoran.sad(lm(p_share ~ 1, shp), 
                         nb=nb, style="C"))) # this works; 
# need summary for saddlepoint syntax

# create LISA cluster identifiers
w <- nb2listw(nb)
DV <- shp$p_share
quadrant <- vector(mode="numeric",length=nrow(lisa_s))
cDV <- DV - mean(DV) 
lagDV <- lag.listw(w, DV)
clagDV <- lagDV - mean(lagDV)

# LISA significance with saddlepoint method
p <- lisa_s[,5]
quadrant <- vector(mode="numeric",length=nrow(lisa_s))
quadrant[cDV >0 & clagDV>0 & p<=.05] <- 1 
quadrant[cDV <0 & clagDV<0 & p<=.05] <- 2      
quadrant[cDV <0 & clagDV>0 & p<=.05] <- 3
quadrant[cDV >0 & clagDV<0 & p<=.05] <- 4

table(quadrant)

# merge LISA of X data into shapefile
# merge a single variable from table
shp$lisa_s.x1 <- lisa_s[,4]
shp$lisa_s.p.x1 <- p
shp$lisa_s.cl.x1 <- as.factor(quadrant)
#names(shp)

shp.sf$lisa_s.x1 <- lisa_s[,4]
shp.sf$lisa_s.p.x1 <- p
shp.sf$lisa_s.cl.x1 <- as.factor(quadrant)



# moran scatterplot of p_share

df = data.frame(y = cDV, Wy = clagDV, cl=quadrant, sig=p, meany=mean(cDV), meanWy=mean(clagDV))

g <- ggplot(df, aes(x = y, y = Wy))+
  geom_point(colour="black", pch=21, size=3,
             aes(fill = factor(cl))) +
  scale_fill_manual(name = "Cluster",
                    values = c("0" = "white",
                               "1" = "red",
                               "2" = "blue",
                               "3" = "lightblue",
                               "4" = "pink"),
                    labels = c("n.s.", "high-high", "low-low", "low-high", "high-low")) +
  #labs(x="", y="", title="LISA Clusters, HR (all)") +
  geom_smooth(method = "lm", se=F, colour="black", linewidth=.7) + 
  geom_vline(xintercept=df$meany,colour="black",linetype="longdash") + 
  geom_hline(yintercept=df$meanWy,colour="black",linetype="longdash")+ 
  xlab("Vote share (centered on mean)") + ylab("spatial lag of vote share (centered on mean)")+
  theme(axis.line=element_line(color="black"),
        axis.title.x=element_text(size=10,vjust=0.1),
        axis.title.y=element_text(size=10,vjust=0.1),
        axis.text= element_text(colour="black", size=10, angle=0,face = "plain"),
        #plot.title = element_text(size = 10, lineheight=.8, face="bold", vjust=1), # make title bold and add space
        legend.title = element_text(size = 8), # legend title size
        legend.text = element_text(size = 8), # legend label size
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), # removes background grid
        panel.background = element_blank(), # removes grey background; could add black axis lines with #axis.line = element_line(colour = "black")) 
        panel.spacing=unit(c(0,0,0,0), "lines"),
        plot.margin=unit(c(0,0,0,0), "mm"))  # sets margin around full plot at top, right, bottom, and left; units can also be "lines" or "cm"
g

png(file="./figures/moranplot_pshare_col_cen.png", height=6, width=6, units="in", res=300) # could also do pdf, jpeg, bmp, tiff
print(g)
dev.off()


#########################
# LISA MAP
#########################

g <- ggplot(data=shp.sf) +
  geom_sf(aes(fill=lisa_s.cl.x1)) +
  scale_fill_manual(name="LISA cluster", 
                    values=c("white", "red", "blue","lightblue","pink"), 
                    breaks = c("0", "1", "2", "3", "4"), 
                    labels=c("n.s.", "high-high", "low-low","low-high","hgh-low"), guide="legend") + 
  labs(x="", y="", title="LISA Clusters, Vote Share") +
  geom_sf(data=lakes.sf[4,], fill="black") +
  #coord_sf(crs=4269) +     # applies to all layers
  #xlim(-3.5, 1.5) +
  #ylim(4.5, 11.5) +
  theme_minimal()
g

png(file="./figures/lisamap_pshare_color.png", height=6, width=9, units="in", res=300) # could also do pdf, jpeg, bmp, tiff
print(g)
dev.off()


##########################################
#
# Local G (Getis-Ord statistic) and Local Geary or Geary's C
# 
##########################################

# localG (getis-ord); returns Z-value that identifies similarity clusters 
# high G (positive) = high-value similarity cluster
# low G (negative) = low-value similarity cluster
# note: does not capture dissimilarity clusters or spatial "outliers"
# note2: two types: G and G*; G excludes focal unit in calculating 
# neighborhood average; G* includes focal unit
localg <- localG(shp$Count_, listw=wq1b)
head(localg)
# localGS() for G*

# using rgeoda
#x <- mlf:: get_var(shp.sf$Count_)   # get squared variance
tempW <- rgeoda::queen_weights(shp.sf)
alpha=0.05
localg_2 <- rgeoda::local_g(tempW, 
                                data.frame(shp.sf$Count_), 
                                permutations = 5000)
localg_2

# if want local G*
localgstar <- rgeoda::local_gstar(tempW, 
                            data.frame(shp.sf$Count_), 
                            permutations = 5000)



clusters <- rgeoda::lisa_clusters(localg_2, cutoff = alpha)
labels <- rgeoda::lisa_labels(localg_2)
pvalue <- rgeoda::lisa_pvalues(localg_2)
colors <- rgeoda::lisa_colors(localg_2)
lisa_patterns <- labels[clusters + 1]
table(clusters)
#pal <- match_palette(lisa_patterns, labels, colors)
labels <- labels[labels %in% lisa_patterns]
shp.sf["localg_2_clusters"] <- clusters
#plot
tm_shape(shp.sf) + tm_fill("localg_2_clusters", labels = labels, 
                           #palette = pal,
                           palette = colors,
                           style = "cat",
                           lwd=0,
                           border.alpha=0) +
  tm_borders(col=NA, lwd=1) +
  tm_layout("Local G Cluster Map", legend.outside = TRUE)
# creates map with border and formatted title/labels/legend

# ggplot
table(clusters)
g <- ggplot(data=shp.sf) +
  geom_sf(aes(fill=factor(localg_2_clusters))) +
  scale_fill_manual(name="cluster", 
                    values=c("white", "red", "blue", "grey"), 
                    breaks = c("0", "1", "2", "4"), 
                    labels=c("n.s.", "high-high", "low-low", "isolated"), guide="legend") + 
  labs(x="", y="", title="Local G, Solar Panels (permutation)") +
  geom_sf(data=lakes.sf[4,], fill="black") +
  #coord_sf(crs=4269) +     # applies to all layers
  #xlim(-3.5, 1.5) +
  #ylim(4.5, 11.5) +
  theme_minimal()
g

png(file="./figures/localgmap_y_color.png", height=6, width=6, units="in", res=300) # could also do pdf, jpeg, bmp, tiff
print(g)
dev.off()

#######################
# if want local G*
localgstar <- rgeoda::local_gstar(tempW, 
                                  data.frame(shp.sf$Count_), 
                                  permutations = 5000)



clusters <- rgeoda::lisa_clusters(localgstar, cutoff = alpha)
labels <- rgeoda::lisa_labels(localgstar)
pvalue <- rgeoda::lisa_pvalues(localgstar)
colors <- rgeoda::lisa_colors(localgstar)
lisa_patterns <- labels[clusters + 1]
table(clusters)
#pal <- match_palette(lisa_patterns, labels, colors)
labels <- labels[labels %in% lisa_patterns]
shp.sf["localgstar_clusters"] <- clusters
#plot
tm_shape(shp.sf) + tm_fill("localgstar_clusters", labels = labels, 
                           #palette = pal,
                           palette = colors,
                           style = "cat",
                           lwd=0,
                           border.alpha=0) +
  tm_borders(col=NA, lwd=1) +
  tm_layout("Local G* Cluster Map", legend.outside = TRUE)
# creates map with border and formatted title/labels/legend

# ggplot
table(clusters)
g <- ggplot(data=shp.sf) +
  geom_sf(aes(fill=factor(localgstar_clusters))) +
  scale_fill_manual(name="cluster", 
                    values=c("white", "red", "blue", "grey"), 
                    breaks = c("0", "1", "2", "4"), 
                    labels=c("n.s.", "high-high", "low-low", "isolated"), guide="legend") + 
  labs(x="", y="", title="Local G*, Solar Panels (permutation)") +
  geom_sf(data=lakes.sf[4,], fill="black") +
  #coord_sf(crs=4269) +     # applies to all layers
  #xlim(-3.5, 1.5) +
  #ylim(4.5, 11.5) +
  theme_minimal()
g

png(file="./figures/localgstarmap_y_color.png", height=6, width=6, units="in", res=300) # could also do pdf, jpeg, bmp, tiff
print(g)
dev.off()

###############################
# localC (Geary's C); 
# can capture similarity and dissimilarity
# based on squared differences, so large values (larged, squared differences)
# capture dissimilarity
# mean=1 if spatially random; values < 1 are similar-values clusters
# small values = similarity clusters (low and high, based on matching observations 
# on a moran scatter plot)
# however, squared differences may also match observations close to mean, so
# these are "other positive" associations
# large values = dissimilarity clusters
localc <- localC(shp$Count_, listw=wq1b)
head(localc)

# can examine output if needed, but visualization useful (map)


# using rgeoda
#x <- mlf:: get_var(shp.sf$Count_)   # get squared variance
tempW <- rgeoda::queen_weights(shp.sf)
alpha=0.05
localc_2 <- rgeoda::local_geary(tempW, 
                                data.frame(shp.sf$Count_), 
                                permutations = 5000)
localc_2

clusters <- rgeoda::lisa_clusters(localc_2, cutoff = alpha)
labels <- rgeoda::lisa_labels(localc_2)
pvalue <- rgeoda::lisa_pvalues(localc_2)
colors <- rgeoda::lisa_colors(localc_2)
lisa_patterns <- labels[clusters + 1]
table(clusters)
#pal <- match_palette(lisa_patterns, labels, colors)
labels <- labels[labels %in% lisa_patterns]
shp.sf["localc_2_clusters"] <- clusters
#plot
tm_shape(shp.sf) + tm_fill("localc_2_clusters", labels = labels, 
                          #palette = pal,
                          palette = colors,
                          style = "cat",
                          lwd=0,
                          border.alpha=0) +
  tm_borders(col=NA, lwd=1) +
  tm_layout("Local Geary C Cluster Map", legend.outside = TRUE)

# ggplot
g <- ggplot(data=shp.sf) +
  geom_sf(aes(fill=factor(localc_2_clusters))) +
  scale_fill_manual(name="cluster", 
                    values=c("white", "red", "blue","yellow","lightblue", "grey"), 
                    breaks = c("0", "1", "2", "3", "4", "6"), 
                    labels=c("n.s.", "high-high", "low-low","other positive",
                             "negative", "isolated"), guide="legend") + 
  labs(x="", y="", title="Local Geary C, Solar Panels (permutation)") +
  geom_sf(data=lakes.sf[4,], fill="black") +
  #coord_sf(crs=4269) +     # applies to all layers
  #xlim(-3.5, 1.5) +
  #ylim(4.5, 11.5) +
  theme_minimal()
g

png(file="./figures/localcmap_y_color.png", height=6, width=6, units="in", res=300) # could also do pdf, jpeg, bmp, tiff
print(g)
dev.off()


# overall, prefer Local Moran


####################################
# save working data

save.image("./data/working/working20230615.RData")


#end
