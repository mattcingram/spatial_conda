################################################################################
#
# Ingram, Matt 
# Reproduction of Brass et al. (2020) in Political Geography
# created: 2023-04-23
# last updated: 2023-06-26
# steps here: explanatory analysis; 
#             spatial models (SLM, SEM, SLX, SAC, SDM, 
#               and GWR)
#
################################################################################

##########################################################
# if returning to project, load last working data file:
load("./data/working/working20230626_explanatory.RData")

###############################################################
# EXPLANATORY ANALYSIS
###############################################################

#######################################
# BASELINE OLS MODEL (not reported in article)

# reminder that reversed % turnout (p_turn) to get % nonvoters
shp$turninv<- (1-shp$p_turn)

# note: in original model in Table 2, % nonvoters is not in model 1, 
# does not appear separately in model 2, and is not in model 3
# however, it is in replication materials shared by authors
# we include it here

# base ols
ols <- lm(Count_~ pov_p_2008 + gini_2008
                   + ferat_2008 + p_share
                   + p_shvol + turninv       
                   + p_tuvol + p_ethfr
                   + Count_3 + Density_RD
                   + Pop_Densit + Count_4
                   + literacy + grid_perCa,
                   data=shp@data)
summary(ols)

# heteroskedasticity
bptest(ols)

# graph test
plot(ols$fitted.values, ols$residuals)
# Lieberman-style graph of yhat vs y
plot(ols$fitted.values, ols$model$Count_)
abline(ols$fitted.values, ols$model$Count_)



#######################################
# BASELINE OLS MODEL WITH CLUSTERED SEs (Model 1 in article)

#Model 1
# using miceadds::lm.cluster(..., cluster = "REGION")

ols1a <- lm.cluster(Count_~ pov_p_2008 + gini_2008
                   + ferat_2008 + p_share
                   + p_shvol + turninv
                   + p_tuvol + p_ethfr
                   + Count_3 + Density_RD
                   + Pop_Densit + Count_4
                   + literacy + grid_perCa,
                   data=shp@data, cluster = "REGION")

# can also use estimatr::lm_robust() (with option ... clusters = REGION)

ols1b <- lm_robust(Count_~ pov_p_2008 + gini_2008
                   + ferat_2008 + p_share
                   + p_shvol + turninv
                   + p_tuvol + p_ethfr
                   + Count_3 + Density_RD
                   + Pop_Densit + Count_4
                   + literacy + grid_perCa,
                   data=shp@data,   # need to specify data object with lm_robust
                   clusters = REGION,
                   se_type = "CR2"   # options are CR0, CR2 (default), or stata
                   )

summary(ols1a)   
summary(ols1b)


# dot plot of coefficients
dwplot(list(ols1a$lm_res, 
            ols1b), ci=.95)

#######################################
# BASELINE OLS WITH CLUSTERED SEs and INTERACTION (Model 2 in article)

#Model 2
# with miceadds::lm.cluster()
ols2a <- lm.cluster(Count_~ pov_p_2008 + gini_2008
                   + ferat_2008 + p_share:turninv
                   + p_share + turninv
                   + p_shvol 
                   + p_tuvol + p_ethfr
                   + Count_3 + Density_RD
                   + Pop_Densit + Count_4
                   + literacy + grid_perCa,
                   data=shp, 
                   cluster = "REGION"
           )

summary(ols2a)

# with estimatr::lm_robust()
ols2b <- lm_robust(Count_~ pov_p_2008 + gini_2008
                    + ferat_2008 + p_share:turninv
                    + p_share + turninv
                    + p_shvol 
                    + p_tuvol + p_ethfr
                    + Count_3 + Density_RD
                    + Pop_Densit + Count_4
                    + literacy + grid_perCa,
                    data=shp@data,    # need to specify data with lm_robust
                    clusters = REGION  # with default se_type = "CR2"
                )

summary(ols2b)

# both of these models generate exactly same results as in article



#######################################
# SPATIAL MODELING
#######################################

# 3 main approaches:
### (1) run diagnostics, then select model
### (2) run most complex model, then evaluate and select model
### (3) build model based on theory

# in practice: 
# if have good theory: let theory guide model specification, 
# check with diagnostics,
# and, in any case, run more than one model to 
# check stability/robustness of results;
# if don't have good theory or working in new area,
# could follow options 1 or 2 in more
# exploratory approach

# here, assume have well-developed theory, and focus on diagnostics (option 1 above)

###########################
# DIAGNOSTICS

# classic test: Lagrange Multiplier test (LM test)
lmtests <- lm.LMtests(ols, listw=wq1, 
                      test=c("LMerr","RLMerr","LMlag","RLMlag","SARMA"))
summary(lmtests)

# output: note that does not quite match original appendix (table A2, p4)
# no difference if use ols1a$lm_res
#        statistic parameter  p.value   
#LMerr     5.3531         1 0.020686 * 
#RLMerr    1.4432         1 0.229622   
#LMlag     9.0335         1 0.002651 **
#RLMlag    5.1236         1 0.023602 * 
#SARMA    10.4767         2 0.005309 **
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# comments:
# LMerr and LMlag both sig, so move to robust tests;
# only LMlag sig; also, mixed SARMA test sig
# this matches discussion in appendix, pp 3-4
# thus: SLM is most appropriate, but could still model SEM or SAC/SARMA to check



# SLM: Model 3 in article

slm <- lagsarlm(Count_~ pov_p_2008 + gini_2008      # formula
                           + ferat_2008 + p_share
                           + p_shvol + turninv
                           + p_tuvol + p_ethfr
                           + Count_3 + Density_RD
                           + Pop_Densit + Count_4
                           + literacy + grid_perCa,
                data=shp,      # data
                listw=wq1)   # W
summary(slm)


# SEM 
sem <- errorsarlm(Count_~ pov_p_2008 + gini_2008
                + ferat_2008 + p_share
                + p_shvol + turninv
                + p_tuvol + p_ethfr
                + Count_3 + Density_RD
                + Pop_Densit + Count_4
                + literacy + grid_perCa,
                data=shp, 
                listw=wq1)
summary(sem)

# SAC/SARMA
sac <- sacsarlm(Count_~ pov_p_2008 + gini_2008
                + ferat_2008 + p_share
                + p_shvol + turninv
                + p_tuvol + p_ethfr
                + Count_3 + Density_RD
                + Pop_Densit + Count_4
                + literacy + grid_perCa,
                data=shp, 
                listw=wq1,    # W for Wy process (SLM) 
                listw2=wr1    # W for We process (SEM)
                )
summary(sac)
# note rho sig, lambda not sig

# SDM
sdm <- lagsarlm(Count_~ pov_p_2008 + gini_2008
                + ferat_2008 + p_share
                + p_shvol + turninv
                + p_tuvol + p_ethfr
                + Count_3 + Density_RD
                + Pop_Densit + Count_4
                + literacy + grid_perCa,
                data=shp, 
                listw=wq1,
                Durbin = TRUE)
summary(sdm)

# SLX
slx <- lmSLX(Count_~ pov_p_2008 + gini_2008
                + ferat_2008 + p_share
                + p_shvol + turninv
                + p_tuvol + p_ethfr
                + Count_3 + Density_RD
                + Pop_Densit + Count_4
                + literacy + grid_perCa,
             data=shp, 
             listw=wq1,
             Durbin = TRUE)
summary(slx)

# aside from LM tests, could now do post-estimation comparison
moran.test(slm$residuals, listw=wq1)
moran.test(sem$residuals, listw=wq1)
moran.test(sac$residuals, listw=wq1)
moran.test(sdm$residuals, listw=wq1)
moran.test(slx$residuals, listw=wq1)

AIC(ols)  # base ols
AIC(ols1a$lm_res) # ols with clustered SEs (model 1)
AIC(ols2a$lm_res) # ols with clustered SEs and interaction (model 2)
AIC(slm)
AIC(sem)
AIC(sac)
AIC(sdm)
AIC(slx)

# overall, SLM has lowest AIC, so looks reasonable

###############################################
# INTERPRETATION

# for SLM, SAC, SDM, and SLX, need to estimate impacts because of spatial multiplier
# derived from Wy process
# B = ()
# for SEM, can interpret betas as OLS coefficients

# note: original article found turnout volatility, female ratio, and road density
# significant

impacts.slm<- impacts(slm, 
                      listw=wq1,
                      R=1000,zstats=TRUE
                      #,useHESS = T  # Hessian matrix not available
)
summary(impacts.slm, zstats=TRUE)
# here, we find same general results (female ratio, turnout volatility, and road
# density are sig., )
# values are not the same, but relative magnitudes are similar

summary(sem)

impacts.sac<- impacts(sac, 
                      listw=wq1,
                      R=1000,zstats=TRUE
                      #,useHESS = T  # Hessian matrix not available
)
summary(impacts.sac, zstats=TRUE)

impacts.sdm<- impacts(sdm, 
                      listw=wq1,
                      R=1000,zstats=TRUE
                      #,useHESS = T  # Hessian matrix not available
)
summary(impacts.sdm, zstats=TRUE)

impacts.slx<- impacts(slx, 
                      listw=wq1,
                      R=1000,zstats=TRUE
                      #,useHESS = T  # Hessian matrix not available
)
summary(impacts.slx, zstats=TRUE)

# SUMMING UP:
# slm consistent with article
# other models show stability of these core results, with some variation
# SAC: pop density also sig
# SDM and SLX" turnout volatility no longer sig, even at .10 level, 
# and literacy now significant at either .05 (SDM) or .10 (SLX) level

###############################################
# GWR (Geographically Weighted Regression)
###############################################

# GWR of model 3 above

# create distance matrix
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

# find optimal bandwidth
bw.bsq <- bw.gwr(model,
                 data=shp, 
                 approach="CV",
                 kernel="bisquare",
                 adaptive=TRUE,
                 dMat=dMat)

# check for heterogeneity
# randomization test to identify which covariates have sig non-stationarity
tic("mc")
mc1 <- montecarlo.gwr(model, 
                      data=shp, 
                      nsims = 999,
                      bw = bw.bsq,
                      kernel="bisquare",
                      adaptive=TRUE, 
                      dMat = dMat)
toc()
# 36 sec

mc1
# shows female ratio and grid_percap are nonstationary/heterogeneous

# check with different bandwidth
bw.g <- bw.gwr(model,
               data=shp, 
               approach="CV",
               kernel="gaussian",
               adaptive=TRUE,
               dMat=dMat)

tic("mc")
mc2 <- montecarlo.gwr(model, 
                      data=shp, 
                      nsims = 999,
                      bw = bw.g,
                      kernel="bisquare",
                      adaptive=TRUE, 
                      dMat = dMat)
toc()
# also about 36 sec

mc2
# again shows fem ratio and grid_perca significant

# note that original authors focused on road density and turnout volatility
# because these were the significant predictors in SLM (article, p9; appendix, p11)

###############################################
# run gwr models with two different bandwidths
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

# GWR results
# can view as table
gwr1
gwr2

# can also compare fit to other spatial models
gwr1$GW.diagnostic$gwR2.adj
gwr2$GW.diagnostic$gwR2.adj

gwr1$GW.diagnostic$AIC
gwr2$GW.diagnostic$AIC



# better to graph/visualize results
# see below

###############################################
# Generate local estimates at 95% confidence
###############################################

# note: authors only focus on road density and turnout volatility
# we do a few more, including main hypotheses and ones 
# identified in montecarlo test

#############################################

# local estimates from gwr1
gwr1$SDF$p_share95 <- gwr1$SDF$p_share
gwr1$SDF$p_share95[abs(gwr1$SDF$p_share_TV)<=1.96] <- 0

gwr1$SDF$p_shvol95 <- gwr1$SDF$p_shvol
gwr1$SDF$p_shvol95[abs(gwr1$SDF$p_shvol_TV)<=1.96] <- 0

gwr1$SDF$p_tuvol95 <- gwr1$SDF$p_tuvol
gwr1$SDF$p_tuvol95[abs(gwr1$SDF$p_tuvol_TV)<=1.96] <- 0

gwr1$SDF$ferat95 <- gwr1$SDF$ferat_2008
gwr1$SDF$ferat95[abs(gwr1$SDF$ferat_2008_TV)<=1.96] <- 0

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

gwr2$SDF$ferat95 <- gwr2$SDF$ferat_2008
gwr2$SDF$ferat95[abs(gwr2$SDF$ferat_2008_TV)<=1.96] <- 0

gwr2$SDF$Density_RD95 <- gwr2$SDF$Density_RD
gwr2$SDF$Density_RD95[abs(gwr2$SDF$Density_RD_TV)<=1.96] <- 0

gwr2$SDF$grid_perCa95 <- gwr2$SDF$grid_perCa
gwr2$SDF$grid_perCa95[abs(gwr2$SDF$grid_perCa_TV)<=1.96] <- 0

#####################################
# add local estimates to shapefile
#####################################

# add estimates from new models

# gwr1
shp$p_share.gwr1 <- gwr1$SDF$p_share95
shp$p_shvol.gwr1 <- gwr1$SDF$p_shvol95
shp$p_tuvol.gwr1 <- gwr1$SDF$p_tuvol95
shp$ferat.gwr1 <- gwr1$SDF$ferat95
shp$road.gwr1 <- gwr1$SDF$Density_RD95
shp$gpc.gwr1 <- gwr1$SDF$grid_perCa95

# gwr2
shp$p_share.gwr2 <- gwr2$SDF$p_share95
shp$p_shvol.gwr2 <- gwr2$SDF$p_shvol95
shp$p_tuvol.gwr2 <- gwr2$SDF$p_tuvol95
shp$ferat.gwr2 <- gwr2$SDF$ferat95
shp$road.gwr2 <- gwr2$SDF$Density_RD95
shp$gpc.gwr2 <- gwr2$SDF$grid_perCa95

# check collinearity of gwr coefficients
col1 <- gwr.collin.diagno(model, data=shp, adaptive = TRUE, 
                          kernel="bisquare", bw=bw.bsq, dMat=dMat)
summary(col1$VIF)
#highest is only around 6 (Count_4, health facilities)


#########################################################
# save data
save.image("./data/working/working20230626_models.RData")
#########################################################


########################################################
#
# MAPS OF GWR COEFFICIENTS
#
# authors focused on tunout volatility and road density because those were sig in SLM
# here, we report all covariates related to core hypotheses (vote share, vote share volatility,
# turnout volatility, elec. grid per capita), plus fem ratio and road density because they 
# were either significant in SLM or montecarlo test showed non-stationarity
#
########################################################

# convert to sf object for graphing
shp.sf <- st_as_sf(shp)

# GWR1:

# vote share volatility

g <- ggplot(data=shp.sf) + 
  geom_sf(aes(fill=p_shvol.gwr1)) + 
  scale_fill_gradient2(name="\u03B2", 
                       #values=rescale(c(0, .05, .2)), 
                       low="blue",
                       mid="white",
                       high="red",
                       midpoint=0,
                       guide="colourbar") + 
  labs(x="", y="", title="Vote Share volatility, local \u03B2 (GWR 1, bisquare)") +
  theme_minimal()
g

png(file="./figures/gwr1_voteshare_volatility.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()

# turnout volatility

g <- ggplot(data=shp.sf) + 
  geom_sf(aes(fill=p_tuvol.gwr1)) + 
  scale_fill_gradient2(name="\u03B2", 
                       #values=rescale(c(0, .05, .2)), 
                       low="blue",
                       mid="white",
                       high="red",
                       midpoint=0,
                       guide="colourbar") + 
  labs(x="", y="", title="Turnout volatility, local \u03B2 (GWR 1, bisquare)") +
  theme_minimal()
g

png(file="./figures/gwr1_turnout_volatility.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()

# female ratio

g <- ggplot(data=shp.sf) + 
  geom_sf(aes(fill=ferat.gwr1)) + 
  scale_fill_gradient2(name="\u03B2", 
                       #values=rescale(c(0, .05, .2)), 
                       low="blue",
                       mid="white",
                       high="red",
                       midpoint=0,
                       guide="colourbar") + 
  labs(x="", y="", title="Female Ratio, local \u03B2 (GWR 1, bisquare)") +
  theme_minimal()
g

png(file="./figures/gwr1_femratio.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()


# road density

g <- ggplot(data=shp.sf) + 
  geom_sf(aes(fill=road.gwr1)) + 
  scale_fill_gradient2(name="\u03B2", 
                       #values=rescale(c(0, .05, .2)), 
                       low="blue",
                       mid="white",
                       high="red",
                       midpoint=0,
                       guide="colourbar") + 
  labs(x="", y="", title="Road density, local \u03B2 (GWR 1, bisquare)") +
  theme_minimal()
g


png(file="./figures/gwr1_road_density.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()

# grid density per capita

g <- ggplot(data=shp.sf) + 
  geom_sf(aes(fill=gpc.gwr1)) + 
  scale_fill_gradient2(name="\u03B2", 
                       #values=rescale(c(0, .05, .2)), 
                       low="blue",
                       mid="white",
                       high="red",
                       midpoint=0,
                       guide="colourbar") + 
  labs(x="", y="", title="Grid density pc, local \u03B2 (GWR 1, bisquare)") +
  theme_minimal()
g


png(file="./figures/gwr1_gpc.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()

#######################
# GWR2
#######################

# vote share volatility

g <- ggplot(data=shp.sf) + 
  geom_sf(aes(fill=p_shvol.gwr2)) + 
  scale_fill_gradient2(name="\u03B2", 
                       #values=rescale(c(0, .05, .2)), 
                       low="blue",
                       mid="white",
                       high="red",
                       midpoint=0,
                       guide="colourbar") + 
  labs(x="", y="", title="Vote Share volatility, local \u03B2 (GWR 2, bisquare)") +
  theme_minimal()
g

png(file="./figures/gwr2_voteshare_volatility.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()

# turnout volatility

g <- ggplot(data=shp.sf) + 
  geom_sf(aes(fill=p_tuvol.gwr2)) + 
  scale_fill_gradient2(name="\u03B2", 
                       #values=rescale(c(0, .05, .2)), 
                       low="blue",
                       mid="white",
                       high="red",
                       midpoint=0,
                       guide="colourbar") + 
  labs(x="", y="", title="Turnout volatility, local \u03B2 (GWR 2, bisquare)") +
  theme_minimal()
g

png(file="./figures/gwr2_turnout_volatility.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()

# female ratio

g <- ggplot(data=shp.sf) + 
  geom_sf(aes(fill=ferat.gwr2)) + 
  scale_fill_gradient2(name="\u03B2", 
                       #values=rescale(c(0, .05, .2)), 
                       low="blue",
                       mid="white",
                       high="red",
                       midpoint=0,
                       guide="colourbar") + 
  labs(x="", y="", title="Female Ratio, local \u03B2 (GWR 2, bisquare)") +
  theme_minimal()
g

png(file="./figures/gwr2_femratio.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()


# road density

g <- ggplot(data=shp.sf) + 
  geom_sf(aes(fill=road.gwr2)) + 
  scale_fill_gradient2(name="\u03B2", 
                       #values=rescale(c(0, .05, .2)), 
                       low="blue",
                       mid="white",
                       high="red",
                       midpoint=0,
                       guide="colourbar") + 
  labs(x="", y="", title="Road density, local \u03B2 (GWR 2, bisquare)") +
  theme_minimal()
g


png(file="./figures/gwr2_road_density.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()

# grid density per capita

g <- ggplot(data=shp.sf) + 
  geom_sf(aes(fill=gpc.gwr2)) + 
  scale_fill_gradient2(name="\u03B2", 
                       #values=rescale(c(0, .05, .2)), 
                       low="blue",
                       mid="white",
                       high="red",
                       midpoint=0,
                       guide="colourbar") + 
  labs(x="", y="", title="Grid density pc, local \u03B2 (GWR 2, bisquare)") +
  theme_minimal()
g


png(file="./figures/gwr2_gpc.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()


####################################
# save working data

save.image("./data/working/working20230626_models.RData")

#end
