# Script information -----------------------------------------------------------
# Title: pil.27.7
# Authors: Joseph Ribeiro, Rosana Ourens-Chans, Susan Kenyon
# Date created: 2022/05/26
# Purpose: This script runs the SPiCT model to assess stock status.
#          Results are not used for advice due to large uncertainty but the model
#          is run each year to check whether its diagnostics have improved.
# Working directory: root (applies to all the TAF .r files)

# Load libraries and working directory -----------------------------------------
library(rstudioapi) # This has been written as an R studio project using RStudio Version 1.4.1717 
library(icesTAF)
library(spict)
library(xlsx)

mkdir("model")

try(setwd(dirname(getActiveDocumentContext()$path)))
wdir = getwd(); print(wdir)

# Load data --------------------------------------------------------------------
base::load("boot/data/processed_data_various.RData")
thisyear = read.table(paste0(wdir,"/boot/data/thisyear.txt"))[[1]]
adviceyear = read.table(paste0(wdir,"/boot/data/adviceyear.txt"))[[1]]
datayear = read.table(paste0(wdir,"/boot/data/datayear.txt"))[[1]]
inpDir <- ("boot/initial/data")

# Get peltic data for biomass index
pil <- read.csv(file.path(inpDir, "Peltic_timeseries_replacement_datafile.csv"))

# Format model data ------------------------------------------------------------
pil = pil[pil$area=='core',c("year","Biomassindex")] #SPiCT model uses only the index from the core area
pil$Quarter = 4 # the survey was done in the 4th quarter
pil$Year = pil$year; pil$year <- NULL
pil$Peltic = pil$Biomassindex; pil$Biomassindex <- NULL

# Insert missing quarters - quick hacky approach...
missingqfor = pil$Year
missingq = c(rep(1,length(missingqfor)),rep(2,length(missingqfor)),rep(3,length(missingqfor)))
missingqfor = c(missingqfor,missingqfor,missingqfor)
addme = data.frame('Year'=missingqfor,'Peltic'=rep(NA,length(missingqfor)),'Quarter'=missingq)
pil = rbind(pil,addme)
pil<- pil[order(as.Date(paste(pil$Year,pil$Quarter,sep='-01-0'))),]

# Also in the 2021 run were blank years going back to 2002, and for some reason the model script breaks without these (possibly because it shortens the catch timeseries somewhere)
addmetoo = data.frame('Year'=c(rep(2002,4),rep(2003,4),rep(2004,4),rep(2005,4),rep(2006,4),rep(2007,4),rep(2008,4),rep(2009,4),rep(2010,4),rep(2011,4),rep(2012,4)),'Peltic'=rep(NA,44),'Quarter'=c(1:4,1:4,1:4,1:4,1:4,1:4,1:4,1:4,1:4,1:4,1:4))
pil = rbind(addmetoo,pil)

# Add in landings from the previous section
pil=merge(pil,LandQuarter2,all = T)

# Specify SPiCT model ----------------------------------------------------------
inp<-list()

inp$timeC <- seq(2013,thisyear+3/4,by=1/4)
inp$obsC <- pil$landings[pil$Year>2012]
inp$timeI <- seq(2013,thisyear+3/4,by=1/4) 
inp$obsI <- pil$Peltic[pil$Year>2012]

inp$nseasons=4
inp$splineorder=3
inp$seasontype<-1 # 1 for B-splines, 2 for stochastic differential equations
inp$dtc<-0.25
#inp$optimiser.control = list(iter.max = 1e5, eval.max = 1e5)
#inp$dteuler<-1/16
inp$priors$logbkfrac <- c(log(0.5),0.5,1) # How do we know what priors to choose? This one seems to be the prior defining the level of depletion
#inp$priors$logn <- c(log(1.478), 0.6, 1) # summary(fit) says this is logn  ~  dnorm[log(2), 2^2] when undefined
#inp$priors$logn <- c(log(1.478), 0.6, 1)
#inp$optimiser.control= list(iter.max = 1e10, eval.max = 1e10)

# If evidence or expert knowledge allows to infer that there was low or no exploitation before the
# beginning of the available data: initial depletion level could be assumed to be close to the carrying
# capacity (e.g. inp$priors$logbkfrac <- c(log(0.8),0.5,1))
# - If evidence or expert knowledge allows to infer that there was high exploitation before the
# beginning of the available data: initial depletion level could be assumed to be a certain fraction of
# the carrying capacity (e.g. inp$priors$logbkfrac <- c(log(0.2),0.5,1))

plot(1:1); dev.new() # Make a window 

inp <- check.inp(inp)
png(filename="output/spict_data_in.png")
plotspict.data(inp)
dev.off()

# fit model
fit <- fit.spict(inp)
png(filename="output/spict_fit.png")
plot(fit)
dev.off()

# model summary
summary(fit)

# Save fitted model to R object
save(list=c("fit","inp"), file='model/fittedmodel.rdata')

# Model diagnostics ------------------------------------------------------------
## for acceptance of the model:
#1) model converges

#2) variances are finit for all parameters:
all(is.finite(fit$sd))

#3) check residuals
fit.res <- calc.osa.resid(fit)
dev.off(); plot(1:1); dev.new()
png(filename="output/spict_dignostic.png")
plotspict.diagnostic(fit.res)
dev.off()

#4) retrospective analysis.
ret<- retro(fit, nretroyear = 3)
png(filename="output/spict_retro.png")
plotspict.retro(ret)
dev.off()

#5) realistic production curve. Bmsy/k should be between 0.1 and 0.9
calc.bmsyk(fit)

#6) uncertainty. intervals shouldn't span more than 1 order of magnitude
calc.om(fit)

#7) sensitivity to initial values
sens.ini <- check.ini(fit, ntrials=30) # JR - issue here as resmat seems to vary.'Distance' should be close to 0 for converged runs but isn't always. "The distance should preferably be close to zero. If that is not the case further investigation is required, i.e. inspection of objective function values, differences in results and residual diagnostics etc. should be performed"
if(mean(sens.ini$check.ini$resmat[,'Distance'])>1){stop("Distance is high, meaning the model is sensitive to initial chosen values, and should be rejected")}

