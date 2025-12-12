# Script information -----------------------------------------------------------
# Title: pil.27.7
# Authors: Joseph Ribeiro, Rosana Ourens-Chans, Susan Kenyon
# Date created: 2022/05/26
# Purpose: This script calculates catch advice for following year and produces figures and tables
#          for report, presentation, and advice sheet
# Working directory: root (applies to all the TAF .r files)

# set working directory and load libraries -------------------------------------
try(setwd(dirname(getActiveDocumentContext()$path)))
wdir = getwd(); print(wdir)

outDir="output"
mkdir(outDir)

library(rstudioapi) # This has been written as an R studio project using RStudio Version 1.4.1717 
library(ggplot2)
library(spict)
library(xlsx)
library(stringr)
library(tidyverse)

# 0: Set options and load data -------------------------------------------------
# Set options and load data
thisyear = read.table(paste0(wdir,"/boot/data/thisyear.txt"))[[1]]
adviceyear = read.table(paste0(wdir,"/boot/data/adviceyear.txt"))[[1]]
datayear = read.table(paste0(wdir,"/boot/data/datayear.txt"))[[1]]
load("boot/data/processed_data_various.RData")

# 1: Load advice table 5a and 5b and update table 5b ---------------------------

historic_advice_5a = read.csv("boot/initial/data/Advice_history_a.csv", fileEncoding = 'UTF-8-BOM',colClasses="character", stringsAsFactors=FALSE, na.strings=".",check.names=FALSE)
historic_advice_5b = read.csv("boot/initial/data/Advice_history_b.csv", fileEncoding = 'UTF-8-BOM',colClasses="character", stringsAsFactors=FALSE, na.strings=".")
official_landings_7 = read.csv("boot/initial/data/official_landings_7.csv", fileEncoding = 'UTF-8-BOM',colClasses="character", stringsAsFactors=FALSE, na.strings=".")
catchadvicesheet = read.csv("boot/data/Catch_advice sheet.csv")
preliminary_landings_1y = read.csv("boot/initial/data/preliminarycatchstatistics_data.csv", fileEncoding = 'UTF-8-BOM',colClasses="character", stringsAsFactors=FALSE, na.strings=".")
official_landings_5b_TO2023 = read.csv("boot/initial/data/ICESCatchDataset2006-2023.csv", fileEncoding = 'UTF-8-BOM',colClasses="character", stringsAsFactors=FALSE, na.strings=".")

# Filter preliminary landings
preliminary_landings_1y = preliminary_landings_1y[preliminary_landings_1y$Species.Latin.Name == 'Sardina pilchardus' & preliminary_landings_1y$Area %in% c("27.7.a", "27.7.b", "27.7.c", "27.7.d", "27.7.e", "27.7.f", "27.7.g", "27.7.h", "27.7.i", "27.7.j.2"),]

# Convert columns to numeric
preliminary_landings_1y[, c("AMS.Catch.TLW.", "BMS.Catch.TLW.")] = lapply(preliminary_landings_1y[, c("AMS.Catch.TLW.", "BMS.Catch.TLW.")], as.numeric)

# Calculate official landings for 5b_2023
official_landings_5b_2023sum = sum(preliminary_landings_1y$AMS.Catch.TLW., na.rm = TRUE) + sum(preliminary_landings_1y$BMS.Catch.TLW., na.rm = TRUE)

# Filter and process official landings for 5b_TO2023
official_landings_5b_TO2023 = official_landings_5b_TO2023[official_landings_5b_TO2023$Species == 'PIL' &  official_landings_5b_TO2023$Area %in% c("27.7"),]

# remove "0 c"
official_landings_5b_TO2023 <- data.frame(lapply(official_landings_5b_TO2023, function(x) {gsub("0 c", "0", x)}))

# Convert columns to numeric
official_landings_5b_TO2023[, c("X2018", "X2019", "X2020", "X2021", "X2022", "X2023")] = lapply(official_landings_5b_TO2023[, c("X2018", "X2019", "X2020", "X2021", "X2022", "X2023")], as.numeric)

# Calculate sums for each year
sums = colSums(official_landings_5b_TO2023[, c("X2018", "X2019", "X2020", "X2021", "X2022", "X2023")], na.rm = TRUE)

# Update historic advice with official landings
years_to_update = c('2018', '2019', '2020', '2021', '2022', '2023')
historic_advice_5b$Official.landings[historic_advice_5b$Year %in% years_to_update] = round(sums)
historic_advice_5b$Official.landings[historic_advice_5b$Year == datayear] = round(official_landings_5b_2023sum)

# update historic advice with ICES landings
historic_advice_5b$ICES.landings[historic_advice_5b$Year == '2022'] = round(sum(Catches$Caton[Catches$Year==2022])/1000) # Actually kg. And all discards are 0 so no need to filter catches for landings
historic_advice_5b$ICES.landings[historic_advice_5b$Year == '2023'] = round(sum(Catches$Caton[Catches$Year==2023&Catches$CatchCategory!="Discards"])/1000) #in 2023 there were discards (11 t)
historic_advice_5b$ICES.landings[historic_advice_5b$Year == '2024'] = round(sum(Catches$Caton[Catches$Year==2024&Catches$CatchCategory!="Discards"])/1000)

# 2. Update table 8 (assessment summary) ---------------------------------------

# Biomass indexes (non-boot)
biomass_full <- read.csv(file.path("boot/initial/data/Peltic_timeseries_replacement_datafile.csv"))
biomass_full = biomass_full[biomass_full$area=='full',c("year","Biomassindex")]

# Biomass indexes (non-boot)
biomass_land_disc_8_in <- read.csv(file.path("boot/initial/data/Peltic_timeseries_replacement_datafile.csv"))
biomass_land_disc_8_in = biomass_land_disc_8_in[biomass_land_disc_8_in$area=='full',c("year","Biomass_sum_cv","Biomassindex","SD.from.cv","biom_95_from_index","biom_5_from_index")]

# Construct landings columns of table 8 - recreate high and low columns
biomass_land_disc_8_in$Year = biomass_land_disc_8_in$year; biomass_land_disc_8_in$year <- NULL
biomass_land_disc_8_in$Biomass.index..total.area. = biomass_land_disc_8_in$Biomassindex
biomass_land_disc_8_in$High = biomass_land_disc_8_in$biom_95_from_index
biomass_land_disc_8_in$Low = biomass_land_disc_8_in$biom_5_from_index#biomass_land_disc_8_in$Low = biomass_land_disc_8_in$Biomass.index..total.area. - (biomass_land_disc_8_in$Biomass_sum_sd * 2)
biomass_land_disc_8_in = biomass_land_disc_8_in[,c("Year","Biomass.index..total.area.","High","Low")]

tomerge=data.frame('Landings'=round(as.numeric(catchadvicesheet[,'Catch.Landings'])),'Discards'=round(as.numeric(catchadvicesheet[,'Catch.Discards'])),'BMS.landing'=round(as.numeric(catchadvicesheet[,'Catch.BMS.landing'])),'Year'=catchadvicesheet[,'Year'])
biomass_land_disc_8 = merge(biomass_land_disc_8_in,tomerge,by='Year',all=T)
biomass_land_disc_8 = round(biomass_land_disc_8)
biomass_land_disc_8[is.na(biomass_land_disc_8)] <- "" # else it prints as NA in the advice sheet

# Mean catches over last 2y
catchadvicesheet$catch_summed = ifelse(!is.na(catchadvicesheet$Catch.BMS.landing), catchadvicesheet$Catch.Landings + catchadvicesheet$Catch.BMS.landing, catchadvicesheet$Catch.Landings)
prev2years=catchadvicesheet[catchadvicesheet$Year %in% c(thisyear-2, thisyear-1),]
# mean_2prevyears=round(sum(prev2years$catch_summed)/2)

# Catch and Biomass index data for different ranges of time, all of which are used further on
added_landings = as.double(biomass_land_disc_8$Landings) + as.double(biomass_land_disc_8$BMS.landing)
years_used_for_istat=is.finite(as.double(biomass_land_disc_8$Biomass.index..total.area.))
Valid_rows_catch_biom=is.finite(as.double(biomass_land_disc_8$Biomass.index..total.area.)) & is.finite(as.double(biomass_land_disc_8$Landings))
Iseries_full = as.double(biomass_land_disc_8[years_used_for_istat,]$Biomass.index..total.area.)
Cseries_full = added_landings[years_used_for_istat]

years_used_for_istat=as.double(biomass_land_disc_8$Year) %in% c(2017:2021)
Ihist = as.double(biomass_land_disc_8[years_used_for_istat,]$Biomass.index..total.area.)

# 4. Define the basis for advice -----------------------------------------------

historic_average_index = c(0,0) # We are averaging the year before with the year before that. Starting with these two zeros align the historic averages with biomass_land_disc_8[years_used_for_istat,]$Year
for(rw in 2:length(Iseries_full)){historic_average_index = append(historic_average_index,(Iseries_full[rw-1]+Iseries_full[rw-2])/2)} # average the year before with the year before that
historic_average_catch = c(0,0) # We are averaging the year before with the year before that. Starting with these two zeros align the historic averages with biomass_land_disc_8[years_used_for_istat,]$Year
for(rw in 2:length(Cseries_full)){historic_average_catch = append(historic_average_catch,(Cseries_full[rw-1]+Cseries_full[rw-2])/2)} # average the year before with the year before that

index_previous = round(mean(c(biomass_full$Biomassindex[biomass_full$year==thisyear-2], biomass_full$Biomassindex[biomass_full$year==thisyear-1])))
index_latest = round(Iseries_full[length(Iseries_full)])

index_1o2 = index_latest/index_previous
lastyearadvice = as.numeric(gsub("[[:punct:]]", "", gsub(" ", "", historic_advice_5b$Catch.corresponding.to.advice[historic_advice_5b$Year==thisyear])))
advised_catch = index_1o2*lastyearadvice

modeladvisedcatch = round(advised_catch[length(advised_catch)])
advisedcatch = modeladvisedcatch
discard_rate='Negligible' # In time, this could change to something that is calculated

# Calculate some index related things
if(index_latest>index_previous){change = 'increase'} 
if(index_latest<index_previous){change = 'decrease'}
index_ratio_0 = round(index_latest/index_previous,3)
index_ratio_sign = round(100*(1-(index_latest/index_previous)))
index_ratio = abs(index_ratio_sign)
index_ratio_inv = (100-index_ratio)/100

# Basis for advice - index cap?
uncertaintycap = 'Not applied'
uncertaintycapval = ''
if(index_ratio>80){uncertaintycapval = '0.8'; uncertaintycap = 'Applied'}

# Basis for advice - biomass safeguard?
istat = round( exp(mean(log(Ihist))) * (2.71828^(-1.645*sd(log(Ihist)))) ) # define istat - this the same as biomass_safeguard_val?
biomass_safeguard_val = round(exp(mean(log(Ihist))) * exp(-1.645*sd(log(Ihist))))
biomass_safeguard = 'Not applicable'
if(index_latest<biomass_safeguard_val){advisedcatch = modeladvisedcatch *(index_latest/biomass_safeguard_val)
                                       biomass_safeguard = 'Applied'}else{biomass_safeguard_val = ''}

if(index_ratio_0>1){advice_change=paste0("+", index_ratio," %")}
if(index_ratio_0<1){advice_change=paste0("-", index_ratio," %")}

# Add advice to table 5b
historic_advice_5b <- historic_advice_5b %>% add_row(Year = as.character(adviceyear),
                                                     ICES.advice = "Precautionary approach",
                                                     Catch.corresponding.to.advice = as.character(advisedcatch))

# Get full stops out of title names
names(historic_advice_5a) <- gsub(x = names(historic_advice_5a), pattern = "\\.", replacement = " ")
names(historic_advice_5b) <- gsub(x = names(historic_advice_5b), pattern = "\\.", replacement = " ")
names(official_landings_7) <- gsub(x = names(official_landings_7), pattern = "\\.", replacement = " ")
names(biomass_land_disc_8) <- gsub(x = names(biomass_land_disc_8), pattern = "\\.", replacement = " ")
#names() <- gsub(x = names(), pattern = "\\.", replacement = " ")

# Save advice info to R object
save.image(file='output/AdviceInfo.rdata')


# 5. Plotting underlying landings and catches ----------------------------------

# Report figure 1
CatchCat$CatchT=CatchCat$Catch/1000
(plot1<-ggplot(CatchCat, aes(Year,CatchT, fill=CatchCategory))+
geom_col(width = 0.4)+ylab("Catches in 1000 t")+xlab("")+scale_x_continuous(breaks=seq(2002,datayear,5))+theme(axis.text.x = element_text(angle=90,vjust=0.5,hjust=0.5, size=13),axis.text.y=element_text(size=13),legend.text = element_text(size=13),legend.title=element_text(size=13),strip.text.x = element_text(size=13),axis.title = element_text(size=13), legend.key.size = unit(0.25,'cm'),legend.position="bottom")+guides(fill=guide_legend(ncol=4, title=""))+labs(fill="Catch category")+ 
scale_fill_manual("legend", values = c( "Landings" = "navyblue", "Discards"  = "lightblue",    "BMS landing" = "lightgreen"))  + theme(panel.background = element_rect(fill = 'white', colour = 'black'))+
theme(  panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line( size=.05, color="lightgrey" ) ,
        panel.grid.minor.y = element_line( size=.05, color="lightgrey" ) ,
        axis.text.x = element_text(angle = 0)))
ggsave(plot1,file=paste(wdir,'/output/CatchbyCat.png', sep="/"), width=6, height=4)

# Report figure 2
(plot2<-ggplot(landCountry, aes(Year,Catch, fill=Country))+
  geom_col()+ylab("Landings (t)")+xlab("") +scale_x_continuous(breaks=seq(2002,datayear,5))+
    theme(axis.text.x = element_text(angle=90,vjust=0.5,hjust=0.5, size=13),
          axis.text.y=element_text(size=13),
          legend.text = element_text(size=13),
          legend.title=element_text(size=13),
          axis.title = element_text(size=13),
          #legend.key.size = unit(1,'cm'),
          legend.position="right")+
    guides(fill=guide_legend(ncol=1, title=""))+
    scale_fill_manual(values=c("#F8766D","#D39200","#93AA00","#00BA38","#00C19F","#00B9E3","#619CFF","#DB72FB","#FF61C3","#FF4533")))
ggsave(plot2,file=paste(wdir,'/output/CatchbyCountry.png', sep="/"), width=6, height=4)

# Not in report - in presentation
(plot3<-ggplot(landDivA, aes(Year,Landings, fill=Area))+
  geom_col()+ylab("Landings (t)")+facet_wrap(.~Country)+xlab("")+
  scale_x_continuous(breaks=seq(2002,datayear,5))+
  theme(axis.text.x = element_text(angle=90,vjust=0.5,hjust=0.5, size=14),
        axis.text.y=element_text(size=14),
        legend.text = element_text(size=14),
        legend.title=element_text(size=14),
        axis.title = element_text(size=14),
        strip.text = element_text(size=14),
        legend.key.size = unit(1,'cm'),
        legend.position="bottom")+
  guides(fill=guide_legend(ncol=5, title=""))+
  scale_fill_manual(values=c("#F8766D","#D39200","#93AA00","#00BA38","#00C19F","#3288bd","#619CFF","#DB72FB","#FF61C3","#FF4533","#A50026","#465e4c")))
ggsave(plot3,file=paste(wdir,'/output/CatchbyCoun&div.png', sep="/"), width=16, height = 9)

# Report figure 3
(plot4<-ggplot(landDivb, aes(Year,Landings, fill=Area))+
  geom_col()+ylab("Landings (t)")+xlab("")+
  scale_x_continuous(breaks=seq(2002,datayear,5))+
  theme(axis.text.x = element_text(angle=90,vjust=0.5,hjust=0.5, size=13),
        axis.text.y=element_text(size=13),
        legend.text = element_text(size=13),
        legend.title=element_text(size=13),
        axis.title = element_text(size=13),
        legend.key.size = unit(1,'cm'),
        legend.position="bottom")+
  guides(fill=guide_legend(ncol=5, title=""))+
  scale_fill_manual(values=c("#F8766D","#D39200","#93AA00","#00BA38","#00C19F","#3288bd","#619CFF","#DB72FB","#FF61C3","#FF4533","#A50026","#465e4c")))
ggsave(plot4,file=paste(wdir,'/output/CatchbyDiv.png', sep="/"), width=12, height=8)

# Not in report - in presentation
(plot5<-ggplot(metier, aes(Year,Landings, fill=metier2))+
  geom_col()+ylab("Landings (t)")+xlab("")+labs(fill="Fleet")+
  scale_x_continuous(breaks=seq(2002,datayear,5))+
  theme(axis.text.x = element_text(angle=90,vjust=0.5,hjust=0.5, size=13),
        axis.text.y=element_text(size=13),
        legend.text = element_text(size=13),
        legend.title=element_text(size=13),
        axis.title = element_text(size=13),
        #legend.key.size = unit(1,'cm'),
        legend.position="bottom")+
  guides(fill=guide_legend(ncol=3, title=""))+
  scale_fill_manual(values=c("#F8766D","#D39200","#93AA00","#00BA38","#00C19F","#3288bd","#619CFF","#DB72FB","#FF61C3", "#FF4533" )))
ggsave(plot5,file=paste(wdir,'/output/CatchbyFleet.png', sep="/"), width=12, height=8)

# Not in report
(plot6 <- ggplot(LandQuarter1, aes(Year,Landings, group=factor(Season), fill=factor(Season)))+
  geom_col()+ylab("Landings (t)")+xlab("Year")+labs(fill="Quarter")+
  facet_wrap(.~Country,scales="free_y")+
  scale_x_continuous(breaks=seq(1987,2019,4))+
  theme(axis.text.x = element_text(angle=90,vjust=0.5,hjust=0.5)))

# Report plot 4
(plot7<-ggplot(LandQuarter2, aes(Year,landings, group=factor(Quarter), col=factor(Quarter)))+
  geom_line(size=1.3)+ylab("Landings (t)")+xlab("")+labs(col="Quarter")+
  scale_x_continuous(breaks=seq(2002,datayear,5))+
  theme(axis.text.x = element_text(angle=90,vjust=0.5,hjust=0.5, size=13),
        axis.text.y=element_text(size=13),
        legend.text = element_text(size=13),
        legend.title=element_text(size=13),
        axis.title = element_text(size=13),
        legend.key.size = unit(1,'cm'),
        legend.position="right")+
  guides(fill=guide_legend(ncol=4, title="Quarter")))
ggsave(plot7,file=paste(wdir,'/output/CatchbyQ.png', sep="/"), width=6, height=4)


# 6: Plotting biomass index ----------------------------------------------------
biomass_data = biomass_land_disc_8_in
biomass_data=biomass_data[is.finite(biomass_data$Biomass.index..total.area.),]
biomass_data$Biomass = biomass_data$Biomass.index..total.area./1000
biomass_data$Low = biomass_data$Low/1000
biomass_data$High = biomass_data$High/1000

(bimplt <- ggplot(biomass_data, aes(x=Year, y=Biomass)) + 
    geom_line(aes(y = Biomass),color="darkgreen",size=1) +
    geom_segment(aes(x = thisyear-0.5, y=index_latest/1000, xend = thisyear+0.5, yend = index_latest/1000),color="orange",size=1) +
    geom_segment(aes(x = thisyear-2.5, y=index_previous/1000, xend = thisyear-0.5, yend = index_previous/1000),color="orange",size=1) +
    geom_segment(aes(x = 2017, y=istat/1000, xend = thisyear, yend = istat/1000),color="darkgreen",size=1) +
    geom_ribbon(aes(ymin=Low,ymax=High, linetype = '95% confidence interval'), fill="darkseagreen4", size=0, alpha=0.5)+ ylab("Biomass index in 1000 t")+
    scale_y_continuous(expand = c(0, 0), limits=c(0,550)) + theme(panel.background = element_rect(fill = 'white', colour = 'black'))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line( size=.05, color="lightgrey" ) ,
        panel.grid.minor.y = element_blank() ,
        legend.title = element_blank() ,
        axis.text.x = element_text(angle=0, size=13),
        axis.text.y=element_text(size=13),
        legend.text = element_text(size=13),
        strip.text.x = element_text(size=13),
        axis.title = element_text(size=13), 
        legend.key.size = unit(0.25,'cm'),
        legend.position="bottom"
))

ggsave(bimplt,file=paste(wdir,'/output/biomass_timseries.png', sep="/"), width=6, height=4)

# 7: Plot model outputs --------------------------------------------------------
load(file='model/fittedmodel.rdata')

plot(1:1); dev.new() # Make a window 

# individual plots
png(paste(outDir,'spict_biomass.png', sep="/"), width=606, height=404,pointsize = 13)
plotspict.biomass(fit)
dev.off()

png(paste(outDir,'spict_bbmsy.png', sep="/"), width=606, height=404,pointsize = 13)
plotspict.bbmsy(fit)
dev.off()

png(paste(outDir,'spict_f.png', sep="/"), width=606, height=404,pointsize = 13)
plotspict.f(fit)
dev.off()

png(paste(outDir,'spict_ffmsy.png', sep="/"), width=606, height=404,pointsize = 13)
plotspict.ffmsy(fit)
dev.off()

png(paste(outDir,'spict_catch.png', sep="/"), width=606, height=404,pointsize = 13)
plotspict.catch(fit)
dev.off()

png(paste(outDir,'spict_fb.png', sep="/"), width=606, height=404,pointsize = 13)
plotspict.fb(fit, ylim=c(0, 1.3), xlim=c(0, 300))
dev.off()

# Forecast

#inp$maninterval<-c(2021,2022)
#inp$maneval <- 2021
rep <- fit.spict(inp)
sumspict.predictions(rep)

png(paste(outDir,'spict_forecast.png', sep="/"), width=870, height=538,pointsize = 13)
plot2(rep)
dev.off()

FFMSY<-as.data.frame(get.par("logFFmsy",fit,exp=T))
FFMSY<-tibble::rownames_to_column(FFMSY, "Year")
ffmsy<-write.csv(FFMSY,paste0(outDir,"/ffmsy.csv"))

png(paste(outDir,'spict_parameters.png', sep="/"), width=870, height=538,pointsize = 13)
plotspict.priors(fit)
dev.off()


# Self sampling programme plots ------------------------------------------------

# New data this year
processorslw <- read.csv(paste0(wdir,"/boot/initial/data/processors_lw.csv"), header=T)
processorslw = processorslw[processorslw$species=='Sardine',]

processorslw$date = as.Date(processorslw$date)
processorslw$year = lubridate::year(processorslw$date)
processorslw <- processorslw[processorslw$weight_g < 300 & processorslw$weight_g != 0,]
processorslw$length_cm <- ifelse(processorslw$length_cm >= 100, processorslw$length_cm/10, processorslw$length_cm)
processorslw <- processorslw[processorslw$length_cm < 40 & processorslw$length_cm != 0,]

processorslw$processor <- as.factor(processorslw$processor)
processorslw = processorslw[!is.na(processorslw$length_cm),]
processorslw$length_cm = floor(processorslw$length_cm*2) / 2

library(data.table)

########## Size distribution provided by the producers

Size_data<-ddply(processorslw,.(length_cm, year, processor), summarize, count=length(length_cm))

# Slide 8
(producers<-ggplot(Size_data, aes(length_cm, count, fill=processor)) + 
  geom_density(stat="identity", alpha=0.8) + 
  facet_grid(year~., scales="free_y")+
  ylab("N")+xlab("Length (cm)") +
  theme(legend.position = "none"))

#geom_vline(data=ddply(Size_data,.(year, Source_nameAn), summarize, meansize=sum(CANUM*length_cm)/sum(CANUM)), aes(xintercept=meansize, col=Source_nameAn),linetype=2, size=1.2) 

png(paste(outDir,'ProducersDat_2020.png', sep="/"), width=687, height=644)
producers
dev.off()



########## Size distribution provided by  fishers

LengthFish<-subset(LengthDat, source=="Fishers"|source=="csma"& !is.na(date))

LengthFish$vesselnamelowercase <- gsub(" ", "", LengthFish$vesselnamelowercase)

LengthFish$vessel1 <- ifelse(grepl("g.harvest|gharvest", LengthFish$vesselnamelowercase), "goldenharvest",
                      ifelse(grepl("p.marksman", LengthFish$vesselnamelowercase), "pelagicmarksman",
                      ifelse(grepl("inter-nos", LengthFish$vesselnamelowercase), "internos", LengthFish$vesselnamelowercase)))

Size_dataf<-ddply(LengthFish,.(length_cm,year,vessel1), summarize, CANUM=length(n))

# slide 8
(fishers<-ggplot(Size_dataf, aes(length_cm,CANUM, fill=vessel1)) +
  geom_density(stat="identity", alpha=0.8) + 
  facet_grid(year~., scales="free_y")+
  ylab("N")+xlab("Length (cm)")+
  theme(legend.position = "none"))
  
# geom_vline(data=ddply(Size_data,.(year, Source_nameAn), summarize, meansize=sum(CANUM*length_cm)/sum(CANUM)), aes(xintercept=meansize, col=Source_nameAn),linetype=2, size=1.2) 
png(paste(outDir,'FishersDat_2021.png', sep="/"), width=687, height=644)
fishers
dev.off()




################################ Size distribution of the samples by year and source
LengthClean<-LengthDat[!is.na(LengthDat$length_cm),] 

# Size_data<-ddply(LengthClean,.(length_cm, year, source), summarize, CANUM=sum(n))
# meansize<-ddply(Size_data,.(year, source), summarize, meansize=sum(CANUM*length_cm)/sum(CANUM))
# nsamplesbysource<-ddply(LengthClean,.(year, source), summarize, n_measurements=sum(n))
# nsamplesbysource2<-ddply(LengthClean,.(year), summarize, n_measurements=sum(n))
# 
# ggplot(Size_data, aes(length_cm,CANUM, fill=source)) + geom_density(stat="identity", alpha=0.8, position="stack") + 
#   facet_grid(year~., scales="free_y")+
#   ylab("N")+xlab("Length (cm)")+
#   geom_vline(data=ddply(Size_data,.(year, source), summarize, meansize=sum(CANUM*length_cm)/sum(CANUM)), aes(xintercept=meansize, col=source),linetype=2, size=1.2) 
# 
# # Slide 9
# aggregDat<-ggplot(Size_data, aes(length_cm,CANUM, fill=source)) + geom_density(stat="identity", alpha=0.8) + 
#   facet_grid(source~year, scales="free_y")+
#   ylab("N")+xlab("Length (cm)")+
#   geom_vline(data=ddply(Size_data,.(year, source), summarize, meansize=sum(CANUM*length_cm)/sum(CANUM)), aes(xintercept=meansize, col=source),linetype=2, size=1.2) 
# 
# 
# png(paste(outDir,'aggregLength_2021.png', sep="/"), width=749, height=529)
# aggregDat
# dev.off()

## pooled data by year

LengthClean<-subset(LengthClean, source!="app"& !is.na(date)&length_cm<40)
Size_datap<-ddply(LengthClean,.(length_cm, year), summarize, CANUM=length(n))
meansize<-ddply(Size_datap,.(year), summarize, meansize=sum(CANUM*length_cm)/sum(CANUM))
meansize$meansize = round(meansize$meansize,1)
print("mean sizes used in report")
print(meansize)

# slide 10
aggregDat2<-ggplot(Size_datap, aes(length_cm,CANUM, fill=factor(year))) + geom_density(stat="identity", alpha=0.8) + 
  facet_grid(year~., scales="free_y")+
  ylab("N")+xlab("Length (cm)")+
  geom_vline(data=meansize, aes(xintercept=meansize, col=factor(year)) )+
  xlim(c(2,28))

png(paste(outDir,'combinedLength_2021.png', sep="/"), width=749, height=529)
aggregDat2
dev.off()

# print("self-sampling summary table")
# print(nsamplesbysource)
# mean(nsamplesbysource2$n_measurements)
