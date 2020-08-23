setwd("C:/Users/Admin/OneDrive - Imperial College London/Research projects/Masters thesis/Field course temp folder/R wd/JL_TFE R Project 2")
setwd("C:/Users/JPL19.IC/OneDrive - Imperial College London/Research projects/Masters thesis/Field course temp folder/R wd")
##################################################
# LOAD DATA
##################################################

# first batch
all.files <- list.files(path = "C:/Users/Admin/OneDrive - Imperial College London/Research projects/Masters thesis/Field course temp folder/R wd/deployments",
                        include.dirs = TRUE,
                        recursive = TRUE,
                        full.names = TRUE,
                        ignore.case = TRUE)

txt.files <- grep(".dat", all.files, value=TRUE)

dat <- data.frame(File = "REMOVE ROW", Calib = "REMOVE ROW", Make = "REMOVE ROW")

for(file in txt.files){
  
  # Find the header length from the first line
  hdr_length <- scan(file, nlines=1, what='character', sep=':')
  hdr_length <- as.numeric(hdr_length[2])
  
  # Load the header records and get the deployment
  header <- scan(file, nlines=hdr_length, sep=':', what=list('character', 'character'))
  header <- as.list(setNames(header[[2]], header[[1]]))
  depl_start <- strptime(header$start, '%Y-%m-%d ')
  depl_ID <- paste0(trimws(header$location), "_", strftime(depl_start, format='%Y%m%d'))
  depl_dur <- paste0(trimws(header$n_days))
  
  # Load the data and add deployment as a field
  data <- read.delim(file, skip=hdr_length)
  data$depl_ID <- depl_ID
  data$depl_dur <- depl_dur
  
  # Merge data frames
  dat <- merge(dat, data, all = TRUE)
}

# second batch from Sui's hard drive 22/07/2020
# need to extract grid point from folder name
all.files <- list.files(path = "C:/Users/JPL19.IC/OneDrive - Imperial College London/Research projects/Masters thesis/Field course temp folder/R wd/deployments2",
                        include.dirs = TRUE,
                        recursive = TRUE,
                        full.names = TRUE,
                        ignore.case = TRUE)

txt.files <- grep(".dat", all.files, value=TRUE)

datmore <- data.frame(File = "REMOVE ROW", Calib = "REMOVE ROW", Make = "REMOVE ROW")

for(file in txt.files){
  
  # Find the header length from the first line
  hdr_length <- scan(file, nlines=1, what='character', sep=':')
  hdr_length <- as.numeric(hdr_length[2])
  
  # Load the header records and get the deployment
  header <- scan(file, nlines=hdr_length, sep=':', what=list('character', 'character'))
  header <- as.list(setNames(header[[2]], header[[1]]))
  depl_start <- strptime(header$start, '%Y-%m-%d ')
  grid_pt <- str_extract(file, 'PT+\\s+\\d+|PT+\\d+')
  depl_ID <- paste0(trimws(header$location),"_",trimws(grid_pt), "_", strftime(depl_start, format='%Y%m%d'))
  depl_dur <- paste0(trimws(header$n_days))
  
  # Load the data and add deployment as a field
  data <- read.delim(file, skip=hdr_length)
  data$depl_ID <- depl_ID
  data$depl_dur <- depl_dur
  
  # Merge data frames
  datmore <- merge(datmore, data, all = TRUE)
}

# merge two batches of data
dat <- merge(dat, datmore, all = TRUE)

# remove ' for low squirrel before writing to csv
dat$Keyword_1[which(dat$Keyword_1 == "Low's Squirrel")] <- "Lows Squirrel"
write.csv(dat, "dat.csv")


##################################################
# DATA FILTERING AND MANIPULATION
##################################################

dat <- read.csv("dat.csv", header = TRUE, stringsAsFactors=FALSE)
dat <- dat[-1,-1]

# independent capture events
dat$New <- 0

idx.New <- grepl("Group New Contact", datmore$Keyword_5)
dat$New[idx.New] <- 1

idx.New <- grepl("New Contact", datmore$Keyword_2)
dat$New[idx.New] <- 1

dat2 <- dat[dat$New == 1,]

#group info
idx.Solitary <- grepl("New Contact", dat2$Keyword_2)
summary(idx.Solitary)
dat2$Group[idx.Solitary == TRUE] <- 0

idx.Group <- grepl("Group New Contact", dat2$Keyword_5)
summary(idx.Group)
dat2$Group[idx.Group] <- 1

# multiple spellings
renames <- list(c("Bird sp.", "Bird"),
                c("Bird, Bird sp.", "Bird"),
                c("Ear-spot Squirrel", "Earspot Squirrel"),
                c("Bornean Ground-cuckoo", "Bornean Ground Cuckoo"),
                c("Ear-spot Squirrel", "Pig-tailed Macaque" ),
                c("Greater Mousedeer", "Greater Mouse-deer"),
                c("Greater Mousedeer, Mousedeer sp.", "Greater Mouse-deer"),
                c("Lesser Mousedeer", "Lesser Mouse-deer"),
                c("Malay Civet", "Malayan Civet"),
                c("Pig-Tailed Macaque", "Pig-tailed Macaque"),
                c("Short-Tailed Babbler", "Short-tailed Babbler"),
                c("Short-Tailed Mongoose", "Short-tailed Mongoose"),
                c("Small Spiny Rat", "Spiny Rat"),
                c("Yellow-Throated Marten", "Yellow-throated Marten"),
                c("Black-Capped Babbler", "Black-capped Babbler"),
                c("Banded Palm Civet", "Banded palm civet"),
                c("Bornean Sun Bear", "Sun Bear"),
                c("	Chesnut-necklaced hill patridge", "Chestnut-necklaced Hill Partridge"),
                c("Crested fireback", "Crested Fireback"),
                c("Leopard cat", "Leopard Cat"),
                c("Little spiderhunter", "Little Spiderhunter"),
                c("Long-tailed porcupine", "Long-tailed Porcupine"),
                c("Marbled cat", "Marbled Cat"),
                c("Masked palm civet", "Masked Palm Civet"),
                c("Orang Utan", "Orangutan"),
                c("Pig-tailed macaque", "Pig-tailed Macaque"),
                c("Short-tailed babbler", "Short-tailed Babbler"),
                c("Species, Bearded Pig", "Bearded Pig"),
                c("Species, Pig-tailed macaque", "Pig-tailed Macaque"),
                c("White-crowned Sharma", "White-crowned Shama"))

for(this_rename in renames){
  dat2$Keyword_1[which(dat2$Keyword_1 == this_rename[1])] <- this_rename[2]
}

# DateTime
dat2$DateTime <- as.POSIXct(strptime(dat2$DateTimeOriginal, "%Y-%m-%d %H:%M:%S", tz = "GMT"))


# export
write.csv(dat2, "dat2.csv")

dat3 <- dat2

# logging dates
require(stringr)
dat3$Block <- str_extract(dat3$depl_ID, '^[A-Z]+')

dat3$logging <- vector(length = nrow(dat3))

log_dates <- data.frame(block = c("B", "D", "E","F"),
                        start = as.POSIXct(c("2015-01-28", "2013-01-02",
                                             "2013-01-02", "2014-07-03")),
                        end   = as.POSIXct(c("2015-06-30", "2015-06-30",
                                             "2015-04-12", "2015-06-30")))

# look up the start and end for each row by the block ID
dat3$log_start <- log_dates$start[match(dat3$Block, log_dates$block)]
dat3$log_end <- log_dates$end[match(dat3$Block, log_dates$block)]
dat3$logging <- with(dat3, ifelse(DateTime < log_start, 'pre-logging',
                                  ifelse(DateTime < log_end, 'during logging', 'post-logging')))

# add El Nino column
require(dplyr)

dat3$date <- as.Date.POSIXct(dat3$DateTime, "%Y-%m-%d")

# drought period
dat4 <- dat3 %>%
  group_by(depl_ID) %>%
  mutate(ElNino = ifelse(max(date) < "2016-01-01", "before",
                         ifelse(min(date) > "2016-05-01", "after", "overlaps")))

#Only grids with postlogging data


idx <- grepl("B10-2|B100-2|D100-1|D100-2|E1-1|E1-2|E100-1|E100-2|
              B10_2|B100_2|D100_1|D100_2|E1_1|E1_2|E100_1|E100_2|", dat4$depl_ID)

idx <- grepl("B10-2|B100-2|D1-2|D10-2|D100-1|D100-2|E1-1|E1-2|E10-1|E10-2|E100-1|E100-2|
              B10_2|B100_2|D1_2|D10_2|D100_1|D100_2|E1_1|E1_2|E10_1|E10_2|E100_1|E100_2", dat4$depl_ID)

dat5 <- dat4[idx,]

# check common species
large_mam <- subset(dat5, Keyword_1 %in% c("Bearded Pig",
                                           "Red Muntjac",
                                           "Pig-tailed Macaque",
                                           "Sambar Deer",
                                           "Malayan Civet",
                                           "Lesser Mouse-deer",
                                           "Malayan Porcupine",
                                           "Banded Civet",
                                           "Yellow Muntjac",
                                           "Sun Bear",
                                           "Orangutan",
                                           "Stink Badger",
                                           "Masked Palm Civet",
                                           "Yellow-throated Marten",
                                           "Leopard Cat",
                                           "Banded Palm Civet",
                                           "Collared Mongoose",
                                           "Sunda Pangolin",
                                           "Marbled Cat",
                                           "Bay Cat",
                                           "Clouded Leopard",
                                           "Long-tailed Porcupine",
                                           "Short-tailed Mongoose",
                                           "Binturong",
                                           "Western Tarsier",
                                           "Red Langur",
                                           "Banded Linsang",
                                           "Common Palm Civet",
                                           "Malay Weasel",
                                           "Maroon Langur",
                                           "Small-toothed Palm Civet",
                                           "Thick-spined Porcupine"))
species_hits <- large_mam %>%
  group_by(Keyword_1, logging) %>%
  summarise(n = n()) %>%
  pivot_wider(names_from = logging, values_from = n, values_fill = 0) %>%
  as.data.frame(species_hits) %>%
  mutate(total = rowSums(.[2:4]))

write.csv(species_hits, "species_hits.csv")

## subset common species
dat6 <- subset(dat5, Keyword_1 %in% c("Red Muntjac", "Bearded Pig", "Sambar Deer",
                                      "Malayan Porcupine", "Pig-tailed Macaque",
                                      "Lesser Mouse-deer"))


## reset factors
dat6$Keyword_1 <- factor(dat6$Keyword_1)

# soil moisture
flux <- read.csv("SAFE_FluxTower_AllMet_2012_2018.csv")
flux$DateTime <- as.POSIXct(flux$ï..timestamp, format = '%d/%m/%Y %H:%M', tz = "GMT")

s_moist <- vector(length = 3051)

for (i in 1:nrow(dat6)){
  
  #get rows in flux that match the date
  
  matching_date <- which(as.Date(flux$DateTime) == (dat6$date[i]))
  
  if (length(closest_dates) != 0){
    
    #get the mean soil moisture for that day
    values <- flux$Be_VW_3_Avg[matching_date]
    
    values <- values[which(values != "#N/A")]
    
    s_moist[i] <- mean(as.numeric(values))
    
  }
  
}

dat6 <- cbind(dat6, s_moist)

# mean soil moisture per depl_ID
require(dplyr)
dat6 <- dat6 %>%
  group_by(depl_ID) %>%
  mutate(s_moist_av = mean(...71, na.rm = TRUE))

# Just to be safe: separate out species counts,
# average temp and deployment level data and then bring back together using merge. 

# deployment counts: 
sp_group_counts <- as.data.frame(xtabs(Group ~ depl_ID + logging + Keyword_1, data=dat6), 
                                 stringsAsFactors=FALSE)

names(sp_group_counts)[names(sp_group_counts) == "Freq"] <- "Group"

sp_counts <- as.data.frame(xtabs(~ depl_ID + logging + Keyword_1, data=dat6), 
                           stringsAsFactors=FALSE)


# deployment level data:
depl_data <- unique(subset(dat6, select=c(Block, depl_ID, depl_dur, logging, ElNino)))
ave_temp <- aggregate(AmbientTemperature ~ logging + depl_ID, data = dat6, FUN='mean')
soil_moist <- aggregate(s_moist ~ depl_ID, data = dat6, FUN = 'mean')

#merge with average temperature
depl_data <- merge(depl_data, ave_temp, all=TRUE)

#merge with group counts
depl_data <- merge(merge(depl_data, sp_group_counts, all=TRUE),
                   ave_temp)

# bring everything together
dat7 <- merge(depl_data, sp_counts, all=TRUE)

# Using xtabs includes _all_ combinations of deployment_id logging and species but
# not all combinations actually exist so we get NAs when we match the deployment data
# to the counts, so drop the resulting NA rows
dat7 <- na.omit(dat7)

#structures
dat7$Species <- factor(dat7$Keyword_1)
dat7 <- subset(dat7, select=-c(Keyword_1))
dat7$Block <- factor(dat7$Block)
dat7$logging <- factor(dat7$logging, levels=c('pre-logging', 'during logging', 'post-logging'))
dat7$depl_dur <- as.numeric(dat7$depl_dur)

##################################################
# MODELLING
###################################################

# nb. glmer.nb does not have a zero inflation argument.

list.of.packages <- c("glmmTMB","ggplot2","dplyr","lme4","pscl","bbmle","lsmeans","lmtest","MASS","R2admb","devtools","lattice","AED","car","stats","scales","DHARMa","effects","multcomp","MuMIn","sjstats","xtable","ggsignif","activity","circular","jpeg","ggpubr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#just load a bunch of stuff
require(glmmTMB)
require(ggplot2); theme_set(theme_bw())
require(dplyr)
require(lme4)
require(pscl)
require(bbmle)
require(lsmeans)
require(lmtest)
require(MASS)
require(R2admb)
require(devtools)
require(lattice)
require(AED)
require(car)
require(stats)
require(scales)
require(DHARMa)
require(effects)
require(multcomp)
require(MuMIn)
require(sjstats)
require(xtable)
require(ggsignif)
require(activity)
require(circular)
require(jpeg)
require(ggpubr)
require(tidyr)

# 1. data exploration

# check for outliers
op <- par(mfrow = c(2, 2), mar = c(3, 3, 3, 1))
dotchart(dat7$Freq, main = "Freq")
dotchart(dat7$depl_dur, main = "depl_dur")
dotchart(dat7$AmbientTemperature, main = "AmbientTemperature")
dotchart(dat7$s_moist, main = "soil moisture")

# transform AmbientTemperature and Freq
dat7$logAmbientTemperature <- log10(dat7$AmbientTemperature)
dat7$logFreq <- log10(dat7$Freq)

# check for collineararity
# plots of AmbientTemperature conditional on each nominal explanatory variable
par(mfrow=c(1,1))

bwplot(AmbientTemperature ~ logging, data = dat7)

bwplot(AmbientTemperature ~ ElNino, data = dat7)

ggplot(dat7, aes(x = logFreq, y = logAmbientTemperature, colour = Species)) +
  geom_point() +
  facet_wrap( ~ Species)

ggplot(dat7, aes(x = logFreq/depl_dur, y = s_moist, colour = Species)) +
  geom_point() +
  facet_wrap( ~ Species)

# look at the distribution 
ggplot(dat7, aes(x = Freq)) +
  geom_histogram(binwidth=1) +
  facet_wrap( ~ Species, scales="free") +
  theme(axis.ticks = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = c(0.89, 0.85), legend.title=element_blank()) +
  #scale_fill_manual(values=c("black", "white", "dark grey")) +
  #scale_colour_manual(values = c("black", "black", "black")) +
  ylab("") +
  xlab("Number of detections")


var(dat7$Freq)
mean(dat7$Freq) # shows overdispersion zeros and counts

# based on this, we want to use a ZIP or ZINB (mixture models), not ZAP or ZANB (hurdle models).

# We can see that the data is a negative binomial distribution 
# but we can test this by comparing P and NB models with lrtest()
f1 <- formula(Freq ~ Species * logging)
P0 <- glm(f1, family = poisson, data = dat7)
NB0 <- glm.nb(f1,data = dat7)

lrtest(P0, NB0) # can also see in summary() that there is overdispersion in P0

#use glmmTMB for ZINB
#explore different families
mod.nb2 <- glmmTMB(Freq ~ Species * logging + (1|depl_ID),
                    ziformula = ~ Species,
                    family = nbinom2, data = dat7,
                    offset = log(depl_dur))

mod.nb1 <- update(mod.nb2, family = "nbinom1")
mod.p1 <- update(mod.nb2, family = "poisson")

AICtab(mod.nb2, mod.nb1, mod.p1)

# 2. model selection

# start with all covariates
# backwards model selection
mod.dredge <- glmmTMB(Freq ~ Species * logging + AmbientTemperature + ElNino + (1|depl_ID), 
               
               ziformula= ~ Species * logging,
               
               family=nbinom2, data=dat7,
               
               offset = log(depl_dur))

#check for best model
dr <- dredge(mod.dredge)

#check for best RE
mod2 <- glmmTMB(Freq ~ Species * logging + (1|depl_ID), 
               
               ziformula= ~ Species,
               
               family=nbinom2, data=dat7,
               
               offset = log(depl_dur))
summary(mod2)

mod3 <- glmmTMB(Freq ~ Species * logging + (1|Block), 
                
                ziformula= ~ Species,
                
                family=nbinom2, data=dat7,
                
                offset = log(depl_dur))
summary(mod3)

mod4 <- glmmTMB(Freq ~ Species * logging + (1|Block/depl_ID), 
                
                ziformula= ~ Species,
                
                family=nbinom2, data=dat7,
                
                offset = log(depl_dur))
summary(mod4)

# 3. model validation
mod <- glmmTMB(Freq ~ Species * logging + (1|depl_ID), 
               
               ziformula= ~ Species + logging,
               
               family=nbinom2, data=dat7,
               
               offset = log(depl_dur))

# residuals
mod_simres <- simulateResiduals(mod)
plot(mod_simres)
qqnorm(resid(mod))
qqline(resid(mod))

# anova
ao <- Anova(mod)

# emmeans
em <- emmeans(mod, pairwise ~ logging | Species, type = "response")
plot(em, comparisons = TRUE, ylab = "")

# test against null model
null.mod <- glmmTMB(Freq ~ 1, 
                 
                 ziformula= ~ 1,
                 
                 family=nbinom2, data=dat7,
                 
                 offset = log(depl_dur))

# variation explained
r.squaredGLMM(mod) # logging, species and the random effect can explain 12% of variation in frequency 

#compare r2 of each component for proportion of variation explained
mod_1 <- glmmTMB(Freq ~ logging + (1|depl_ID), 
                        
              ziformula= ~ logging,
                        
              family=nbinom2, data=dat7,
                        
              offset = log(depl_dur))

mod_2 <- glmmTMB(Freq ~ Species + (1|depl_ID), 
               
               ziformula= ~ Species,
               
               family=nbinom2, data=dat7,
               
               offset = log(depl_dur))

r2(mod_1)
r2(mod_2)

# predicted values and visualise
# all combinations of logging and Species
newd <- as.data.frame(cbind(unique(dat7[, c("logging","Species")]), depl_ID = "B10-2-16_20120806", Freq = 0, depl_dur = 1))
rownames(newd) <- NULL

# then call predict
pred <- predict(mod, newd, re.form = ~0, type = "response", se.fit = TRUE)

newd <- transform(newd,
                  fit = pred$fit,
                  se = pred$se.fit,
                  fit.up = pred$fit + pred$se.fit,
                  fit.down = pred$fit - pred$se.fit)

# plot #1 colour
plot1_col <- ggplot(dat7, aes(x = Species, y = Freq/depl_dur, fill = logging)) +
               geom_point(alpha = 1/3, position = position_jitterdodge(), pch = 21) +
               geom_pointrange(newd, mapping = aes(x = Species, y = fit, ymin = fit.down, ymax= fit.up),
                               size = 0.8, shape = 21, position = position_dodge(0.8)) +
               scale_fill_manual(values=c("#00AFBB", "#E7B800", "#FC4E07")) +
               scale_colour_manual(values = c("black", "black", "black")) +
               #axis
               ylab("Rate of detection (per day)") +
               xlab("") +
               theme(axis.ticks = element_blank(),
                     panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     legend.position = c(0.9, 0.8), legend.title=element_blank()) +
               scale_x_discrete(breaks=unique(dat7$Species), 
                                labels=addline_format(c("Bearded Pig", "Lesser Mouse-deer", "Malayan Porcupine",
                                                        "Pig-tailed Macaque", "Red Muntjac", "Sambar Deer")))

# plot #2 colour
addline_format <- function(x,...){
  gsub('\\s','\n',x)
}

plot2_col <- ggplot(dat7, aes(x = Species, y = Freq/depl_dur, fill = logging)) +
               geom_pointrange(newd, mapping = aes(x = Species, y = fit, ymin = fit.down, ymax= fit.up),
                               size = 0.8, shape = 21, position = position_dodge(0.8)) +
               scale_fill_manual(values=c("#00AFBB", "#E7B800", "#FC4E07")) +
               scale_colour_manual(values = c("black", "black", "black")) +
               
               #axis
               ylab("Rate of detection (per day)") +
               xlab("") +
               theme(axis.ticks = element_blank(),
                     panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     legend.position = "none") +
               scale_x_discrete(breaks=unique(dat7$Species), 
                                labels=addline_format(c("Bearded Pig", "Lesser Mouse-deer", "Malayan Porcupine",
                                                        "Pig-tailed Macaque", "Red Muntjac", "Sambar Deer"))) +
               scale_y_continuous(breaks = c(0.02, 0.04, 0.06, 0.08, 0.1, 0.12, 0.14, 0.16, 0.18, 0.2)) +
  
               #signif stars
               geom_signif(stat="identity",
                           data=data.frame(x=c(2), xend=c(2.27),
                                           y=c(0.02), annotation=c("*"),
                                           logging = c("during logging", "post-logging")),
                                           aes(x=x,xend=xend, y=y, yend=y, annotation=annotation)) +
               geom_signif(stat="identity",
                           data=data.frame(x=c(3.73, 4), xend=c(4, 4.27),
                                           y=c(0.06, 0.07), annotation=c("*", "*"),
                                           logging = c("pre-logging", "during logging"), c("during logging", "post-logging")),
                           aes(x=x,xend=xend, y=y, yend=y, annotation=annotation)) +
               
               geom_signif(stat="identity",
                           data=data.frame(x=c(5.73), xend=c(6.27),
                                           y=c(0.06), annotation=c("**"),
                                           logging = c("pre-logging", "post-logging")),
                           aes(x=x,xend=xend, y=y, yend=y, annotation=annotation))

ggarrange(plot1_col, plot2_col, ncol = 1, nrow = 2, labels = c("A", "B"))

# plot #1 greyscale
plot1_grey <- ggplot(dat7, aes(x = Species, y = Freq/depl_dur, fill = logging)) +
                geom_point(alpha = 1/6, position = position_jitterdodge(), pch = 21) +
                geom_pointrange(newd, mapping = aes(x = Species, y = fit, ymin = fit.down, ymax= fit.up),
                                size = 0.8, shape = 21, position = position_dodge(0.8)) +
                scale_fill_manual(values=c("black", "white", "dark grey")) +
                scale_colour_manual(values = c("black", "black", "black")) +
                #axis
                ylab("Rate of detection (per day)") +
                xlab("") +
                theme(axis.ticks = element_blank(),
                      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                      legend.position = c(0.9, 0.8), legend.title=element_blank()) +
                scale_x_discrete(breaks=unique(dat7$Species), 
                                 labels=addline_format(c("Bearded Pig", "Lesser Mouse-deer", "Malayan Porcupine",
                                                         "Pig-tailed Macaque", "Red Muntjac", "Sambar Deer")))

# plot #2 greyscale
addline_format <- function(x,...){
  gsub('\\s','\n',x)
}

plot2_grey <- ggplot(dat7, aes(x = Species, y = Freq/depl_dur, fill = logging)) +
                geom_pointrange(newd, mapping = aes(x = Species, y = fit, ymin = fit.down, ymax= fit.up),
                                size = 0.8, shape = 21, position = position_dodge(0.8)) +
                scale_fill_manual(values=c("black", "white", "dark grey")) +
                scale_colour_manual(values = c("black", "black", "black")) +
                
                #axis
                ylab("Rate of detection (per day)") +
                xlab("") +
                theme(axis.ticks = element_blank(),
                      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                      legend.position = "none") +
                scale_x_discrete(breaks=unique(dat7$Species), 
                                 labels=addline_format(c("Bearded Pig", "Lesser Mouse-deer", "Malayan Porcupine",
                                                         "Pig-tailed Macaque", "Red Muntjac", "Sambar Deer"))) +
                scale_y_continuous(breaks = c(0.02, 0.04, 0.06, 0.08, 0.1, 0.12, 0.14, 0.16, 0.18, 0.2)) +
                
                #signif stars
                geom_signif(stat="identity",
                            data=data.frame(x=c(2), xend=c(2.27),
                                            y=c(0.02), annotation=c("*"),
                                            logging = c("during logging", "post-logging")),
                            aes(x=x,xend=xend, y=y, yend=y, annotation=annotation)) +
                geom_signif(stat="identity",
                            data=data.frame(x=c(3.73, 4), xend=c(4, 4.27),
                                            y=c(0.06, 0.07), annotation=c("*", "*"),
                                            logging = c("pre-logging", "during logging"), c("during logging", "post-logging")),
                            aes(x=x,xend=xend, y=y, yend=y, annotation=annotation)) +
                
                geom_signif(stat="identity",
                            data=data.frame(x=c(5.73), xend=c(6.27),
                                            y=c(0.06), annotation=c("**"),
                                            logging = c("pre-logging", "post-logging")),
                            aes(x=x,xend=xend, y=y, yend=y, annotation=annotation))

ggarrange(plot1_grey, plot2_grey, ncol = 1, nrow = 2, labels = c("A", "B"))


# Tables of coefficients
# https://texviewer.herokuapp.com/

# model summary
ss <- summary(mod)
pxt <- function(x,title) {
  12
  cat(sprintf("{\n\n\\textbf{%s}\n\\ \\\\\\vspace{2pt}\\ \\\\\n",title))
  print(xtable(x), floating=FALSE); cat("\n\n")
  cat("\\ \\\\\\vspace{5pt}\\ \\\\\n")
}


pxt(lme4::formatVC(ss$varcor$cond),"random effects variances")
pxt(coef(ss)$cond,"conditional fixed effects")
pxt(coef(ss)$zi,"conditional zero-inflation effects")

# emeans
xtable(em$emmeans)
xtable(em$contrasts)

#anova
xtable(ao)


##################################################
# PROBABILITY OF BEING SEEN IN A GROUP
###################################################
dat7 %>%
  group_by(Species, logging) %>%
  summarise(Group = sum(Group))

# remove rows where freq = 0, to avoid NAs (0/0)
dat8 <- dat7[dat7$Freq != 0,]

# Rates
p <- bwplot((Group/depl_dur)/(Freq/depl_dur) ~ logging | Species, data=dat8, scale=list(y=list(relation='free')))
print(p)

# subsets
pigs <- dat8[dat8$Species == "Bearded Pig",]
porcs <- dat8[dat8$Species == "Malayan Porcupine",]

pigsnporcs <- dat8[dat8$Species == "Bearded Pig" | dat8$Species == "Malayan Porcupine",]


# distribution
ggplot(pigsnporcs, aes(x = Group/Freq, fill = logging)) +
  geom_histogram() +
  facet_wrap(~Species) +
  ylab("Count")

pigs.mod <- lm(Group/Freq ~ logging, data = pigs)

summary(pigs.mod)

porcs.mod <- lm(Group/Freq ~ logging, data = porcs)

summary(porcs.mod)


pigs.em <- emmeans(pigs.mod, pairwise ~ logging)
plot(pigs.em, comparisons = TRUE, ylab = "")

porcs.em <- emmeans(porcs.mod, pairwise ~ logging)
plot(porcs.em, comparisons = TRUE, ylab = "")

# plot together
pigsnporcs.mod <- lm(Group/Freq ~ Species * logging, data = pigsnporcs)
pigsnporcs.em <- emmeans(pigsnporcs.mod, pairwise ~ logging | Species)

q <- plot(pigsnporcs.em,
          plotit = FALSE)

# col
ggplot(q, aes(x = Species, fill = logging)) +
  geom_pointrange(q, mapping = aes(y = the.emmean, ymin = lower.CL, ymax= upper.CL),
                  size = 0.8, shape = 21, position = position_dodge(0.8)) +
  scale_fill_manual(values=c("#00AFBB", "#E7B800", "#FC4E07")) +
  scale_colour_manual(values = c("black", "black", "black")) +
  
  #axis
  ylab("Probability of being detected in a group") +
  xlab("") +
  theme(axis.ticks = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = c(0.87, 0.8), legend.title=element_blank()) +
  scale_y_continuous(limits = c(-0.05, 1), breaks = c(0.00, 0.25, 0.50, 0.75, 1.00)) +
  
  #signif stars
  geom_signif(stat="identity",
              data=data.frame(x=c(0.73, 0.73, 1), xend=c(1, 1.27, 1.27),
                              y=c(0.79, 0.87, 0.82), annotation=c("*", "*", "**"),
                              logging=c("pre-logging", "during logging", "post-logging")),
              aes(x=x, xend=xend, y=y, yend=y, annotation=annotation))
  #geom_signif(stat="identity",
   #           data=data.frame(x=c(1.73, 2), xend=c(2.27, 2.27),
    #                          y=c(-0.043, 0.27), annotation=c("**", "**"),
     #                         logging = c("pre-logging", "during logging")),
      #        aes(x=x,xend=xend, y=y, yend=y, annotation=annotation))

# grey
ggplot(q, aes(x = Species, fill = logging)) +
  geom_pointrange(q, mapping = aes(y = the.emmean, ymin = lower.CL, ymax= upper.CL),
                  size = 0.8, shape = 21, position = position_dodge(0.8)) +
  scale_fill_manual(values=c("black", "white", "dark grey")) +
  scale_colour_manual(values = c("black", "black", "black")) +
  
  #axis
  ylab("Probability of being detected in a group") +
  xlab("") +
  theme(axis.ticks = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = c(0.87, 0.8), legend.title=element_blank()) +
  scale_y_continuous(limits = c(-0.05, 1), breaks = c(0.00, 0.25, 0.50, 0.75, 1.00)) +
  
  #signif stars
  geom_signif(stat="identity",
              data=data.frame(x=c(0.73, 0.73, 1), xend=c(1, 1.27, 1.27),
                              y=c(0.79, 0.87, 0.82), annotation=c("*", "*", "**"),
                              logging=c("pre-logging", "during logging", "post-logging")),
              aes(x=x, xend=xend, y=y, yend=y, annotation=annotation))
#geom_signif(stat="identity",
#           data=data.frame(x=c(1.73, 2), xend=c(2.27, 2.27),
#                          y=c(-0.043, 0.27), annotation=c("**", "**"),
#                         logging = c("pre-logging", "during logging")),
#        aes(x=x,xend=xend, y=y, yend=y, annotation=annotation))

# checking adult/juvenile ratios
group_pre <- subset(dat6, Group == 1 & logging == "pre-logging" & Keyword_1 == "Malayan Porcupine")
group_dur <- subset(dat6, Group == 1 & logging == "during logging" & Keyword_1 == "Malayan Porcupine")
group_post <- subset(dat6, Group == 1 & logging == "post-logging" & Keyword_1 == "Malayan Porcupine")
#pre
idx.juv <- grepl("Juvenile", group_pre$Keyword_11)
summary(idx.juv)

idx.adult <- grepl("Adult", group_pre$Keyword_11)
summary(idx.adult)
#dur
idx.juv <- grepl("Juvenile", group_dur$Keyword_11)
summary(idx.juv)

idx.adult <- grepl("Adult", group_dur$Keyword_11)
summary(idx.adult)
#post
idx.juv <- grepl("Juvenile", group_post$Keyword_11)
summary(idx.juv)

idx.adult <- grepl("Adult", group_post$Keyword_11)
summary(idx.adult)


##################################################
# ACTIVITY
###################################################

# time spent active

# time formats
dat6$t <- strftime(dat6$DateTime, format = "%H:%M:%S")
dat6$tt <- as.POSIXct(dat6$t, format="%H:%M:%S")
dat6$dt <- (as.numeric(as.POSIXct(paste("2014-01-01", dat6$t))) - 
        as.numeric(as.POSIXct("2014-01-01 0:0:0")))/60/60/24

# test if pig activity is significantly different at logging periods
tpigpre <- 2*pi*dat6$dt[dat6$Keyword_1=="Bearded Pig" & dat6$logging == "pre-logging"]
tpigdur <- 2*pi*dat6$dt[dat6$Keyword_1=="Bearded Pig" & dat6$logging == "during logging"]
tpigpost <- 2*pi*dat6$dt[dat6$Keyword_1=="Bearded Pig" & dat6$logging == "post-logging"]

# default boostrap reps is 999
fpigpre <- fitact(tpigpre, sample="data")
fpigdur <- fitact(tpigdur, sample="data")
fpigpost <- fitact(tpigpost, sample="data")

fpigpre@act
fpigdur@act
fpigpost@act

compareAct(list(fpigpre, fpigdur))
compareAct(list(fpigpre, fpigpost))
compareAct(list(fpigdur, fpigpost))

# test if porcupine activity is significantly different at logging periods
tporcpre <- 2*pi*dat6$dt[dat6$Keyword_1=="Malayan Porcupine" & dat6$logging == "pre-logging"]
tporcdur <- 2*pi*dat6$dt[dat6$Keyword_1=="Malayan Porcupine" & dat6$logging == "during logging"]
tporcpost <- 2*pi*dat6$dt[dat6$Keyword_1=="Malayan Porcupine" & dat6$logging == "post-logging"]

# default boostrap reps is 999
fporcpre <- fitact(tporcpre, sample="data")
fporcdur <- fitact(tporcdur, sample="data")
fporcpost <- fitact(tporcpost, sample="data")

fporcpre@act
fporcdur@act
fporcpost@act

compareAct(list(fporcpre, fporcdur))
compareAct(list(fporcpre, fporcpost))
compareAct(list(fporcdur, fporcpost))

# plot 
par(mfrow = c(3,2))
plot(fpigpre)
plot(fporcpre)
plot(fpigdur)
plot(fporcdur)
plot(fpigpost)
plot(fporcpost)


# visualise activity patterns

dat6$dt <- (as.numeric(as.POSIXct(paste("2014-01-01", dat6$t))) - 
              as.numeric(as.POSIXct("2014-01-01 0:0:0")))/60/60

# first do the pigs
tpigpre <- dat6$dt[dat6$Keyword_1=="Bearded Pig" & dat6$logging == "pre-logging"]
tpigdur <- dat6$dt[dat6$Keyword_1=="Bearded Pig" & dat6$logging == "during logging"]
tpigpost <- dat6$dt[dat6$Keyword_1=="Bearded Pig" & dat6$logging == "post-logging"]

# circular
tpigpre <- circular(tpigpre, units = "hours")
tpigdur <- circular(tpigdur, units = "hours")
tpigpost <- circular(tpigpost, units = "hours")

# next do the porcs
tporcpre <- dat6$dt[dat6$Keyword_1=="Malayan Porcupine" & dat6$logging == "pre-logging"]
tporcdur <- dat6$dt[dat6$Keyword_1=="Malayan Porcupine" & dat6$logging == "during logging"]
tporcpost <- dat6$dt[dat6$Keyword_1=="Malayan Porcupine" & dat6$logging == "post-logging"]

# circular
tporcpre <- circular(tporcpre, units = "hours")
tporcdur <- circular(tporcdur, units = "hours")
tporcpost <- circular(tporcpost, units = "hours")


# plot greyscale
par(mfrow = c(3,2))

#edit plot function and remove the lines
plot.modal.region <- function(x, plot.type=c('line', 'circle'), xlab=NULL, ylab=NULL, xlim=NULL, ylim=NULL, main=NULL, polygon.control=list(), ...) {
  polygon.control.default <- list(density = NULL, angle = 45, border = NULL, col = NA, lty = par("lty"), fillOddEven = FALSE)
  npc <- names(polygon.control)
  npcd <- names(polygon.control.default)
  polygon.control <- c(polygon.control, polygon.control.default[setdiff(npcd, npc)])
  plot.type <- match.arg(plot.type)
  if (is.null(xlab))
    xlab <- paste('bw=', round(x$density$bw,3), sep='')
  if (is.null(ylab))
    ylab <- 'Kernel Density Estimates'
  if (is.null(main))
    main <- 'Areas under the curve'
  plot(x$density, plot.type=plot.type, xlab=xlab, ylab=ylab, main=main, xlim=xlim, ylim=ylim, ...)
  if (plot.type=='line') {
    #abline(h=x$level, lty=2)
    #abline(v=c(x$zeros), lty=2)
    for (i in 1:nrow(x$zeros)) {
      zero1 <- x$zeros[i,1]
      zero2 <- x$zeros[i,2]
      inside <- x$density$x >= zero1 & x$density$x <= zero2
      polygon(x=c(zero2, zero1, x$density$x[inside], zero2),
              y=c(0,0,x$density$y[inside], 0),
              density = polygon.control$density,
              angle = polygon.control$angle,
              border = polygon.control$border,
              col = polygon.control$col,
              lty = polygon.control$lty,
              fillOddEven = polygon.control$fillOddEven)
      }
  } else {
    warning('Not Yet Implemented for plot.type=circle')
  }
}

# pre
# pigs
tpigpreres <- modal.region(tpigpre, bw=5, q = 0.5)
tpigpreres2 <- modal.region(tpigpre, bw=5, q = 0.95)

plot.modal.region(tpigpreres2, ylab="", xlab="", main ="pre-logging", ylim = c(0, 0.5),
     polygon.control = list(col = "light grey"))
par(new=TRUE)
plot.modal.region(tpigpreres, ylab="", xlab="", main ="", ylim = c(0, 0.5),
                  polygon.control = list(col = "black"))
#icon
imgpig<-readJPEG("C:/Users/Admin/OneDrive - Imperial College London/Research projects/Masters thesis/Writing/Figures and Tables/pig.jpg")
rasterImage(imgpig,0.1,0.25,4.1,0.45)

# porcs
tporcpreres <- modal.region(tporcpre, bw=5, q = 0.5)
tporcpreres2 <- modal.region(tporcpre, bw=5, q = 0.95)

plot.modal.region(tporcpreres2, ylab="", xlab="", main ="pre-logging", ylim = c(0, 0.5),
                  polygon.control = list(col = "light grey"))
par(new=TRUE)
plot.modal.region(tporcpreres,ylab="", xlab="", main ="", ylim = c(0, 0.5),
                  polygon.control = list(col = "black"))

#icon
imgporc<-readJPEG("C:/Users/Admin/OneDrive - Imperial College London/Research projects/Masters thesis/Writing/Figures and Tables/porc.jpg")
rasterImage(imgporc,4.5,0.27,8.5,0.42)

# dur
# pigs
tpigdurres <- modal.region(tpigdur, bw=5, q = 0.5)
tpigdurres2 <- modal.region(tpigdur, bw=5, q = 0.95)

plot.modal.region(tpigdurres2, ylab="", xlab="", main ="during logging", ylim = c(0, 0.5),
     polygon.control = list(col = "light grey", border = TRUE))
par(new=TRUE)
plot.modal.region(tpigdurres, ylab="", xlab="", main ="", ylim = c(0, 0.5),
                  polygon.control = list(col = "black"))
#icon
imgpig<-readJPEG("C:/Users/Admin/OneDrive - Imperial College London/Research projects/Masters thesis/Writing/Figures and Tables/pig.jpg")
rasterImage(imgpig,0.1,0.25,4.1,0.45)

# porcs
tporcdurres <- modal.region(tporcdur, bw=5, q = 0.5)
tporcdurres2 <- modal.region(tporcdur, bw=5, q = 0.95)

plot.modal.region(tporcdurres2, ylab="", xlab="", main ="during logging", ylim = c(0, 0.5),
                  polygon.control = list(col = "light grey", border = TRUE))
par(new=TRUE)
plot.modal.region(tporcdurres,ylab="", xlab="", main ="", ylim = c(0, 0.5),
                  polygon.control = list(col = "black"))

#icon
imgporc<-readJPEG("C:/Users/Admin/OneDrive - Imperial College London/Research projects/Masters thesis/Writing/Figures and Tables/porc.jpg")
rasterImage(imgporc,5.5,0.27,9.5,0.42)

# post
# pigs
tpigpostres <- modal.region(tpigpost, bw=5, q = 0.5)
tpigpostres2 <- modal.region(tpigpost, bw=5, q = 0.95)

plot.modal.region(tpigpostres2, ylab="", xlab="", main ="post-logging", ylim = c(0, 0.5),
     polygon.control = list(col = "light grey"))
par(new=TRUE)
plot.modal.region(tpigpostres, ylab="", xlab="", main ="", ylim = c(0, 0.5),
                  polygon.control = list(col = "black"))
#icon
imgpig<-readJPEG("C:/Users/Admin/OneDrive - Imperial College London/Research projects/Masters thesis/Writing/Figures and Tables/pig.jpg")
rasterImage(imgpig,0.1,0.25,4.1,0.45)

# porcs
tporcpostres <- modal.region(tporcpost, bw=5, q = 0.5)
tporcpostres2 <- modal.region(tporcpost, bw=5, q = 0.95)

plot.modal.region(tporcpostres2, ylab="", xlab="", main ="post-logging", ylim = c(0, 0.5),
     polygon.control = list(col = "light grey"))
par(new=TRUE)
plot.modal.region(tporcpostres,ylab="", xlab="", main ="", ylim = c(0, 0.5),
                  polygon.control = list(col = "black"))

# icon
imgporc<-readJPEG("C:/Users/Admin/OneDrive - Imperial College London/Research projects/Masters thesis/Writing/Figures and Tables/porc.jpg")
rasterImage(imgporc,5.5,0.27,9.5,0.42)

# axes labels
mtext("Hour in day", side=1, outer=TRUE, line=-2)
mtext("Density", side=2, outer=TRUE, line=-1.75)

##################################################

# plot colour
par(mfrow = c(3,2))

#edit plot function and remove the lines
plot.modal.region <- function(x, plot.type=c('line', 'circle'), xlab=NULL, ylab=NULL, xlim=NULL, ylim=NULL, main=NULL, polygon.control=list(), ...) {
  polygon.control.default <- list(density = NULL, angle = 45, border = NULL, col = NA, lty = par("lty"), fillOddEven = FALSE)
  npc <- names(polygon.control)
  npcd <- names(polygon.control.default)
  polygon.control <- c(polygon.control, polygon.control.default[setdiff(npcd, npc)])
  plot.type <- match.arg(plot.type)
  if (is.null(xlab))
    xlab <- paste('bw=', round(x$density$bw,3), sep='')
  if (is.null(ylab))
    ylab <- 'Kernel Density Estimates'
  if (is.null(main))
    main <- 'Areas under the curve'
  plot(x$density, plot.type=plot.type, xlab=xlab, ylab=ylab, main=main, xlim=xlim, ylim=ylim, ...)
  if (plot.type=='line') {
    #abline(h=x$level, lty=2)
    #abline(v=c(x$zeros), lty=2)
    for (i in 1:nrow(x$zeros)) {
      zero1 <- x$zeros[i,1]
      zero2 <- x$zeros[i,2]
      inside <- x$density$x >= zero1 & x$density$x <= zero2
      polygon(x=c(zero2, zero1, x$density$x[inside], zero2), y=c(0,0,x$density$y[inside], 0), density = polygon.control$density, angle = polygon.control$angle, border = polygon.control$border, col = polygon.control$col, lty = polygon.control$lty, fillOddEven = polygon.control$fillOddEven)
    }
  } else {
    warning('Not Yet Implemented for plot.type=circle')
  }
}

# pre
# pigs
tpigpreres <- modal.region(tpigpre, bw=10, q = 0.5)
tpigpreres2 <- modal.region(tpigpre, bw=10, q = 0.95)

plot.modal.region(tpigpreres2, ylab="", xlab="", main ="pre-logging", ylim = c(0, 0.3),
                  polygon.control = list(col = "light grey"))
par(new=TRUE)
plot.modal.region(tpigpreres, ylab="", xlab="", main ="", ylim = c(0, 0.3),
                  polygon.control = list(col = "#00AFBB"))
#icon
imgpig<-readJPEG("C:/Users/Admin/OneDrive - Imperial College London/Research projects/Masters thesis/Writing/Figures and Tables/pig.jpg")
rasterImage(imgpig,0.1,0.2,4.1,0.3)

# porcs
tporcpreres <- modal.region(tporcpre, bw=10, q = 0.5)
tporcpreres2 <- modal.region(tporcpre, bw=10, q = 0.95)

plot.modal.region(tporcpreres2, ylab="", xlab="", main ="pre-logging",
                  polygon.control = list(col = "light grey"))
par(new=TRUE)
plot.modal.region(tporcpreres,ylab="", xlab="", main ="", polygon.control = list(col = "#00AFBB"))

#icon
imgporc<-readJPEG("C:/Users/Admin/OneDrive - Imperial College London/Research projects/Masters thesis/Writing/Figures and Tables/porc.jpg")
rasterImage(imgporc,4.5,0.32,8.5,0.42)



# dur
# pigs
tpigdurres <- modal.region(tpigdur, bw=10, q = 0.5)
tpigdurres2 <- modal.region(tpigdur, bw=10, q = 0.95)

plot.modal.region(tpigdurres2, ylab="", xlab="", main ="during logging", ylim = c(0, 0.3),
                  polygon.control = list(col = "light grey", border = TRUE))
par(new=TRUE)
plot.modal.region(tpigdurres, ylab="", xlab="", main ="", ylim = c(0, 0.3),
                  polygon.control = list(col = "#E7B800"))
#icon
imgpig<-readJPEG("C:/Users/Admin/OneDrive - Imperial College London/Research projects/Masters thesis/Writing/Figures and Tables/pig.jpg")
rasterImage(imgpig,0.1,0.2,4.1,0.3)

# porcs
tporcdurres <- modal.region(tporcdur, bw=10, q = 0.5)
tporcdurres2 <- modal.region(tporcdur, bw=10, q = 0.95)

plot.modal.region(tporcdurres2, ylab="", xlab="", main ="during logging",
                  polygon.control = list(col = "light grey", border = TRUE))
par(new=TRUE)
plot.modal.region(tporcdurres,ylab="", xlab="", main ="", polygon.control = list(col = "#E7B800"))

#icon
imgporc<-readJPEG("C:/Users/Admin/OneDrive - Imperial College London/Research projects/Masters thesis/Writing/Figures and Tables/porc.jpg")
rasterImage(imgporc,5.5,0.32,9.5,0.42)


# post
# pigs
tpigpostres <- modal.region(tpigpost, bw=10, q = 0.5)
tpigpostres2 <- modal.region(tpigpost, bw=10, q = 0.95)

plot.modal.region(tpigpostres2, ylab="", xlab="", main ="post-logging", ylim = c(0, 0.3),
                  polygon.control = list(col = "light grey"))
par(new=TRUE)
plot.modal.region(tpigpostres, ylab="", xlab="", main ="", ylim = c(0, 0.3),
                  polygon.control = list(col = "#FC4E07"))
#icon
imgpig<-readJPEG("C:/Users/Admin/OneDrive - Imperial College London/Research projects/Masters thesis/Writing/Figures and Tables/pig.jpg")
rasterImage(imgpig,0.1,0.2,4.1,0.3)

# porcs
tporcpostres <- modal.region(tporcpost, bw=10, q = 0.5)
tporcpostres2 <- modal.region(tporcpost, bw=10, q = 0.95)

plot.modal.region(tporcpostres2, ylab="", xlab="", main ="post-logging",
                  polygon.control = list(col = "light grey"))
par(new=TRUE)
plot.modal.region(tporcpostres,ylab="", xlab="", main ="", polygon.control = list(col = "#FC4E07"))

# icon
imgporc<-readJPEG("C:/Users/Admin/OneDrive - Imperial College London/Research projects/Masters thesis/Writing/Figures and Tables/porc.jpg")
rasterImage(imgporc,5.5,0.32,9.5,0.42)

# axes labels
mtext("Hour in day", side=1, outer=TRUE, line=-2)
mtext("Density", side=2, outer=TRUE, line=-1.75)

# NOTES FOR FURTHER ANALYSES
# coefficient of overlap
require(overlap)
par(op)

# time formats
range(dat6$dt)

#convert to radians
dt <- dat6$dt /24 * 2 * pi

tpigpre <- dt[dat6$Keyword_1=="Bearded Pig" & dat6$logging == "pre-logging"]
tpigdur <- dt[dat6$Keyword_1=="Bearded Pig" & dat6$logging == "during logging"]
tpigpost <- dt[dat6$Keyword_1=="Bearded Pig" & dat6$logging == "post-logging"]

#overlap of core activity between logging periods

#subset core activity
prepigcore.idx <- (tpigpre > as.numeric(tpigpreres$zeros[1,1])/24 * 2 * pi & tpigpre < as.numeric(tpigpreres$zeros[1,2])/24 * 2 * pi) |
                  (tpigpre > as.numeric(tpigpreres$zeros[2,1])/24 * 2 * pi & tpigpre < as.numeric(tpigpreres$zeros[2,2])/24 * 2 * pi)
prepigcore <- as.numeric(tpigpre[prepigcore.idx])

durpigcore.idx <- (tpigdur > as.numeric(tpigdurres$zeros[1,1])/24 * 2 * pi & tpigdur < as.numeric(tpigdurres$zeros[1,2]))/24 * 2 * pi |
                  (tpigdur > as.numeric(tpigdurres$zeros[2,1])/24 * 2 * pi & tpigdur < as.numeric(tpigdurres$zeros[2,2]))/24 * 2 * pi |
                  (tpigdur > as.numeric(tpigdurres$zeros[3,1])/24 * 2 * pi & tpigdur < as.numeric(tpigdurres$zeros[3,2]))/24 * 2 * pi |
                  (tpigdur > as.numeric(tpigdurres$zeros[4,1])/24 * 2 * pi & tpigdur < as.numeric(tpigdurres$zeros[4,2]))/24 * 2 * pi |
                  (tpigdur > as.numeric(tpigdurres$zeros[5,1])/24 * 2 * pi & tpigdur < as.numeric(tpigdurres$zeros[5,2]))/24 * 2 * pi 
durpigcore <- as.numeric(tpigdur[durpigcore.idx])/24 * 2 * pi

postpigcore.idx <- (tpigpost > as.numeric(tpigpostres$zeros[1,1]) & tpigpost < as.numeric(tpigpostres$zeros[1,2]))
postpigcore <- as.numeric(tpigpost[postpigcore.idx])
    
#overlap 
overlapPlot(prepigcore, durpigcore)
overlapPlot(prepigcore, postpigcore)
overlapPlot(durpigcore, postpigcore)

predurest <- overlapEst(prepigcore, durpigcore, type = "Dhat1")
prepostest <- overlapEst(prepigcore, postpigcore, type = "Dhat1")
durpostest <- overlapEst(durpigcore, postpigcore, type = "Dhat1")
overlapTrue(overlapEst(prepigcore, durpigcore))



#bootstrap for CI. 10,000 reps
predurBS <- bootstrap(prepigcore, durpigcore, 1000, type="Dhat1")
prepostBS <- bootstrap(prepigcore, postpigcore, 1000, type="Dhat1")
durpostBS <- bootstrap(durpigcore, postpigcore, 1000, type="Dhat1")

mean(predurBS)
mean(prepostBS)
mean(durpostBS)

#extract CI from norm0
bootCI(predurest, predurBS)
bootCI(prepostest, prepostBS)
bootCI(durpostest, durpostBS)

#
watson.two.test(prepigcore, durpigcore)
watson.two.test(prepigcore, postpigcore)
watson.two.test(durpigcore, postpigcore)
