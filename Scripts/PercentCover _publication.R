##This script calculates and graphs cover by spps over time
##Alec Kretchun, PSU, 2017

#Read things, set up directories
library(raster)
library(ggplot2)
library(plotly)

w.dir <- "K:/NWCSC_Idaho/LANDIS_outputs/"
gcm.list <- list.dirs(w.dir, recursive = FALSE, full.names = FALSE)
gcm.list <- gcm.list[1:4] #Removing GFDL
em.list <- rep(c("low", "high"), 3)
reps <- 1:5 #replicate numbers

#Read LANDIS-II biomass output maps
timesteps <- seq(5, 100, 5)
spps <- c("juniocci", "poputrem", "pseumenz")
spps.select <- spps[2] ##Manually change species

##ecoregion maps for snowbanks analysis
ecos.map <- raster("I:/NWCSC-Idaho/LANDIS_modeling/RCEW/RCEW_nolowsnow.img")
ecos.df <- as.data.frame(ecos.map)

#spp.df.replicate = matrix(ncol=length(reps), nrow=20)
reps.dat <- NULL
spp.df.replicates <- NULL
perc.df.all <- NULL
scen.reps <- vector(length=20)

snow.reps.dat <- NULL
snow.spp.df.replicates <- NULL
snow.scen.reps <- vector(length=20)

#gcm.select <- gcm.list[1]
##Loop to convert rasters to vectors and create dataframe of all timesteps

for (k in 1:length(gcm.list)){
  gcm.select <- gcm.list[k] 
  em.select <- em.list[k]
  for (i in 1:length(reps)){
    rep <- reps[i]
    for (j in 1:length(timesteps)){
      file.loc <- paste(w.dir, gcm.select, "/replicate", rep, "/max-spp-age/", spps.select, "-", timesteps[j], ".img", sep="") 
      step.raster <- raster(file.loc)
      step.df <- as.data.frame(step.raster)
      step.df <- as.vector(step.df[,1])
      #spp.df.replicate[,i] <- step.df ##dataframe of all timesteps (columns)
      step.df[step.df > 0] <- 1
      step.count <- sum(step.df)
      step.perc <- (step.count/546383) * 100 #finding area cover %. there are 546383 sites
      scen.reps[j] <- step.perc
      reps.dat <- cbind.data.frame(em.select, gcm.select, rep, 1:20, scen.reps)
      
      ##isolating snowbanks
      snow.step <- cbind(step.df, ecos.df)
      snow.step.noNA <- snow.step[!is.na(snow.step[,2]),]
      snow.only.step <- snow.step.noNA[snow.step.noNA[,2] == 401,]
      snow.count <- sum(snow.only.step[,1])
      snow.perc <- (snow.count/62363) * 100 #finding area cover %. there are 62363 sites
      snow.scen.reps[j] <- snow.perc
      snow.reps.dat <- cbind.data.frame(em.select, gcm.select, rep, 1:20, snow.scen.reps)
      
    }
    spp.df.replicates <- rbind.data.frame(spp.df.replicates, reps.dat)
    snow.spp.df.replicates <- rbind.data.frame(snow.spp.df.replicates, snow.reps.dat)
  }  
}
rm(k)
rm(i)
rm(j)

colnames(spp.df.replicates) <- c("em_scen", "gcm", "replicate", "timestep", "percent_cover")
colnames(snow.spp.df.replicates) <- c("em_scen", "gcm", "replicate", "timestep", "percent_cover")

p <- ggplot(spp.df.replicates) + geom_line(aes(x=timestep, y=percent_cover, group = gcm, colour = gcm))
                            
p

##Need to reorient dataframe into em_scen, timestep, min, max, mean
percents <- spp.df.replicates$percent_cover 
em_scens <- spp.df.replicates$em_scen
timesteps <- spp.df.replicates$timestep
  
em.df.mean <- tapply(percents, list(em_scens, timesteps), mean)
em.df.mean <- t(em.df.mean)
em.df.mean <- c(as.vector(em.df.mean[,1]), as.vector(em.df.mean[,2]))
em.df.min <- tapply(percents, list(em_scens, timesteps), min)
em.df.min <- t(em.df.min)
em.df.min <- c(as.vector(em.df.min[,1]), as.vector(em.df.min[,2]))
em.df.max <- tapply(percents, list(em_scens, timesteps), max)
em.df.max <- t(em.df.max)
em.df.max <- c(as.vector(em.df.max[,1]), as.vector(em.df.max[,2]))

em.df.full <- cbind.data.frame(rep(c("RCP45", "RCP85"), each=20), rep(1:20, 2), em.df.mean, em.df.min, em.df.max)
colnames(em.df.full) <- c("Emissions", "timestep", "percent_mean", "percent_min", "percent_max")

#graphing landscape
plot.cols <- c("blue", "chocolate4")
#Graphing new structure
p2 <- ggplot(em.df.full, aes(x=timestep, y=percent_mean, group = Emissions, colour = Emissions)) +
  geom_ribbon(aes(ymin=percent_min, ymax=percent_max, fill=Emissions), alpha=0.3) + #
  scale_fill_manual(values=c(RCP45=plot.cols[1],RCP85=plot.cols[2]))+
  geom_line(aes(size=Emissions), show.legend = TRUE) +
  scale_size_manual(values = c(1, 1)) +
  scale_color_manual(values=plot.cols) +
  theme_bw() +
  #theme(panel.background = element_rect(fill = 'lightgrey', colour = 'black')) +
  labs(x = "Time step", y="Percent cover") +
  ggtitle("Populus tremuloides")

p2

#write.csv(em.df.full, "J:/NWCSC-Idaho/Analysis/OutputDataSets/AspenOccupancy_landscape.csv")


##replicating for snowbanks
percents <- snow.spp.df.replicates$percent_cover 
em_scens <- snow.spp.df.replicates$em_scen
timesteps <- snow.spp.df.replicates$timestep

em.df.mean <- tapply(percents, list(em_scens, timesteps), mean)
em.df.mean <- t(em.df.mean)
em.df.mean <- c(as.vector(em.df.mean[,1]), as.vector(em.df.mean[,2]))
em.df.min <- tapply(percents, list(em_scens, timesteps), min)
em.df.min <- t(em.df.min)
em.df.min <- c(as.vector(em.df.min[,1]), as.vector(em.df.min[,2]))
em.df.max <- tapply(percents, list(em_scens, timesteps), max)
em.df.max <- t(em.df.max)
em.df.max <- c(as.vector(em.df.max[,1]), as.vector(em.df.max[,2]))

em.df.full <- cbind.data.frame(rep(c("RCP45", "RCP85"), each=20), rep(1:20, 2), em.df.mean, em.df.min, em.df.max)
colnames(em.df.full) <- c("Emissions", "timestep", "percent_mean", "percent_min", "percent_max")

##graphing. this is the same as above but im being real lazy
p2 <- ggplot(em.df.full, aes(x=timestep, y=percent_mean, group = Emissions, colour = Emissions)) +
  geom_ribbon(aes(ymin=percent_min, ymax=percent_max, fill=Emissions), alpha=0.3) + #
  scale_fill_manual(values=c(RCP45=plot.cols[1],RCP85=plot.cols[2]))+
  geom_line(aes(size=Emissions), show.legend = TRUE) +
  scale_size_manual(values = c(1, 1)) +
  scale_color_manual(values=plot.cols) +
  theme_bw() +
  #theme(panel.background = element_rect(fill = 'lightgrey', colour = 'black')) +
  labs(x = "Time step", y="Percent cover") +
  ggtitle("Populus tremuloides")

p2

write.csv(em.df.full, "J:/NWCSC-Idaho/Analysis/OutputDataSets/AspenOccupancy_snowbank.csv")
