##This script graphs probabilities of establishment and CWD 
##Import CWD vslues from 'Zonal cimate formatter.R'
##Alec Kretchun, 2017, PSU

library(FIACH)
library(ggplot2)
library(qlcMatrix)

##Read things, set up directories
out.dir <- "I:/NWCSC-Idaho/LANDIS_modeling/RCEW/R_derived_inputs/"
pest.dir <- "I:/NWCSC-Idaho/Data/RCEW/BCM/RCEW/Futures_monthly/EcoregionSummaries/Pests/Correction_25/" ##NEED TO MANUALLY UPDATE
gcm.list <- list.files(pest.dir, full.names = TRUE)

gcms <- c("access", "canesm", "gfdl")
em.scens <- c("45", "85")
spps <- c("juniper", "aspen", "douglasfir") #Change order to match old oerder?
landis.spps <- c("juniocci", "poputrem", "pseumenz")

template <- read.table("I:/NWCSC-Idaho/LANDIS_modeling/RCEW/BiomassDynamicInputs_gfdl_esm2m_rcp45.txt", skip=5)
#eco.names <- c(1, 2, 3, 4, 10, 20, 30, 40, 100, 200, 300, 400, 401)
eco.names <- letters[1:13] ##had to use letters for sorting later on
eco.list.one.spp <- rep(eco.names, 94)

##Extract Pest for each spps from target GCM from csvs
target.gcm.name <- paste(gcms[3], em.scens[2], sep="") ##NEED TO MANUALLY UPDATE FOR EACH GCM AND EM SCENARIO
target.gcm.names <- grep(target.gcm.name, gcm.list, value=TRUE) 

plot.cols <- c("darkgreen", "coral", "purple")

spps.pests <- NULL
for (i in 1:length(spps)){
  spp.select <- grep(spps[i], target.gcm.names, value=TRUE)    
  spp.dat <- read.csv(spp.select)  
  spp.dat <- spp.dat[,2:14]
  colnames(spp.dat) <- eco.names
  spp.dat.t <- as.data.frame(t(spp.dat)) ##Re-arranging dataframe
  spp.dat.unlist <- unlist(spp.dat.t) ##turning dataframe into vector
  spp.pests <- cbind(as.character(eco.list.one.spp), landis.spps[i],spp.dat.unlist)
  
  ##Averaging by ecoregions and plotting
  time.pests <- cbind(rep(1:94, each = 13), spp.pests)
  eco.avg.pest <- tapply(as.numeric(time.pests[,4]), time.pests[,1], mean)
  par(mar = c(5,4,4,5))
  plot(eco.avg.pest, type="l", lwd = 2, ylim = c(0, 2.0), col = plot.cols[i], 
       main = "High emissions climate", xlab = "Year", xaxt="n", yaxt="n", ylab="") #changed i to 2
  axis(1, at = c(0, 24, 49, 74, 93), labels = c(2006, 2030, 2055, 2080, 2100))
  par(new=TRUE, mar = c(5,4,4,5))
  
  ##CReating data frame of all species/ecoregions/timesteps
  spps.pests <- rbind(spps.pests, spp.pests)
}
rm(i)
species.names <- c("J. occidentalis", "P. tremuloides", "P. menziesii")
axis(side=2, at = c(0, .5, 1), labels=c(0.0, 0.5, 1.0))
mtext(side=2, line= 2, expression('Annual avg P'[est]))

par(new=TRUE)
# plot(cwd.annual.avg, type="l", ylim = c(100, 900), lwd=1.5, 
#      axes=F, xlab=NA, ylab=NA, col="black")
# axis(side=4)
# mtext(side=4, line= 3, "Annual CWD (mm)")
# legend("topleft", legend =  species.names, fill=plot.cols)

plot(pmort.average, type="l", ylim = c(-1,1), lwd=2, 
     axes=F, xlab=NA, ylab=NA, col="black")
axis(side=4, at = c(0, .5, 1), labels=c(0.0, 0.5, 1.0))
mtext(side=4, line= 2, expression('Annual avg P'[mort]))
legend("topleft", legend =  species.names, fill=plot.cols)

mean(pmort.average[84:94])

##Getting index of cwd/pest
aspen.pests <- subset(spps.pests, spps.pests[,2] == "poputrem")

time.pests <- cbind(rep(1:94, each = 13), aspen.pests) #averaging by ecoregion
eco.avg.pest <- tapply(as.numeric(time.pests[,4]), time.pests[,1], mean)

regen.index <- pmort.average/eco.avg.pest


#write.table(regen.index, file = paste("J:/NWCSC-Idaho/Data/PestCWDIndex/regen_index_", target.file.name, ".txt", sep=""),
#            col.names = FALSE, row.names = FALSE, quote=FALSE)

##Selecting out for snowbank ecoregion
snowbank.pests.select <- subset(aspen.pests, aspen.pests[,1] == "m")
snowbank.pests <- as.numeric(snowbank.pests.select[,3])
snowbank.pmorts <- pmorts[,13]

plot(snowbank.pests, type="l")
lines(eco.avg.pest, col="red")

##writing pest and pmort for snowbanks
write.table(snowbank.pests, file = paste("J:/NWCSC-Idaho/Data/PestCWDIndex/snowbank_pest_", target.file.name, ".txt", sep=""),
            col.names = FALSE, row.names = FALSE, quote=FALSE)

write.table(snowbank.pmorts, file = paste("J:/NWCSC-Idaho/Data/PestCWDIndex/snowbank_pmort_", target.file.name, ".txt", sep=""),
            col.names = FALSE, row.names = FALSE, quote=FALSE)

regen.index.snow <- pmorts[,13]/snowbank.pests #pmorts is from Zonal_climate_formatter


#write.table(regen.index.snow, file = paste("J:/NWCSC-Idaho/Data/PestCWDIndex/snowbank_regen_index_", target.file.name, ".txt", sep=""),
#            col.names = FALSE, row.names = FALSE, quote=FALSE)

### I exported all of these individual climate indexes into an external csv called Pest_CWD_index.csv ##
## landscape
regen.index.dat <- read.csv("J:/NWCSC-Idaho/Data/PestCWDIndex/Pest_CWD_index.csv")
regen.index.means <- rowMeans(regen.index.dat[,c(1,3,5)])
regen.index.sd <- rowsd(regen.index.dat)
regen.index.max <- rowMax(as.matrix(regen.index.dat))
regen.index.min <- rowMin(as.matrix(regen.index.dat))

regen.df <- cbind.data.frame(c(1:94),"Landscape", regen.index.means, as.vector(regen.index.max), as.vector(regen.index.min))
colnames(regen.df) <- c("Year","Area", "Mean", "Min", "Max")

##snowbank

regen.index.snow.dat <- read.csv("J:/NWCSC-Idaho/Data/PestCWDIndex/Pest_CWD_index_snowbank.csv")
regen.index.snow.dat[is.na(regen.index.snow.dat)] <- 0
regen.index.snow.dat[(regen.index.snow.dat==Inf)] <- 0

regen.index.snow.means <- rowMeans(regen.index.snow.dat[,c(4,6)]) #regen.index.snow.dat[,] 
regen.index.snow.sd <- rowsd(regen.index.snow.dat)
regen.index.snow.max <- rowMax(as.matrix(regen.index.snow.dat))
regen.index.snow.min <- rowMin(as.matrix(regen.index.snow.dat))

regen.df.snow <- cbind.data.frame(c(1:94),"Snowbanks", as.vector(regen.index.snow.means), as.vector(regen.index.snow.max), as.vector(regen.index.snow.min))
colnames(regen.df.snow) <- c("Year","Area", "Mean", "Min", "Max")

regen.df.all <- rbind(regen.df, regen.df.snow)


##PLotting it
plot.cols <- c("blue", "chocolate4")

eb <- aes(ymax = regen.df.all$Mean ,
          ymin = regen.df.all$Min)

ggplot(data = regen.df.all, aes(x = Year, y = Mean, colour=Area)) + 
  geom_line(size = 2) + 
  #geom_ribbon(eb, alpha = 0.2) +
  #theme(panel.background = element_rect(fill = 'lightgrey', colour = 'black')) +
  labs(y = "Mean Probability Index", x = "Time step") +
  ggtitle("Probability index (Pmort/Pest) over time - RCP8.5")+
  scale_color_manual(values=plot.cols) +
  theme_bw()

