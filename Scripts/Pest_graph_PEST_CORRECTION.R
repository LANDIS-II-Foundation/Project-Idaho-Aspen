##This script graphs probabilities of establishment and CWD 
##Import CWD vslues from 'Zonal cimate formatter.R'
##Alec Kretchun, 2017, PSU

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
target.gcm.name <- paste(gcms[3], em.scens[1], sep="") ##NEED TO MANUALLY UPDATE FOR EACH GCM AND EM SCENARIO
target.gcm.names <- grep(target.gcm.name, gcm.list, value=TRUE) 

plot.cols <- c("darkgreen", "coral", "purple")

eco.avg.pests <- NULL
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
  eco.avg.pest <- eco.avg.pest/3
  eco.avg.pests <- cbind(eco.avg.pests, eco.avg.pest)
  print(max(eco.avg.pest))
  par(mar = c(5,4,4,5))
  plot(eco.avg.pest, type="l", lwd = 2, ylim = c(0, 1.0), col = plot.cols[i], 
       main = " ", xlab = "Year", xaxt="n", yaxt="n", ylab="")
  axis(1, at = c(0, 24, 49, 74, 93), labels = c(2006, 2030, 2055, 2080, 2100))
  par(new=TRUE, mar = c(5,4,4,5))
  
  ##CReating data frame of all species/ecoregions/timesteps
  spps.pests <- rbind(spps.pests, spp.pests)
}
rm(i)

#caclcaulting avg beginnig period Pest and end period Pest
beg.pests <- colMeans(eco.avg.pests[9:19,])
end.pests <- colMeans(eco.avg.pests[84:94,])

print(beg.pests)
print(end.pests)

species.names <- c("J. occidentalis", "P. tremuloides", "P. menziesii")
axis(side=2, at = c(0, .5, 1), labels=c(0.0, 0.5, 1.0))
mtext(side=2, line= 2, expression('Annual avg P'[est]))
legend("topleft", legend =  species.names, fill=plot.cols)





