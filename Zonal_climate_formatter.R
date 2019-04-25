##This script manipulates and summarizes zonal climate data that was produced by 'Ecoregion_climate_ZOnalStats.R'
##It will be used for various climate-related input purposes in LANDIS-II
##Alec Kretchun, PSU, 2016
library(zoo)

#Set up directories, read things
w.dir <- "I:/NWCSC-Idaho/Data/RCEW/BCM/RCEW/Futures_monthly/EcoregionSummaries/AllBCMvariables/"
climate.files <- list.files(w.dir)
climate.vars <- c("cwd", "pck", "ppt", "tmn", "tmx") #Climate variable shorthand found in climate files
years <- 2006:2099 ##first and last years are incomplete in the raw climate data
years.rep <- rep(years, each=12)
ecoregions <- c(1, 2, 3, 4, 10, 20, 30, 40, 100, 200, 300, 400, 401)

#Reading in data
target.file.name <- climate.files[6] #Manually selecting target file right now
climate.dat <- read.csv(paste(w.dir, target.file.name, sep="")) #Reading in climate csv
cwd.dat <- climate.dat[grep(climate.vars[1], climate.dat[,1]),] ##extracting single climate variable
cwd.rows <- unique(grep(paste(years, collapse="|"), cwd.dat[,1], value = FALSE))
cwd.dat.complete <- cwd.dat[cwd.rows, ] ##Removing incomplete years
##ppt
ppt.dat <- climate.dat[grep(climate.vars[3], climate.dat[,1]),] ##extracting single climate variable
ppt.rows <- unique(grep(paste(years, collapse="|"), ppt.dat[,1], value = FALSE))
ppt.dat.complete <- ppt.dat[ppt.rows, ] ##Removing incomplete years

##temp
maxtemp.dat <- climate.dat[grep(climate.vars[5], climate.dat[,1]),] ##extracting single climate variable
#mintemp.dat <- climate.dat[grep(climate.vars[4], climate.dat[,1]),]
#temp.dat <- (maxtemp.dat+mintemp.dat)/2
maxtemp.rows <- unique(grep(paste(years, collapse="|"), maxtemp.dat[,1], value = FALSE))
maxtemp.dat.complete <- maxtemp.dat[maxtemp.rows, ] ##Removing incomplete years


cwd.no.years <- cwd.dat.complete[,2:14]
ppt.no.years <- ppt.dat.complete[,2:14]
maxtemp.no.years <- maxtemp.dat.complete[,2:14]
plot.cols <- rainbow(13)

cwd.limit <- 700 #this is for calculating 

#Getting annual averages of CWD
cwd.means <- NULL
ppt.means <- NULL
maxtemp.means <- NULL

for(j in (1:length(ecoregions))){
  cwd.ecotarget <- cwd.no.years[,j]
  cwd.annual.avg <- tapply(cwd.ecotarget, years.rep, sum)
  cwd.means <- cbind(cwd.means, cwd.annual.avg)
  plot(cwd.annual.avg, type="l", ylim = c(0, 800), col = plot.cols[j], ylab = "Monthly cumulative CWD", xaxt="n")
  #plot(cwd.annual.avg, type="l", ylim = c(0, 1200), col = plot.cols[j], ylab = "Annual CWD", xaxt="n")
  #axis(1, at = c(0, 25, 50, 75, 94), labels = c(2006, 2031, 2056, 2081, 2100))
  #par(new=TRUE)
  ##ppt
  ppt.ecotarget <- ppt.no.years[,j]
  ppt.annual.avg <- tapply(ppt.ecotarget, years.rep, mean)
  plot(ppt.annual.avg, type="l", ylim = c(0, 200), col = plot.cols[j], ylab = "Monthly average ppt", xaxt="n")
  axis(1, at = c(0, 25, 50, 75, 94), labels = c(2006, 2031, 2056, 2081, 2100))
  
  maxtemp.ecotarget <- maxtemp.no.years[,j]
  maxtemp.annual.avg <- tapply(maxtemp.ecotarget, years.rep, mean)
  
}

legend("topleft", legend =  ecoregions, fill=plot.cols, ncol=6)
rm(j)

cor(ppt.annual.avg, cwd.annual.avg)

##Trying out rolling mean function. Incorporate this in to the loop above. 
# cwd.roll <- rollapply(cwd.annual.avg, width=2, FUN=sum, na.rm = TRUE, align="left") ##Anderegg used a 13 year window to get threshold 
# plot(cwd.roll, type="l", ylim = c(0, 1000), col = plot.cols[1], ylab = "Annual CWD", xaxt="n")
# axis(1, at = c(0, 25, 50, 75, 94), labels = c(2006, 2031, 2056, 2081, 2100))
# #ppt.roll <- rollapply(ppt.annual.avg, width=12, FUN=sum, na.rm = TRUE, align="left")
# cor(ppt.roll, cwd.roll)

##Bin values above/below CWD threshold as 0 or 30 Pmort. For use in BIomass Succession Input helper or DynamicInputHelper
pmorts <- ifelse(cwd.means < cwd.limit, 0, 0.30)
t.pmorts <- t(pmorts)
colnames(pmorts) <- c("eco1", "eco2", "eco3", "eco4", "eco5", "eco6", "eco7", "eco8", "eco9", "eco10", 
                      "eco11", "eco12", "eco13")

pmort.average <- rowMeans(pmorts)

mean(pmort.average[84:94])
#write.csv(pmorts, file= "I:/NWCSC-Idaho/Data/RCEW/BCM/RCEW/Futures_monthly/EcoregionSummaries/CWDs/ACCESS45_CWDS.csv") ##writing cwds to 

##This loops creates adds 0's for pmort of other species
# cwd.all.ecos <- NULL
# cwd.all.years <- NULL
# 
# for (j in years){
#   pmort.year.select <- t.pmorts[,j]
#   for (k in 1:length(ecoregions)){ 
#     cwd.single.eco <- c(0,0,0, pmort.year.select[k])
#     cwd.all.ecos <- c(cwd.all.ecos, cwd.single.eco)
#     
#   }
#   cwd.all.years <- cbind(cwd.all.years, cwd.all.ecos, j)
# }
# 
cwd.df <- NULL

##Creates column for each year which includes 0's for non-aspen species
for (k in 1:length(ecoregions)){
  cwd.blank.row <- rep(0, length(years))
  cwd.df.single <- rbind(cwd.blank.row, t.pmorts[k,], cwd.blank.row, cwd.blank.row) #used to be rbind
  cwd.df <- rbind(cwd.df, cwd.df.single) #used to be rbind
  
}
rm(k)

row.names(cwd.df) <- NULL

cwd.vec.all <- NULL
for (i in 1:length(years)){
  cwd.vec <- as.vector(cwd.df[,i])
  cwd.vec.all <- c(cwd.vec.all, cwd.vec)
}

  
  
print(target.file.name)
