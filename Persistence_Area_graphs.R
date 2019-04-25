##This script graphs some summary datasets on persistence and area occupied
##They are created by Persistence_caclulator.R
##Alec Kretchun, 2017

##Read Tings

library(ggplot2)
#library(matrixStats)

##Directories and datasets

w.dir <- "I:/NWCSC-Idaho/Analysis/Graphs/"
low.pers.dat <- read.csv((paste(w.dir, "Tables/ACCESS45_presence.csv", sep="")))
low.pers.dat1 <- read.csv((paste(w.dir, "Tables/CANESM45_presence.csv", sep="")))
low.pers.dat2 <- read.csv((paste(w.dir, "Tables/GFDL45_presence.csv", sep="")))
hi.pers.dat <- read.csv((paste(w.dir, "Tables/ACCESS85_presence.csv", sep="")))
hi.pers.dat1 <- read.csv((paste(w.dir, "Tables/CANESM85_presence.csv", sep="")))
hi.pers.dat2 <- read.csv((paste(w.dir, "Tables/GFDL85_presence.csv", sep="")))


area.dat <- read.csv((paste(w.dir, "Tables/Species_areas.csv", sep="")))

##Extracting persistenbce data (only using episodic)
epi.dat <- rbind(low.pers.dat[2,-21],low.pers.dat1[2,-21],low.pers.dat2[2,-21],
                 hi.pers.dat[2,-21],  hi.pers.dat1[2,-21],  hi.pers.dat2[2,-21])
epi.dat <- epi.dat[,2:20]
#row.names(epi.dat) <- rep(c("RCP4.5", "RCP8.5"), each=3)
colnames(epi.dat) <- seq(5, 95, 5)
epi.dat <- as.matrix(epi.dat)

##Grouping by climate scenario
##Also weighting by total area
#epi.dat.45 <- c(as.vector(epi.dat[1,]/546383), as.vector(epi.dat[2,]/546383), as.vector(epi.dat[3,]/546383))
epi.means.45 <- colMeans(epi.dat[1:3,])
epi.means.85 <- colMeans(epi.dat[4:6,])
epi.dat.all <-rbind(epi.means.45,epi.means.85)

#epi.dat.all <- cbind(epi.dat.45, epi.dat.85)

##Graphing
##Graphing side by side barplot of aspen persistence ~ climate scenarios
scens <- c("RCP 4.5", "RCP 8.5")
plot.cols <- c("mediumblue", "chocolate4")
x <- barplot(epi.dat.all, beside=T, col=plot.cols, ylim= c(0, 10000),
        main = "Aspen persistence", ylab = "Total sites", xlab="Years present")
legend("topright", scens, 
       fill=plot.cols, bty="n")

##Graphing persistence by % of sites on scatterpot
# plot((epi.dat[1,]/546383), as.numeric(colnames(epi.dat)), xlim=c(0, 0.025), pch=16, cex=1.5,
#      xlab="% of sites", ylab="Years present")
# par(new=TRUE)
# plot((epi.dat[2,]/546383), as.numeric(colnames(epi.dat)), xlim=c(0, 0.025), pch=17, cex=1.5, col="red",
#      xlab="", ylab="")

##Scatterplot for all GCMs
plot((epi.dat.45[,1]*100), epi.dat.45[,2], pch=16, xlim=c(0, 2.5), col = plot.cols[1], cex=1.5,
     xlab="% of sites", ylab="Years present")
abline(lm((epi.dat.45[,1])~epi.dat.45[,2]))
par(new=TRUE)
plot((epi.dat.85[,1]*100), epi.dat.85[,2], xlim=c(0, 2.5), pch=16,col = plot.cols[2], cex=1.5,
     xlab="% of total sites", ylab="Years present", main = "Site level aspen persistence")

##Graphing species occupied area by climate
two.spps <- c("P. tremuloides", "P. menziesii")
plot.cols.two <- rep(plot.cols, each=2)
line.tys.two <- c(1,2,1,2)
scens.two <- rep(scens, each=2)
for (i in 1:nrow(area.dat)){
  row.select <- area.dat[i,3:22]
  plot(as.numeric(row.select), type="l", lwd = 2, ylim=c(0, 30), col = plot.cols.two[i], 
       lty=line.tys.two[i], main = "Percent sites occupied by species", xlab = "Year", 
       xaxt="n", ylab="Landcape area occupied")
  axis(1, at = c(1, 5, 10, 15, 20), labels = c(2006, 2030, 2055, 2080, 2100))
  par(new=TRUE)
}
rm(i)
legend("topleft", legend = c(scens, two.spps), 
       col =c(plot.cols, "black", "black"), 
       bty="n",
       lty=c(0,0,1,2), lwd=c(0,0,1,1),
       pch = c(15, 15, NA, NA), 
       pt.bg = c(plot.cols), ncol=2,
       pt.cex=1.5)
