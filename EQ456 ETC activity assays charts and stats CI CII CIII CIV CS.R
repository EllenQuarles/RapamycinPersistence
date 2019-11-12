#### Title:             EQ456 ETC activity assays charts and stats CI CII CIII CIV CS
#### Author:            Ellen Quarles
#### Date Created:      20170730
#### Date last updated: 20170730
# R version 3.3.2 (2016-10-31) -- "Sincere Pumpkin Patch"
# Platform: x86_64-w64-mingw32/x64 (64-bit)
#### Purpose:           Using EQ456 ETC complex activity assay data, build pdfs
#                       of charts for all reasonable comparisons between groups
#                       and perform exploratory and hypothesis testing statistics
#                       on the same data.
#### Contact for Q's:   Ellen Quarles, equarles@gmail.com, Rabinovitch Lab, UW-Seattle
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

##### Setup #####
setwd("D:/ETC activity assays round 2")
data <- read.csv("All round 2 ETC activity assay analyzed data for R.csv", stringsAsFactors = F) # I remove "empty1" and "empty2" wells

# Make a df with just the MouseNo, nmol/minmg, and the Complex tested
numdata <- data[,c(1,6,7)]
numdata <- numdata[-c(280,286),]
# Make a df of only the sample information
samp.data <- data[,c(1:4)]
samp.data <- unique(samp.data)

require(reshape2)

condition.sex <- dcast(samp.data, Condition ~ Sex, length)


samps <- as.data.frame(matrix(nrow=94, ncol=6))
colnames(samps) <- c("MouseNo","CI","CII","CIII","CIV","CS")
samps$MouseNo <- samp.data$MouseNo

for (i in 1:94) {
  id <- samps$MouseNo[i]
  if ("CI" %in% numdata[numdata$MouseNo==id, "Complex"] == TRUE) ci <- numdata[numdata$MouseNo==id & numdata$Complex=="CI", "nmol.minmg"] else ci <- NA
  samps[i,"CI"] <- ci
}

for (i in 1:94) {
  id <- samps$MouseNo[i]
  if ("CII" %in% numdata[numdata$MouseNo==id, "Complex"] == TRUE) ci <- numdata[numdata$MouseNo==id & numdata$Complex=="CII", "nmol.minmg"] else ci <- NA
  samps[i,"CII"] <- ci
}

for (i in 1:94) {
  id <- samps$MouseNo[i]
  if ("CIII" %in% numdata[numdata$MouseNo==id, "Complex"] == TRUE) ci <- numdata[numdata$MouseNo==id & numdata$Complex=="CIII", "nmol.minmg"] else ci <- NA
  samps[i,"CIII"] <- ci
}

for (i in 1:94) {
  id <- samps$MouseNo[i]
  if ("CIV" %in% numdata[numdata$MouseNo==id, "Complex"] == TRUE) ci <- numdata[numdata$MouseNo==id & numdata$Complex=="CIV", "nmol.minmg"] else ci <- NA
  samps[i,"CIV"] <- ci
}

for (i in 1:94) {
  id <- samps$MouseNo[i]
  if ("CS" %in% numdata[numdata$MouseNo==id, "Complex"] == TRUE) ci <- numdata[numdata$MouseNo==id & numdata$Complex=="CS", "nmol.minmg"] else ci <- NA
  samps[i,"CS"] <- ci
}


# Now calculate the activity values normalized to citrate synthase activity per sample (CS).
samps$CI.CS <- samps$CI / samps$CS
samps$CII.CS <- samps$CII / samps$CS
samps$CIII.CS <- samps$CIII / samps$CS
samps$CIV.CS <- samps$CIV / samps$CS

# Combine sample data and numerical data frames.
all.samps <- merge(samp.data, samps, by="MouseNo")

# Remove 8 week data, since we won't be using it for now.
s16 <- all.samps[all.samps$Week!="8",]



write.csv(s16, "EQ456 ETC activity data normalized.csv", row.names=F)
# Clear the environment.
# Then grab the normalized values that you made using s16 csv.
data <- read.csv("EQ456 ETC activity data normalized1.csv")
# Make sure all vector classes are assigned the way you want 
# (factors for MouseNo..., numeric for numbers)

at.x <- seq(1,by=.65,length.out=4)

pdf("ETC activity boxplots CI II III IV.pdf", width=8, height=5)
par(mfrow=c(2,4))

dataM <- data[data$Sex=="M",]

boxplot(CI.CS.norm ~ Condition, data=dataM,
        las=1, border="grey", col="grey", medcol="black",
        lwd=1, ylim=c(0,2.5), lty=1, staplelty=0, cex.axis=0.8,
        whisklwd=2, boxwex=0.5, boxlty=0, at=at.x)
stripchart(CI.CS.norm ~ Condition, vertical = TRUE, data = dataM, 
           method = "jitter", 
           add = TRUE, pch = 20, col = 'black', at=at.x)

boxplot(CII.CS.norm ~ Condition, data=dataM,
        las=1, border="grey", col="grey", medcol="black",
        lwd=1, ylim=c(0,2.5), lty=1, staplelty=0, cex.axis=0.8,
        whisklwd=2, boxwex=0.5, at=at.x)
stripchart(CII.CS.norm ~ Condition, vertical = TRUE, data = dataM, 
           method = "jitter", 
           add = TRUE, pch = 20, col = 'black', at=at.x)

boxplot(CIII.CS.norm ~ Condition, data=dataM,
        las=1, border="grey", col="grey", medcol="black",
        lwd=1, ylim=c(0,2.5), lty=1, staplelty=0, cex.axis=0.8,
        whisklwd=2, boxwex=0.5, at=at.x)
stripchart(CIII.CS.norm ~ Condition, vertical = TRUE, data = dataM, 
           method = "jitter", 
           add = TRUE, pch = 20, col = 'black', at=at.x)

boxplot(CIV.CS.norm ~ Condition, data=dataM,
        las=1, border="grey", col="grey", medcol="black",
        lwd=1, ylim=c(0,2.5), lty=1, staplelty=0, cex.axis=0.8,
        whisklwd=2, boxwex=0.5, at=at.x)
stripchart(CIV.CS.norm ~ Condition, vertical = TRUE, data = dataM, 
           method = "jitter", 
           add = TRUE, pch = 20, col = 'black', at=at.x)



dataF <- data[data$Sex=="F",]

boxplot(CI.CS.norm ~ Condition, data=dataF,
        las=1, border="grey", col="grey", medcol="black",
        lwd=1, ylim=c(0,2.5), lty=1, staplelty=0, cex.axis=0.8,
        whisklwd=2, boxwex=0.5, at=at.x)
stripchart(CI.CS.norm ~ Condition, vertical = TRUE, data = dataF, 
           method = "jitter", 
           add = TRUE, pch = 20, col = 'black', at=at.x)

boxplot(CII.CS.norm ~ Condition, data=dataF,
        las=1, border="grey", col="grey", medcol="black",
        lwd=1, ylim=c(0,2.5), lty=1, staplelty=0, cex.axis=0.8,
        whisklwd=2, boxwex=0.5, at=at.x)
stripchart(CII.CS.norm ~ Condition, vertical = TRUE, data = dataF, 
           method = "jitter", 
           add = TRUE, pch = 20, col = 'black', at=at.x)

boxplot(CIII.CS.norm ~ Condition, data=dataF,
        las=1, border="grey", col="grey", medcol="black",
        lwd=1, ylim=c(0,2.5), lty=1, staplelty=0, cex.axis=0.8,
        whisklwd=2, boxwex=0.5, at=at.x)
stripchart(CIII.CS.norm ~ Condition, vertical = TRUE, data = dataF, 
           method = "jitter", 
           add = TRUE, pch = 20, col = 'black', at=at.x)

boxplot(CIV.CS.norm ~ Condition, data=dataF,
        las=1, border="grey", col="grey", medcol="black",
        lwd=1, ylim=c(0,2.5), lty=1, staplelty=0, cex.axis=0.8,
        whisklwd=2, boxwex=0.5, at=at.x)
stripchart(CIV.CS.norm ~ Condition, vertical = TRUE, data = dataF, 
           method = "jitter", 
           add = TRUE, pch = 20, col = 'black', at=at.x)

dev.off()
