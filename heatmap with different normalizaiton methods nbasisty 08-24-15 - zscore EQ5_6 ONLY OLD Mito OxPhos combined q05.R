# #####Heatmap script for generating IPA canonical pathway heatmaps with various clustering methods#########
# 
# #####This script will produce a categorical heatmap
# 
# #### EVERY SINGLE column in your raw datafile that correseponds to a column of the heatmap must share a common
# ##### string of letters. Example - "OCR abund", "OCL abund", and "ORP abund" will be my heatmap groups, so I will
# ##### rename them "OCR abund_supernate", "OCL abund_supernate", and "ORP abund_supernate". Now I know for sure that 
# ##### extracting columns that contain "_supernate" will only extract those 3 specific columns. 
# 
# ###########Required Files###########
# #1)Dataset in .txt format
# #2)A .txt file (excel spreadsheet saved as .txt) specifying the categories in the first row, and the proteins contained
# # within each category. Within the same row as each category name, each contained protein must be in a separate cell
# # following the category name. 
# 
# ###########Checklist/Instructions############
# # 1)Required files are saved in .txt format 
# # 2)Install/load require packages
# # 3)Set working directory - the location you will be generating the files
# # 4)Specify your dataset file and IPA file
# # 5)Designate which columns in the dataset your data of interest is in
# # 6)Designate column names as they will appear on your heatmaps
# # 7)Designate the filenames of the heatmaps
# # 8)Designate the column with your uniprotIDs (or rownames)
# # 9) (OPTIONAL) For making visual adjustments to the heatmap images scroll to the near bottom of this script 
# # and follow the directions
# # 10)Copy and paste the entire script after step 2 and run it
# ####################################
# 
# # 1)Required files saved in .txt format
# #It's easier to have these saved in the same directory, but not nessary. You can specify the location of the file when
# #reading it in. Make sure these are saved somewhere before you proceed. 
# 
# # 2)Install/load require packages: Run the script below (2 lines)to install and load the packages to inset gene ID's and
# #create heatmaps. These should be run prior to running the rest of the script. 
# 
# install.packages("gplots")
# install.packages("RColorBrewer")
library(gplots)
library("RColorBrewer")



#################Fill in the information below and then run the entire script from this point on#################
####################(Begin editing the script here)##############################################################
# 3)Set working directory - the location you will be generating the files. 
#Enter the location of a folder you would like to generate between the quotation marks below
#working_directory <- "C:\Users\Rab\Desktop\shane"

# 4)Specify the filenames of your dataset and IPA pathways file. 

#Make the 2 following adjustments:
#A) Replace "dataset.txt" with the file name of the dataset (keep the quotes and .txt extension there). 
data_file <- "EQ5_6 20160901 updated skyline post steps 0_1_2 2nd try single columns ALL OLD.txt" #<- edit here

#B) Replace "ipa file.txt" with the name of the file containing the pathways and proteins for the heatmap
ipa_file <- "20160901 EQ5_6 heart prot IPA paths q05 old to old  canonical_pathways_forheatmap_bypathlength Mito OxPhos.txt"
title <- "EQ5_6 Female RP upd sky q05"
#NOTE: If your dataset isn't in the working directory you specified in #3 then replace dataset1 with the 
#full path to your file.


# 5)Designate which columns in the dataset to use for the heatmap
# In the quotes below specify a unique character string which is in the name of the columns of data going 
# into the heatmap. 

#NOTE: If the column name had a space in it, the space is converted to a period. IE "Slope est" would be "Slope.est"
data1_cols <- ".log2mean" 


# 6)Designate column names as they will appear on your heatmaps
#Make 2 replacements:
#Replace the items in quotes with the labels you would like for each column of the heatmap separated 
#by commas.These labels will apply to the data from part A of step 5 above. 
data1_colnames     <- c("16Control","16Rapa","16Pers")
# Young log2_Avg  HeartStandard log2_Avg


# 7)Designate the the output heatmap filenames. Replace "CR Rapa Mouse Liver..." with your preferred title.

fn1 <- "EQ5Pers_16wk upd sky q05 "

# 8)Designate the column with your uniprotIDs (or rownames). Replace "UniProtKB_short" with the name of the column 
#containing the Uniprot ID's. Spaces should be converted to periods.

rows <- "geneid.short"

# 9) WHAT KIND OF NORMALIZATION WOULD YOU LIKE TO DO? 
## TYPE ONE OF THE FOLLOWING after 'norm ='
####Option 1 - "none" - makes the heatmap with the numbers as is (no scaling, centering, or normalization). Use this if each column is a log ratio.
####Option 2 - "zscore" - makes a z-score heatmap... i.e. centered to rownmeans and scaled to standard deviation
####Option 3 - "rowmean" - same as z-scores except scaled to row means rather than standard deviation
####Option 4 - "pcor" - Based on a 'Partial Correaltion' Makes a heatmap of each treatment group that factors out 
############ any correlation that can be explained by their shared correlation with a "control group". 
############### (Specify the control group in 9B)
############### ... For example, you can compare OWT, OCR, and ORP while controlling for the fact that all three groups will 
############### always have high correlation based on peptide ionizaiton effects. In this case one option is to factor out the
############### correlation each one shares with a baseline or control group...YWT. The heatmap shows what remains once all 
############## of the correlation expalined by YWT is removed. 

norm <- "zscore"    #####can be "pcor", "none", "zscore", "rowmean"

# 9b) If you chose "pcor", what is your control group/condition called? (case sensitive)

ctl_group <- "Young"

##############################Don't edit below this point (exept for adjusting step 9)#####################
###############After the above information is filled out, copy and paste the entire script from step 4 on and run it######
#setwd(working_directory)
abund <- list()#create empty list
abund[1] <- list(read.delim(data_file,header=TRUE,sep="\t",stringsAsFactors=FALSE))#read data_file in as first element of the list
ipa_list  <- list() #create empty list
ipa_list[1]  <- list("ipa_file") #read ipa_file as first element of the list
paths <- read.delim(ipa_file[[1]],header=FALSE,sep="\t",stringsAsFactors=FALSE) #read ipa_list as data.frame


# set the number of pathways to display
ipa.num = 10#nrow(paths) # all pathways

#get the time and date stamp
stime <- Sys.time()
stime <- gsub(":",".",stime)

# reality check: see how many IPA molecules are in the abundance input file
if(0){
  heart <- heart[order(heart[,"Symbol_IPA"]),]
  mysplit <- function(x)strsplit(x,",")
  x <- unlist(lapply(paths[,"Molecules"],mysplit))
  x <- sort(unique(x))
  # colnames(heart)
  sum(z <- x %in% heart[,"Symbol_IPA"])
}


#Begin function which retrieves values for each pathway and scales/clusters data
for(i in 1:length(abund)){
  #for(i in c(3)){
  ab <- abund[[i]] #make data frame from input dataset
  # set up the heatmap matrix
  pathway.size <- NULL 
  ###############
  data1_colnumbers <- grep(data1_cols,colnames(ab)) #create vector of data1 column locations
  #data1_colnumbers <- c(259, 280, 62, 11, 20, 41)
  ################
  #create empty objects which will later hold data for the 4 output heatmaps
  dat.all.d1.d1hc <- NULL

  for(ijk in 1:ipa.num){
    ###################
    dat1           <- matrix(0,ncol=length(data1_colnumbers),nrow=nrow(ab)) #create data1 object to temporary hold info for each pathway
    colnames(dat1) <- data1_colnames #apply specified column names to dataset1
    ####################
    #Replace empty UniProtKB_short entries with protein 
    rownames(dat1) <- ab[,rows]
    length(idx <- which(rownames(dat1)==" "))
    if(length(idx)>0)rownames(dat1)[idx] <- ab[idx,"Protein"]
    length(idx <- which(rownames(dat1)==""))
    if(length(idx)>0)rownames(dat1)[idx] <- ab[idx,"Protein"]
    
    
    syms <- unlist(paths[ijk,2:length(paths[ijk,])])
    idx.syms <- toupper(ab[,rows]) %in% toupper(syms)
    
    ##################
    dat1[idx.syms,] <- as.matrix(ab[idx.syms,data1_colnumbers]) 
    ###################
    
    # remove rows that are all zero or NA
    #class(dat1) <- "numeric"
    #dat1[is.na(dat1)] <- 0
    rs1 <- rowSums(dat1, na.rm=TRUE)
    length(idx1 <- which(rs1==0))
    dim(dat1 <- dat1[-idx1,])
    #dat1 <- na.omit(dat1)
    # change any values that are NA to 0
    
    # 
    # extra row scaling 17sep2012
    #
    
    ### for option "none"
    if(norm=="none")dat1_scaled <- dat1 #unscaled
    
    ###For option "zscore"
    if(norm=="zscore")dat1_scaled <- t(scale(t(dat1))) #zscore
    
    ###For option "rowmean"
    if(norm=="rowmean")dat1_scaled <- log2(dat1/rowMeans(dat1)) #row mean scaled
    
    ###FOr option "pcor"
    if(norm=="pcor")dat1_scaled <- dat1 #pcor correction done in the end right before calling heatmap function
    
    
    
   #dat1_scaled <- log2(dat1/rowMeans(dat1)) #row scale/center dataset1
   #dat1_scaled <- dat1 #unscaled
    hc1 <- hclust(dist(dat1_scaled), "ward.D") ##Clustering for dataset 1
    dat_d1_d1hc <- dat1_scaled[hc1$order,] #Create dataset 1 clustered by dataset 1
    pathway.size <- c(pathway.size,nrow(dat_d1_d1hc)) #number of rows for this pathway
    #print(paste(ijk,nrow(dat1)))
    
    # Add each pathway back to the master list which will contain all pathways
    dat.all.d1.d1hc <- rbind(dat.all.d1.d1hc,dat_d1_d1hc)
    #Add column names to the master list
    colnames(dat.all.d1.d1hc) <- data1_colnames

    
    
  }
  
  RowSideColors <- NULL
  catnum <- NULL
  nn <- 1:20
  cols <- c(brewer.pal(8,"Set1"), brewer.pal(12,"Paired"))#sample(colors(distinct=TRUE),length(pathway.size))
  #cols <- rep(c("black","gray"),length(pathway.size))
  for(i in 1:length(pathway.size)){
    RowSideColors <- c(RowSideColors,rep(cols[i],pathway.size[i]))  
    catnum <- c(catnum,rep(nn[i], pathway.size[i]) )
  }
  
  options(expressions=500000)
  
  #9) OPTIONAL - visually editing heatmap
  ########################Instructions for visually editing heatmap########################
  #There are 4 heatmaps below total. If you make changes you Here are a few changes you can make:
  
  #Image size - Within the tiff() command there are width = 800 and height = 8000 currently specified. You can 
  #change these numbers to change the width and height of the heatmap
  
  #Font size - Within the tiff() command, change the value after pointsize= to adjust font size.
  
  #Color contrast - The contrast can be adjusted in the "mx <- max(abs(dat.all.d1.d1hc))" command. To increase contrast
  #add a /2 at the end of the command (or a number greater than 2 for even more contrast). This would look like 
  #"mx <- max(abs(dat.all.d1.d1hc))/2". NOTE: Each heatmap has its own mx value. The first heatmap is mx, second is mx2,
  #third is mx3, fourth is mx4. Make sure you adjust the correct one for your heatmap.
  
  #Colors - Within the heatmap.2() command, adjust the value after "col="" to change the color. Look online for which
  #color options are available.
  
  #Column dendrogram - Within the heatmap.2() command, adjust "dendrogram=" value to either column for a dendrogram, or
  #none for no dendrogram.
  
  #layout(mat = lmat, widths = lwid, heights = lhei)
  

}


####################Pcor = true##############################
if(norm=="pcor"){
  
  res_map <- matrix(data=NA, nrow=nrow(dat.all.d1.d1hc), ncol=ncol(dat.all.d1.d1hc)-1)
  ctl_col <- grep(ctl_group, colnames(dat.all.d1.d1hc))
  k=0
  for(i in 1:ncol(dat.all.d1.d1hc)){if(i!=ctl_col){
    k=k+1
    res_map[,k] <- residuals(lm(dat.all.d1.d1hc[,i]~ dat.all.d1.d1hc[,ctl_col]))
  }}
  colnames(res_map) <- colnames(dat.all.d1.d1hc)[-ctl_col]
  rownames(res_map) <- rownames(dat.all.d1.d1hc)
  
  mx <- max(abs(res_map), na.rm=TRUE)/2
  graphics.off() 
  tiff(filename = paste(fn1,stime,"_categories.tiff",sep=""), width = 1500, antialias ="default", height = 7000, units = "px", pointsize = 40)
  heatmap.2(res_map,main=title, scale="none", na.rm=FALSE, na.color="grey", breaks=seq(-mx,mx,mx*0.001), col=bluered, trace="none",key=FALSE,symkey=FALSE, margins = c(25, 5), Rowv=FALSE, Colv=FALSE, dendrogram="none",cexCol=1.8,RowSideColors=RowSideColors, lwid=c(4,4))
  legend( "left", legend = paths[[1]][1:ipa.num],col = cols, fill=cols, cex=0.55)
  graphics.off()
  #
  

  
  hm2 <- res_map[match(unique(rownames(res_map)), rownames(res_map)),]
  ###Heatmap2 - no categories
  # mx <- max(abs(res_map))#/2
  graphics.off() 
  tiff(filename = paste(fn1,stime,norm, "_raw.tiff",sep=""), width = 1500, antialias ="default", height = 4500, units = "px", pointsize = 40)
  heatmap.2(hm2, main=title,scale="none",breaks=seq(-mx,mx,mx*0.001), col=bluered, trace="none",key=FALSE,symkey=FALSE, margins = c(25, 5), Rowv=TRUE, Colv=TRUE, dendrogram="both",cexCol=1.8)
  graphics.off()
  
  rawnums <- cbind(catnum, res_map)
  write.csv(hm2,paste(fn1,stime,norm, "pcor dendo heatmap data.csv"))
  write.csv(rawnums, paste(fn1,stime,"pcor category heatmap data.csv"))
  
  
}
############################End Pcore############

####################For all other options################
if(norm!="pcor"){
mx <- max(abs(dat.all.d1.d1hc), na.rm=TRUE)
graphics.off() 
tiff(filename = paste(fn1,stime, norm, "_categories.tiff",sep=""), width = 2000, antialias ="default", height = 10000, units = "px", pointsize = 40)
heatmap.2(dat.all.d1.d1hc,main=title, scale="none", na.rm=FALSE, na.color="grey", breaks=seq(-mx,mx,mx*0.001), col=bluered, trace="none",key=FALSE,symkey=FALSE, margins = c(25, 5), Rowv=FALSE, Colv=FALSE, dendrogram="none",cexCol=1.8,RowSideColors=RowSideColors, lwid=c(4,4))
legend( "left", legend = paths[[1]][1:ipa.num],col = cols, fill=cols, cex=0.55)
graphics.off()
#


hm2 <- dat.all.d1.d1hc[match(unique(rownames(dat.all.d1.d1hc)), rownames(dat.all.d1.d1hc)),]
###Heatmap2 - no categories
# mx <- max(abs(dat.all.d1.d1hc))#/2
graphics.off() 
tiff(filename = paste(fn1,stime,norm, "_raw.tiff",sep=""), width = 1500, antialias ="default", height = 6500, units = "px", pointsize = 40)
heatmap.2(hm2, main=title,scale="none",breaks=seq(-mx,mx,mx*0.001), col=bluered, trace="none",key=FALSE,symkey=FALSE, margins = c(25, 5), Rowv=TRUE, Colv=TRUE, dendrogram="both",cexCol=1.8)
graphics.off()

rawnums <- cbind(catnum, dat.all.d1.d1hc)
write.csv(hm2, paste(fn1,stime, norm, "scaled raw data.csv"))
write.csv(rawnums, paste(fn1,stime, norm, "scaled category data.csv"))

}
#} # END for(iii in 1:length(abund)){

####################END of For all other options################