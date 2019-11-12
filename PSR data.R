# Title: PSR data.R
# Date created: 2019.05.18
# Last updated: 
# Author: Ellen Quarles
# R version 3.5.0 (2018-04-23) -- "Joy in Playing"
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# 
# Purpose: To graph and run statistical tests on the picrosirius red data.

#####################################################
##### Set up #####

# Set working directory
setwd("E:/Rapa Pers Manuscript/Figure Passive stiffness")

library(data.table)

# Import data
library(readr)
raw <- read_csv("PSR for R.csv", 
    col_types = cols(Condition = col_factor(levels = c("C", "P", "R","Y")), 
                     percFibr = col_number()))
raw$MouseID <- as.factor(raw$MouseID)
View(raw)


# I need to A) get the median values per mouse for percFibr.
agg.raw <- aggregate(raw[,3], list(raw$Condition,raw$MouseID), FUN=median, na.rm=T)
colnames(agg.raw) <- c("Condition", "MouseID", "percFibr")


# Now B) graph the data.
stripchart(percFibr ~ Condition, data=agg.raw,
           vertical=T, las=1, pch=19, method="jitter")

# And C) test for significant differences in means of each condition.
test1 <- aov((percFibr ~ Condition), data=agg.raw)
anova(test1) # no sig differences








