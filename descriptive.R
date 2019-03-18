
#' ---
#' title: "descriptive.R -- code for exploring data" 
#' author: "Kaylea Champion"
#' ---


#This file produces a range of descriptive statistics and plots about our data
#It's designed to be sourced from hlm_workflow.Rmd, not run standalone.
library(dplyr)
library(tidyverse)
library(gridExtra)


png("wpHist.png")
wp <- hist(data$white_proportion, main="Histogram of White Proportion Values", xlab="White Proportion")
dev.off()

png("bpHist.png")
bp <- hist(data$black_proportion, main="Histogram of Black Proportion Values", xlab="Black Proportion")
dev.off()

png("apHist.png")
ap <- hist(data$asian_proportion, main="Histogram of Asian Proportion Values", xlab="Asian Proportion")
dev.off()

png("lpHist.png")
lp <- hist(data$latinx_proportion, main="Histogram of Latinx Proportion Values", xlab="Latinx Proportion")
dev.off()



numDF <- select_if(data, is.numeric)
dropList <- paste("Topic", seq(1:40), sep="")
numDF <- numDF[, !colnames(numDF) %in% dropList] #eliminate topics
dropList <- paste("Topic", seq(1:40), "_p", sep="") 
numDF <- numDF[, !colnames(numDF) %in% dropList] #eliminate topic_ps
numDF$Topic7 <- data$Topic7 #add back the good ones
numDF$Topic18 <- data$Topic18 # ''
numDF$Topic20 <- data$Topic20 # ''
numDF$Topic25 <- data$Topic25 # ''
numDF$Topic34 <- data$Topic34 # ''


#clean out topics not in use for cleaner display
scor <- cor(x=numDF, method="spearman")
pcor <- cor(x=numDF, method="pearson")
#observation that white/black and white/asian are highly correlated

cor.test(numDF$white_proportion, numDF$black_proportion, method="pearson")
cor.test(numDF$white_proportion, numDF$black_proportion, method="spearman")

cor.test(numDF$white_proportion, numDF$asian_proportion, method="pearson")
cor.test(numDF$white_proportion, numDF$asian_proportion, method="spearman")

