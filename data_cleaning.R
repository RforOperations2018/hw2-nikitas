#Data Cleaning

library(ggplot2)
library(plyr)
library(dplyr)
library(knitr)
library(xtable)
library(corrplot)
library(stringr)
library(tidyr)
library(reshape)
library(glmnet)
library(raster)
library(gridExtra)

# set working directory
setwd("C:/Users/nikit/Documents/git/hw2-nikitas")

# load dataset
pitt <- read.csv("311_pitt.csv", header = TRUE, sep = ',', stringsAsFactors = FALSE)

# separate created_on variable into year, month, and time
pitt_1 <- separate(pitt, CREATED_ON, c("YEAR", "MONTH", "TIME"), sep = '-')

# find row #s where geographic location is out of bounds
index <- which(pitt_1$GEO_ACCURACY == "OUT_OF_BOUNDS")

# replace neighborhood values where the location is out of bounds
pitt_1$NEIGHBORHOOD[index] <- c("OUT_OF_BOUNDS")

# removing data where neighborhood is missing
pitt_2 <- pitt_1[-which(pitt_1$NEIGHBORHOOD == ''),]

# removing data where council district is missing
pitt_3 <- pitt_2[-which(is.na(pitt_2$COUNCIL_DISTRICT)),]

# removing data where ward is missing
pitt_4 <- pitt_3[-which(is.na(pitt_3$WARD)),]

# removing data where police zone is missing
pitt_5 <- pitt_4[-which(is.na(pitt_4$POLICE_ZONE)),]

# changing numeric representation of request status to factors
pitt_5$STATUS[which(pitt_5$STATUS == 0)] <- "New"
pitt_5$STATUS[which(pitt_5$STATUS == 1)] <- "Closed"
pitt_5$STATUS[which(pitt_5$STATUS == 3)] <- "Open"

# saving final pitt file
pitt_clean <- pitt_5

# write final csv file 
write.csv(pitt_clean, "pitt_clean.csv", row.names = FALSE)

