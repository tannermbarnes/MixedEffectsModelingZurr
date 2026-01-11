library(AED)
data("Nereis")
#read text file from the path ZuurDataMixedModelling/Nereis.txt
Nereis <- read.table("ZuurDataMixedModelling/Nereis.txt", header = TRUE)
head(Nereis)
str(Nereis)
summary(Nereis)

#make the current folder the working directory
setwd("code")
