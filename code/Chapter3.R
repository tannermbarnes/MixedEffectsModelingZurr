########################################################################################################################################
########### Chapter 3 Things are not always linear; Additive modelling #################################################################
########################################################################################################################################
# Set working directory to project root (adjust 'wd' if needed)
setwd("/Users/tannermbarnes/Library/CloudStorage/OneDrive-MichiganTechnologicalUniversity/Books/MixedEffectsModelingZurr")
library(tidyverse)

# Additive Modelling
# In additive modeling, we use a smoothing function to link Yi and Xi.
ISIT <- read.table("ZuurDataMixedModelling/ISIT.txt", header = TRUE)
op <- par(mfrow = c(2,2), mar = c(5, 4, 1, 2))
Sources16 <- ISIT$Sources[ISIT$Station == 16]
Depth16 <- ISIT$SampleDepth[ISIT$Station == 16]
plot(Depth16, Sources16, type = "p")
library(gam)
m1 <- gam(Sources16 ~ lo(Depth16, span = 0.5))
plot(m1, se = TRUE)

m2 <- predict(m1, se = TRUE)
plot(Depth16, Sources16, type = "p")
I1 <- order(Depth16)
lines(Depth16[I1], m2$fit[I1], lty = 1)
lines(Depth16[I1], m2$fit[I1] + 2 * m2$se.fit[I1], lty = 2)
lines(Depth16[I1], m2$fit[I1] - 2 * m2$se.fit[I1], lty = 2)
par(op)

