#######################################################################################################
########### Chapter 2 Data Exploration#################################################################
#######################################################################################################
# Set working directory to project root (adjust 'wd' if needed)
setwd("/Users/tannermbarnes/Library/CloudStorage/OneDrive-MichiganTechnologicalUniversity/Books/MixedEffectsModelingZurr")
library(tidyverse)


Nereis <- read.table("ZuurDataMixedModelling/Nereis.txt", header = TRUE)
#make a dotchart of concentration
dotchart(Nereis$concentration,
xlab = "Concentration", ylab = "Order of observations", main = "Cleveland dotplot")
#make a dotchart of concentration by group by nutrient factor
dotchart(Nereis$concentration,
groups = factor(Nereis$nutrient),
xlab = "Concentration", ylab = "Nutrient", main = "Cleveland dotplot", pch = Nereis$nutrient)

# Cleveland plots are useful to detect outliers and violation of homogeneity of variance. 
# Homogeneity means that the spread of data values is the same for all variables, 

# 2.1.2 Pairplots
pairs(Nereis)  
# no obvious relationships between concentration and biomass, but seems to be a relationship between concentration and nutrient,
# make a boxplot of concentration by nutrient but factor nutrient
boxplot(concentration ~ factor(nutrient), data = Nereis, varwidth = TRUE,
xlab = "Nutrient", ylab = "Concentration", main = "Boxplot of Concentration by Nutrient")
# seems to be a difference in concentration between nutrient levels

# New Data
TeethNitrogen <- read.table("ZuurDataMixedModelling/TeethNitrogen.txt", header = TRUE)
library(lattice)

p <- xyplot(X15N ~ Age | factor(Tooth),
            type = "l",
            xlab = "Estimated age",
            col = 1,
            ylab = expression(paste(delta^{15}, "N")),
            strip = function(bg = "white", ...) strip.default(bg = "white", ...),
            data = TeethNitrogen)

print(p)

# 2.2 The linear regression model
# The unexplained information is captured by the residuals and these are assumed to be normally distributed with experctation 0 and variance σ2.
# The intercept and slope are unknown and we take a sample to estimate them. With a confidence interval 
# These confidence intervals tell us that if we repeat the experiement a large number of times, how often the real slope and intercept are in the interval based on the 
# confidence bands. A typical choice is the 95% confidence interval.
# 2.3 Always apply the simplest statistical technique on your dat, but ensure it is applied correctly. 
# In ecology, the data are seldom modelled adequately with simple linear regression models.
# How do we verify model assumptions?
# Normality
# homogeneity 
# fixed X (X represents explanatory variables)
# independence of residuals
# a correct model specification (linear versus non-linear)

# 2.3.2 Normality = make a histogram of all observations at that particular X value
# The residuals represent the data that is left over after removing the effect of the explanatory variables. 
# Don't base judgement on normlity of raw data, apply a model and inspect the residuals

# 2.3.3 Heterogeneity of variance also called heteroscedasticity
# The spread of the data is not the same at each X value
# Pool the residuals and plot them against the fitted values
# The easiest option to deal with heteroscedasticity is a data transformation.

# 2.3.4 Fixed X
# The explanatory variable X must be fixed. This means that the values of X are set by the experimenter and are not random variables.

# 2.3.5 Independence of residuals
# Most serious problem as it invalidates important tests such as the F-test and t-test.
# Violation if the Y value at Xi is influenced by other Xi values. 

# 2.3.6 Example 1; Wedge Clam Data
Clams <- read.table("ZuurDataMixedModelling/Clams.txt", header = TRUE)
Clams$LNAFD <- log(Clams$AFD)
# log the length
Clams$LNLength <- log(Clams$LENGTH)
Clams$fMONTH <- factor(Clams$MONTH)
coplot(LNAFD ~ LNLength | fMONTH, data = Clams)
m1 <- lm(LNAFD ~ LNLength * fMONTH, data = Clams)
drop1(m1) #the drop 1 command compares the full model with a model in which the interaction is dropped, and an F-test is used to test whether the interaction is significant

op <- par(mfrow = c(2, 2), mar = c(5, 4, 1, 2))
plot(m1, add.smooth = FALSE, which = 1)
E <- resid(m1)
hist(E, xlab = "Residuals", main = "")
plot(Clams$LNLENGTH, E, xlab = "Log Length", ylab = "Residuals")
plot(Clams$fMONTH, E, xlab = "Month", ylab = "Residuals")
par(op)
E1 <- E[Clams$LNLENGTH <= 2.75]
E2 <- E[Clams$LNLENGTH > 2.75]
var.test(E1, E2) #F-test to compare variances of two samples
# The null hypothesis of the F-test is that the variances of the two samples are equal to 0. 
bartlett.test(E ~ Clams$MONTH) #Bartlett test to compare variances of more than two samples
# The Bartlett test is rather sensitive to non-normality. If the data are not normal, use Levene's test.



# # 2.3.7 Example 2: Moby's Teeth
TN <- TeethNitrogen
m2 <- lm(X15N ~ Age, subset = (TN$Tooth == "Moby"), 
    data = TN)
op <- par(mfrow = c(2, 2))
plot(m2, add.smooth = FALSE)
par(op)
N.Moby <- TN$X15N[TN$Tooth == "Moby"]
Age.Moby <- TN$Age[TN$Tooth == "Moby"]
plot(y = N.Moby, x = Age.Moby, xlab = "Age", ylab = expression(paste(delta^{15}, "N Moby")))
abline(m2)
summary(m2)
# Have to reject this model because there is a clear violcation of independence. 
# Higher nitrogen values at age X are influenced by nitrogen levels at age X-1.



# Example 3; Nereis
Nereis <- read.table("ZuurDataMixedModelling/Nereis.txt", header = TRUE)
# factor biomass with fbiomass using mutate and factor nutrient with fnutrient
Nereis <- Nereis %>%
  mutate(fbiomass = factor(biomass),
         fnutrient = factor(nutrient))
m3 <- lm(concentration ~ fbiomass * fnutrient, data = Nereis)
drop1(m3, test = "F")
op <- par(mfrow = c(1, 2))
plot(resid(m3) ~ Nereis$fbiomass, xlab = "Biomass", ylab = "Residuals")
plot(resid(m3) ~ Nereis$fnutrient, xlab = "Nutrient", ylab = "Residuals")
par(op)


# 2.3.9 Example 4; Pelagic Bioluminescence
ISIT <- read.table("ZuurDataMixedModelling/ISIT.txt", header = TRUE)
ISIT$fstation <- factor(ISIT$Station)
library(lattice)
xyplot(Sources~SampleDepth | fstation, data = ISIT, 
    xlab = "Sample Depth", ylab = "Source", 
    strip = function(bg = "white", ...) strip.default(bg = "white", ...), 
    panel = function(x,y) {
        panel.grid(h =-1, v=2)
        I1 <- order(x)
        panel.lines(x[I1], y[I1], col =1)
    })

