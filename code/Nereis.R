# Set path to ZuurDataMixedModelling (override with env var ZUUR_DATA_DIR)
data_dir <- Sys.getenv("ZUUR_DATA_DIR")
if (!nzchar(data_dir)) {
  data_dir <- file.path(getwd(), "ZuurDataMixedModelling")
}
if (!dir.exists(data_dir)) stop("Data directory not found: ", data_dir)

# Load Nereis data (no 'AED' package required)
Nereis <- read.table(file.path(data_dir, "Nereis.txt"), header = TRUE)
names(Nereis)
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