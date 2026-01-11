# Set working directory to project root (adjust 'wd' if needed)
wd <- "/Users/tannermbarnes/Library/CloudStorage/OneDrive-MichiganTechnologicalUniversity/Books/MixedEffectsModelingZurr/"
if (dir.exists(wd)) {
  setwd(wd)
} else {
  message("Project directory not found: ", wd, "\nPlease set working directory manually or adjust 'wd' variable.")
}







# Set path to ZuurDataMixedModelling (override with env var ZUUR_DATA_DIR)
data_dir <- Sys.getenv("ZUUR_DATA_DIR")
if (!nzchar(data_dir)) {
  data_dir <- file.path(getwd(), "ZuurDataMixedModelling")
}
if (!dir.exists(data_dir)) stop("Data directory not found: ", data_dir)

TeethNitrogen <- read.table(file.path(data_dir, "TeethNitrogen.txt"), header = TRUE)
library(lattice)
xyplot(X15N ~ Age | factor(Tooth), type = "1", 
    xlab = "Estimated age", col = 1, 
    ylab = expression(paste(delta^{15}, "N")), 
    strip = function(bg = 'white', ...)
    data = TeethNitrogen)
