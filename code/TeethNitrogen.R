# Set working directory to project root (adjust 'wd' if needed)
wd <- "/Users/tannermbarnes/Library/CloudStorage/OneDrive-MichiganTechnologicalUniversity/Books/MixedEffectsModelingZurr/"
if (dir.exists(wd)) {
  setwd(wd)
} else {
  message("Project directory not found: ", wd, "\nPlease set working directory manually or adjust 'wd' variable.")
}

# make working directory /Users/tannermbarnes/Library/CloudStorage/OneDrive-MichiganTechnologicalUniversity/Books/MixedEffectsModelingZurr/






TeethNitrogen <- read.table(file.path(getwd(), "ZuurDataMixedModelling", "TeethNitrogen.txt"), header = TRUE)
library(lattice)
xyplot(X15N ~ Age | factor(Tooth), type = "1", 
    xlab = "Estimated age", col = 1, 
    ylab = expression(paste(delta^{15}, "N")), 
    strip = function(bg = 'white', ...)
    data = TeethNitrogen)

