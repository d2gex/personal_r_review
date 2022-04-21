library("readxl")
library("dplyr")
library("plyr")

# Read data
coral_species <- read.csv2("trabajo/multivariable-analysis/data/corals_spec.csv")

# Calculate the total covers per year rather per year and transect
data <- coral_species %>% mutate(years = paste("Y", substr(.[[1]], 2,3)))
data <- data[, c(-1)] %>% select(years, everything())
data <- aggregate(. ~ years, data = data, FUN = "sum")
write.csv(data, "trabajo/multivariable-analysis/data/corals_spec_totals.csv")

# Rename matrix so that transect names disappear. We are only interested in
# having anonymous transects within each year

anonymous_transects <- coral_species %>% mutate(years = substr(.[[1]],3,3))
anonymous_transects <- anonymous_transects[, c(-1)] %>% select(years, everything())
write.csv(anonymous_transects, "trabajo/multivariable-analysis/data/corals_anonymos_transects.csv")

