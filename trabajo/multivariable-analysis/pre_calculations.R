library("readxl")
library("dplyr")
library("plyr")
coral_species <- read.csv2("trabajo/multivariable-analysis/data/corals_spec.csv")
data <- coral_species %>% mutate(years = paste("Y", substr(.[[1]], 2,3)))
data <- data[, c(-1)] %>% select(years, everything())
data <- aggregate(. ~ years, data = data, FUN = "sum")
write.csv2(data, "trabajo/multivariable-analysis/data/corals_spec_totals.csv")

