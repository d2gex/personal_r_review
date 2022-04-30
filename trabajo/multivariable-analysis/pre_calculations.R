library("readxl")
library("dplyr")
library("plyr")
library("stringr")

# Read data
coral_species <- read.csv2("trabajo/multivariable-analysis/data/corals_spec.csv")

# Calculate the total covers per year rather per year and transect
total_covers <- function() {
  data <- coral_species %>% mutate(years = paste("Y", substr(.[[1]], 2,3)))
  data <- data[, c(-1)] %>% select(years, everything())
  data <- aggregate(. ~ years, data = data, FUN = "sum")
  write.csv(data, "trabajo/multivariable-analysis/data/corals_spec_totals.csv")
}

# Rename matrix so that transect names disappear. We are only interested in
# having anonymous transects within each year

rename_transect_within_year <- function() {
  anonymous_transects <- coral_species %>% mutate(years = substr(.[[1]],3,3))
  anonymous_transects <- anonymous_transects[, c(-1)] %>% select(years, everything())
  write.csv(anonymous_transects, "trabajo/multivariable-analysis/data/corals_anonymos_transects.csv")
}


generate_genera_col_names <- function() {
  col_names <- names(coral_species)
  col_names <- lapply(col_names, function(item) {
    dot_pos <- str_locate(item, '\\.')[,1]
    genera_only <- substr(item, 1, (dot_pos-1))
    return(genera_only)
  })
  data <- coral_species
  names(data) <- col_names
  return (data)
}

generate_genera_matrix <- function(data) {
  # get unique names of all columns but the first row
  col_names_to_fusion <- unique(names(data))[c(-1)]
  # Sum up the values of the columns with the same name
  names(data)[1] <- 'years'
  summed_data <- sapply(col_names_to_fusion,
                        function(col_name) {
                          rowSums(data[names(data) == col_name])
                        })
  years_df <- data.frame(years=data[,1])
  summed_data <- cbind(summed_data, years_df) %>% select(years, everything())
  names(summed_data)[1] <- ""
  return(summed_data)
}


data <- generate_genera_col_names()
data <- generate_genera_matrix(data)
write.csv(data, "trabajo/multivariable-analysis/data/genera_only_matrix.csv", 
          row.names = FALSE)

data <- read.csv("trabajo/multivariable-analysis/data/corals_spec_totals.csv")



