source("trabajo/multivariable-analysis/pre_calculations.R")

coral_species <- read.csv2("trabajo/multivariable-analysis/data/corals_spec.csv")

# (1) Generate genera matrix

coral_genera <- generate_genera_col_names(coral_species)
coral_genera <- generate_genera_matrix(coral_genera)
write.csv(coral_genera, 
          "trabajo/multivariable-analysis/data/corals_genera.csv", 
          row.names = FALSE)

# (2) Generate matrix for both genera and species where the transects are renamed
# as their year's last digit

species_renamed_transects <- rename_transect_within_year(coral_species)
genera_renamed_transects <- rename_transect_within_year(coral_genera)
write.csv(species_renamed_transects, 
          "trabajo/multivariable-analysis/data/species_by_year_transects.csv",
          row.names = FALSE)
write.csv(genera_renamed_transects, 
          "trabajo/multivariable-analysis/data/genera_by_year_transects.csv", 
          row.names = FALSE)

# (3) Total cover per year for both genera and species

total_species <- total_covers_per_year(coral_species)
total_genera <- total_covers_per_year(coral_genera)
write.csv(total_species, 
          "trabajo/multivariable-analysis/data/corals_spec_totals.csv",
          row.names = FALSE)
write.csv(total_genera, 
          "trabajo/multivariable-analysis/data/corals_genera_totals.csv", 
          row.names = FALSE)

# (4) Get only the 1981 results in order to evaluate the community structure
coral_species_81 <- get_year_only_data(coral_species, '81')
coral_genera_81 <- get_year_only_data(coral_genera, '81')
write.csv(coral_species_81, "trabajo/multivariable-analysis/data/coral_species_81.csv")
write.csv(coral_genera_81, "trabajo/multivariable-analysis/data/coral_genera_81.csv")

# (5) Generate transpose of total cover matrices for both species and genera
coral_spec_totals_transpose <- 
  read.csv("trabajo/multivariable-analysis/data/corals_spec_totals.csv")
coral_spec_totals_transpose <- transpose_matrix(coral_spec_totals_transpose)
write.csv(coral_spec_totals_transpose,
          "trabajo/multivariable-analysis/data/transpose/transpose_coral_spec_totals.csv")

coral_genera_totals_transpose <- 
  read.csv("trabajo/multivariable-analysis/data/corals_genera_totals.csv")
coral_genera_totals_transpose <- transpose_matrix(coral_genera_totals_transpose)
write.csv(coral_genera_totals_transpose,
          "trabajo/multivariable-analysis/data/transpose/transpose_coral_genera_totals.csv")

