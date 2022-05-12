library("readxl")
library("dplyr")
library("plyr")
library("stringr")
library("data.table")

# Read data


# Calculate the total covers per year rather per year and transect
total_covers_per_year <- function(data) {
  #' Calculate the total cover per year and per column name
  results <- data %>% mutate(years = paste("Y", substr(.[[1]], 2,3)))
  results <- results[, c(-1)] %>% select(years, everything())
  results <- aggregate(. ~ years, data = results, FUN = "sum")
  return (results)

}

# Rename matrix so that transect names disappear. We are only interested in
# having anonymous transects within each year

rename_transect_within_year <- function(data) {
  #' Rename all transects within each year with the year's last digit
  by_year_transects <- data %>% mutate(years = substr(.[[1]],3,3))
  by_year_transects <- by_year_transects[, c(-1)] %>% 
    select(years, everything())
  return (by_year_transects)
  
}


generate_genera_col_names <- function(data) {
  #' It generate the same dimension matrix as the given one but the column names
  #' are those of the genera instead of genera.specie
  #' @param data original dataframe
  col_names <- names(data)
  col_names <- lapply(col_names, function(item) {
    dot_pos <- str_locate(item, '\\.')[,1]
    genera_only <- substr(item, 1, (dot_pos-1))
    return(genera_only)
  })
  results <- data
  names(results) <- col_names
  return (results)
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

get_year_only_data <- function(data,  year_prefix) {
  names(data)[1] <- 'years'
  return(
    data %>% filter(str_detect(years, year_prefix) )
  )
}

transpose_matrix <- function (data) {
  t_data <- transpose(data)
  colnames(t_data) <- rownames(data)
  rownames(t_data) <- colnames(data)
  return(t_data)
}
