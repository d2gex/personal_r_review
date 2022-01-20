source("config.R")
library(tidyverse)
library(useful)
library(readxl)
library(ggplot2)
chlorophyll_data_path = file.path(DATA_PATH, "chlorophyll_data.xlsx")


read_data <- function(data_path) {
  #'@description read in an excel file and prepare it for data manipulation
  
  up_to = 9
  col_names = colnames(read_excel(chlorophyll_data_path, skip=up_to))
  ch_data = read_excel(data_path, skip=up_to + 1)
  names(ch_data) <- col_names
  return (ch_data)
}


get_closest_pair_to_value <- function(vector, value) {
  #'@description get the pair of closest values to a vector
  return(vector[abs(vector-value) %in% sort(abs(vector-value))[1:2]])
}

calculate_vector_pair_operation <- function(vector, up_to, operation) {
  #'@description subtract every two pairs in a vector
  
  vector_dup <-c(0, vector[1:up_to], 0)
  if (missing(operation)) {
    result <- abs(vector - vector_dup)
  }
  else if (operation == "+") {
    result <- abs(vector + vector_dup)
  }
  else {
    result <- abs(vector* vector_dup)
  }
  return (c(0,
            result[-c(1, length(vector))],
            0))
  
}

prepare_df <- function(ch_data) {
  # Prepare data frame for post-processing purposes

  # rename columns to human readable columns
  ch_data <- ch_data %>%
    rename(
      'date' = `mon/day/yr`,
      'z' = `Depth,m`,
      'i_z' = `PAR/Irradiance`,
      'chlorophyll' = Fluo.Clorofila,
      'turbid' = `Turbid.,`
    )
  # convert all col names to lowercase
  names(ch_data) <- tolower(names(ch_data))

  return(ch_data)

}

add_i0_to_df <- function(ch_data) {
  #'@description calculate i_0 with regards to i_z and add it to the dataframe


  # shift i_z down by one and replace the first value with i_z_first
  i_z_first = ch_data$i_z[1]
  ch_data <- transform(ch_data, i_0 = c(i_z_first, i_z[-nrow(ch_data)]))


  # sort column names so that i_z and i_o are side by side
  i_z_offset = grep("i_z", colnames(ch_data))
  i_0_offset = ncol(ch_data)
  col_names = colnames(ch_data)
  col_names <- c(col_names[1:i_z_offset],
                 col_names[i_0_offset],
                 col_names[(i_z_offset + 1):(i_0_offset - 1)])
  ch_data = ch_data[, col_names]
  return(ch_data)

}

add_a_z_to_df <- function(ch_data) {
  #'@description find the increments between depths
  
  # Find increments of z, making the first and last as NA as they aren't needed
  z = ch_data$z
  up_to = length(z) - 2
  a_z = calculate_vector_pair_operation(z, length(z) - 2)
  return(transform(ch_data, a_z=a_z))
  
}

add_k_to_df <- function(ch_data) {
  #'@description Calculate k by using [second, penultimate] rows and 
  #'add it to the dataframe. 
  
  # crop i_z, i_0 and z to the [second, penultimate] rows
  i_z <- ch_data$i_z[-c(1, length(ch_data$i_z))]
  i_0 <- ch_data$i_0[-c(1, length(ch_data$i_0))]
  a_z <- ch_data$a_z[-c(1, length(ch_data$a_z))]
  
  # calculate k as the log difference over z
  k<- (log(i_z) - log(i_0)) / a_z
  # repopulate k to havfe te same dimension as the length of i_z/i_0 again
  return(transform(ch_data, k = c(NA, k, NA)))
  
}

calculate_k <- function(ch_data){
  print(ch_data$k)
  return(mean(ch_data$k, na.rm = TRUE))
}

calculate_photic_depth <- function(ch_data, k) {
  #'@description calculate the depth of the photic zone based in 1/100 of i_0
  i_0 = ch_data$i_0[1]
  photic_i_z = (1/100) * i_0
  return((log(photic_i_z / i_0) / k))
}

add_a_ch_to_df <- function(ch_data) {
  #'@description Calculate the increments for the chlorophyll
  
  # Find increments of z, making the first and last as NA as they aren't needed
  chlorophyll = ch_data$chlorophyll
  up_to = length(chlorophyll) - 2
  a_ch = calculate_vector_pair_operation(chlorophyll, 
                                         length(chlorophyll) - 2, 
                                         operation = '+')
  return(transform(ch_data, a_ch=a_ch))
  
}

calculate_chlorophyll_m2 <- function(ch_data, photic_depth) {
  #'@description Calculate the amount of chlorophyll in mg/m2 using trapezium
  #'method
  
  # Get chlorophyll and z increments from [2, right below photic zone]
  z = ch_data$z
  up_to_photic_zone <- (!is.na(z)) & (z < photic_depth) 
  a_z <- z[up_to_photic_zone][2:length(a_z)]
  chlo <- ch_data$chlorophyll[up_to_photic_zone][2:length(a_z)]
  
  # Get chlorophyll and z increments around photic zone
  z_pair_around_photic <- get_closest_pair_to_value(z, photic_depth)
  around_photic_zone <-ch_data$z %in% z_pair_around_photic
  a_z_additional <- ch_data$a_z [around_photic_zone]
  chlo_additional <- ch_data$chlorophyll [around_photic_zone]
  
  # Calculate trapezium formula for interval [2, right below photic zone]
  
  
  return(list(a_z, a_z_additional, chlo, chlo_additional))
}



draw <- function(ch_data) {
  ggplot(data = ch_data, mapping= aes(x = z)) +
    geom_point(pch = 21, size = 2, aes(y=i_z), colour="blue") +
    geom_smooth(se=F, aes(y=i_z), colour="blue") +
    geom_point(pch = 21, size = 2, mapping = aes(y=i_0), colour="green") +
    geom_smooth(se=F, aes(y=i_0), colour="green")
    
}

chlorophyll_data <- read_data(chlorophyll_data_path)
chlorophyll_data <- prepare_df(chlorophyll_data)
chlorophyll_data <- add_i0_to_df(chlorophyll_data)
chlorophyll_data <- add_a_z_to_df(chlorophyll_data)
chlorophyll_data <- add_k_to_df(chlorophyll_data)
chlorophyll_data <- add_a_ch_to_df(chlorophyll_data)
k_coeficient = calculate_k(chlorophyll_data)
photic_depth = calculate_photic_depth (chlorophyll_data, k_coeficient)
details = calculate_chlorophyll_m2(chlorophyll_data, photic_depth)

print(k_coeficient)
print(photic_depth)
View(chlorophyll_data)



draw(chlorophyll_data)
ggplot(data = chlorophyll_data, mapping= aes(x = z)) +
  geom_point(pch = 21, size = 2, mapping = aes(y=k), colour="pink") +
  geom_smooth(se=F, aes(y=k), colour="pink")

ggplot(data = chlorophyll_data, mapping= aes(x = z, y=i_0 - i_z)) +
  geom_point(pch = 21, size = 2) 

  



