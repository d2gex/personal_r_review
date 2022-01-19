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
  print(col_names)
  ch_data = ch_data[, col_names]
  return(ch_data)

}

add_a_z_to_df <- function(ch_data) {
  #'@description find the increments between discreed depths
  
  # Find increments of z, making the first and last as NA as they aren't needed
  z_1 <- ch_data$z
  up_to = length(z_1) - 2
  z_2 <-c(0, z_1[1:up_to], 0)
  a_z <- c(0, abs(z_1 - z_2)[-c(1, length(z_1))], 0)
  return(transform(ch_data, a_z=a_z))
  
}

add_k_to_df <- function(ch_data) {
  #'@description Calculate k by using [second, penultimate] rows and 
  #'add it to the dataframe. 
  
  # crop i_z, i_0 and z to the [second, penultimate] rows
  i_z <- ch_data$i_z[-c(1, length(i_z))]
  i_0 <- ch_data$i_0[-c(1, length(i_0))]
  a_z <- ch_data$a_z[-c(1, length(a_z))]
  
  # calculate k as the log difference over z
  k<- (log(i_z) - log(i_0)) / log(a_z)
  print(k)
  # repopulate k to havfe te same dimension as the length of i_z/i_0 again
  return(transform(ch_data, k = c(NA, k, NA)))
  
}

calculate_k <- function(ch_data){
  return(mean(ch_data$k, na.rm = TRUE))
}

draw <- function(ch_data) {
  ggplot(data = ch_data, mapping= aes(x = z)) +
    geom_point(pch = 21, size = 2, aes(y=i_z), colour="blue") +
    geom_smooth(se=F, aes(y=i_z), colour="blue") +
    geom_point(pch = 21, size = 2, mapping = aes(y=i_0), colour="green") +
    geom_smooth(se=F, aes(y=i_0), colour="green")
    
}

chlorophyll_data = read_data(chlorophyll_data_path)
chlorophyll_data = prepare_df(chlorophyll_data)
chlorophyll_data = add_i0_to_df(chlorophyll_data)
chlorophyll_data = add_a_z_to_df(chlorophyll_data)
chlorophyll_data = add_k_to_df(chlorophyll_data)
k_coeficient = calculate_k(chlorophyll_data)
print(k_coeficient)
View(chlorophyll_data)



draw(chlorophyll_data)
ggplot(data = chlorophyll_data, mapping= aes(x = z)) +
  geom_point(pch = 21, size = 2, mapping = aes(y=k), colour="pink") +
  geom_smooth(se=F, aes(y=k), colour="pink")

ggplot(data = chlorophyll_data, mapping= aes(x = z, y=i_0 - i_z)) +
  geom_point(pch = 21, size = 2) 

  



