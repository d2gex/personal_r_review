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

add_k_to_df <- function(ch_data) {
  #'@description Calculate k and add it to the dataframe
  
  return(transform(ch_data, 
                   k= (log(ch_data$i_z) - log(ch_data$i_0)) / log(ch_data$z)))
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
chlorophyll_data = add_k_to_df(chlorophyll_data)
draw(chlorophyll_data)
ggplot(data = chlorophyll_data, mapping= aes(x = z)) +
  geom_point(pch = 21, size = 2, mapping = aes(y=k), colour="pink") +
  geom_smooth(se=F, aes(y=k), colour="pink")

ggplot(data = chlorophyll_data, mapping= aes(x = z, y=i_0 - i_z)) +
  geom_point(pch = 21, size = 2) +

  
View(chlorophyll_data)



