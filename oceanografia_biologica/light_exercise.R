source("config.R")
chlorophyll_data_path = file.path(DATA_PATH, "chlorophyll_data.xlsx")
chlorophyll_data = read_excel(chlorophyll_data_path, skip=9)

prepare_data <- function(data) {
  
}

clean_data <- function(data) {
  
  # get rid of subheadings on row 1
  data <- data[-c(1), ]
  # rename columns to human readable columns
  data <- data %>%
    rename(
      'date' = `mon/day/yr`,
      'z' = `Depth,m`,
      'i_z' = `PAR/Irradiance`,
      'chlorophyll' = Fluo.Clorofila,
      'turbid' = `Turbid.,`
    )
  # convert all col names to lowercase
  data <- set_names(data, tolower(names(data)))
  
  return(data)
  
}



chlorophyll_data = clean_data(chlorophyll_data)







