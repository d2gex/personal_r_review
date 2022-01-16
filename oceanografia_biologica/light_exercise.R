source("config.R")
chlorophyll_data_path = file.path(DATA_PATH, "chlorophyll_data.xlsx")
chlorophyll_data = read_excel(chlorophyll_data_path, skip=9)



clean_data <- function(raw_data) {
  
  # get rid of subheadings on row 1
  chlorophyll_data <- chlorophyll_data[-c(1), ]
  
  # rename
  chlorophyll_data <- chlorophyll_data %>%
    rename(
      'date' = `mon/day/yr`,
      'depth' = `Depth,m`,
      'irradiance' = `PAR/Irradiance`,
      'chlorophyll' = Fluo.Clorofila,
      'turbid' = `Turbid.,`
    )
  chlorophyll_data <- set_names(chlorophyll_data, tolower(names(chlorophyll_data)))
  
  return(chlorophyll_data)
  
}

chlorophyll_data = clean_data(chlorophyll_data)






