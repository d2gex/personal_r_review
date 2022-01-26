source("oceanografia_biologica/light_exercise.R")

chlorophyll_data <- read_data(chlorophyll_data_path)
outputs = run_exercise(chlorophyll_data)
draw(outputs)
write.csv(outputs$ch_data, file.path(DATA_PATH, "dataframe_results.csv"))
View(outputs)
