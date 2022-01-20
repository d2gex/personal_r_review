source("oceanografia_biologica/light_exercise.R")

chlorophyll_data <- read_data(chlorophyll_data_path)
outputs = run_exercise(chlorophyll_data)
View(outputs)
