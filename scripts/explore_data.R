library(dplyr)

# Setting working directory to current file
dir_path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir_path)

data_1987 <- read.csv("../data/1987.csv.bz2")
data_2001 <- read.csv("../data/2001.csv.bz2")
airports <- read.csv("../data/airports.csv")
carriers <- read.csv("../data/carriers.csv")
plane_data <- read.csv("../data/plane-data.csv")
variable_descriptions <- read.csv("../data/variable-descriptions.csv")

variable_descriptions

head(plane_data, 100)

head(carriers)

head(airports)

head(data_1987)

head(data_2001)

airports %>%
  filter(city == 'New York')
