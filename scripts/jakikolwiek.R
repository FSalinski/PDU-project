library('dplyr')

# Setting working directory to current file
dir_path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir_path)

airports <- read.csv("/Users/bartoszekpokora/PycharmProjects/pythonProject/PD4/dataverse_files/airports.csv")
carriers <- read.csv("/Users/bartoszekpokora/PycharmProjects/pythonProject/PD4/dataverse_files/carriers.csv")
plane_data <- read.csv("/Users/bartoszekpokora/PycharmProjects/pythonProject/PD4/dataverse_files/plane-data.csv")

data2008 <- read.csv("/Users/bartoszekpokora/PycharmProjects/pythonProject/PD4/dataverse_files/2008.csv.bz2")
data2007 <- read.csv("/Users/bartoszekpokora/PycharmProjects/pythonProject/PD4/dataverse_files/2007.csv.bz2")
data2006 <- read.csv("../data/2006.csv.bz2")

distance <- function(lat1, lon1, lat2, lon2){
  return acos(sin(lat1)*sin(lat2)+cos(lat1)*cos(lat2)*cos(lon2-lon1))
}

data <- f2008[1,] %>%
select(Origin, Dest, Distance)
data
lat1 <- airports[]