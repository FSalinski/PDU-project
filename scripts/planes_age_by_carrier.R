library(dplyr)
library(ggplot2)

# Setting working directory to current file
dir_path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir_path)

planes <- read.csv("../data/plane-data.csv")
data_2003 <- read.csv("../data/2003.csv.bz2")
data_2004 <- read.csv("../data/2004.csv.bz2")
data_2005 <- read.csv("../data/2005.csv.bz2")
data_2006 <- read.csv("../data/2006.csv.bz2")
data_2007 <- read.csv("../data/2007.csv.bz2")
data_2008 <- read.csv("../data/2008.csv.bz2")

data_2003_2008 <- rbind(data_2003, data_2004,
                        data_2005, data_2006,
                        data_2007, data_2008)

planes_age_by_carrier <- function(planes_data, data){
  data %>%
    left_join(planes_data, by = c("TailNum" = "tailnum")) %>%
    filter(as.integer(year) > 1900) %>%
    mutate(plane_age = (Year - as.integer(year))) %>%
    group_by(UniqueCarrier) %>%
    summarise(average_plane_age = mean(plane_age)) -> df
  
  df
}

df <- planes_age_by_carrier(planes[,c("tailnum","year")], data_2003_2008[,c("Year","UniqueCarrier","TailNum")])

plot_results <- function(data){
  p <- ggplot(data, aes(x=reorder(UniqueCarrier, -average_plane_age),
                        y=average_plane_age))
  p <- p + geom_bar(stat="identity", fill="blue")
  p <- p + labs(x="Carrier ID",
                y="Average plane age (during flight)",
                title="Average age of planes during flight by carrier")
  p
}

p <- plot_results(df)
p
