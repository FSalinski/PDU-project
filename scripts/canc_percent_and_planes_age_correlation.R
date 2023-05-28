library(dplyr)
library(ggplot2)

# Setting working directory to current file
dir_path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir_path)

carriers <- read.csv("../data/carriers.csv")
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

cancellation_percent_by_carrier <- function(data){
  data %>%
    group_by(UniqueCarrier, CancellationCode) %>%
    summarise(NumOfCancelled = length(CancellationCode)) %>%
    mutate(freq = (NumOfCancelled / sum(NumOfCancelled)) * 100) %>%
    filter(CancellationCode != "") -> df
  
  df
}

planes_age <- planes_age_by_carrier(planes[,c("tailnum","year")], data_2003_2008[,c("Year","UniqueCarrier","TailNum")])

canc_percent <- cancellation_percent_by_carrier(data_2003_2008)

df <- inner_join(planes_age, canc_percent, by="UniqueCarrier")

plot_results <- function(data) {
  p <- ggplot(data, aes(x=average_plane_age, y=freq,
                        color=CancellationCode, shape=CancellationCode))
  p <- p + geom_point(size=3)
  p <- p + geom_smooth(method=lm, se=FALSE, fullrange=TRUE)
  p <- p + labs(title = "Correlation between average age of a plane and percentage of cancelled flights (for each carrier)",
                x = "Average age of a plane (during flight)", y = "Percentage of cancelled flights")
  p <- p + scale_color_discrete(labels=c("Carrier", "Weather", "NAS", "Security"),
                                name = "Reason of cancellation")
  p <- p + scale_shape_discrete(labels=c("Carrier", "Weather", "NAS", "Security"),
                                name = "Reason of cancellation")
  p
}

p <- plot_results(df)
p
