library(dplyr)
library(ggplot2)

# Setting working directory to current file
dir_path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir_path)

carriers <- read.csv("../data/carriers.csv")
data_2003 <- read.csv("../data/2003.csv.bz2")
data_2004 <- read.csv("../data/2004.csv.bz2")
data_2005 <- read.csv("../data/2005.csv.bz2")
data_2006 <- read.csv("../data/2006.csv.bz2")
data_2007 <- read.csv("../data/2007.csv.bz2")
data_2008 <- read.csv("../data/2008.csv.bz2")

data_2003_2008 <- rbind(data_2003, data_2004,
                        data_2005, data_2006,
                        data_2007, data_2008)


cancelleation_percent_by_carrier <- function(data){
  data %>%
    group_by(UniqueCarrier, CancellationCode) %>%
    summarise(NumOfCancelled = length(CancellationCode)) %>%
    mutate(freq = (NumOfCancelled / sum(NumOfCancelled))) %>%
    filter(CancellationCode != "") -> df
  
  df %>%
    group_by(UniqueCarrier) %>%
    summarise(freq = sum(freq)) %>%
    arrange(desc(freq)) %>%
    head(10) -> top10
  
  df <- df[df$UniqueCarrier %in% top10$UniqueCarrier,]
  
  df
}

data <- cancelleation_percent_by_carrier(data_2003_2008)

plot_results <- function(data){
  p <- ggplot(data, aes(x=reorder(UniqueCarrier, -freq),
               y=freq, fill=CancellationCode))
  p <- p + geom_bar(stat="identity")
  p <- p + labs(title="Percentage of cancelled flights by carrier (2003-2008)", x="Carrier ID",
                y="% of cancelled fligths", fill="Reason of cancellation")
  p <- p + scale_fill_discrete(labels=c("Carrier", "Weather", "NAS", "Security"))
  p
}

p <- plot_results(data)
p
