# Setting working directory to current file
dir_path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir_path)

airports <- read.csv("../data/airports.csv")
carriers <- read.csv("../data/carriers.csv")
plane_data <- read.csv("../data/plane-data.csv")
variable_descriptions <- read.csv("../data/variable-descriptions.csv")
y2005 <- read.csv("../data/2005.csv.bz2")
library("dplyr")
library("ggplot2")
library("maps")



#-------------------------------------------------------------------------------
#Huragan Ivan
#13.09.2004-26.09.2004


#-------------------------------------------------------------------------------
#Huragan Katrina (tabelka)

katrina <- function(x){k <- inner_join(y2005, airports, join_by(Dest==iata)) %>%
  filter((Month==8 & DayofMonth>=23) | (Month==9 & DayofMonth<=27)) %>%
  group_by(state, Month, DayofMonth) %>% #state odnosi się do Dest
  summarise(TotalFlights=length(Dest), Cancelled=sum(Cancelled==1), 
            Carrier=sum(CancellationCode=="A"), Weather=sum(CancellationCode=="B"),
            NAS=sum(CancellationCode=="C")) %>%
  filter(state==x)
k <- k %>%
  mutate(Perc_Canc=round((NAS + Weather)/TotalFlights*100), DM=paste(DayofMonth, Month, sep=".0"))
return(k)}
katrina("LA") -> la

flightsLA <- function(x){
  ggplot(la, aes(reorder(DM, 1:36), TotalFlights, group=1)) +
    geom_line(color="cyan3", linetype=1, size=1) +
    geom_point(color="orange", size=2) +
    labs(x="", y="Flights", title="Flights to Louisiana") +
    guides(x=guide_axis(angle = 40))
}
flightsLA(la)

cancLA <- function(x){
  ggplot(la, aes(reorder(DM, 1:36), Perc_Canc, group=1)) +
    geom_line(color="darkgoldenrod3", linetype=1, size=1) +
    geom_point(color="red", size=2) +
    labs(x="", y="% canc.", title="% of flights to Louisiana cancelled due to weather conditions") +
    guides(x=guide_axis(angle = 40))
}
cancLA(la)



#-------------------------------------------------------------------------------
#Generator wykresów

data <- function(d, m){
  k <- inner_join(y2005, airports, join_by(Dest==iata)) %>%
    filter(Month==m & DayofMonth==d) %>%
    group_by(state) %>% #state odnosi się do Dest
    summarise(TotalFlights=length(Dest), Cancelled=sum(Cancelled==1),
              Weather=sum(CancellationCode=="B"),
              NAS=sum(CancellationCode=="C"))
  k <- k %>%
    mutate(Perc_Canc=round((NAS + Weather)/TotalFlights*100))
  
  state_names <- as_data_frame(tolower(state.name))
  state_names[2] <- as_data_frame(state.abb)
  colnames(state_names) <- c("State", "Abb")

  k <- inner_join(state_names, k, join_by(Abb==state))
  return(k)}

p <- function(x){states <- map_data("state")
choro <- inner_join(states, x, join_by(region==State))
ggplot(choro, aes(long, lat)) +
  geom_polygon(aes(group = group, fill = Perc_Canc)) +
  coord_map("albers",  lat0 = 45.5, lat1 = 29.5) +
  scale_fill_gradient2(limits=c(0,100), low = "darkblue", high = "red",
                       midpoint = 20, mid = "lightblue", guide_axis(title="% canc.")) +
  labs(title="24.08", x="", y="")
}
p(data(24, 8)) #24.08-3.09