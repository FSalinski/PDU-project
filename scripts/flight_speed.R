
library(ggplot2)
library(maps)
library(dplyr)

# Setting working directory to current file
dir_path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir_path)

# Wczytanie airplane
airports <- read.csv("../data/airports.csv")
carriers <- read.csv("../data/carriers.csv")
plane_data <- read.csv("../data/plane-data.csv")

data2008 <- read.csv("../data/2008.csv.bz2")
data2007 <- read.csv("../data/2007.csv.bz2")
data2006 <- read.csv("../data/2006.csv.bz2")
data2005 <- read.csv("../data/2005.csv.bz2")

# Łączymy dane z lat 2005 - 2008 w jedną tabelkę
data2008_2005 <- rbind(data2008, data2007, data2006, data2005)


# Dodajemy dane koordynatów lotniska Origin i Dest
add_coordinates_data <- function(data){
  
  # Dodajemy koordynaty obu lotnisk do danych lotów
  data_w_coordinates <- data %>%
    inner_join(airports, by = c("Origin" = "iata")) %>%
    select(Origin, Dest, Distance, AirTime, lat1 = lat, long1 = long) %>%
    inner_join(airports, by = c("Dest" = "iata")) %>%
    select(Origin, Dest, Distance, AirTime, lat1, long1, lat2 = lat, long2 = long)
  
  # Obliczamy różnicę latitude i longitude dla każdej pary
  data_w_coordinates$lat_diff <- abs(data_w_coordinates$lat1 - data_w_coordinates$lat2)
  data_w_coordinates$long_diff <- abs(data_w_coordinates$long1 - data_w_coordinates$long2)
  
  return(data_w_coordinates)
}

data_w_coordinates <- add_coordinates_data(data2008_2005)


# Bierzemy tylko te pary lotnisk, które są dużo bardziej oddalone wschód-zachód niż
# północ-południe i usuwamy odstające lotniska poza głownym kontynentem oraz loty krótkie i nieodbyte
data_selection <- function(data){
  
  airport_pairs <- data %>%
    filter(long_diff > 8 * lat_diff) %>%
    filter(long2 > -130 & long1 > -130) %>%
    filter(lat1 > 25 & lat2 > 25) %>%
    filter(Distance > 400) %>%
    filter(!is.na(AirTime))
  
  return(airport_pairs)
}

airport_pairs <- data_selection(data_w_coordinates)


# Tworzymy mapę wybranych połączeń
plot_routes <- function(data){
  
  # Pobranie danych geograficznych Stanów Zjednoczonych
  mapa <- map_data("state") 
  
  # Utworzenie mapy
  ggplot() +
    geom_polygon(data = mapa, aes(x = long, y = lat, group = group), fill = "lightgray", color = "black") +
    geom_segment(data = unique(data), aes(x = long1, y = lat1, xend = long2, yend = lat2), color = "blue", size = 0.1) +
    coord_fixed() +
    labs(x = "Długość geograficzna", y = "Szerokość geograficzna", title = "Mapa wybranych połączeń")
  
}

routes_map <- plot_routes(airport_pairs)
routes_map

# Podzielenie lotów na loty z wschodu na zachód i odwrotnie
west_east_flights <- airport_pairs %>%
  filter(long1 < long2)

east_west_flights <- airport_pairs %>%
  filter(long1 > long2)

# Liczymy sredni czas w powietrzu dla obu kierunków (minuty)
west_east_AirTime <- mean(west_east_flights$AirTime)
east_west_AirTime <- mean(east_west_flights$AirTime)

# Liczymy średni dystans lotu dla obu kierunków (mile)
west_east_Distance <- mean(west_east_flights$Distance)
east_west_Distance <- mean(east_west_flights$Distance)

# Liczymy średnią prędkość dla obu kierunków (mile na godzine)
west_east_speed <- west_east_Distance / west_east_AirTime * 60
east_west_speed <- east_west_Distance / east_west_AirTime * 60


ggplot(data = data_frame(c(west_east_speed, east_west_speed), ), aes(x = c(west_east_speed, east_west_speed), y = c("West-east flight speed \n (in the direction of the earth's rotation)", "East-west flight speed \n (opposite to the earth's rotation)"))) +
  geom_bar(stat = "identity", fill = "darkorange", width = 0.5) +
  labs(x = "Speed", y = "Flight direction", title = "Speed comparison for both directions")

west_east_speed / east_west_speed