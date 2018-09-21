install.packages(c("ggplot2", "devtools", "dplyr", "stringr"))
install.packages(c("maps", "mapdata"))
install.packages("ggmap")
install.packages("VIM")
install.packages("lubridate")
install.packages("leaflet")

library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(VIM)
library(lubridate)
library(leaflet)
library(plyr)

taxi = read.csv("c:/data/nyctaxi/train.csv")
taxismall = taxi[1:10000,]

#taxismall = complete.cases(taxismall)

# add aerial distance
#taxismall$distance <- sqrt((taxismall$pickup_latitude-taxismall$dropoff_latitude)^2 + (taxismall$pickup_longitude-taxismall$dropoff_longitude)^2)

summary(taxismall)

set.seed(20)
clusterdata <- taxismall[,c(6:7)]
clusters <- kmeans(clusterdata,6)
taxismall$cluster <- as.factor(clusters$cluster)

# check if there are missing values
#aggr(taxismall)

# extract year/month/day
taxismall$pickupweekday <- wday(taxismall$pickup_datetime)

sbbox <- make_bbox(lon = taxismall$pickup_longitude, lat = taxismall$pickup_latitude, f = .1)
sq_map <- get_map(location = sbbox, maptype = "terrain", source = "google")
ggmap(sq_map) + geom_point(data = taxismall, mapping = aes(x = pickup_longitude, y = pickup_latitude), color = as.factor(taxismall$cluster))
#NYCMap <- get_map("New York")

pal <- colorNumeric(
  palette = "Blues",
  domain = as.numeric(taxismall$cluster))

#https://rstudio.github.io/leaflet/colors.html
leaflet(data = taxismall) %>% addProviderTiles("Esri.NatGeoWorldMap") %>%
  addCircleMarkers(~pickup_longitude, ~pickup_latitude, radius = 1, 
                   color = ~pal(as.numeric(taxismall$cluster)), fillOpacity = 0.3)

# reformating featues
taxismall <- taxismall %>%
              mutate(pickup_datetime = ymd_hms(pickup_datetime),
                     dropoff_datetime = ymd_hms(dropoff_datetime),
                     passenger_count = factor(passenger_count))

# anomaly in duration
taxismall %>%
  ggplot(aes(trip_duration)) + 
  geom_histogram(fill = "red", bins = 150) +
  scale_x_log10()

# pickup dates
taxismall %>%
  ggplot(aes(pickup_datetime)) + 
  geom_histogram(fill = "red", bins = 100)