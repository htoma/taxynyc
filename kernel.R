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
library(tibble)
library(dplyr)

taxis <- as.tibble(fread('c:/data/nyctaxi/train.csv'))

# check data 
summary(taxis)

# reformating featues
taxis <- taxis %>%
  mutate(pickup_datetime = ymd_hms(pickup_datetime),
         dropoff_datetime = ymd_hms(dropoff_datetime),
         passenger_count = factor(passenger_count))

# filter only january data (and then a random sample of 10k points)
january <- taxis[month(taxis$pickup_datetime) == 1,]
january <- sample_n(january, 20000)

# check if there are missing values
aggr(january)

# k-means clustering
set.seed(20)
clusterdata <- january[,c(6:7)]
clusters <- kmeans(clusterdata,6)
january$cluster <- as.factor(clusters$cluster)

# leaflef map
#https://rstudio.github.io/leaflet/colors.html
pal <- colorNumeric(
  palette = "Reds",
  domain = as.numeric(january$cluster))

leaflet(data = january) %>% addProviderTiles("Esri.NatGeoWorldMap") %>%
  addCircleMarkers(~pickup_longitude, ~pickup_latitude, radius = 1, 
                   color = ~pal(as.numeric(january$cluster)), fillOpacity = 0.3)

# ggmap way
sbbox <- make_bbox(lon = january$pickup_longitude, lat = january$pickup_latitude, f = .1)
sq_map <- get_map(location = sbbox, maptype = "terrain", source = "google")
ggmap(sq_map) + geom_point(data = january, 
                           mapping = aes(x = pickup_longitude, y = pickup_latitude), 
                           color = as.factor(january$cluster))


# anomaly in duration
january %>%
  ggplot(aes(trip_duration)) + 
  geom_histogram(fill = "red", bins = 150) +
  scale_x_log10()

# pickup dates
january %>%
  ggplot(aes(pickup_datetime)) + 
  geom_histogram(fill = "red", bins = 100)