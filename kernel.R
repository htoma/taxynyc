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
library(geosphere)
library(forcats)

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

# passenger count
january %>%
  group_by(passenger_count) %>%
  count() %>%
  ggplot(aes(passenger_count, n, fill = passenger_count)) +
  geom_col() +
  theme(legend.position = "none")

# pickups per day of the week
january %>%
  mutate(wday = wday(pickup_datetime, label = TRUE))  %>%
  group_by(wday) %>%
  count() %>%
  ggplot(aes(wday, n, color = wday)) +
  geom_point(size = 4) +
  labs(x = "Day of the week", y = "Total number of pickups") +
  theme(legend.position = "none")
  
# pickups per hour of the day
january %>%
  mutate(hpick = hour(pickup_datetime))  %>%
  group_by(hpick) %>%
  count() %>%
  ggplot(aes(hpick, n)) +
  geom_col() +
  labs(x = "Hour of the day", y = "Total number of pickups") +
  theme(legend.position = "none")

# passenger count relation with trip duration
january %>%
  ggplot(aes(passenger_count, trip_duration, color = passenger_count)) +
  geom_boxplot() +
  scale_y_log10() +
  theme(legend.position = "none")

# coordinates
jfk_coord <- tibble(lon = -73.778889, lat = 40.639722)
la_guardia_coord <- tibble(lon = -73.872611, lat = 40.77725)

pickup_coord <- january %>%
                  select(pickup_longitude, pickup_latitude)
dropoff_coord <- january %>%
                  select(dropoff_longitude, dropoff_latitude)
january$distance <- distCosine(pickup_coord, dropoff_coord)
january$jfk_dist_pickup <- distCosine(pickup_coord, jfk_coord)
january$jfk_dist_dropoff <- distCosine(dropoff_coord, jfk_coord)
january$lg_dist_pickup <- distCosine(pickup_coord, la_guardia_coord)
january$lg_dist_dropoff <- distCosine(dropoff_coord, la_guardia_coord)

january <- january %>%
            mutate(speed = distance / trip_duration * 3.6,
                   date = date(pickup_datetime),
                   wday = wday(pickup_datetime, label = TRUE),
                   wday = fct_relevel(wday, c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun")),
                   hour = hour(pickup_datetime),
                   work = (hour %in% seq(8,18)) & (wday %in% c("Mon", "Tues", "Wed", "Thurs", "Fri")),
                   jfk_trip = (jfk_dist_pickup < 2e3) | (jfk_dist_dropoff < 2e3),
                   lg_trip = (lg_dist_pickup < 2e3) | (lg_dist_dropoff < 2e3),
                   blizzard = !( (date < ymd("2016-01-22") | (date > ymd("2016-01-29"))))
                   )
# distance vs trip duration
january %>%
  ggplot(aes(distance, trip_duration)) +
  geom_point() + #geom_bin2d(bins = c(500,500)) # for 2D histogram
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "Direct distance [m]", y = "Trip duration [s]")

# speed histogram
january %>%
  filter(speed > 2 & speed < 1e2) %>% # this is a place to find anomalies, also extreme speeds
  ggplot(aes(speed)) +
  geom_histogram(fill = "red", bins = 50) +
  labs(x = "Average speed [km/h]")

# speed by time of the day and day of the week
p1 <- january %>%
        filter(speed > 2 & speed < 1e2) %>%
        group_by(wday) %>%
        summarise(median_speed = median(speed)) %>%
        ggplot(aes(wday, median_speed)) +
        geom_point(size = 4) +
        labs(x = "Day of the week", y = "Median speed [km/h]")

p2 <- january %>%
        filter(speed > 2 & speed < 1e2) %>%
        group_by(hour) %>%
        summarise(median_speed = median(speed)) %>%
        ggplot(aes(hour, median_speed)) +
        geom_point(size = 4) +
        geom_smooth(method = "loess", span = 1/2) +
        labs(x = "Hour of the day", y = "Median speed [km/h]")

january %>%
  filter(speed > 2 & speed < 1e2) %>%
  group_by(wday, hour) %>%
  summarise(median_speed = median(speed)) %>%
  ggplot(aes(hour, wday, fill = median_speed)) +
  geom_tile() +  
  labs(x = "Hour of the day", y = "Day of the week") +
  scale_fill_distiller(palette = "Spectral")
