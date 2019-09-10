library(readxl)
library(tidyverse)
library(janitor)

##

rallies <- 
  read_xlsx("data-in/appc/rallies.xlsx", sheet = 3, skip = 1) %>%
  clean_names()

##

trump   <- rallies[, 1:5]
clinton <- rallies[, 14:18]

names(clinton) <- names(trump)

##

rallies <- 
  bind_rows(mutate(trump, candidate = "trump"),
            mutate(clinton, candidate = "clinton")) %>%
  drop_na(date_of_rally)

##

geocoderesults <- tibble()

##

library(googleway)

##

for (i in 1:nrow(rallies)) {
  
  index <- i
  
  rally <- paste(rallies$venue[index],
                 rallies$city[index],
                 rallies$state[index],
                 sep = ", ")
  
  location <- google_geocode(rally, key = "YOURKEY")
  
  located  <- tibble(rally = rally, 
                     name = c(location$results$formatted_address),
                     lat = c(location$results$geometry$location[1]),
                     lon = c(location$results$geometry$location[2]))
  
  geocoderesults <- bind_rows(geocoderesults, located)
  
  Sys.sleep(1)
  
}

##

latlon <- tibble()

for (i in 1:nrow(geocoderesults)) {
  
  index <- i 
  
  Y <- geocoderesults$lat[[index]][1]
  X <- geocoderesults$lon[[index]][1]
  
  iteration <- tibble(lat = Y,
                      lon = X)
  
  latlon <- bind_rows(latlon, iteration)
  
}

##

locations <-
  geocoderesults %>%
  select(-lat, -lon) %>%
  bind_cols(latlon)

##

locations <- 
  locations %>%
  group_by(rally) %>%
  slice(1)

##

rallies_located <- 
  rallies %>%
  mutate(rally = paste(venue, city, state, sep = ", ")) %>%
  left_join(locations)

##

write_csv(rallies_located, "rallies.csv")

##

rallies <- read_csv("data-out/rallies.csv")

##

library(sf)

##

counties <- st_read("data-out/counties.geojson", crs = 102003)

##

map <- 
  rallies %>%
  select(-GEOID) %>%
  st_as_sf(coords = c("lon", "lat"), remove = FALSE, crs = 4326) %>%
  st_transform(102003)

ggplot() +
  geom_sf(data = counties, aes(), fill = NA) +
  geom_sf(data = map, aes(colour = DMA), show.legend = FALSE)

##

st_join(map, counties) %>%
  glimpse() %>%
  st_drop_geometry() %>%
  select(GEOID) %>%
  mutate(works = 1) %>%
  left_join(rallies)

##

library(lubridate)

##

rallies %>%
  filter(date > as_date('2016-07-18')) %>%
  group_by(GEOID, candidate) %>%
  summarise(num_rallies_post_convention = n()) %>%
  spread(candidate, num_rallies_post_convention) %>%
  rename(clinton_rallies_county_post_convention = clinton,
         trump_rallies_county_post_convention = trump) %>%
  write_csv("rally_counties.csv")

##

rallies %>%
  filter(date > as_date('2016-07-18')) %>%
  group_by(GEOID, candidate) %>%
  summarise(num_rallies_post_convention = n()) %>%
  spread(candidate, num_rallies_post_convention) %>%
  left_join(dma) %>%
  select(-GEOID) %>%
  right_join(dma) %>%
  group_by(DMA) %>%
  summarise(trump_rallies_dma_post_convention = sum(trump, na.rm = TRUE),
            clinton_rallies_dma_post_convention = sum(clinton, na.rm = TRUE)) %>%
  left_join(dma) %>%
  select(GEOID, DMA, everything()) %>%
  write_csv("rallies_DMA.csv")

##

t_rallies <-
  map %>%
  filter(date > as_date('2016-07-18') & candidate == "trump") %>%
  st_centroid() %>%
  st_coordinates()

c_rallies <-
  map %>%
  filter(date > as_date('2016-07-18') & candidate == "clinton") %>%
  st_centroid() %>%
  st_coordinates()

##

centroids <- 
  counties %>%
  st_centroid() %>%
  st_coordinates()

##

