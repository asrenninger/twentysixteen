########################################################
## Section 1: Create rallies dataset
## ## Read and convert to long
## ## Geocode each rally
########################################################

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

########################################################
## Section 2: Determine media markets
## ## Read in Nielson data
## ## Join rallies to it
########################################################

library(sf)

##

dma <- read_delim("https://dataverse.harvard.edu/api/access/datafile/:persistentId?persistentId=doi:10.7910/DVN/IVXEHT/A56RIW", 
                  "\t", escape_double = FALSE, trim_ws = TRUE) %>%
  mutate(STATEFP = if_else(STATEFP < 10, paste("0", STATEFP, sep = ""), paste(STATEFP)),
         CNTYFP = if_else(CNTYFP < 100 & CNTYFP > 9, paste("0", CNTYFP, sep = ""),
                          if_else(CNTYFP < 10, paste("00", CNTYFP, sep = ""), paste(CNTYFP)))) %>%
  mutate(GEOID = paste(STATEFP, CNTYFP, sep = "")) %>%
  mutate(GEOID = if_else(GEOID == "12025", "12086", GEOID)) %>%
  select(DMA, GEOID)

##

counties <- st_read("data-out/counties.geojson", crs = 102003)

##

markets <- 
  counties %>%
  left_join(dma) %>%
  group_by(DMA) %>%
  summarise()

##

rallies <- 
  read_csv("data-out/rallies.csv") %>%
  st_as_sf(coords = c("lon", "lat"), remove = FALSE) %>%
  st_set_crs(4326) %>%
  st_transform(102003)

##

coverage <-
  rallies %>%
  st_join(markets) %>%
  st_join(counties) %>%
  st_as_sf()

##

library(lubridate)
library(glue)

##

dated <- 
  coverage %>%
  separate(date_of_rally, sep = ", ", into = c("day", "date")) %>%
  separate(date, sep = " ", into = c("month", "date")) %>%
  mutate(date = glue("{month} {date}th, 2016")) %>%
  select(-month, -day) %>%
  mutate(date = mdy(date)) %>%
  drop_na(date)

coverage %>%
  group_by(DMA, candidate) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  group_by(DMA) %>%
  slice(1) %>%
  ungroup() %>%
  st_drop_geometry() %>%
  left_join(markets) %>%
  st_as_sf() %>%
  select(candidate) %>%
  plot(pal = c('blue', 'red'))

dated %>%
  select(-geometry) %>%
  write_csv("rallies.csv")

##

rallies <- read_csv("data-out/rallies.csv")

########################################################
## Section 2: Feature engineering
## ## Where were there rallies (DMA and county)?
## ## How far were rallies?
########################################################

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
  rename(trump_rallies_county_post_convention = trump,
         clinton_rallies_county_post_convention = clinton) %>%
  write_csv("rallies_counties.csv")

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

ggplot() +
  geom_point(data = centroids %>% as_tibble(), aes(X, Y), alpha = 0.5) +
  geom_point(data = t_rallies %>% as_tibble(), aes(X, Y), colour = 'red') +
  geom_point(data = c_rallies %>% as_tibble(), aes(X, Y), colour = 'blue')

##

library(spdep)
library(FNN)

##

nn <- get.knnx(t_rallies, centroids, k = 1)

trump_first <-
  as.data.frame(nn$nn.dist) %>%
  rownames_to_column(var = "counties") %>%
  gather(trump, dist_trump, V1) %>%
  arrange(as.numeric(counties)) %>%
  group_by(counties) %>%
  summarise(d_trump = mean(dist_trump)) %>%
  arrange(as.numeric(counties)) %>% 
  select(-counties) %>%
  bind_cols(counties) %>%
  select(GEOID, d_trump)

nn <- get.knnx(c_rallies, county_points, k = 1)

clinton_first <-
  as.data.frame(nn$nn.dist) %>%
  rownames_to_column(var = "counties") %>%
  gather(clinton, dist_clinton, V1) %>%
  arrange(as.numeric(counties)) %>%
  group_by(counties) %>%
  summarise(d_clinton = mean(dist_clinton)) %>%
  arrange(as.numeric(counties)) %>% 
  select(-counties) %>%
  bind_cols(counties) %>%
  select(GEOID, d_clinton)

##

nn <- get.knnx(t_rallies, centroids, k = 3)

trump_third <-
  as.data.frame(nn$nn.dist) %>%
  rownames_to_column(var = "counties") %>%
  gather(trump, dist_trump, V1:V3) %>%
  arrange(as.numeric(counties)) %>%
  group_by(counties) %>%
  summarize(d_trump3 = mean(dist_trump)) %>%
  arrange(as.numeric(counties)) %>% 
  select(-counties) %>%
  bind_cols(counties) %>%
  select(GEOID, d_trump3)

nn <- get.knnx(c_rallies, centroids, k = 3)

clinton_third <-
  as.data.frame(nn$nn.dist) %>%
  rownames_to_column(var = "counties") %>%
  gather(clinton, dist_clinton, V1:V3) %>%
  arrange(as.numeric(counties)) %>%
  group_by(counties) %>%
  summarise(d_clinton3 = mean(dist_clinton)) %>%
  arrange(as.numeric(counties)) %>% 
  select(-counties) %>%
  bind_cols(counties) %>%
  select(GEOID, d_clinton3)

##

rallies_distance <-
  counties %>%
  left_join(trump_first) %>%
  left_join(trump_third) %>%
  left_join(clinton_first) %>%
  left_join(clinton_third)

##

rallies_distance %>%
  st_drop_geometry() %>%
  write_csv("rallies_distances.csv")
