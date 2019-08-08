library(tidyverse)
library(sf)

##

county_points <- 
  counties %>%
  st_centroid() %>%
  st_coordinates() %>%
  as_tibble()

##

library(FNN)

##

nn <- get.knnx(county_points, county_points, k = 5)

##

bases <- tibble()

##

for (i in 1:nrow(counties)) {

  iteration <-
    counties %>%
    slice(i) %>%
    pull(GEOID)
  
  index <- 
    nn$nn.index %>%
    as_tibble() %>%
    bind_cols(counties) %>%
    select(-geometry) %>%
    gather(county, index, V1:V5) %>%
    filter(GEOID == iteration) %>%
    pull(index)
  
  region <- 
    counties %>%
    slice(index) %>%
    st_drop_geometry() %>%
    mutate(region = 1)
  
  reference <- 
    mix %>%
    left_join(region) %>%
    drop_na() %>%
    group_by(NAICS, year) %>%
    summarise(employment = sum(employees)) %>%
    mutate(change = (employment - lag(employment)) / employment) %>%
    ungroup()
  
  base <- 
    reference %>%
    filter(NAICS == "00") %>%
    select(-NAICS) %>%
    rename(total_employment = employment,
           total_change = change) %>%
    right_join(reference) %>%
    mutate(base_share = employment / total_employment,
           GEOID = iteration) %>%
    select(GEOID, year, NAICS, base_share)
  
  bases <- bind_rows(bases, base)
  
}

##

basics <- 
  mix %>%
  filter(NAICS == "00") %>%
  select(-NAICS) %>%
  rename(total_employment = employees) %>%
  select(GEOID, year, total_employment) %>%
  right_join(mix) %>%
  mutate(area_share = employees / total_employment) %>%
  left_join(bases) %>%
  mutate(loquo = area_share / base_share) %>%
  drop_na()

##

basics %>%
  filter(employees > 100 & area_share > 0.25) %>%
  arrange(desc(loquo))
  
