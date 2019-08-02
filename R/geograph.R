########################################################
## Section 1: Tiger files
## ## Download counties with Tigris
## ## Add in Media Markets
########################################################

library(tigris)
library(sf)

##

options(tigris_use_cache = TRUE)

##

counties <- 
  counties(class = "sf", cb = TRUE, resolution = '5m') %>%
  filter(!str_detect(STATEFP, "15|02|60|66|69|72|78")) %>%
  select(GEOID) %>%
  st_transform(102003) 


states <- 
  states(class = "sf", cb = TRUE, resolution = '5m') %>%
  filter(!str_detect(STATEFP, "15|02|60|66|69|72|78")) %>%
  select(GEOID) %>%
  st_transform(102003) 


##

library(rmapshaper)

simplified <- st_simplify(states, dTolerance = 50000, preserveTopology = TRUE)
simplified <- ms_simplify(states, keep = 0.001)

plot(states)
plot(counties)

read_csv("~/Desktop/R/git/twentysixteen/data-in/leip/2016.csv") %>%
  clean_names() %>%
  mutate(fips = if_else(fips < 10000, paste("0", fips, sep = ""), paste(fips))) %>%
  transmute(GEOID = str_sub(fips, 1, 2),
            clinton_trump = clinton_trump) %>%
  group_by(GEOID) %>%
  summarise(clinton_trump = sum(clinton_trump)) %>%
  mutate(result = case_when(clinton_trump < 0 ~ "trump",
                            clinton_trump > 0 ~ "clinton")) %>%
  left_join(simplified) %>%
  st_as_sf() %>%
  ggplot(aes(fill = result)) +
  geom_sf(size = 0.5, colour = '#ffffff', show.legend = FALSE) +
  scale_fill_manual(values = c(pal[2], pal[8])) +
  theme_map() +
  ggsave("logo.png", height = 4, width = 6, dpi = 300)
  
##

plot <- 
  ggplot(counties) +
  geom_sf(aes(fill = as.numeric(st_area(geometry))),
          colour = NA, size = 0,
          show.legend = FALSE)

ggsave("test.png", height = 6, width = 6, dpi = 300)
