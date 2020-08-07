########################################################
## Submission to PaCSS
## ## Propensity scores
## ## Maps of counties
## ## Comparison with House races
########################################################

library(tidyverse)
library(janitor)
library(broom)

##

regression <- read_csv("data-out/regression.csv")

ggplot(data = 
         regression %>%
         select(change_2008, change_2012) %>%
         mutate(change_2008 = change_2008 - change_2012) %>%
         gather(variable, value, change_2008:change_2012) %>%
         mutate(variable = str_replace_all(variable, pattern = "_", replacement = " ")),
       aes(value)) +
  geom_histogram(bins = 50) +
  facet_wrap(~ variable, nrow = 2) +
  theme_minimal()

##

before <- 
  scoring %>%
  select(one_of(covariates)) %>%
  map(~ t.test(.x ~ scoring$rally)$p.value) %>%
  as_tibble() %>% 
  gather() %>% 
  mutate(key = str_replace_all(key, pattern = "_", replacement = " ")) %>%
  mutate(signif = ifelse(value < .05, "significant", "insignificant")) %>% 
  ggplot(aes(x = reorder(key, value), y = value)) + 
  geom_point(aes(), colour = '#555655', size = 5) + 
  geom_segment(aes(xend = reorder(key, value), yend = 0), colour = '#555655') +
  geom_hline(yintercept = 0.05, colour = '#00004C', size = 1, linetype = 3) +
  #geom_text(aes(x = "percent fair poor", y = 0.2, label = "line marks 0.05 threshold"), hjust = 0, colour = '#7F0000') + 
  scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8)) +
  coord_flip() +
  ylab("p-value") +
  xlab("") +
  theme_ver() +
  ggsave("p-value_unmatched.png", height = 8, width = 10, dpi = 300)

##

covariates <- c("household_size", "density",
                "education", "median_age", 
                "percent_fair_poor", "years_of_potential_life_lost_rate",
                "pct_black", "pct_foreign", "unemployed",
                "gini", "home", "earn", "foreclosure_rate")

##

matching <- 
  scoring %>%
  select(GEOID, change_2008, change_2012, all_of(covariates), d_trump:naz) %>%
  drop_na()

matching %>%
  mutate(rallied = if_else(trump_rallies_dma_post_convention > 0, 1, 0)) %>%
  group_by(rallied) %>%
  summarise(n = n())

##

library(MatchIt)
library(optmatch)

##

names(matching)

##

matched <- matchit(rally ~  density + household_size +
                     education + median_age + 
                     percent_fair_poor + years_of_potential_life_lost_rate +
                     pct_black + pct_foreign +
                     gini + home + earn + foreclosure_rate,
                   data = matching, method = "nearest",
                   ratio = 1)

##

social <- match.data(matched)

##

after <- 
  social %>%
  select(one_of(covariates)) %>%
  map(~ t.test(.x ~ social$rally)$p.value) %>%
  as_tibble() %>% 
  gather() %>% 
  mutate(key = str_replace_all(key, pattern = "_", replacement = " ")) %>%
  mutate(signif = ifelse(value < .05, "significant", "insignificant")) %>% 
  ggplot(aes(x = reorder(key, value), y = value)) + 
  geom_point(aes(), colour = '#555655', size = 5) + 
  geom_segment(aes(xend = reorder(key, value), yend = 0), colour = '#555655') +
  geom_hline(yintercept = 0.05, colour = '#00004C', size = 1, linetype = 3) +
  geom_text(aes(x = "unemployed", y = 0.7, label = "line marks 0.05 threshold"), hjust = 0, colour = '#7F0000') + 
  scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8)) +
  coord_flip() +
  ylab("p-value") +
  xlab("") +
  theme_ver() +
  ggsave("p-value_matched.png", height = 8, width = 10, dpi = 300)

##



with(social, t.test((change_2012 * 100) ~ rally))

##

library(tigris)
library(sf)

##

counties <- 
  counties(class = "sf", cb = TRUE, resolution = '5m') %>%
  filter(!str_detect(STATEFP, "15|02|60|66|69|72|78")) %>%
  select(GEOID) %>%
  st_transform(102003) 

##

states <- 
  states(class = "sf", cb = TRUE, resolution = '5m') %>%
  filter(!str_detect(STATEFP, "15|02|60|66|69|72|78")) %>%
  select(GEOID) %>%
  st_transform(102003) 


matched %>% 
  left_join(counties) %>% 
  st_as_sf() %>% 
  select(rally) %>% 
  plot()

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

outer <-
  dma %>%
  left_join(counties) %>%
  st_as_sf() %>%
  group_by(DMA) %>%
  summarise() %>%
  filter(str_detect(DMA, "PHILADELPHIA")) %>%
  st_buffer(1000) %>%
  st_cast("LINESTRING")

inner <- 
  dma %>%
  left_join(counties) %>%
  st_as_sf() %>%
  group_by(DMA) %>%
  summarise() %>%
  filter(str_detect(DMA, "PHILADELPHIA")) %>%
  st_buffer(-1000) %>%
  st_cast("LINESTRING")

middle <-
  dma %>%
  left_join(counties) %>%
  st_as_sf() %>%
  group_by(DMA) %>%
  summarise() %>%
  st_cast("MULTILINESTRING")

##

touching <- 
  counties %>%
  st_touches(middle) %>% 
  as_tibble() %>% 
  pull(row.id)

##

rallies <- read_csv("data-out/rallies.csv")

##

visited <- 
  rallies %>%
  left_join(counties) %>%
  st_as_sf() %>%
  group_by(DMA) %>%
  summarise(trump_rallies = mean(trump_rallies_dma_post_convention),
            clinton_rallies = mean(clinton_rallies_dma_post_convention)) %>%
  filter(trump_rallies != 0 | clinton_rallies != 0) %>%
  ungroup() %>%
  mutate(dissolve = 1) %>%
  group_by(dissolve) %>%
  summarise() %>%
  st_cast("MULTILINESTRING")

boundary <-
  counties %>% 
  mutate(dissolve = 1) %>%
  group_by(dissolve) %>%
  summarise() %>%
  st_cast("MULTILINESTRING")

##

plot(visited)
plot(boundary)

##

borders <- 
  counties %>%
  st_drop_geometry() %>%
  slice(pull(as_tibble(st_touches(counties, visited)), row.id)) %>%
  pull(GEOID)

coastal <- 
  counties %>%
  st_drop_geometry() %>%
  slice(pull(as_tibble(st_touches(counties, boundary)), row.id)) %>%
  pull(GEOID)

##

regression <- read_csv("data-out/regression.csv")

##

spatial <- 
  regression %>%
  mutate(rallied = if_else(trump_rallies_dma_post_convention > 0, 1, 0)) %>%
  filter(GEOID %in% borders) %>%
  filter(!GEOID %in% coastal | rallied == 0)

##

after <- 
  spatial %>%
  select(one_of(covariates)) %>%
  map(~ t.test(.x ~ spatial$rallied)$p.value) %>%
  as_tibble() %>% 
  gather() %>% 
  mutate(key = str_replace_all(key, pattern = "_", replacement = " ")) %>%
  mutate(signif = ifelse(value < .05, "significant", "insignificant")) %>% 
  ggplot(aes(x = reorder(key, value), y = value)) + 
  geom_point(aes(), colour = '#555655', size = 5) + 
  geom_segment(aes(xend = reorder(key, value), yend = 0), colour = '#555655') +
  geom_hline(yintercept = 0.05, colour = '#00004C', size = 1, linetype = 3) +
  #geom_text(aes(x = "unemployed", y = 0.7, label = "line marks 0.05 threshold"), hjust = 0, colour = '#7F0000') + 
  scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8)) +
  coord_flip() +
  ylab("p-value") +
  xlab("") +
  theme_ver() +
  ggsave("p-value_spatial.png", height = 8, width = 10, dpi = 300)


with(spatial, t.test((change_2012 * 100) ~ rallied))

##

spatial %>% 
  select(GEOID, rallied) %>%
  left_join(counties) %>%
  st_as_sf() %>% 
  plot()

##

pal <- read_csv("~/Desktop/R/Palettes/seismic.txt", col_names = FALSE) %>% pull(X1)

##

map_social <- ggplot(social %>%
                       select(GEOID, rally) %>%
                       left_join(counties) %>%
                       st_as_sf()) +
  geom_sf(data = states, aes(), fill = NA, colour = '#000000', lwd = 0.5, alpha = 0.5) +
  geom_sf(aes(fill = factor(rally)), colour = '#ffffff', lwd = 0.1) +
  scale_fill_manual(values = c(pal[1], pal[9]), 
                    guide = 'none') +
  theme_map() +
  ggsave("map_social.png", height = 8, width = 10, dpi = 300)

map_spatial <- ggplot(spatial %>% 
                        select(GEOID, rallied) %>%
                        left_join(counties) %>%
                        st_as_sf()) +
  geom_sf(data = states, aes(), fill = NA, colour = '#000000', lwd = 0.5, alpha = 0.5) +
  geom_sf(aes(fill = factor(rallied)), colour = '#ffffff', lwd = 0.1) +
  scale_fill_manual(values = c(pal[1], pal[9]), 
                    guide = 'none') +
  theme_map() +
  ggsave("map_spatial.png", height = 8, width = 10, dpi = 300)
 
##

library(scales)

##

map_change <- ggplot(regression %>% 
                        select(GEOID, change_2012) %>%
                        left_join(counties) %>%
                        st_as_sf()) +
  geom_sf(data = states, aes(), fill = NA, colour = '#ffffff', lwd = 0.5, alpha = 0.5) +
  geom_sf(aes(fill = change_2012 * 100), colour = '#ffffff', lwd = 0.1) +
  scale_fill_gradientn(colours = rev(pal[5:9]), 
                       limits = c(-30, 0),
                       oob = squish,
                       name = '% change in (dem) margin from 2012',
                       guide = guide_continuous <- 
                         guide_colorbar(direction = "horizontal",
                                        barheight = unit(2, units = "mm"),
                                        barwidth = unit(50, units = "mm"),
                                        draw.ulim = FALSE,
                                        title.position = 'top',
                                        label.position = 'bottom',
                                        title.hjust = 0.5,
                                        label.hjust = 0.5)) +
  theme_map() +
  theme(legend.position = 'bottom') + 
  ggsave("map_change.png", height = 8, width = 10, dpi = 300)
 
##

points <- read_csv("data-out/rallies.csv")

##

map_clinton <- 
  ggplot() +
  geom_sf(data = states, 
          aes(), fill = NA, colour = '#000000', lwd = 0.5, alpha = 0.5) +
 # geom_sf(data = regression %>% 
#            select(GEOID, flips) %>%
#            filter(flips == "DDR") %>%
#            left_join(counties) %>%
#            st_as_sf() %>%
#            group_by(flips) %>%
#            summarise(),
#          aes(), fill = pal[6], colour = pal[8], lwd = 0.5, alpha = 0.5) +
  geom_point(data = points %>%
               filter(candidate == 'clinton') %>%
               st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
               st_transform(102003) %>%
               st_coordinates() %>%
               as_tibble(),
             aes(x = X, y = Y), colour = pal[1], size = 4) +
  theme_map() + 
  ggsave("map_clinton.png", height = 8, width = 10, dpi = 300)

map_trump <- 
  ggplot() +
  geom_sf(data = states, 
          aes(), fill = NA, colour = '#000000', lwd = 0.5, alpha = 0.5) +
 # geom_sf(data = regression %>% 
#            select(GEOID, flips) %>%
#            filter(flips == "DDR") %>%
#            left_join(counties) %>%
#            st_as_sf() %>%
#            group_by(flips) %>%
#            summarise(),
#          aes(), fill = pal[6], colour = pal[8], lwd = 0.5, alpha = 0.5) +
  geom_point(data = points %>%
               filter(candidate == 'trump') %>%
               st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
               st_transform(102003) %>%
               st_coordinates() %>%
               as_tibble(),
             aes(x = X, y = Y), colour = pal[9], size = 4) +
  theme_map() +
  ggsave("map_trump.png", height = 8, width = 10, dpi = 300)

##

library(ggdag)

##

dag  <- dagify(v ~ e,
               v ~ c,
               v ~ s,
               e ~ c,
               s ~ c, 
               exposure = "c",
               outcome = "v")

ggdag(dag) + theme_dag_blank() + ggsave("dag.png", height = 4, width = 4, dpi = 300)

##

library(glue)
library(readxl)

##

congress <-
  reduce(
    map(seq(2008, 2018, by = 2), function(x) {
      glue("data-in/leip/splitting/House_Election_Data_{x}.xlsx") %>%
        read_xlsx(sheet = 3, skip = 0) %>%
        clean_names() %>%
        mutate(fips = if_else(fips < 10000, paste("0", as.character(fips), sep = ""), as.character(fips))) %>%
        select(fips, total_vote, 7:18, -9) %>%
        set_names(c("GEOID", ids)) %>%
        mutate(year = x) %>%
        mutate(dem = as.numeric(dem),
               rep = as.numeric(rep),
               ind = as.numeric(ind),
               other = as.numeric(ind)) %>%
        drop_na(GEOID)
    }),
    bind_rows
  )

##

bind_rows(congress %>%
            filter(GEOID %in% spatial$GEOID) %>%
            left_join(select(spatial, GEOID, rallied)) %>%
            mutate(diff = dem - rep) %>%
            select(GEOID, year, diff, rallied) %>%
            group_by(year, rallied) %>%
            summarise(change = mean(diff, na.rm = TRUE)) %>%
            ungroup() %>% 
            rename(rally = rallied) %>%
            mutate(group = "spatial"), 
          congress %>%
            filter(GEOID %in% social$GEOID) %>%
            left_join(select(social, GEOID, rally)) %>%
            mutate(diff = dem - rep) %>%
            select(GEOID, year, diff, rally) %>%
            group_by(year, rally) %>%
            summarise(change = mean(diff, na.rm = TRUE)) %>%
            ungroup() %>%
            mutate(group = "social")) %>%
  ggplot(aes(year, change, colour = factor(rally))) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = c(2008, 2010, 2012, 2014, 2016, 2018)) +
  scale_colour_manual(values = c(pal[1], pal[9]), name = 'rally indicator') +
  facet_wrap(~ group, ncol = 1) +
  ylab("Margin (dem) for congress") +
  theme_hor() +
  theme(legend.position = c(0.8, 0.8)) +
  ggsave("congress.png", height = 8, width = 6, dp i = 300)

  


  

