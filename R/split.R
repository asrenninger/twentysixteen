########################################################
## Section 1: Gather additional data
## ## Read in Leip data
## ## Join in spatial attributes
########################################################

library(tidyverse)
library(janitor)
library(readxl)

##

xl_data <- "data-in/leip/splitting/Pres_Election_Data_2016.xlsx"
excel_sheets(path = xl_data)

##

reference <- 
  read_excel(path = xl_data, sheet = 3, skip = 0) %>% 
  clean_names() %>%
  rename(county = x_1, state = x_2) %>%
  select(fips, county, state) %>%
  drop_na(fips)

##

results <- 
  reference %>%
  filter(state != "T") %>%
  filter(fips > 1000) %>%
  mutate(GEOID = if_else(fips < 10000, paste("0", as.character(fips), sep = ""), as.character(fips))) %>%
  select(-fips)

##

races <- c("Pres_Election", "House_Election", "Sen_Election", "Gov_Election")

##

for (i in 1:length(races)) {
  
  index <- races[i]
  xl_data <- glue("data-in/leip/splitting/{index}_Data_2016.xlsx")
  
  ids <- 
    c("total", "margin", "percent","dem", "rep", "ind", "other",
      "dem_raw", "rep_raw", "ind_raw", "ind_2_raw", "ind_3_raw")
  
  ids <- glue("{ids}_{index}") %>% str_to_lower()
  
  result <-
    read_excel(path = xl_data, sheet = 3, skip = 0) %>% 
    clean_names() %>%
    mutate(fips = if_else(fips < 10000, paste("0", as.character(fips), sep = ""), as.character(fips))) %>%
    select(fips, total_vote, 7:18, -9) %>%
    set_names(c("GEOID", ids)) %>%
    drop_na(GEOID)
  
  results <- 
    left_join(results, result)
  
}

##

library(sf)

##

spatial_data <-
  bind_rows(results %>%
              mutate(clinton_trump = dem_raw_pres_election - rep_raw_pres_election,
                     winner = "trump") %>%
              filter(clinton_trump < 0) %>%
              mutate(split_raw = rep_raw_pres_election - dem_raw_house_election,
                     split_per = ((rep_raw_pres_election - dem_raw_house_election) / total_pres_election) * -1) %>%
              select(GEOID, county, state, winner, clinton_trump, split_raw:split_per) %>%
              mutate(split_net = if_else(split_per > 0, 1, 0)),
            results %>%
              mutate(clinton_trump = dem_raw_pres_election - rep_raw_pres_election,
                     winner = "clinton") %>%
              filter(clinton_trump >= 0) %>%
              mutate(split_raw = dem_raw_pres_election - rep_raw_house_election,
                     split_per = (dem_raw_pres_election - rep_raw_house_election) / total_pres_election) %>%
              select(GEOID, county, state, winner, clinton_trump, split_raw:split_per) %>%
              mutate(split_net = if_else(split_per < 0, 1, 0))) %>%
  left_join(counties) %>%
  st_as_sf()

########################################################
## Section 2: Measure and test splits
## ## Map it...
## ## Test it...
########################################################

states <- st_read("data-out/states.geojson", crs = 102003)

##

guide_continuous <- 
  guide_colorbar(direction = "horizontal",
                 barwidth = unit(50, units = "mm"),
                 barheight = unit(2, units = "mm"),
                 draw.ulim = FALSE,
                 title.position = 'top',
                 label.position = 'bottom',
                 title.hjust = 0.5,
                 label.hjust = 0.5)

##

ggplot() +
  geom_sf(data = spatial_data,
          aes(fill = split_per), colour = NA, size = 0) +
  geom_sf(data = states, 
          aes(), fill = NA, colour = '#ffffff', size = 0.25) +
  geom_sf(data = 
            spatial_data %>%
            filter(split_net != 0) %>%
            group_by(split_net) %>%
            summarise(),
          aes(), fill = NA, colour = '#7d7d7d', size = 0.5) +
  scale_fill_gradientn(colours = rev(pal),
                       name = "pres-house | total ballots", 
                       limits = c(-0.5, 0.5), 
                       oob = squish,
                       guide = guide_continuous) +
  theme_map_legend() +
  ggsave("test.png") 

##

splitting <-
  bind_rows(results %>%
              mutate(clinton_trump = dem_raw_pres_election - rep_raw_pres_election,
                     winner = "trump") %>%
              filter(clinton_trump < 0) %>%
              mutate(split_raw = rep_raw_pres_election - dem_raw_house_election,
                     split_per = ((rep_raw_pres_election - dem_raw_house_election) / total_pres_election)) %>%
              select(GEOID, county, state, winner, clinton_trump, split_raw:split_per) %>%
              mutate(split_net = if_else(split_per > 0, 1, 0)),
            results %>%
              mutate(clinton_trump = dem_raw_pres_election - rep_raw_pres_election,
                     winner = "clinton") %>%
              filter(clinton_trump >= 0) %>%
              mutate(split_raw = dem_raw_pres_election - rep_raw_house_election,
                     split_per = (dem_raw_pres_election - rep_raw_house_election) / total_pres_election) %>%
              select(GEOID, county, state, winner, clinton_trump, split_raw:split_per) %>%
              mutate(split_net = if_else(split_per < 0, 1, 0)))

##

splitting %>% 
  select(-county, -state) %>%
  right_join(rallied) %>%
  drop_na(rallied) %>%
  with(t.test(split_per ~ rallied))

##

splitting %>% 
  select(-county, -state) %>%
  right_join(matched) %>%
  drop_na(rally) %>%
  with(t.test(split_per ~ rally))

##

ids <- 
  c("total", "margin", "percent","dem", "rep", "ind", "other",
    "dem_raw", "rep_raw", "ind_raw", "ind_2_raw", "ind_3_raw")

##

congress <-
  reduce(
    map(seq(2008, 2018, by = 2), function(x) {
      glue("~/Desktop/R/git/twentysixteen/data-in/leip/splitting/House_Election_Data_{x}.xlsx") %>%
        read_xlsx(sheet = 3, skip = 0) %>%
        clean_names() %>%
        mutate(fips = if_else(fips < 10000, paste("0", as.character(fips), sep = ""), as.character(fips))) %>%
        select(fips, total_vote, 7:18, -9) %>%
        set_names(c("GEOID", ids)) %>%
        mutate(year = x) %>%
        mutate(dem = as.numeric(dem),
               rep = as.numeric(rep),
               ind = as.numeric(ind),
               other = as.numeric(ind))
      }),
    bind_rows
  )

##

splitting <-
  bind_rows(results %>%
              mutate(clinton_trump = dem_raw_pres_election - rep_raw_pres_election,
                     winner = "trump") %>%
              filter(clinton_trump < 0) %>%
              mutate(split_raw = rep_raw_pres_election - dem_raw_sen_election,
                     split_per = ((rep_raw_pres_election - dem_raw_sen_election) / total_pres_election)) %>%
              select(GEOID, county, state, winner, clinton_trump, split_raw:split_per) %>%
              mutate(split_net = if_else(split_per > 0, 1, 0)),
            results %>%
              mutate(clinton_trump = dem_raw_pres_election - rep_raw_pres_election,
                     winner = "clinton") %>%
              filter(clinton_trump >= 0) %>%
              mutate(split_raw = dem_raw_pres_election - rep_raw_sen_election,
                     split_per = (dem_raw_pres_election - rep_raw_sen_election) / total_pres_election) %>%
              select(GEOID, county, state, winner, clinton_trump, split_raw:split_per) %>%
              mutate(split_net = if_else(split_per < 0, 1, 0)))

##

splitting %>% 
  select(-county, -state) %>%
  right_join(rallied) %>%
  drop_na(rallied) %>%
  with(t.test(split_per ~ rallied))

##

splitting %>% 
  select(-county, -state) %>%
  right_join(matched) %>%
  drop_na(rally) %>%
  with(t.test(split_per ~ rally))



