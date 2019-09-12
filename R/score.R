library(janitor)
library(broom)

##

regression <- read_csv("data-out/regression.csv")

## remove principal cities

scoring <-
  regression %>%
  group_by(DMA) %>%
  arrange(desc(population)) %>%
  slice(-1) %>%
  ungroup()

##

scoring <- 
  scoring %>%
  left_join(counties) %>%
  st_as_sf() %>%
  st_centroid() %>%
  st_coordinates() %>%
  as_tibble() %>%
  bind_cols(scoring) %>%
  mutate(rally = case_when(trump_rallies_dma_post_convention > 1 ~ 1,
                           trump_rallies_dma_post_convention < 2 ~ 0)) %>%
  mutate(naz = rowSums(is.na(scoring))) %>%
  drop_na(population)

##

scoring$naz <- ntile(scoring$naz, 5)

##

write_csv(scoring, "scoring.csv")

##

library(matrixStats)
library(VIM)

##

scoring_naz <- kNN(scoring, variable = names(scoring), dist_var = c("X", "Y", "population", "density"),
                   imp_var = FALSE,
                   numFun = weightedMean, weightDist = TRUE, k = 10)

##

write_csv(scoring_naz, "full_naz.csv")

##

population <- read_csv("data-out/population.csv") %>%
changes <- read_csv("data-out/changes.csv")

##

matching <- 
  scoring %>%
  select(change_2012, names(census), d_trump:naz) %>%
  drop_na()

##

library(MatchIt)
library(optmatch)

##

names(matching)

##

matched <- matchit(rally ~ population + density + household_size +
                     education +
                     median_age + pct_black + 
                     gini + home + earn + unemployment
                   ,
                   data = matching, method = "nearest",
                   ratio = 1)

##

matched_nearest <- match.data(matched)

##

logistically <- glm(rally ~ population + density + household_size +
                      education +
                      median_age + pct_black + 
                      gini + home + earn + unemployment, 
                    data = full, family = binomial(link = 'logit'))

summary(logistically)

scores <- predict(logistically, type = 'link')

##

full$score <- scores

##

matched <- matchit(rally ~ population + density + household_size +
                     education +
                     median_age + pct_black + 
                     gini + home + earn + unemployment,
                   data = matching, method = "nearest",
                   ratio = 2)

##

matched_optimal <- match.data(matched)

##

summary(matched)

##

with(matched_nearest, t.test(change_2012 ~ rally))
with(matched_optimal, t.test(change_2012 ~ rally))

##

lm((change_2012 * 100) ~ rally, 
   data = matched_nearest) %>%
  summary()

lm((change_2012 * 100) ~ rally, 
   data = matched_optimal) %>%
  summary()

##

matched_nearest %>%
  left_join(counties) %>%
  st_as_sf() %>%
  select(change_2012) %>%
  plot()

##

matched_full <- 
  matched_nearest %>%
  left_join(full)

names(matched_full)

##

lm((change_2012 * 100) ~ rally +
     ssi_rate +
     pcp_rate + percent_fair_poor +
     change_pills + dod_rate, 
   data = matched_full) %>%
  summary()

##

lm((change_2012 * 100) ~ rally +
     pct_moved_int_ch + pct_moved_int + 
     pct_foreign_lat, 
   data = matched_full) %>%
  summary()

##

lm((change_2012 * 100) ~ rally +
     rate_ch +
     foreclosure_rate + gini + home + rent, 
   data = matched_full) %>%
  summary()

##

lm((change_2012 * 100) ~ rally +
     rate_ch +
     foreclosure_rate + gini + home + rent +
     pct_moved_int_ch + pct_moved_int + 
     pct_foreign_lat +
     ssi_rate +
     pcp_rate + percent_fair_poor +
     change_pills + dod_rate, 
   data = matched_full) %>%
  summary()

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
  filter(str_detect(DMA, "PHILADELPHIA"))  %>%
  st_cast("MULTILINESTRING")

##

counties <- st_read("data-out/counties.geojson", crs = 102003)

##

ggplot() +
  geom_sf(data = counties[touching, ], aes(), fill = NA, colour = 'green') +
  geom_sf(data = outer, aes(), colour = 'black') +
  geom_sf(data = inner, aes(), colour = 'red') +
  geom_sf(data = middle, aes(), colour = 'blue')

##

touching <- st_touches(counties, middle) %>% as_tibble() %>% pull(row.id)

##

library(tidyverse)
library(sf)

##

counties <- st_read("data-out/counties.geojson")

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

##

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

rallied <- 
  regression %>%
  filter(GEOID %in% borders) %>%
  filter(GEOID %in% coastal) %>%
  mutate(rallied = if_else(trump_rallies_dma_post_convention > 0, 1, 0))

##

rallied %>%
  left_join(counties) %>%
  st_as_sf() %>%
  select(change_2012) %>%
  plot()

##

library(stargazer)

##

with(rallied, t.test((margin * 100) ~ rallied))

##

rallied %>%
  drop_na(rallied) %>%
  ggplot(aes(x = (change_2012 * 100))) +
  geom_histogram(aes(fill = rallied), show.legend = FALSE) +
  facet_wrap(~ rallied, nrow = 2)

##

md <- rallied %>% 
  group_by(rallied) %>% 
  summarize(N = length(change_2012), 
            Mean = mean(change_2012 * 100),
            SD = sd(change_2012 * 100),
            SE = SD / sqrt(N)) 

td <- rallied %>% 
  summarize(rallied = "Total",
            N = length(change_2012 * 100), 
            Mean = mean(change_2012 * 100),
            SD = sd(change_2012 * 100),
            SE = SD/sqrt(N))

dd <- rbind(md,td)

p <- 
  ggplot(data = rallied, 
         aes(y = change_2012 * 100, x = rallied, fill = rallied)) 

p + stat_summary(fun.y = "mean", 
                 geom = "bar") 

p + stat_summary(fun.y = "mean", 
                 geom = "bar") + 
  stat_summary(fun.data = "mean_cl_normal", 
               geom = "errorbar", 
               width = 0.1)
##

rallied %>%
  drop_na(rallied) %>%
  ggplot(aes()) +
  stat_summary(fun.y = "mean", 
                   geom = "bar") + 
  stat_summary(fun.data = "mean_cl_normal", 
               geom = "errorbar", 
               width = 0.1)

