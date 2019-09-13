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

covariates <- c("population", "household_size",
                "education", "median_age", 
                "percent_fair_poor", 
                "pct_black", "pct_foreign", 
                "gini", "home", "earn")

covariates <- c("household_size", "density",
                "education", "median_age", 
                "percent_fair_poor", "years_of_potential_life_lost_rate",
                "pct_black", "pct_foreign", "unemployed",
                "gini", "home", "earn", "foreclosure_rate")

##

matching <- 
  scoring %>%
  select(change_2012, covariates, d_trump:naz) %>%
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

matched <- matchit(rally ~  density + household_size +
                     education + median_age + 
                     percent_fair_poor + years_of_potential_life_lost_rate +
                     pct_black + pct_foreign +
                     gini + home + earn + foreclosure_rate,
                   data = matching, method = "nearest",
                   ratio = 1)

##

matched_optimal <- match.data(matched)

##

summary(matched)

##

names(scoring)
sum(is.na(scoring$percent_fair_poor))


##

covariates <- c("population", "density", "household_size",
                "education", "median_age", 
                "percent_fair_poor", 
                "pct_black", "pct_white", "pct_foreign", 
                "gini", "home", "earn","unemployment")

covariates <- c("household_size",
                "education", "median_age", 
                "percent_fair_poor", 
                "pct_black", "pct_foreign", 
                "gini", "home", "earn")

##

matched_nearest %>%
  select(one_of(covariates)) %>%
  map(~ t.test(.x ~ matched_nearest$rally)$p.value) %>%
  as_tibble() %>% 
  gather() %>% 
  mutate(signif = ifelse(value < .05, "significant", "insignificant")) %>% 
  ggplot(aes(x = reorder(key, value), y = value)) + 
  geom_point(aes(), colour = '#555655', size = 5) + 
  geom_segment(aes(xend = reorder(key, value), yend = 0), colour = '#555655') +
  geom_hline(yintercept = 0.05, colour = '#7b6576', size = 1, linetype = 3) +
  geom_text(aes(x = "population", y = 0.2, label = "line marks 0.05 threshold"), hjust = 0, colour = '#7b6576') + 
  coord_flip() +
  ylab("p-value") +
  xlab("") +
  theme_ver() +
  ggsave("p-value.png", height = 8, width = 10, dpi = 300)

matched_optimal %>%
  select(one_of(covariates)) %>%
  map(~ t.test(.x ~ matched_optimal$rally)$p.value) %>%
  as_tibble() %>% 
  gather() %>% 
  mutate(signif = ifelse(value < .05, "significant", "insignificant")) %>% 
  ggplot(aes(x = reorder(key, value), y = value)) + 
  geom_point(aes(), colour = '#555655', size = 5) + 
  geom_segment(aes(xend = reorder(key, value), yend = 0), colour = '#555655') +
  geom_hline(yintercept = 0.05, colour = '#7b6576', size = 1, linetype = 3) +
  geom_text(aes(x = "unemployed", y = 0.5, label = "line marks 0.05 threshold"), hjust = 0, colour = '#7b6576') + 
  coord_flip() +
  ylab("p-value") +
  xlab("") +
  theme_ver() +
  ggsave("p-value.png", height = 8, width = 10, dpi = 300)

ggplot(matched_optimal) +
  geom_histogram(aes(percent_fair_poor, fill = rally)) +
  facet_wrap(~ rally, nrow = 2)

##

names(matched_full)

covariates <- c("household_size",
                "education", "median_age", 
                "percent_fair_poor", "years_of_potential_life_lost_rate",
                "pct_black", "pct_foreign", "unemployed",
                "gini", "home", "earn", "foreclosure_rate")

regression %>%
  left_join(counties) %>%
  st_as_sf() %>%
  select(foreclosure_rate) %>%
  plot()

##

matched_full %>%
  select(one_of(covariates)) %>%
  map(~ t.test(.x ~ matched_full$rally)$p.value) %>%
  as_tibble() %>% 
  gather() %>% 
  mutate(signif = ifelse(value < .05, "significant", "insignificant")) %>% 
  ggplot(aes(x = reorder(key, value), y = value)) + 
  geom_point(aes(), colour = '#555655', size = 5) + 
  geom_segment(aes(xend = reorder(key, value), yend = 0), colour = '#555655') +
  geom_hline(yintercept = 0.05, colour = '#7b6576', size = 1, linetype = 3) +
  geom_text(aes(x = "foreclosure_rate", y = 0.2, label = "line marks 0.05 threshold"), hjust = 0, colour = '#7b6576') + 
  coord_flip() +
  ylab("p-value") +
  xlab("") +
  theme_ver() +
  ggsave("p-value.png", height = 8, width = 10, dpi = 300)

##

with(matched_nearest, t.test((change_2012 * 100) ~ rally))
with(matched_optimal, t.test((change_2012 * 100) ~ rally))

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
  left_join(scoring)

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
     pct_moved_int + 
     pct_foreign_lat + pct_foreign, 
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
  mutate(rallied = if_else(trump_rallies_dma_post_convention > 0, 1, 0)) %>%
  filter(GEOID %in% borders) %>%
  filter(GEOID %in% coastal | rallied == 0) 

##

rallied %>%
  left_join(counties) %>%
  st_as_sf() %>%
  select(change_2012) %>%
  plot()

##

ggplot() +
  geom_sf(data = boundary %>%
            st_set_crs(102003),
          aes(), alpha = 0.5) +
  geom_sf(data = rallied %>%
            left_join(counties) %>%
            st_as_sf() %>%
            st_set_crs(102003),
          aes(fill = change_2012),
          colour = NA, size = 0,
          show.legend = FALSE) +
  geom_sf(data = visited %>%
            st_set_crs(102003),
          aes(),
          colour = '#7b6576') +
  theme_map()

##

library(stargazer)

##

with(rallied, t.test((change_2012 * 100) ~ rallied))

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
  drop_na(rallied) %>%
  summarize(rallied = "Total",
            N = length(change_2012 * 100), 
            Mean = mean(change_2012 * 100),
            SD = sd(change_2012 * 100),
            SE = SD / sqrt(N))

dd <- rbind(md,td)

formattable::formattable(drop_na(dd, rallied))


ggplot(data = rallied %>%
         drop_na(rallied) %>%
         mutate(rallied = if_else(rallied == 1, "visited", "not visited")), 
       aes(y = change_2012 * 100, x = factor(rallied), fill = factor(rallied))) + 
  stat_summary(fun.y = "mean", 
               geom = "bar") + 
  stat_summary(fun.y = "mean", 
               geom = "bar") + 
  stat_summary(fun.data = "mean_cl_normal", 
               geom = "errorbar", 
               width = 0.1) +
  scale_fill_manual(values = c("#67b1b8", "#f05154"), 
                    guide = 'none') +
  xlab("") +
  ylab("% change from 2012 to 2016") +
  theme_ver() +
  ggsave("inside-outside.png", height = 8, width = 10, dpi = 300)

##

rallied %>%
  select(one_of(covariates, "density")) %>%
  map(~ t.test(.x ~ rallied$rallied)$p.value) %>%
  as_tibble() %>% 
  gather() %>% 
  mutate(signif = ifelse(value < .05, "significant", "insignificant")) %>% 
  ggplot(aes(x = reorder(key, value), y = value)) + 
  geom_point(aes(), colour = '#555655', size = 5) + 
  geom_segment(aes(xend = reorder(key, value), yend = 0), colour = '#555655') +
  geom_hline(yintercept = 0.05, colour = '#7b6576', size = 1, linetype = 3) +
  geom_text(aes(x = "density", y = 0.5, label = "line marks 0.05 threshold"), hjust = 0, colour = '#7b6576') + 
  coord_flip() +
  ylab("p-value") +
  xlab("") +
  theme_ver() +
  ggsave("test.png", height = 8, width = 10, dpi = 300)

hrallied %>%
  drop_na(rallied) %>%
  ggplot(aes(x = pct_foreign)) +
  geom_histogram(aes(fill = rallied)) +
  facet_wrap(~ rallied, nrow = 2)

rallied %>%
  group_by(rallied) %>%
  summarise(m = mean(pct_foreign),
            s = sd(pct_foreign),
            v = var(pct_foreign))

with(matched_full, t.test(dentist_rate ~ rally))

regression %>%
  group_by(flips) %>%
  summarise(n = n())
