########################################################
## Section 1: Propensity Score Matching
## ## Create a data set sans NA values
## ## perform scoring and matching
########################################################

library(janitor)
library(broom)

##

regression <- read_csv("data-out/regression.csv")

## 

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

write_csv(scoring_naz, "scoring_naz.csv")

##

fun(10)

before <- 
  scoring %>%
  select(one_of(covariates)) %>%
  map(~ t.test(.x ~ scoring$rally)$p.value) %>%
  as_tibble() %>% 
  gather() %>% 
  mutate(signif = ifelse(value < .05, "significant", "insignificant")) %>% 
  ggplot(aes(x = reorder(key, value), y = value)) + 
  geom_point(aes(), colour = '#555655', size = 5) + 
  geom_segment(aes(xend = reorder(key, value), yend = 0), colour = '#555655') +
  geom_hline(yintercept = 0.05, colour = '#00004C', size = 1, linetype = 3) +
  geom_text(aes(x = "percent_fair_poor", y = 0.2, label = "line marks 0.05 threshold"), hjust = 0, colour = '#7F0000') + 
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
  select(GEOID, change_2008, change_2012, covariates, d_trump:naz) %>%
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

matched <- match.data(matched)

##

summary(matched)

crosswalk <- read_csv("data-out/crosswalk.csv")
left <- read_csv("data-out/left.csv")

regression %>%
  left_join(crosswalk) %>%
  left_join(left) %>%
  tabyl(state, flips) %>%
  adorn_totals("row") %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting() %>%
  adorn_ns() %>%
  adorn_title("combined") %>%
  formattable::formattable()

##

regression %>%
  left_join(crosswalk) %>%
  left_join(left) %>%
  mutate(rally = case_when(trump_rallies_dma_post_convention > 0 ~ 1,
                           TRUE ~ 0)) %>%
  tabyl(flips, rally) %>%
  drop_na()

##

fun(10)

after <- 
  matched %>%
  select(one_of(covariates)) %>%
  map(~ t.test(.x ~ matched$rally)$p.value) %>%
  as_tibble() %>% 
  gather() %>% 
  mutate(signif = ifelse(value < .05, "significant", "insignificant")) %>% 
  ggplot(aes(x = reorder(key, value), y = value)) + 
  geom_point(aes(), colour = '#555655', size = 5) + 
  geom_segment(aes(xend = reorder(key, value), yend = 0), colour = '#555655') +
  geom_hline(yintercept = 0.05, colour = '#00004C', size = 1, linetype = 3) +
 # geom_text(aes(x = "unemployed", y = 0.5, label = "line marks 0.05 threshold"), hjust = 0, colour = '#7F0000') + 
  scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8)) +
  coord_flip() +
  ylab("p-value") +
  xlab("") +
  theme_ver() +
  ggsave("p-value_matched.png", height = 8, width = 10, dpi = 300)

##

library(grid)
library(gridExtra)

plots <- list(before, after)

blank <- grid.rect(gp = gpar(col = 'transparent', fill = 'transparent'))

##

lay <- rbind(c(1, 1, 1, 2, 2, 2),
             c(1, 1, 1, 2, 2, 2),
             c(1, 1, 1, 2, 2, 2))

agg <- grobTree(rectGrob(gp = gpar(fill = 'transparent', lwd = 0)), 
                grid.arrange(grobs = plots, layout_matrix = lay))


ggsave(agg, filename = "aggregate.png", height = 10, width = 20, dpi = 300)

##

with(matched, t.test((change_2012 * 100) ~ rally))

##

lm((change_2012 * 100) ~ rally, 
   data = matched) %>%
  summary()
          
##

matched_full <- 
  matched %>%
  left_join(scoring)

##

lm((change_2012 * 100) ~ rally +
     ssi_rate + 
     pcp_rate + dentist_rate +
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
     earn_ch +
     predatory_loans, 
   data = matched_full) %>%
  summary()

##

lm((change_2012 * 100) ~ rally +
     ssi_rate + 
     pcp_rate + dentist_rate +
     change_pills + dod_rate +
     pct_moved_int + 
     pct_foreign_lat + pct_foreign +
     rate_ch + 
     earn_ch +
     predatory_loans, 
   data = matched_full) %>%
  summary()

view(regression)

########################################################
## Section 1: Discontinuity Test
## ## Grab counties on both sides of media markets
## ## Test the difference
########################################################

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

regression %>%
  distinct(DMA) %>%
  pull() %>%
  unique()

regression %>%
  mutate(rallied = if_else(trump_rallies_dma_post_convention > 0, 1, 0)) %>%
  group_by(rallied) %>%
  summarise(n = n())

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

vote %>% 
  mutate(winner = case_when(`Clinton-Trump` < 0 ~ "Trump", 
                            `Clinton-Trump` > 0 ~ "Clinton"),
         absolutes = abs(`Clinton-Trump`)) %>%
  left_join(markets) %>% 
  ggplot() +
  geom_sf(data = markets, aes(), fill = '#555655', 
          colour = '#FAFBFB', size = 0.05, alpha = 0.5) +
  geom_sf(aes(fill = `Clinton-Trump`), 
          colour = NA, size = 0) +
  geom_sf(data = markets %>% group_by(DMA) %>% summarise(), 
          aes(), fill = NA, 
          colour = '#FAFBFB', size = 0.25, alpha = 0.5) +
  scale_fill_gradient2(low = '#F05154', high = '#62ACC9', mid = '#FAFBFB', 
                       limits = c(-10000, 10000), midpoint = 0, oob = squish,
                       labels = c("10k +", " 5k", "Tie", " 5k", "10k +"),
                       name = "Votes") +
  theme_map()

ggplot() +
  geom_sf(data = regression %>% 
            left_join(counties) %>%
            st_as_sf() %>%
            st_set_crs(102003),
          aes(fill = change_2012), colour = NA, size = 0, alpha = 0.25) +
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
  scale_fill_gradient2(low = pal[9], high = pal[1], mid = pal[5], 
                       limits = c(-0.2, 0.2), midpoint = 0, oob = squish,
                    #   labels = c("10k +", " 5k", "Tie", " 5k", "10k +"),
                       name = "margin") +
  theme_map() +
  ggsave("context.png", height = 8, width = 10, dpi = 300)

ggplot() +
  geom_sf(data = 
            regression %>% 
            left_join(counties) %>%
            st_as_sf() %>%
            st_set_crs(102003),
          aes(fill = change_2012), colour = NA, size = 0, alpha = 0.25) +
  geom_sf(data = boundary %>%
            st_set_crs(102003),
          aes(), alpha = 0.5) +
  geom_sf(data = matched %>%
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
  scale_fill_gradient2(low = pal[9], high = pal[1], mid = pal[5], 
                       limits = c(-0.2, 0.2), midpoint = 0, oob = squish,
                       #   labels = c("10k +", " 5k", "Tie", " 5k", "10k +"),
                       name = "margin") +
  theme_map() +
  ggsave("context.png", height = 8, width = 10, dpi = 300)

##

library(stargazer)

##

with(rallied, t.test((change_2012 * 100) ~ rallied))

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

stargazer(drop_na(dd, rallied), type = 'html')

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
  scale_fill_manual(values = c(pal[1], pal[9]), 
                    guide = 'none') +
  scale_y_continuous(breaks = c(0, -5, -10)) +
  xlab("") +
  ylab("% change from 2012 to 2016") +
  theme_ver() +
  ggsave("inside-outside.png", height = 8, width = 10, dpi = 300)

bind_rows(transmute(rallied, 
                    rally = rallied,
                    change_2012,
                    technique = "spatial"),
          transmute(matched, 
                    rally = rally,
                    change_2012,
                    technique = "social")) %>%
  drop_na(rally) %>%
  mutate(rallied = if_else(rally == 1, "visited", "not visited")) %>%
  ggplot(aes(y = change_2012 * 100, x = rallied, fill = rallied)) + 
  stat_summary(fun.y = "mean", 
               geom = "bar") + 
  stat_summary(fun.y = "mean", 
               geom = "bar") + 
  stat_summary(fun.data = "mean_cl_normal", 
               geom = "errorbar", 
               width = 0.1) +
  facet_wrap(~ technique) +
  scale_fill_manual(values = c(pal[1], pal[9]), 
                    guide = 'none') +
  xlab("") +
  ylab("% change from 2012 to 2016") +
  theme_ver() +
  ggsave("combined.png", height = 8, width = 16, dpi = 300)

ggplot(data = matched %>%
         drop_na(rally) %>%
         mutate(rallied = if_else(rally == 1, "visited", "not visited")), 
       aes(y = change_2012 * 100, x = factor(rallied), fill = factor(rallied))) + 
  stat_summary(fun.y = "mean", 
               geom = "bar") + 
  stat_summary(fun.y = "mean", 
               geom = "bar") + 
  stat_summary(fun.data = "mean_cl_normal", 
               geom = "errorbar", 
               width = 0.1) +
  scale_fill_manual(values = c(pal[1], pal[9]), 
                    guide = 'none') +
  xlab("") +
  ylab("% change from 2012 to 2016") +
  theme_ver() +
  ggsave("matched-not.png", height = 8, width = 10, dpi = 300)

with(matched, t.test(change_2012 ~ rally))
with(rallied, t.test(change_2012 ~ rallied))


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
  ggsave("p-value.png", height = 8, width = 10, dpi = 300)

rallied %>%
  group_by(rallied) %>%
  summarise(m = mean(pct_foreign),
            s = sd(pct_foreign),
            v = var(pct_foreign))

with(matched_full, t.test(change_2008 ~ rally))

regression %>%
  group_by(flips) %>%
  summarise(n = n())

matched_full %>%
  select(rally, covariates) %>%
  mutate(density = log(density),
         unemployment = log(unemployed)) %>%
  gather(variable, value, household_size:foreclosure_rate) %>%
  ggplot() +
  geom_density(aes(value, fill = factor(rally)), alpha = 0.5) +
  facet_wrap(~ variable, scales = 'free')

scoring %>%
  select(rally, covariates) %>%
  mutate(density = log(density),
         unemployment = log(unemployed)) %>%
  gather(variable, value, household_size:foreclosure_rate) %>%
  ggplot() +
  geom_density(aes(value, fill = factor(rally)), alpha = 0.5) +
  facet_wrap(~ variable, scales = 'free')

########################################################
## Section 3: Repeat with difference-in-differences
## ## Propensity scoring and matching
## ## Tests of discontinuity
########################################################

toy <- 
  matched_full %>%
  mutate(diff_n_diff = change_2008 - change_2012) %>%
  mutate(change_2008 = (change_2012 - change_2008) * 100)

##

with(toy, t.test(diff_n_diff ~ rally))

lm((change_2012 * 100) ~ rally,
   data = toy) %>%
  summary()

lm((change_2012 * 100) ~ rally + change_2008,
   data = toy) %>%
  summary()

##

toy <- 
  rallied %>%
  mutate(diff_n_diff = change_2008 - change_2012) %>%
  mutate(change_2008 = (change_2012 - change_2008) * 100)

##

toy %>%
  filter(GEOID != 36081 & GEOID != 36005 & GEOID != 36047) %>%
  with(., t.test(diff_n_diff ~ rallied))

##

with(toy, t.test(diff_n_diff ~ rallied))

lm((change_2012 * 100) ~ rallied,
   data = toy) %>%
  summary()

lm((change_2012 * 100) ~ rallied + change_2008,
   data = toy) %>%
  summary()

glimpse(toy)

ggplot(as_tibble(toy)) +
  geom_point(aes(x = d_trump, y = diff_n_diff, colour = factor(rallied))) +
  geom_smooth(aes(x = d_trump, y = diff_n_diff, colour = factor(rallied)),
              method = lm)

lm((change_2012 * 100) ~ rallied + d_trump,
   data = toy) %>%
  summary()

ggplot(as_tibble(toy)) +
  geom_point(aes(x = d_trump, y = change_2012, colour = factor(rally))) +
  geom_smooth(aes(x = d_trump, y = change_2012, colour = factor(rally)),
              method = lm)

scoring %>%
  group_by(rally) %>%
  summarise(n = n())

##

rallied %>%
  group_by(rallied) %>%
  summarise(n = n())

##

tiebreak <- 
  rallied %>%
  left_join(combined)

with(tiebreak, t.test((change_2016 * 100) ~ rallied))

sum(regression$trump_rallies_county_post_convention, na.rm = TRUE)
sum(regression$clinton_rallies_county_post_convention, na.rm = TRUE)
