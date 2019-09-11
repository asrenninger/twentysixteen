library(janitor)
library(broom)

##

regression <- read_csv("data-out/regression.csv")

## remove principal cities

full <-
  regression %>%
  group_by(DMA) %>%
  arrange(desc(population)) %>%
  slice(-1) %>%
  ungroup()

##

full <- 
  full %>%
  left_join(counties) %>%
  st_as_sf() %>%
  st_centroid() %>%
  st_coordinates() %>%
  as_tibble() %>%
  bind_cols(full) %>%
  mutate(naz = rowSums(is.na(full))) %>%
  mutate(rally = case_when(trump_rallies_dma_post_convention > 1 ~ 1,
                           trump_rallies_dma_post_convention < 2 ~ 0))

##

full$naz <- ntile(full$naz, 5)

##

write_csv(full, "full.csv")

##

library(matrixStats)

##

full_naz <- kNN(full, variable = names(full), dist_var = c("X", "Y", "population", "density"),
                imp_var = FALSE,
                numFun = weightedMean, weightDist = TRUE, k = 5)

##

write_csv(full_NAr, "full_naz.csv")

##

matching <- 
  full %>%
  select(change_2012, names(census), d_trump:naz) %>%
  mutate(rally = case_when(trump_rallies_dma_post_convention > 1 ~ 1,
                           trump_rallies_dma_post_convention < 2 ~ 0)) %>%
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










#cor.test(data = freq[freq$uniqueID == "ep001",],
#         ~ proportion + `avg`)
#
#

full_NAr <- read_csv("full_NAr.csv")

cor.test(data = matched_optimal[matched_optimal$uhoh == 1, ], ~ years_of_potential_life_lost_rate + percent_fair_poor)
cor.test(data = matched_optimal, ~ density + gini)
cor.test(data = matched_optimal, ~ white + blue)

scoring <- 
  full_NAr %>%
  clean_names() %>%
  mutate(rally = case_when(trump_rallies > 1 ~ 1,
                           trump_rallies < 2 ~ 0)) %>%
  na.omit()

scoring_Filtered <- 
  full_NAr %>%
  clean_names() %>%
  mutate(rally = case_when(trump_rallies > 1 ~ 1,
                           trump_rallies < 2 ~ 0)) %>%
  filter(population < 1500000) %>%
  filter(population > 5000) %>%
  na.omit()

matching_nearest <- matchit(rally ~ population +
                              foreclosures + all + blue + white + dependency + overdoses +
                              percent_18 + percent_65_and_over + percent_hispanic + percent_african_american +
                              household_income + household_size + gini + percent_unemployed + graduation_rate + 
                              percent_moved_abroad + percent_moved_nation + percent_foreign +
                              years_of_potential_life_lost_rate + percent_fair_poor
                            ,
                            data = scoring, method = "nearest",
                            ratio = 1)

matching_optimal <- matchit(rally ~ foreclosures + blue + white + dependency + overdoses +
                              percent_18 + percent_65_and_over + population + percent_hispanic +
                              household_income + household_size + gini + percent_unemployed + graduation_rate + 
                              percent_moved_abroad + percent_moved_nation + percent_foreign +
                              years_of_potential_life_lost_rate + percent_fair_poor
                            ,
                            data = scoring, method = "nearest",
                            ratio = 1)

matching_filters <- matchit(rally ~ population +
                              foreclosures + all + blue + white + dependency + overdoses +
                              percent_18 + percent_65_and_over + percent_hispanic + percent_african_american +
                              household_income + household_size + gini + percent_unemployed + graduation_rate + 
                              percent_moved_abroad + percent_moved_nation + percent_foreign +
                              years_of_potential_life_lost_rate + percent_fair_poor
                            ,
                            data = scoring_Filtered, method = "nearest",
                            ratio = 1)

matched_nearest <- match.data(matching_nearest)
matched_optimal <- match.data(matching_optimal)
matched_filters <- match.data(matching_filters)

with(matched_nearest, t.test(clinton_trump ~ rally))
with(matched_optimal, t.test(clinton_trump ~ rally))
with(matched_filters, t.test(clinton_trump ~ rally))

with(matched_nearest, t.test(trump_dl ~ rally))
with(matched_nearest, t.test(clinton_dl ~ rally))
with(matched_nearest, t.test(jill_stein ~ rally))
with(matched_nearest, t.test(gary_johnson ~ rally))

with(matched_filters, t.test(participation ~ rally))

with(matched_nearest, t.test((clinton_trump / total) ~ rally))
with(matched_optimal, t.test((clinton_trump / total) ~ rally))
with(matched_filters, t.test((clinton_trump / total) ~ rally))

with(matched_optimal, t.test((clinton_trump / total) ~ rally))
with(matched_optimal, t.test(((clinton_trump - gary_johnson - jill_stein - other_combined) / total) ~ rally))

with(scoring, t.test(clinton_trump ~ rally))

lm(clinton_trump ~ rally, data = scoring) %>% summary()
lm(clinton_trump ~ rally, data = matched_optimal) %>% summary()
lm(clinton_trump ~ rally + density + population, data = matched_nearest) %>% summary()

lm(((clinton_trump - gary_johnson - jill_stein - other_combined) / total) ~ rally, data = matched_filters) %>% summary()
lm(((clinton_trump - gary_johnson - jill_stein - other_combined) / total) ~ rally + d_trump, data = matched_filters) %>% summary()

lm((clinton_trump / total) ~ rally, data = matched_filters) %>% summary()
lm((clinton_trump / total) ~ rally + d_trump, data = matched_filters) %>% summary()

lm(clinton_trump ~ rally, data = scoring) %>% summary()
lm(clinton_trump ~ rally, data = matched_optimal) %>% summary()
lm(clinton_trump ~ rally, data = matched_nearest) %>% summary()

lm(((clinton_trump - gary_johnson - jill_stein - other_combined) / total) ~ rally, data = scoring) %>% summary()
lm(((clinton_trump - gary_johnson - jill_stein - other_combined) / total) ~ rally, data = matched_optimal) %>% summary()
lm(((clinton_trump - gary_johnson - jill_stein - other_combined) / total) ~ 
     rally +
     d_trump
   , 
   data = matched_optimal) %>% summary()



names(matched_optimal)

sum(matched_optimal$clinton_trump) /sum(matched_optimal$total)
sum(matched_optimal$clinton_trump) /sum(matched_optimal$total)

matched_optimal_w <-
  matched_optimal %>%
  bind_rows(mutate(full_NAr, rally = 0, distance = 0) %>%
              clean_names() %>%
              filter(!fips %in% matched_optimal$fips) %>%
              filter(uhoh == 1))


var_names <- 
  life %>%
  clean_names() %>%
  select(-fips, -state, -county, -cohort_size, -presence_of_violation) %>%
  names()

regdf <- 
  full_NAr %>%
  clean_names() %>%
  select(uhoh, one_of(var_names))

regdf <- 
  full_NAr %>%
  clean_names() %>%
  mutate(vote_share = clinton_trump / total) %>%
  filter(clinton_trump < 500000) %>%
  select(vote_share, one_of(var_names))

glm(uhoh ~ . , 
    family = "binomial" (link = "logit"), 
    data = regdf) %>% 
  pR2()

lm(vote_share ~ . , 
   family = "binomial" (link = "logit"), 
   data = regdf) %>% 
  summary()

matched_optimal %>%
  ggplot(aes(population)) +
  geom_histogram()

matched_optimal %>%
  filter(population > 1000 & population < 500000) %>%
  ggplot(aes(population, clinton_trump)) +
  geom_point(aes(colour = flips)) +
  geom_smooth(method = lm)



ggplot(matched_optimal_w %>% 
         filter(clinton_trump < (min(clinton_trump) * -1)) %>%
         mutate(dist = factor(ntile(d_trump, 3))), 
       aes(d_trump, (clinton_trump / total))) +
  geom_point(aes(colour = factor(uhoh))) +
  geom_smooth(method = lm) +
  theme_ver()
  

stargazer(lm(clinton_trump ~ rally, data = matched_optimal),
          lm(clinton_trump ~ rally + distance, data = matched_optimal),
          lm(clinton_trump ~ rally + distance + late_quartile, data = matched_optimal),
          type = 'text')

scoring_cov <- c("foreclosures", "blue", "white", "dependency", "overdoses",
                 "percent_18", "percent_65_and_over", "density", "percent_hispanic",
                 "household_income", "household_size", "gini", "percent_unemployed", "graduation_rate",
                 "percent_moved_abroad", "percent_moved_nation", "percent_foreign",
                 "years_of_potential_life_lost_rate", "percent_fair_poor")

scoring %>%
  group_by(rally) %>%
  select(one_of(scoring_cov)) %>%
  summarise_all(funs(mean(., na.rm = TRUE)))

scoring %>%
  select(one_of(scoring_cov)) %>%
  map(~ t.test(.x ~ scoring$rally)$p.value) %>%
  as_tibble() %>% 
  gather() %>% 
  mutate(signif = ifelse(value < .05, "significant", "ns")) %>% 
  ggplot(aes(x = reorder(key, value), y = value)) + 
  geom_point(aes(color = signif)) + 
  coord_flip() +
  ylab("p value")

matched_nearest %>%
  select(one_of(scoring_cov)) %>%
  map(~ t.test(.x ~ matched_nearest$rally)$p.value) %>%
  as_tibble() %>% 
  gather() %>% 
  mutate(signif = ifelse(value < .05, "significant", "ns")) %>% 
  ggplot(aes(x = reorder(key, value), y = value)) + 
  geom_point(aes(color = signif)) + 
  coord_flip() +
  ylab("p value")



bind_rows(mutate(matched_nearest %>%
                   select(one_of(scoring_cov)) %>%
                   map(~ t.test(.x ~ matched_nearest$rally)$p.value) %>%
                   as_tibble() %>% 
                   gather() %>% 
                   mutate(signif = ifelse(value < .05, "< 0.05", "> 0.05")), type = "matched"),
          mutate(scoring %>%
                   select(one_of(scoring_cov)) %>%
                   map(~ t.test(.x ~ scoring$rally)$p.value) %>%
                   as_tibble() %>% 
                   gather() %>% 
                   mutate(signif = ifelse(value < .05, "< 0.05", "> 0.05")), type = "unmatched")) %>%
  ggplot(aes(x = reorder(key, value), y = value)) + 
  geom_point(aes(color = signif)) + 
  coord_flip() +
  facet_wrap(~ type, nrow = 2) +
  scale_colour_manual(values = c('#d7bc6a', '#2d2c41')) +
  ylab("p value") +
  xlab("variable") +
  theme_ver()

matched_nearest %>%
  select(one_of(scoring_cov)) %>%
  map(~ t.test(.x ~ matched_nearest$rally)) %>%
  map_dbl(~ .x$p.value)

test_tibble <-
  tibble(
    variable = scoring_cov,
    mean_umatched_0 = 
      scoring %>%
      filter(rally == 0) %>%
      select(one_of(scoring_cov)) %>%
      map_dbl(~ mean(.x)) %>%
      round(4),
    mean_umatched_1 = 
      scoring %>%
      filter(rally == 1) %>%
      select(one_of(scoring_cov)) %>%
      map_dbl(~ mean(.x)) %>%
      round(4),
    p_value_u  = 
      scoring %>%
      select(one_of(scoring_cov)) %>% 
      map(~ t.test(.x ~ scoring$rally)) %>%
      map_dbl(~ .x$p.value) %>%
      round(4),
    mean_matched_0 = 
      matched_nearest %>%
      filter(rally == 0) %>%
      select(one_of(scoring_cov)) %>%
      map_dbl(~ mean(.x)) %>%
      round(4),
    mean_matched_1 = 
      matched_nearest %>%
      filter(rally == 1) %>%
      select(one_of(scoring_cov)) %>%
      map_dbl(~ mean(.x)) %>%
      round(4),
    p_value_m  = 
      matched_nearest %>%
      select(one_of(scoring_cov)) %>% 
      map(~ t.test(.x ~ matched_nearest$rally)) %>%
      map_dbl(~ .x$p.value) %>%
      round(4)

  )

install.packages("formattable")
library(formattable)

test_tibble %>%
  formattable(list(p_value_u = color_tile("transparent", '#2d2c41'), 
                   p_value_m = color_tile("transparent", '#2d2c41')))

names(test_tibble)

with(full_NAr, t.test(trump_dl ~ uhoh))

full_NAr %>%
  clean_names() %>%
  select(one_of(scoring_cov)) %>%
  map(~ t.test(.x ~ full_NAr$uhoh)$p.value) %>%
  as_tibble() %>% 
  gather() %>% 
  mutate(signif = ifelse(value < .05, "significant", "ns")) %>% 
  ggplot(aes(x = reorder(key, value), y = value)) + 
  geom_point(aes(color = signif)) + 
  coord_flip() +
  ylab("p value")

full_NAr %>%
  clean_names() %>%
  mutate(winner = case_when(clinton_trump < 0 ~ 1, 
                            clinton_trump > 0 ~ 0)) %>%
  filter(winner == 1 | uhoh == 1) %>%
  select("white", "blue", "all") %>%
  map(~ t.test(.x ~ full_NAr %>%
                 clean_names() %>%
                 mutate(winner = case_when(clinton_trump < 0 ~ 1, 
                                           clinton_trump > 0 ~ 0)) %>%
                 filter(winner == 1 | uhoh == 1) %>%
                 pull(uhoh)))

