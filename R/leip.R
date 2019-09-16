########################################################
## Section 1: Election Atlas
## ## Read in Excel sheets
## ## Create long dataframe
########################################################

library(tidyverse)
library(glue)
library(readxl)

##

votes <- 
  reduce(
  map(c(2008, 2012, 2016), function(x) {
    glue("~/Desktop/R/git/twentysixteen/data-in/leip/Pres_Election_Data_{x}.xlsx") %>%
      read_xlsx(sheet = 3) %>%
      select(1:3, 7:8, 10:17, "FIPS") %>%
      set_names(c("county", "state", "total", "margin", "pct", 
                  "democrat_pct", "republican_pct", "third_pct", "other_pct",
                  "democrat_raw", "republican_raw", "third_raw", "other_raw",
                  "GEOID")) %>%
      select(GEOID, everything()) %>%
      mutate(year = x) %>%
      drop_na(county)
    }),
    bind_rows
  )

##

votes <-
  votes %>%
  filter(state != "T" & state != "PR") %>%
  mutate(GEOID = if_else(GEOID < 10000, paste("0", GEOID, sep = ""), paste(GEOID))) %>%
  filter(str_sub(GEOID, 1, 2) != "02")

##

write_csv(votes, "votes.csv")

##

read_csv("data-out/votes.csv") %>% 
  mutate(d = democrat_raw - republican_raw) %>%
  filter(year == 2016) %>%
  select(d, democrat_raw, republican_raw, total, GEOID, county, state) %>%
  mutate(pct_1 = democrat_raw / total,
         pct_2 = d / total) %>%
  arrange(desc(d))

vote <- read_csv("data-out/votes.csv") %>%
  filter(year == 2016) %>%
  select(GEOID, democrat_raw, republican_raw, total)

##

test <-
  votes %>%
  select(GEOID, year, total, democrat_raw, republican_raw, third_raw, other_raw) %>%
  group_by(GEOID) %>%
  mutate(total_lagged = lag(total),
         democrat_lagged = lag(democrat_raw),
         republican_lagged = lag(republican_raw),
         third_lagged = lag(third_raw),
         other_lagged = lag(other_raw)) %>% 
  mutate(total_lagged_2008 = lag(total_lagged),
         democrat_lagged_2008 = lag(democrat_lagged),
         republican_lagged_2008 = lag(republican_lagged),
         third_lagged_2008 = lag(third_lagged),
         other_lagged_2008 = lag(other_lagged)) %>%
  filter(year == 2016)

##

margins <-
  votes %>%
  select(GEOID, year, total, democrat_pct, republican_pct, third_pct, other_pct) %>%
  group_by(GEOID) %>%
  mutate(total_lagged = lag(total),
         democrat_lagged = lag(democrat_pct),
         republican_lagged = lag(republican_pct),
         third_lagged = lag(third_pct),
         other_lagged = lag(other_pct)) %>% 
  mutate(total_lagged_2008 = lag(total_lagged),
         democrat_lagged_2008 = lag(democrat_lagged),
         republican_lagged_2008 = lag(republican_lagged),
         third_lagged_2008 = lag(third_lagged),
         other_lagged_2008 = lag(other_lagged)) %>%
  ungroup() %>%
  filter(year == 2016) %>%
  mutate(margin = democrat_pct - republican_pct,
         margin_2012 = democrat_lagged - republican_lagged,
         margin_2008 = democrat_lagged_2008 - republican_lagged_2008) %>%
  mutate(change_2012 = margin - margin_2012,
         change_2008 = margin_2012 - margin_2008) %>%
  select(GEOID, margin, change_2008, change_2012, margin_2008, margin_2012)

##

flips <-
  votes %>%
  select(GEOID, year, total, democrat_pct, republican_pct, third_pct, other_pct) %>%
  group_by(GEOID) %>%
  mutate(total_lagged = lag(total),
         democrat_lagged = lag(democrat_pct),
         republican_lagged = lag(republican_pct),
         third_lagged = lag(third_pct),
         other_lagged = lag(other_pct)) %>% 
  mutate(total_lagged_2008 = lag(total_lagged),
         democrat_lagged_2008 = lag(democrat_lagged),
         republican_lagged_2008 = lag(republican_lagged),
         third_lagged_2008 = lag(third_lagged),
         other_lagged_2008 = lag(other_lagged)) %>%
  ungroup() %>%
  filter(year == 2016) %>%
  mutate(eight = case_when(democrat_lagged_2008 < republican_lagged_2008 ~ "R",
                           TRUE ~ "D"),
         twelve = case_when(democrat_lagged < republican_lagged ~ "R",
                            TRUE ~ "D"),
         sixteen = case_when(democrat_pct < republican_pct ~ "R",
                             TRUE ~ "D")) %>%
  mutate(flips = paste(eight, twelve, sixteen, sep = "")) %>%
  select(GEOID, flips) 

##

margins %>%
  left_join(flips) %>%
  write_csv("left.csv")
