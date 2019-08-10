########################################################
## Section 1: Election Atlas
## ## Read in Excel sheets
## ## Create long dataframe
########################################################

library(tidyverse)
library(glue)
library(readxl)

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
  filter(state != "T") %>%
  mutate(GEOID = if_else(GEOID < 10000, paste("0", GEOID, sep = ""), paste(GEOID)))

##

write_csv(votes, "votes.csv")

##


