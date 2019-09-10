########################################################
## Section 1: Social Security Disability Insurance
## ## 
## ## 
########################################################

library(readxl)
library(tidyverse)
library(janitor)

##

ssi <- read_excel("data-in/ssa/ssi_c16.xlsx") %>%
  clean_names() %>%
  remove_empty(c("rows", "cols"))

##

ssi %>%
  transmute(GEOID = if_else(fips < 10000, paste("0", fips, sep = ""), paste(fips)),
            ssi_recipients = ssi_recipients) %>%
  left_join(population) %>%
  select(1, 2, population) %>%
  mutate(ssi_rate = (ssi_recipients / population) * 100) %>%
  select(-population) %>%
  write_csv("ssi.csv")
