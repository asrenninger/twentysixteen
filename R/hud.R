library(tidyverse)
library(janitor)

##

foreclosed <- read_csv("data-in/hud/nsp_foreclosures.csv")

##

foreclosed %>%
  rename(GEOID = countycode) %>%
  mutate(foreclosure_rate = estimated_number_foreclosures / estimated_number_mortgages) %>%
  write_csv("foreclosures.csv")

##  