read_csv("data-out/jobs.csv")
read_csv("data-out/batch.csv") %>% glimpse()

library(tidycensus)

vars <- load_variables(year = 2016, dataset = "acs5", cache = TRUE)
test <- 
  vars %>%
  filter(str_detect(label, "Bachelor's degree"))

vars %>%
  filter(name == "B19083_001")

replicating <- 
  get_acs(geography = "county", variables = "B15002_015",           
        year = 2016, output = "wide") %>% 
  rename(white_nobach = B15002_015E) %>%
  select(GEOID, white_nobach)

turnout <- 
  read_csv("data-in/census/turnout.csv") %>%
  clean_names() %>%
  filter(state != "US") %>%
  filter(sex_race_and_hispanic_origin == "Total") %>%
  mutate(state = str_to_title(state),
         turnout = as.numeric(total_voted) / as.numeric(total_registered)) %>%
  select(state, turnout) %>%
  rename(state_name = state) %>%
  left_join(fips_codes) %>%
  select(state, turnout) %>%
  group_by(state) %>%
  summarise(turnout = mean(turnout))

unemployment <- 
  read_csv("data-out/jobs.csv") %>%
  filter(year == "2016") %>%
  mutate(unemployment_rate = rate) %>%
  select(GEOID, rate)

education <- 
  read_csv("data-out/batch.csv") %>% 
  filter(year == 2016) %>%
  select(GEOID, peeps) %>%
  left_join(replicating) %>%
  mutate(pct_white_nobach = white_nobach / peeps) %>%
  arrange(pct_white_nobach) %>%
  left_join(crosswalk) %>%
  select(GEOID, peeps, pct_white_nobach)

rallies <-
  dated %>%
  group_by(GEOID, candidate) %>%
  summarise(rallies = n()) %>%
  spread(candidate, rallies) %>%
  replace_na(list(clinton = 0, trump = 0))

replicating <- 
  crosswalk %>%
  left_join(rallies) %>%
  left_join(education) %>%
  left_join(turnout) %>%
  left_join(left) %>%
  replace_na(list(clinton = 0, trump = 0)) %>%
  group_by(state) %>%
  summarise(margin = weighted.mean(margin, total, na.rm = TRUE),
            margin_2012 = weighted.mean(margin_2012, total_lagged, na.rm = TRUE),
    trump = sum(trump),
            clinton = sum(clinton),
            pct_white_nobach = weighted.mean(pct_white_nobach, peeps, na.rm = TRUE),
            turnout = mean(turnout)) %>%
  filter(!str_detect(state, "AK|AS|DC|HI|GU|VI|PR|UM|MP")) %>%
  mutate(utah = case_when(state == "UT" ~ 1,
                          TRUE ~ 0)) %>%
  mutate(swing = case_when(str_detect(state, swings) ~ 1,
                           TRUE ~ 0))

replicating <-
  replicating %>%
  mutate(margin = margin * 100,
         margin_2012 = margin_2012 * 100,
         pct_white_nobach = pct_white_nobach * 100,
         turnout = turnout * 100)

lm(margin ~ . ,
   data = select(replicating, - state)) %>%
  summary()

library(stargazer)

stargazer(lm(margin ~ . ,
             data = select(replicating, - state)),
          type = 'html')


swings <- "Arizona|Colorado|Florida|Iowa|Minnesota|Nevada|New Hampshire|Ohio|Virginia|Michigan|Pennsylvania|Wisconsin"
swings <- "AZ|CO|FL|IA|MN|NV|NH|OH|VA|MI|PA|WI"

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
         change_2008 = margin - margin_2008) %>%
  select(GEOID, margin, change_2008, change_2012, margin_2008, margin_2012, total, total_lagged, total_lagged_2008)

left <- 
  margins %>%
  left_join(flips)






