
# ATTN: This code will largely not run because I have chosen to exclude files that were originally provided under embargo. Instead, I hope this file can showcase how I code on a breaking deadline.

# Global actions

## Libraries and load

options(scipen = 999)

library(tidyverse)
library(sf)
library(tidycensus)
library(readxl)

source("functions/fxns.R")

moco_geoid <- "0500000us24031"

## Read shell table for joining

# This contains all of the variable descriptions
shell_24 <- read_excel("data/shells/ACS2024_Table_Shells.xlsx") |> 
  clean_df() |> 
  select(-line, -data_release)

## Loading variables

# Load census variables from 2018-2023 for acs1
census_vars <- map_dfr(c(2018,2019,2021,2022,2023), function(y) {
  load_variables(y, "acs1", cache = TRUE) %>%
    mutate(year = y)
})

# Functions

## function to join w var names

# Each table contained rows representing geoids around the U.S. and the columns represented different variables within the table. This function swapped the variable names with their descriptions as provided by the shell table. The function assumes that the df only has one row (one geoid) at this point. 

join_varnames <- function(df) {
  
  df <- df |>
    # for all columns aside from geo_id, pivot longer so that the variables are now the row names
    pivot_longer(2:last_col(), names_to = "var", values_to = "value") |>
    # remove the geo_id column
    select(-geo_id) |>
    # var takes the form b10003_e001. This splits it into columns of 10003 (table name), e (estimate/moe), and 001 (the variable)
    separate(
      var,
      into = c("base", "type", "num"),
      sep = "_|(?<=[em])",
      remove = FALSE
    ) |>
    select(-var) |>
    # condenses the table so that there is an estimate and moe column instead of rows
    pivot_wider(names_from = type, values_from = value) |>
    # create a var column in the form of the shell variables: b10003_001
    mutate(var = paste0(base, "_", num)) |>
    select(-base, -num) |>
    # join the descriptions from the shell
    left_join(shell_24 |> select(unique_id, stub) |> rename(var = unique_id),
              by = "var") |>
    # creates a df of variable name (eg b10003_001), estimate value, moe value, and description (stub)
    select(var, e, m, stub)
  
  return(df)
  
}

## general pop.

sex_by_age <- read_delim("data/raw/montgomery_county/sex_by_age.txt", delim = "|") |> 
  clean_df() |> 
  filter(geo_id == moco_geoid)

sex_by_age_joined <- sex_by_age |> 
  join_varnames()

men_by_age <- sex_by_age_joined |> 
  slice(3:25)

men_over_65 <- sex_by_age_joined |> 
  slice(20:25)



women_by_age <- sex_by_age_joined |> 
  slice(27:49)

women_over_65 <- sex_by_age_joined |> 
  slice(44:49)

sum(men_over_65$e,women_over_65$e)

## Demographics

### Foreign born (salvadorians)

# find proportion of foreign born per county

foreign_birth <- read_delim("data/raw/montgomery_county/place_of_foreign_birth.txt", delim = "|") |> 
  clean_df() |> 
  filter(geo_id == moco_geoid)

foreign_birth_joined<- foreign_birth |> 
  join_varnames()

# universe is foreign born (e.g. immigrants)
northern_triangle_foreign_born <- read_delim("data/raw/montgomery_county/place_of_foreign_birth.txt", delim = "|") |> 
  clean_df() |> 
  select(geo_id, b05006_e001, b05006_e157, b05006_m157, b05006_e158, b05006_e159, ) |> 
  filter(str_starts(geo_id, "050")) |> 
  mutate(
    salvador = b05006_e157,
    salvador_m = b05006_m157,
    total = b05006_e157 + b05006_e158 + b05006_e159,
    salvador_prop = b05006_e157/b05006_e001,
    prop = (total)/b05006_e001
  )

### Med income by foreign born

foreign_med_income <- read_delim("data/raw/montgomery_county/foreign_med_income.txt", delim = "|") |> 
  clean_df() |> 
  filter(geo_id == moco_geoid)

foreign_med_income_joined<- foreign_med_income |> 
  join_varnames()

foreign_med_income_vars <- census_vars |> 
  slice(2427,2431) |> 
  pull(name)

# same as above but stores it as a df
foreign_med_income_vars_table <- census_vars |> 
  slice(2427,2431) |> 
  clean_df() |> 
  rename(variable = name) |> 
  select(variable, label)

get_foreign_med_income <- function() {
  years <- c(2018, 2019, 2021, 2022, 2023)  # exclude 2020
  
  map_dfr(years, function(y) {
    get_acs(
      survey = "acs1",
      year = y,
      variables = foreign_med_income_vars,
      geography = "county",
      state = "MD",
      county = "Montgomery",
      geometry = FALSE
    ) %>%
      mutate(year = y)
  })
}

# Call the function
foreign_med_income_18_23 <- get_foreign_med_income() |> 
  clean_df()

foreign_med_income_18_23_test <- foreign_med_income_18_23 |> 
  left_join(foreign_med_income_vars_table, by = "variable") |> 
  select(year, variable, label, estimate, moe) |> 
  select(-moe) |> 
  pivot_wider(names_from = year, values_from = estimate) |> 
  left_join(foreign_med_income_joined |> select(var, e) |> rename(variable = var, x2024 = e), by = "variable") |> 
  clean_names() |> 
  mutate(across(c(x2018, x2019, x2021, x2022, x2023, x2024), ~ .x / first(.x), .names = "{.col}_scaled"))


foreign_med_income <- read_delim("data/raw/montgomery_county/foreign_med_income.txt", delim = "|") |> 
  clean_df() |> 
  select(geo_id, b06011_e001, b06011_e005) |> 
  filter(str_starts(geo_id, "050")) |> 
  mutate(
    diff = b06011_e001 - b06011_e005
  )

### Foreign born (no country origin) + citizenship

foreign_born <- read_delim("data/raw/montgomery_county/foreign_born.txt", delim = "|") |> 
  clean_df() |> 
  filter(geo_id == moco_geoid)

foreign_born_joined<- foreign_born |> 
  join_varnames()

foreign_born <- read_delim("data/raw/montgomery_county/foreign_born.txt", delim = "|") |> 
  clean_df() |> 
  select(geo_id, b05003_e001, b05003_e005, b05003_e010, b05003_e016, b05003_e021) |> 
  filter(str_starts(geo_id, "050")) |> 
  mutate(
    prop_foreign = (b05003_e005 + b05003_e010 + b05003_e016 + b05003_e021)/b05003_e001
  )

citizen <- read_delim("data/raw/montgomery_county/foreign_born.txt", delim = "|") |> 
  clean_df() |> 
  select(geo_id, b05003_e001, b05003_e007, b05003_e012, b05003_e018, b05003_e023) |> 
  filter(str_starts(geo_id, "050")) |> 
  mutate(
    prop_foreign = (b05003_e007 + b05003_e012 + b05003_e018 + b05003_e023)/b05003_e001
  )

### Race

race <- read_delim("data/raw/montgomery_county/race.txt", delim = "|") |> 
  clean_df() |> 
  filter(geo_id == moco_geoid)

race_joined<- race |> 
  join_varnames()

# find vars that you're seeking and store as vector. I've been doing this by slicing from the table
race_vars <- census_vars |> 
  slice(362:366) |> 
  pull(name)

# same as above but stores it as a df
race_vars_table <- census_vars |> 
  slice(362:366) |> 
  clean_df() |> 
  rename(variable = name) |> 
  select(variable, label)

get_race <- function() {
  years <- c(2018, 2019, 2021, 2022, 2023)  # exclude 2020
  
  map_dfr(years, function(y) {
    get_acs(
      survey = "acs1",
      year = y,
      variables = race_vars,
      geography = "county",
      state = "MD",
      county = "Montgomery",
      geometry = FALSE
    ) %>%
      mutate(year = y)
  })
}

# Call the function
race_18_23 <- get_race() |> 
  clean_df()

race_18_23_test <- race_18_23 |> 
  left_join(race_vars_table, by = "variable") |> 
  select(year, variable, label, estimate, moe) |> 
  select(-moe) |> 
  pivot_wider(names_from = year, values_from = estimate) |> 
  left_join(race_joined |> select(var, e) |> rename(variable = var, x2024 = e), by = "variable") |> 
  clean_names() |> 
  mutate(across(c(x2018, x2019, x2021, x2022, x2023, x2024), ~ .x / first(.x), .names = "{.col}_scaled"))

### Latino

latino_background <- read_delim("data/raw/montgomery_county/latino_background.txt", delim = "|") |> 
  clean_df() |> 
  filter(geo_id == moco_geoid)

latino_background_joined <- latino_background |> 
  join_varnames()

# universe is total pop.
north_triangle_latino <- read_delim("data/raw/montgomery_county/latino_origin.txt", delim = "|") |> 
  clean_df() |> 
  select(geo_id, b03001_e001, b03001_e010, b03001_e011, b03001_e014) |> 
  filter(str_starts(geo_id, "050")) |> 
  mutate(
    triangle_prop = (b03001_e010 + b03001_e011 + b03001_e014)/b03001_e001,
    salvador_prop = b03001_e014/b03001_e001
  )

### Spanish spoken

language_spoken <- read_delim("data/raw/montgomery_county/language_spoken.txt", delim = "|") |> 
  clean_df() |> 
  filter(geo_id == moco_geoid)

language_spoken_joined <- language_spoken |> 
  join_varnames()

### Fed workers

class_of_worker <- read_delim("data/raw/montgomery_county/class_of_worker_full_time.txt", delim = "|") |> 
  clean_df() |> 
  filter(geo_id == moco_geoid)

class_of_worker_joined<- class_of_worker |> 
  join_varnames()

men_fed <- class_of_worker_joined |> 
  slice(9)

women_fed <- class_of_worker_joined |> 
  slice(19)

fed_workers <- bind_rows(
  men_fed,
  women_fed
) |> 
  summarize(e = sum(e))

# 69544/432678 (total is full-time, year-round civilian employed pop. 16 and over)
# 16% of MoCo are fed workers

class_of_worker <- read_delim("data/raw/montgomery_county/class_of_worker_full_time.txt", delim = "|") |> 
  clean_df() |> 
  select(geo_id, b24090_e001, b24090_e009, b24090_e019) |> 
  filter(str_starts(geo_id, "050")) |> 
  mutate(
    total = b24090_e009 + b24090_e019,
    prop = (b24090_e009 + b24090_e019)/b24090_e001
  )

### Fed workers (full and part time)

fed_workers_all <- read_delim("data/raw/montgomery_county/fed_workers_all.txt", delim = "|") |> 
  clean_df() |> 
  filter(geo_id == moco_geoid)

fed_workers_all_joined<- fed_workers_all |> 
  join_varnames()

fed_workers_all <- read_delim("data/raw/montgomery_county/fed_workers_all.txt", delim = "|") |> 
  clean_df() |> 
  select(geo_id, b24080_e001, b24080_e009, b24080_e019) |> 
  filter(str_starts(geo_id, "050")) |> 
  mutate(
    total = b24080_e009 + b24080_e019,
    prop = (b24080_e009 + b24080_e019)/b24080_e001
  )

## Housing

### Household income

household_income <- read_delim("data/raw/montgomery_county/household_income.txt", delim = "|") |> 
  clean_df() |> 
  filter(geo_id == moco_geoid)

household_income_joined<- household_income |> 
  join_varnames() 

household_income_vars <- census_vars |> 
  slice(11741:11757) |> 
  pull(name)

household_income_vars_table <- census_vars |> 
  slice(11741:11757) |> 
  clean_df() |> 
  rename(variable = name) |> 
  select(variable, label)

get_household_income <- function() {
  years <- c(2018, 2019, 2021, 2022, 2023)  # exclude 2020
  
  map_dfr(years, function(y) {
    get_acs(
      survey = "acs1",
      year = y,
      variables = household_income_vars,
      geography = "county",
      state = "MD",
      county = "Montgomery",
      geometry = FALSE
    ) %>%
      mutate(year = y)
  })
}

# Call the function
household_income_18_23 <- get_household_income() |> 
  clean_df()

household_income_18_23_test <- household_income_18_23 |> 
  left_join(household_income_vars_table, by = "variable") |> 
  select(year, variable, label, estimate, moe) |> 
  select(-moe) |> 
  pivot_wider(names_from = year, values_from = estimate) |> 
  left_join(household_income_joined |> select(var, e) |> rename(variable = var, x2024 = e), by = "variable") |> 
  clean_names() |> 
  mutate(across(c(x2018, x2019, x2021, x2022, x2023, x2024), ~ .x / first(.x), .names = "{.col}_scaled"))

income_0_45 <- household_income_18_23_test |> 
  slice(2:9) |> 
  summarize(x2018_scaled = sum(x2018_scaled),
            x2019_scaled = sum(x2019_scaled),
            x2021_scaled = sum(x2021_scaled),
            x2022_scaled = sum(x2022_scaled),
            x2023_scaled = sum(x2023_scaled),
            x2024_scaled = sum(x2024_scaled)
  )

income_45_125 <- household_income_18_23_test |> 
  slice(10:14) |> 
  summarize(x2018_scaled = sum(x2018_scaled),
            x2019_scaled = sum(x2019_scaled),
            x2021_scaled = sum(x2021_scaled),
            x2022_scaled = sum(x2022_scaled),
            x2023_scaled = sum(x2023_scaled),
            x2024_scaled = sum(x2024_scaled)
  )

income_125_200 <- household_income_18_23_test |> 
  slice(15:17) |> 
  summarize(x2018_scaled = sum(x2018_scaled),
            x2019_scaled = sum(x2019_scaled),
            x2021_scaled = sum(x2021_scaled),
            x2022_scaled = sum(x2022_scaled),
            x2023_scaled = sum(x2023_scaled),
            x2024_scaled = sum(x2024_scaled)
  )

middle_class <- bind_rows(
  income_0_45 = income_0_45,
  income_45_125 = income_45_125,
  income_125_200 = income_125_200,
  .id = "source"
)

write.csv(middle_class, "data/output/montgomery_county/middle_class.csv")
write.csv(household_income_18_23_test, "data/output/montgomery_county/household_income.csv")

### Gross rent

gross_rent <- read_delim("data/raw/montgomery_county/gross_rent.txt", delim = "|") |> 
  clean_df() |> 
  filter(geo_id == moco_geoid)

gross_rent_joined<- gross_rent |> 
  join_varnames() 

gross_rent_vars <- census_vars |> 
  slice(24620:24646) |> 
  pull(name)

gross_rent_vars_table <- census_vars |> 
  slice(24620:24646) |> 
  clean_df() |> 
  rename(variable = name) |> 
  select(variable, label)

get_gross_rent <- function() {
  years <- c(2018, 2019, 2021, 2022, 2023)  # exclude 2020
  
  map_dfr(years, function(y) {
    get_acs(
      survey = "acs1",
      year = y,
      variables = gross_rent_vars,
      geography = "county",
      state = "MD",
      county = "Montgomery",
      geometry = FALSE
    ) %>%
      mutate(year = y)
  })
}

# Call the function
gross_rent_18_23 <- get_gross_rent() |> 
  clean_df()

gross_rent_18_23_test <- gross_rent_18_23 |> 
  left_join(gross_rent_vars_table, by = "variable") |> 
  select(year, variable, label, estimate, moe) |> 
  select(-moe) |> 
  pivot_wider(names_from = year, values_from = estimate) |> 
  left_join(gross_rent_joined |> select(var, e) |> rename(variable = var, x2024 = e), by = "variable") |> 
  clean_names() |> 
  mutate(across(c(x2018, x2019, x2021, x2022, x2023, x2024), ~ .x / first(.x), .names = "{.col}_scaled"))

rent_0_900 <- gross_rent_18_23_test |> 
  slice(3:18,27) |> 
  summarize(x2018_scaled = sum(x2018_scaled),
            x2019_scaled = sum(x2019_scaled),
            x2021_scaled = sum(x2021_scaled),
            x2022_scaled = sum(x2022_scaled),
            x2023_scaled = sum(x2023_scaled),
            x2024_scaled = sum(x2024_scaled)
  )

rent_900_1500 <- gross_rent_18_23_test |> 
  slice(19:21) |> 
  summarize(x2018_scaled = sum(x2018_scaled),
            x2019_scaled = sum(x2019_scaled),
            x2021_scaled = sum(x2021_scaled),
            x2022_scaled = sum(x2022_scaled),
            x2023_scaled = sum(x2023_scaled),
            x2024_scaled = sum(x2024_scaled)
  )

rent_1500_3000 <- gross_rent_18_23_test |> 
  slice(22:24) |> 
  summarize(x2018_scaled = sum(x2018_scaled),
            x2019_scaled = sum(x2019_scaled),
            x2021_scaled = sum(x2021_scaled),
            x2022_scaled = sum(x2022_scaled),
            x2023_scaled = sum(x2023_scaled),
            x2024_scaled = sum(x2024_scaled)
  )

rent_3000_up <- gross_rent_18_23_test |> 
  slice(25:26) |> 
  summarize(x2018_scaled = sum(x2018_scaled),
            x2019_scaled = sum(x2019_scaled),
            x2021_scaled = sum(x2021_scaled),
            x2022_scaled = sum(x2022_scaled),
            x2023_scaled = sum(x2023_scaled),
            x2024_scaled = sum(x2024_scaled)
  )

rent_brackets <- bind_rows(
  rent_0_900 = rent_0_900,
  rent_900_1500 = rent_900_1500,
  rent_1500_3000 = rent_1500_3000,
  rent_3000_up = rent_3000_up,
  .id = "source"
)

write.csv(rent_brackets, "data/output/montgomery_county/rent_brackets.csv")
write.csv(gross_rent_18_23_test, "data/output/montgomery_county/gross_rent_18_23.csv")

### Median value of housing

# universe is owner occupied housing units
median_housing <- read_delim("data/raw/montgomery_county/median_housing.txt", delim = "|") |> 
  clean_df() |> 
  filter(geo_id == moco_geoid)

median_housing_joined<- median_housing |> 
  join_varnames() 

### Means of travel

means_of_transport <- read_delim("data/raw/montgomery_county/means_of_transport.txt", delim = "|") |> 
  clean_df() |> 
  filter(geo_id == moco_geoid)

means_of_transport_joined<- means_of_transport |> 
  join_varnames() 

# nationwide analysis

means_of_transport <- read_delim("data/raw/montgomery_county/means_of_transport.txt", delim = "|") |> 
  clean_df() |> 
  select(geo_id, b08006_e001, b08006_e002, b08006_e008, b08006_e010) |> 
  filter(str_starts(geo_id, "050")) |> 
  mutate(
    car_percent = b08006_e002/b08006_e001,
    transit_percent = b08006_e008/b08006_e001,
    subway_percent = b08006_e010/b08006_e001
  )

# 371,580 took car, truck or van.
# 55,422 used public transit which includes bus, subway/elevated rail, commuter rail, or light rail
# 33,459 used the subway/elevated rail
# 571,120 is universe which is workers 16 years and over

### Commute times

commute_times <- read_delim("data/raw/montgomery_county/commute_times.txt", delim = "|") |> 
  clean_df() |> 
  filter(geo_id == moco_geoid)

commute_times_joined<- commute_times |> 
  join_varnames() 

commute_times_joined_filter <- commute_times_joined |> 
  slice(2:13) |> 
  mutate(
    prop = e/450964
  )

less_30 <- commute_times_joined_filter |> 
  slice(1:6) |> 
  summarize(
    prop = sum(prop)
  )

mid_30_45 <- commute_times_joined_filter |> 
  slice(7:9) |> 
  summarize(
    prop = sum(prop)
  )

more_45 <- commute_times_joined_filter |> 
  slice(10:12) |> 
  summarize(
    prop = sum(prop)
  )

commute_times <- read_delim("data/raw/montgomery_county/commute_times.txt", delim = "|") |> 
  clean_df() |> 
  select(geo_id, b08012_e001, b08012_e011, b08012_e012, b08012_e013) |> 
  filter(str_starts(geo_id, "050")) |> 
  mutate(
    total = b08012_e011 + b08012_e012 + b08012_e013,
    prop = total/b08012_e001
  )

## Education

### Educational attainment

educational_attainment <- read_delim("data/raw/montgomery_county/educational_attainment.txt", delim = "|") |> 
  clean_df() |> 
  filter(geo_id == moco_geoid)

educational_attainment_joined<- educational_attainment |> 
  join_varnames() 

men_over_masters <- educational_attainment_joined |> 
  slice(16:18) |> 
  summarize(
    e = sum(e)
  )

women_over_masters <- educational_attainment_joined |> 
  slice(33:35) |> 
  summarize(
    e = sum(e)
  )

over_masters <- bind_rows(
  men_over_masters,
  women_over_masters
) |> 
  summarize(e = sum(e))

educational_attainment <- read_delim("data/raw/montgomery_county/educational_attainment.txt", delim = "|") |> 
  clean_df() |> 
  select(geo_id, b15002_e001, b15002_e015, b15002_e016, b15002_e017, b15002_e018, b15002_e032, b15002_e033, b15002_e034, b15002_e035) |> 
  filter(str_starts(geo_id, "050")) |> 
  mutate(
    men_b = b15002_e015,
    women_b = b15002_e032,
    men_m = b15002_e016 + b15002_e017 + b15002_e018,
    women_m = b15002_e033 + b15002_e034 + b15002_e035,
    prop_m = (men_m+women_m)/b15002_e001,
    prop_b = (men_b + women_b)/b15002_e001,
    prop_college = prop_m + prop_b
  )

### Public/private education

public_private <- read_delim("data/raw/montgomery_county/public_private.txt", delim = "|") |> 
  clean_df() |> 
  filter(geo_id == moco_geoid)

public_private_joined<- public_private |> 
  join_varnames() 

# find vars that you're seeking and store as vector. I've been doing this by slicing from the table
public_private_vars <- census_vars |> 
  slice(7736,7738,7747,7766,7775) |> 
  pull(name)

# same as above but stores it as a df
public_private_vars_table <- census_vars |> 
  slice(7736,7738,7747,7766,7775) |> 
  clean_df() |> 
  rename(variable = name) |> 
  select(variable, label)

get_public_private <- function() {
  years <- c(2018, 2019, 2021, 2022, 2023)  # exclude 2020
  
  map_dfr(years, function(y) {
    get_acs(
      survey = "acs1",
      year = y,
      variables = public_private_vars,
      geography = "county",
      state = "MD",
      county = "Montgomery",
      geometry = FALSE
    ) %>%
      mutate(year = y)
  })
}

# Call the function
public_private_18_23 <- get_public_private() |> 
  clean_df() |> 
  left_join(public_private_vars_table, by = "variable") |> 
  select(year, variable, label, estimate, moe) |> 
  select(-moe) |> 
  pivot_wider(names_from = year, values_from = estimate) |> 
  left_join(public_private_joined |> select(var, e) |> rename(variable = var, x2024 = e), by = "variable") |> 
  clean_names() |> 
  mutate(across(c(x2018, x2019, x2021, x2022, x2023, x2024), ~ .x / first(.x), .names = "{.col}_scaled"))

# Viz

## Foreign born

foreign_birth <- read_delim("data/raw/montgomery_county/place_of_foreign_birth.txt", delim = "|") |> 
  clean_df() |> 
  filter(geo_id == moco_geoid)

foreign_birth_joined<- foreign_birth |> 
  join_varnames()

total_foreign_born <- 364713
total <- 1082273

largest <- foreign_birth_joined |> 
  arrange(-e) |> 
  mutate(
    of_fb = 100*e/total_foreign_born,
    of_total = 100*e/total
  ) |> 
  slice(9,15,17:19,22:27,31:35,38:48,50:60)

write.csv(largest,"data/output/montgomery_county/immigrant.csv")