# Load libraries

library(tidyverse)
library(tidycensus)
library(mapview)
library(sf)

# Retrieve census data for each group

asian_pop_09_14 <- map(
  .x = 2009:2014, 
  .f = ~ get_acs(
    geography = "tract",
    state = "MD",
    county = "027",
    variable = c("B02006_005","B02006_010","B02006_002","B01003_001"),
    year = .x,
    output = "wide",
    survey = "acs5",
    geometry = TRUE
  ) 
)


asian_pop_15_21 <- map(
  .x = 2015:2021, 
  .f = ~ get_acs(
    geography = "tract",
    state = "MD",
    county = "027",
    variable = c("B02015_007","B02015_012","B02015_002","B01003_001"),
    year = .x,
    output = "wide",
    survey = "acs5",
    geometry = TRUE
  ) 
)

asian_pop_22_23 <- map(
  .x = 2022:2023, 
  .f = ~ get_acs(
    geography = "tract",
    state = "MD",
    county = "027",
    variable = c("B02015_002","B02015_005","B02015_021","B01003_001"),
    year = .x,
    output = "wide",
    survey = "acs5",
    geometry = TRUE
  ) 
)

# Clean data

# standardize pop column name and remove MOE
asian_pop_09_14 <- map(
  .x = asian_pop_09_14,
  .f = ~ rename(.x, chinese = B02006_005E, korean = B02006_010E, indian = B02006_002E, chinese_margin = B02006_005M, korean_margin = B02006_010M, indian_margin = B02006_002M)
)

asian_pop_15_21 <- map(
  .x = asian_pop_15_21,
  .f = ~ rename(.x, chinese = B02015_007E, korean = B02015_012E, indian = B02015_002E, indian_margin = B02015_002M, chinese_margin = B02015_007M, korean_margin = B02015_012M)
)
asian_pop_22_23 <- map(
  .x = asian_pop_22_23,,
  .f = ~ rename(.x, chinese = B02015_002E, korean = B02015_005E, indian = B02015_021E, chinese_margin = B02015_002M, korean_margin = B02015_005M, indian_margin = B02015_021M)
)

# Add year column to distinguish after row bind
asian_pop_09_14 <- map2_dfr(asian_pop_09_14, 2009:2014, ~ mutate(.x, year = .y))
asian_pop_15_21 <- map2_dfr(asian_pop_15_21, 2015:2021, ~ mutate(.x, year = .y))
asian_pop_22_23 <- map2_dfr(asian_pop_22_23, 2022:2023, ~ mutate(.x, year = .y))


# Combine the dataframes
asian_pop <- bind_rows(asian_pop_09_14, asian_pop_15_21,asian_pop_22_23)

# Fix population name columns
asian_pop <- asian_pop |> 
  rename(pop = B01003_001E) |> 
  select(-B01003_001M)

# Calculate percentages

asian_pop <- asian_pop |> 
  mutate(
    indian_percent = indian/pop,
    korean_percent = korean/pop,
    chinese_percent = chinese/pop
  )

# BEGIN FACT CHECK
  
# Koreans were once Howard County’s predominant Asian group, making up an estimated 3.6% of its residents in 2009.

asian_pop_grouped <- asian_pop |> 
  group_by(year) |> 
  summarize(
    indian = sum(indian),
    chinese = sum(chinese),
    korean = sum(korean),
    pop = sum(pop)
  ) |> 
  mutate(
    indian_percent = indian/pop,
    chinese_percent = chinese/pop,
    korean_percent = korean/pop
  ) |> 
  st_drop_geometry()

asian_pop_grouped |>
  filter(year == 2009) |>
  select(year, korean_percent) |>
  mutate(korean_percent = round(korean_percent*100, 1))

# In a census tract just north of Ellicott City — Hogan, whose wife is Korean, officially dubbed part of it “Koreatown” in 2021 — Koreans once made up 10.9% of the inhabitants while Indians made up 5.7%.

tract_09 <- asian_pop |> 
  filter(NAME == "Census Tract 6026, Howard County, Maryland", year == 2009) |> 
  select(indian_percent, korean_percent, year, NAME) |> 
  mutate(
    indian_percent = round(indian_percent*100, 1),
    korean_percent = round(korean_percent*100, 1)
  )

tract_09

mapview(tract_09)

# That dynamic has flipped. Indians now make up almost 24.8% of the tract and Koreans account for 4%.

tract_23 <- asian_pop |> 
  filter(NAME == "Census Tract 6026; Howard County; Maryland", year == 2023) |> 
  select(indian_percent, korean_percent, year, NAME) |> 
  mutate(
    indian_percent = round(indian_percent*100, 1),
    korean_percent = round(korean_percent*100, 1)
  )

tract_23

mapview(tract_23) + mapview(tract_09, col.regions = "red")

# The trend is countywide. The number of Howard County’s Indian residents has more than doubled since 2009, now accounting for 6.7% of the county’s total population. Koreans currently make up 3.8%.

doubled <- asian_pop_grouped |> 
  select(year, indian, indian_percent, korean_percent) |> 
  filter(year %in% c(2009,2023)) |> 
  mutate(
    indian_percent = round(indian_percent*100, 1),
    korean_percent = round(korean_percent*100, 1)
  )

doubled

doubled$indian[2]/doubled$indian[1]

# The Indian community isn’t the only Asian one growing quickly in Howard County. The number of Chinese people has more than doubled since 2009, and they now account for 4% of the county’s population, surpassing the Korean population.

doubled <- asian_pop_grouped |> 
  select(year, chinese, korean, chinese_percent) |> 
  filter(year %in% c(2009,2023)) |> 
  mutate(
    chinese_percent = round(chinese_percent*100, 1)
  )
doubled

doubled$chinese[2]/doubled$chinese[1]

# Chinese have become the second most populous Asian group in Howard County.

asian_pop_grouped |> 
  select(year, chinese, korean, indian) |> 
  filter(year == 2023) |>
  pivot_longer(
    cols = c(chinese, korean, indian),
    names_to = "ethnicity",
    values_to = "count"
  ) |>
  arrange(desc(count))

#It accounted for only 2.2% of the county’s population in 2009

asian_pop_grouped |> 
  select(year, chinese_percent) |> 
  filter(year == 2009) |> 
  mutate(
    chinese_percent = round(chinese_percent*100,1)
  )

# FACT CHECK VIZ
  
# Create csv for line chart to see how groups are changing

# Group and create sums of each set of tracts
asian_pop_grouped <- asian_pop |> 
  group_by(year) |> 
  summarize(
    indian = sum(indian),
    chinese = sum(chinese),
    korean = sum(korean),
    pop = sum(pop)
  ) |> 
  mutate(
    indian_percent = indian/pop,
    chinese_percent = chinese/pop,
    korean_percent = korean/pop
  ) |> 
  st_drop_geometry()

# Binning percentages for visualization

asian_pop_23 <- asian_pop |> 
  filter(year == 2023) |> 
  mutate(
    korean_percent = round(korean_percent * 100, 2),
    indian_percent = round(indian_percent * 100, 2)
  )

asian_pop_23 <- asian_pop_23 |> 
  mutate(
    korean_percent_bin = case_when(
      korean_percent == 0 ~ "0%",
      korean_percent > 0 & korean_percent < 3 ~ "Less than 3%",
      korean_percent >= 3 & korean_percent < 5 ~ "3% to 5%",
      korean_percent >= 5 & korean_percent < 10 ~ "5% to 10%",
      korean_percent >= 10 & korean_percent < 15 ~ "10% to 15%",
      korean_percent >= 15 ~ "More than 15%"
    ),
    indian_percent_bin = case_when(
      indian_percent == 0 ~ "0%",
      indian_percent > 0 & indian_percent < 3 ~ "Less than 3%",
      indian_percent >= 3 & indian_percent < 5 ~ "3% to 5%",
      indian_percent >= 5 & indian_percent < 10 ~ "5% to 10%",
      indian_percent >= 10 & indian_percent < 15 ~ "10% to 15%",
      indian_percent >= 15 ~ "More than 15%"
    )
  )

# FACT CHECK MOE
  
# Plot with MOE at the county level

asian_pop_09_14 <- map(
  .x = 2009:2014, 
  .f = ~ get_acs(
    geography = "county",
    state = "MD",
    county = "027",
    variable = c("B02006_005","B02006_010","B02006_002","B01003_001"),
    year = .x,
    output = "wide",
    survey = "acs5",
    cache_table = TRUE,
    geometry = TRUE
  ) 
)


asian_pop_15_21 <- map(
  .x = 2015:2021, 
  .f = ~ get_acs(
    geography = "county",
    state = "MD",
    county = "027",
    variable = c("B02015_007","B02015_012","B02015_002","B01003_001"),
    year = .x,
    output = "wide",
    survey = "acs5",
    geometry = TRUE
  ) 
)

asian_pop_22_23 <- map(
  .x = 2022:2023, 
  .f = ~ get_acs(
    geography = "county",
    state = "MD",
    county = "027",
    variable = c("B02015_002","B02015_005","B02015_021","B01003_001"),
    year = .x,
    output = "wide",
    survey = "acs5",
    geometry = TRUE
  ) 
)

# standardize pop column name and remove MOE
asian_pop_09_14 <- map(
  .x = asian_pop_09_14,
  .f = ~ rename(.x, chinese = B02006_005E, korean = B02006_010E, indian = B02006_002E, chinese_margin = B02006_005M, korean_margin = B02006_010M, indian_margin = B02006_002M)
)

asian_pop_15_21 <- map(
  .x = asian_pop_15_21,
  .f = ~ rename(.x, chinese = B02015_007E, korean = B02015_012E, indian = B02015_002E, indian_margin = B02015_002M, chinese_margin = B02015_007M, korean_margin = B02015_012M)
)
asian_pop_22_23 <- map(
  .x = asian_pop_22_23,,
  .f = ~ rename(.x, chinese = B02015_002E, korean = B02015_005E, indian = B02015_021E, chinese_margin = B02015_002M, korean_margin = B02015_005M, indian_margin = B02015_021M)
)

# Add year column to distinguish after row bind
asian_pop_09_14 <- map2_dfr(asian_pop_09_14, 2009:2014, ~ mutate(.x, year = .y))
asian_pop_15_21 <- map2_dfr(asian_pop_15_21, 2015:2021, ~ mutate(.x, year = .y))
asian_pop_22_23 <- map2_dfr(asian_pop_22_23, 2022:2023, ~ mutate(.x, year = .y))


# Combine the dataframes
asian_pop <- bind_rows(asian_pop_09_14, asian_pop_15_21,asian_pop_22_23)

# Fix population name columns
asian_pop <- asian_pop |> 
  rename(pop = B01003_001E) |> 
  select(-B01003_001M)

asian_pop <- asian_pop |>
  mutate(
    chinese_lower = chinese - chinese_margin,
    chinese_upper = chinese + chinese_margin,
    korean_lower = korean - korean_margin,
    korean_upper = korean + korean_margin,
    indian_lower = indian - indian_margin,
    indian_upper = indian + indian_margin
  )

# Plot chart

margins <- ggplot(asian_pop, aes(x = year)) +
  # Add the filled area (ribbon)
  geom_ribbon(aes(ymin = chinese_lower, ymax = chinese_upper, fill = "Chinese"), alpha = 0.3) + 
  geom_ribbon(aes(ymin = indian_lower, ymax = indian_upper, fill = "Indian"), alpha = 0.3) +
  geom_ribbon(aes(ymin = korean_lower, ymax = korean_upper, fill = "Korean"), alpha = 0.3) +
  scale_fill_manual(
    name = "Population Group", # This is the title of your legend
    values = c(
      "Chinese" = "red",   # Maps the "Chinese" label to red fill
      "Indian" = "blue",    # Maps the "Indian" label to blue fill
      "Korean" = "green"    # Maps the "Korean" label to green fill
    )) +
  theme_minimal() +
  theme(legend.position = "right")

interactive <- ggplotly(margins, tooltip = c("year", "chinese_lower", "chinese_upper", "indian_lower", "indian_upper", "korean_lower", "korean_upper"))

interactive