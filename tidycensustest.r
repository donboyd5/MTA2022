library(tidyverse)
library(tidycensus)
library(censusapi)
library(btools)
library(bdata)
library(viridis)

get_decennial(
  geography = "tract",
  variables = "DP2_C29",
  state = "TX",
  county = "Tarrant",
  year = 2000,
  sumfile = "sf3profile"
)

tmp <- get_decennial(
  geography = "tract",
  variables = "DP2_C29",
  state = "NY",
  county = "Washington",
  geometry=TRUE,
  year = 2010,
  sumfile = "sf3profile"
)

plot(tmp)

age10 <- get_decennial(geography = "state", 
                       variables = "P013001", 
                       year = 2010)
age20 <- get_decennial(geography = "state", 
                       variables = "P013001", 
                       year = 2020)

age10 |> 
  ggplot(aes(x = value, y = reorder(NAME, value))) + 
  geom_point()

v21 <- load_variables(2021, "acs5", cache = TRUE)

ny <- get_acs(geography = "county", 
              variables = c(medincome = "B19013_001"), 
              state = "NY", 
              year = 2021)

v21 |> filter(name=="B19013_001")

ny |> 
  lcnames() |> 
  mutate(idx=estimate / median(estimate)) |> 
  arrange(desc(idx)) |> 
  print(n = 70)

washco <- get_acs(geography = "place", # place, tract, block group
                  variables = "B19013_001", # median income
                  state = "NY", 
                  county = "Washington", 
                  geometry = TRUE, 
                  survey="acs5",
                  year = 2021)

washco |> 
  ggplot(aes(fill = estimate, color = estimate)) +
  geom_sf() +
  coord_sf(crs = 26914) +
  scale_fill_viridis(option = "magma") +
  scale_color_viridis(option = "magma")

# census api
# https://www.hrecht.com/censusapi/
# https://github.com/hrecht/censusapi
# https://www.census.gov/data/developers/data-sets.html
# devtools::install_github("hrecht/censusapi")

library(censusapi)

# metadata
capis <- listCensusApis()
tmp <- capis |> filter(name=="cbp")
tmp2 <- capis |> filter(str_detect(name, "sahie"))

listCensusMetadata(
  name,
  vintage = NULL,
  type = "variables",
  group = NULL,
  variable_name = NULL,
  include_values = FALSE
)

sahie_variables <- listCensusMetadata(
  name = "timeseries/healthins/sahie",
  type = "variables")
head(sahie_variables)

cbpvars <- listCensusMetadata(
  name = "2020/cbp",
  type = "variables")

# List the variable groups available in the 5-year 2020 American Community Survey.
acs_groups <- listCensusMetadata(
  name = "acs/acs5",
  vintage = 2021,
  type = "groups")
head(acs_groups)

# https://api.census.gov/data/2020/cbp/variables.html

# Get total population and median household income for places (cities, towns, villages)
# in one state from the 5-year ACS.
acs_simple <- getCensus(
  name = "acs/acs5",
  vintage = 2021,
  vars = c("NAME", "B01001_001E", "B19013_001E"),
  region = "place:*",
  regionin = "state:36")
ht(acs_simple)

ny_rural_income <- getCensus(
  name = "acs/acs5",
  vintage = 2021,
  region = "state:36",
  GEOCOMP = "43",
  vars = "B19013_001E"
)

# https://api.census.gov/data/2020/cbp?get=NAME,NAICS2017_LABEL,ESTAB,PAYANN,PAYQTR1,EMP&for=county:*&in=state:*&NAICS2017=00&LFO=001&EMPSZES=001&key=YOUR_KEY_GOES_HERE


cbp <- getCensus(
  name = "cbp",
  vintage = 2020,
  vars = c("SUMLEVEL", "GEO_ID", "GEOCOMP", "NAME", "SECTOR", "SUBSECTOR", "NAICS2017", "NAICS2017_LABEL", "EMP", "ESTAB", "PAYANN"),
  region = "county:*",
  regionin = "state:36")
ht(cbp)

was1 <- cbp |> 
  filter(GEO_ID=="0500000US36115")

tmp <- count(cbp, NAICS2017, NAICS2017_LABEL)

# public sector statistics ----
# https://www.census.gov/data/developers/data-sets/annual-public-sector-stats.html
# https://api.census.gov/data/timeseries/govs.html
# https://api.census.gov/data/timeseries/govs/variables.html
# https://api.census.gov/data/timeseries/govs/examples.html
# https://www.census.gov/programs-surveys/cog/technical-documentation.html
# https://www2.census.gov/programs-surveys/cog/technical-documentation/Public%20Sector%20-Annual%20-%20CoG%20API%20Documentation.xlsx

# 2015 and 2016 Total and Employee Contributions for Public Pensions:
# api.census.gov/data/timeseries/govs?get=SVY_COMP,AGG_DESC,AGG_DESC_LABEL,GOVTYPE,YEAR,AMOUNT_FORMATTED&for=us&AGG_DESC=PP0001&AGG_DESC=PP0002&YEAR=2016&YEAR=2015

gvars <- listCensusMetadata(
  name = "timeseries/govs",
  type = "variables")
gvars

# https://api.census.gov/data/timeseries/govs/groups.html
ggroups <- listCensusMetadata(
  name = "timeseries/govs",
  type = "groups")
# http://api.census.gov/data/timeseries/govs/groups/GS00SS02.json  # reve pub elem sec school dists

# https://api.census.gov/data/timeseries/govs?get=SVY_COMP,GOVTYPE,NAME,AGG_DESC,AMOUNT,YEAR&for=us:1&time=2018&key=YOUR_KEY_GOES_HERE

# "EXPENDTYPE", "EXPENDTYPE_LABEL" variables not useful for some things

gov1 <- getCensus(
  name = "timeseries/govs",
  # vintage = 2020, # vintage not available for time series
  time = 2020,  # time is required
  vars = c("SUMLEVEL", "SVY_COMP", "SVY_COMP_LABEL", "STATE", "GEO_ID", "GOVTYPE", "GOVTYPE_LABEL",
           "NAME", "AGG_DESC", "AGG_DESC_LABEL", "AMOUNT"),
  region = "state",  # state:36 gets NY; state or state:* gets 50 + DC
  AGG_DESC="LF0001", # optional limiting clause
  show_call=TRUE)

count(gov1, STATE, NAME)

