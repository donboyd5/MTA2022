
# get census data useful for allocation, etc.


# libraries ---------------------------------------------------------------

source(here::here("r", "libraries.r"))
source(here::here("r", "libraries_ts.r"))
source(here::here("r", "constants.r"))
source(here::here("r", "functions.r"))


# get state population for NY, over time ----------------------------------








# get data ----------------------------------------------------------------

# https://walker-data.com/tidycensus/reference/get_estimates.html
# mappings: 
#   https://www.census.gov/data/developers/data-sets/popest-popproj/popest/popest-vars/2019.html
# DATE_CODE: Estimate Date
# 1 = April 1, 2010 Census population or housing unit count
# 2 = April 1, 2010 population or housing unit estimates base
# 3 = July 1, 2010 population or housing unit estimate
# 4 = July 1, 2011 population or housing unit estimate
# 5 = July 1, 2012 population or housing unit estimate
# 6 = July 1, 2013 population or housing unit estimate
# 7 = July 1, 2014 population or housing unit estimate
# 8 = July 1, 2015 population or housing unit estimate
# 9 = July 1, 2016 population or housing unit estimate
# 10 = July 1, 2017 population or housing unit estimate
# 11 = July 1, 2018 population or housing unit estimate
# 12 = July 1, 2019 population or housing unit estimate

#   https://www.census.gov/data/developers/data-sets/popest-popproj/popest/popest-vars/2021.html
# DATE_CODE: Estimate Date
# 1 = April 1, 2020 Census population or housing unit count
# 2 = April 1, 2020 population or housing unit estimates base
# 3 = July 1, 2020 population or housing unit estimate
# 4 = July 1, 2021 population or housing unit estimate

# population estimates

df0 <- get_estimates(
  geography="state",
  product = "population",
  variables = NULL,
  # breakdown = NULL,
  # breakdown_labels = FALSE,
  # year = 2019,
  state = "NY",
  county = NULL,
  time_series = TRUE,
  output = "tidy",
  geometry = FALSE,
  keep_geo_vars = FALSE,
  shift_geo = FALSE,
  key = NULL,
  show_call = FALSE)
df0

df1 <- get_estimates(
  geography="county",
  product = "population",
  variables = NULL,
  # breakdown = NULL,
  # breakdown_labels = FALSE,
  # year = 2019,
  state = "NY",
  county = NULL,
  time_series = TRUE,
  output = "tidy",
  geometry = FALSE,
  keep_geo_vars = FALSE,
  shift_geo = FALSE,
  key = NULL,
  show_call = FALSE)
df1

count(df1, NAME) # 62
count(df1, DATE) # 1-12 


df2 <- get_estimates(
  geography="place",
  product = "population",
  variables = NULL,
  # breakdown = NULL,
  # breakdown_labels = FALSE,
  # year = 2019,
  state = "NY",
  county = NULL,
  time_series = TRUE,
  output = "tidy",
  geometry = FALSE,
  keep_geo_vars = FALSE,
  shift_geo = FALSE,
  key = NULL,
  show_call = FALSE)

df2 


count(df2, NAME) # 62
count(df2, DATE) # 1-12
df2 |> filter(str_detect(NAME, coll("New York City", ignore_case = TRUE))) # 3651000

censuspop1 <- bind_rows(df0, df1, df2 |> filter(GEOID=="3651000")) |> 
  select(censusfips=GEOID, censusname=NAME, name=variable, value, date=DATE) |> 
  mutate(name=str_to_lower(name),
         name=case_when(date==1 ~ paste0(name, "_cen2010"),
                        date==2 ~ paste0(name, "_base2010"),
                        TRUE ~ name),
         year=case_when(date %in% 1:2 ~ 2010,
                        date %in% 3:12 ~ date + 2007,
                        TRUE ~ 9999))
glimpse(censuspop1)
count(censuspop1, name)
count(censuspop1, censusname)
summary(censuspop1)

censuspop <- censuspop1 |> 
  select(-date)

saveRDS(censuspop, here::here("data", "census", "censuspop.rds"))

# get_estimates(
#   geography,
#   product = NULL,
#   variables = NULL,
#   breakdown = NULL,
#   breakdown_labels = FALSE,
#   year = 2019,
#   state = NULL,
#   county = NULL,
#   time_series = FALSE,
#   output = "tidy",
#   geometry = FALSE,
#   keep_geo_vars = FALSE,
#   shift_geo = FALSE,
#   key = NULL,
#   show_call = FALSE,
#   ...
# )



library(censusapi)
# https://www.hrecht.com/censusapi/news/index.html
# https://www.hrecht.com/censusapi/reference/index.html
df <- listCensusApis()
ht(df)
glimpse(df)
names(df)
count(df, str_sub(title, 1, 80), sort = TRUE) |> head(10)
# unique(df$title)
count(df, str_sub(name, 1, 80), sort = TRUE) |> head(50)
count(df, type)
tsdf <- df |> filter(type=="Timeseries")
aggdf <- df |> filter(type=="Aggregate")

tmp <- aggdf |> 
  filter(str_detect(title, coll("Population Estimate", ignore_case = TRUE)))

# pep/population https://api.census.gov/data/pep/population/variables.json not found
# https://api.census.gov/data/

listCensusMetadata(
  name="pep/population",
  vintage = NULL,
  type = "variables",
  group = NULL,
  variable_name = NULL,
  include_values = FALSE
)

listCensusMetadata(
  name="pep/population",
  vintage = NULL,
  type = "variables",
  group = NULL,
  variable_name = NULL,
  include_values = FALSE
)

df |> filter(str_detect(title, "Resilience"))

listCensusMetadata(
  name = "cre",
  vintage = 2019,
  type = "geographies") # US, state, county, tract

