
# libraries ---------------------------------------------------------------

source(here::here("r", "libraries.r"))
source(here::here("r", "libraries_ts.r"))
source(here::here("r", "constants.r"))
source(here::here("r", "functions.r"))


# data ---------------------------------------------------------------

## dtf Department of Taxation and Finance ----
# https://www.tax.ny.gov/data/stats/
# https://www.tax.ny.gov/research/stats/statistics/stat_fy_collections.htm
# https://www.tax.ny.gov/pdf/2020-2021%20collections/FY20_21%20Tables.xls
# https://www.tax.ny.gov/research/collections/sfy20-21/March_2021_tax_collections.xls

# osc Office of the State Comptroller ----


pmtdf <- readRDS(here::here("data", "dtf", "pmt_collections.rds"))


# check March 2020 and March 2021
pmtdf |> 
  filter(date %in% as.Date(c("2021-03-01", "2022-03-01"))) |> 
  select(date, local_mtapmt, local_mtapmtpitnet, totpmt)

# check the two state fiscal years
pmtdf |> 
  mutate(sfy=ifelse(month(date) <= 3, year(date), year(date) + 1)) |> 
  filter(sfy < 2023) |> 
  group_by(sfy) |> 
  summarise(across(c(local_mtapmt, local_mtapmtpitnet, totpmt), sum)) |> 
  filter(sfy>=2010) |> 
  gt() |> 
  fmt_number(columns = -c(sfy))
  

pmtdf |> 
  mutate(sfy=ifelse(month(date) <= 3, year(date), year(date) + 1)) |> 
  filter(sfy == 2022) |> 
  select(-date) |> 
  summarise(across(-c(sfy), sum)) |> 
  gt() |> 
  fmt_number(columns = -c(sfy))

pmtdf |> 
  mutate(sfy=ifelse(month(date) <= 3, year(date), year(date) + 1)) |> 
  filter(sfy == 2022) |> 
  arrange(date) |> 
  select(date, local_mtapmt)
