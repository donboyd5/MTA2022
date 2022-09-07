
# create a county-level file with allocation variables


# desired final output ----------------------------------------------------

# tibble of data for the MTA counties, not other groups (???)

# an allocators tibble with the following variables
# geofips char -- 6 character, what to do about nyc, nys, others?
# uniname char -- generic area descriptor
# src -- data source
# year integer -- may be cy, sfy, sales year, or something else close to MTA fyear
# yeartype character - the kind of year
# name -- variable name
# value numeric
# units ???


# libraries ---------------------------------------------------------------

source(here::here("r", "libraries.r"))
source(here::here("r", "libraries_ts.r"))
source(here::here("r", "constants.r"))
source(here::here("r", "functions.r"))


# constants ---------------------------------------------------------------
# BEA proprietors' income is much larger than IRS self-employment income
# in the BEA documentation for non-farm proprietors' income,
# https://www.bea.gov/resources/methodologies/nipa-handbook/pdf/chapter-11.pdf
# Table 11.3—Relation of Nonfarm Proprietors’ Income in the National Income and Product
# Accounts to Corresponding Measures as Published by the Internal Revenue Service
# has estimates for 2015 of a relevant IRS concept ($831.7b) and NFPI ($1,130.8b)
# I use that ratio to adjust propinc
propinc_taxable_ratio <- 831.7 / 1130.8


# get previously prepared allocators data ---------------------------------
xwalkny <- readRDS(here::here("data", "xwalks", "xwalkny.rds"))

pmt1 <- readRDS(here::here("data", "pmtbase.rds"))

mft1 <- readRDS(here::here("data", "dtf", "mft.rds"))
mrt1 <- readRDS(here::here("data", "dtf", "mrt.rds"))
rett1 <- readRDS(here::here("data", "dtf", "rett.rds"))
sut1 <- readRDS(here::here("data", "dtf", "sut.rds"))

mtamrt1 <- readRDS(here::here("data", "mta", "mtamrt_monthly.rds"))


# prep files for the allocators data frame --------------------------------

## pmt ----
glimpse(pmt1) 
count(pmt1, year) # 2015-2020
count(pmt1, area) # just the mta counties

# quick check on values
# pmtbase |> 
#   select(year, area, wages_pmt, nonwage_pmt, taxbase_pmt) |> 
#   mutate(calc=wages_pmt + nonwage_pmt,
#          diff=calc - taxbase_pmt) |> 
#   filter(diff!=0)

pmt2 <- pmt1 |>
  rename(unifips=fips) |> 
  left_join(xwalkny |> select(unifips, uniname), by="unifips") |> 
  # get only the vars we are 100% sure we might ever use for allocation
  select(unifips, uniname, year, earnings_bea, wages_bea, wages_pmt, nonwage_pmt, taxbase_pmt) |> 
  mutate(src="pmtbase",
         yeartype="cy") |>
  pivot_longer(cols=-c(unifips, uniname, year, yeartype, src))
ht(pmt2)
count(pmt2, unifips, uniname)
glimpse(pmt2)

pmt <- pmt2

## dtf mft ----
count(mft1, county)
count(mft1, name)
mft2 <- mft1 |> 
  rename(year=fyear) |> 
  inner_join(xwalkny |> select(unifips, uniname, county=dtf_mft), by="county") |> 
  mutate(src="dtf_mft",
         yeartype="sfy")

count(mft2, unifips, uniname, county)
ht(mft2)

mft <- mft2

## dtf mrt sfy Table 23 ----
count(mrt1, county)
count(mrt1, name)

# name             n
# <chr>        <int>
# 1 mrt_add1       320  regional transport tax
# 2 mrt_add2       320  SONYMA tax
# 3 mrt_county     320  ~ total - add1 - add2 - specfund - for MOST counties (but rurals seem to get less; refunds?)
# 4 mrt_local      320  local tax enacted by a county or NYC under state law
# 5 mrt_number     320  number of mortgages recorded
# 6 mrt_specfund   320  tax for MTA and for Erie County?
# 7 mrt_tot        320  total collected (includes local)

mrt2 <- mrt1 |> 
  filter(name != "mrt_number") |> 
  rename(year=fyear) |> 
  inner_join(xwalkny |> select(unifips, uniname, county=dtf_mrt), by="county") |> 
  mutate(src="dtf_mrt",
         yeartype="sfy")

count(mrt2, unifips, uniname, county)
ht(mrt2)
summary(mrt2) # there are nas because some counties don't have some pieces of these taxes

mrt <- mrt2


## dtf rett ----
# rett
rett2 <- rett1 |> 
  rename(year=fyear) |> 
  inner_join(xwalkny |> select(unifips, uniname, county=dtf_rett), by="county") |> 
  mutate(src="dtf_rett",
         yeartype="sfy")

count(rett2, unifips, uniname, county)
ht(rett2)
summary(rett2)

rett <- rett2

## dtf sut ----
sut2 <- sut1 |> 
  rename(year=fyear) |> 
  inner_join(xwalkny |> select(unifips, uniname, county=dtf_sut), by="county") |> 
  mutate(src="dtf_sut",
         yeartype="sfy")

count(sut2, unifips, uniname, county)
ht(sut2)
summary(sut2)
sut <- sut2


## mta mrt ----
# mtamrt1
count(mtamrt1, test)
count(mtamrt1, tax)
# tax               n
# <chr>         <int>
# 1 mrt1           2596
# 2 mrt2           2832
# 3 net_urban90     236
# 4 urban_mrt100    236
# 5 urban_rptt100   236
# 6 urban_tot90     236
count(mtamrt1, area)
count(mtamrt1, date)

mtamrt2 <- mtamrt1 |> 
  select(name=tax, area, date, value) |> 
  mutate(uniname=case_when(area=="4boro" ~ "not used",
                           area=="NYC" ~ "New York City",
                           area=="Richmand" ~ "Staten Island",
                           TRUE ~ area)) |> 
  inner_join(xwalkny |> select(unifips, uniname), by="uniname") 
count(mtamrt2, unifips, uniname, area)

mtamrt3 <- mtamrt2 |> 
  mutate(year=ifelse(month(date)==12, year(date) + 1, year(date)),
         yeartype="DecNov",
         src="mtamrt") |> 
  group_by(uniname, name, year) |> 
  mutate(n=n()) |> 
  ungroup()
count(mtamrt3, n)

mtamrt4 <- mtamrt3 |> 
  filter(n==12) |> 
  select(-n, -area)
count(mtamrt4, name)
count(mtamrt4, uniname)
glimpse(mtamrt4)

mtamrt5 <- mtamrt4 |> 
  group_by(unifips, uniname, name, year, yeartype, src) |> 
  summarise(value=sum(value), .groups="drop")
glimpse(mtamrt5)
count(mtamrt5, year)  
summary(mtamrt5)

mtamrt <- mtamrt5


# stack the allocators files and drop file-specific area names ----

names(pmt)
names(mft)
names(mrt)
names(rett)
names(sut)
names(mtamrt)

keepvars <- c("unifips", "uniname", "name", "year", "yeartype", "src", "value")

stack <- bind_rows(pmt, mft, mrt, rett, sut, mtamrt) |> 
  select(all_of(keepvars)) |> 
  filter(year %in% 2015:2021)

count(stack, unifips, uniname)
count(stack, name)
count(stack, year)
count(stack, yeartype)
count(stack, src)

saveRDS(stack, here::here("data", "allocators.rds"))

