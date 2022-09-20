
# create a county-level file with allocation variables


# desired final output ----------------------------------------------------

# tibble of data for the MTA counties, not other groups (???)

# an allocators tibble with the following variables
# geofips char -- 6 character, what to do about nyc, nys, others?
# uniname char -- generic area descriptor
# src -- data source
# year integer -- may be cy, sfy, sales year, or something else close to MTA fyear
# yeartype character - the kind of year
# allocator -- variable name
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

# mtamrt1 <- readRDS(here::here("data", "mta", "mtamrt_monthly.rds"))

mta1 <- readRDS(here::here("data", "mta", "mta_alloc.rds"))    

censuspop1 <- readRDS(here::here("data", "census", "censuspop.rds"))


# prep files for the allocators data frame --------------------------------

## censuspop ----
censuspop <- censuspop1 |> 
  filter(!str_starts(name, "density")) |> 
  left_join(xwalkny |> select(unifips, uniname, censusname), by="censusname") |> 
  mutate(src="census", yeartype="cy") |> 
  select(unifips, uniname, year, src, yeartype, name, value)
glimpse(censuspop)
summary(censuspop)
count(censuspop, unifips, uniname)
count(censuspop, name)

## mta values ----
mta <- mta1
mta
count(mta, name)

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
         name="sutrev",
         yeartype="sfy")

count(sut2, unifips, uniname, county)
ht(sut2)
summary(sut2)
sut <- sut2


## mta mrt ----
# # mtamrt1
# count(mtamrt1, test)
# count(mtamrt1, tax)
# # tax               n
# # <chr>         <int>
# # 1 mrt1           2596
# # 2 mrt2           2832
# # 3 net_urban90     236
# # 4 urban_mrt100    236
# # 5 urban_rptt100   236
# # 6 urban_tot90     236
# count(mtamrt1, area)
# count(mtamrt1, date)
# 
# mtamrt2 <- mtamrt1 |> 
#   select(name=tax, area, date, value) |> 
#   mutate(uniname=case_when(area=="4boro" ~ "not used",
#                            area=="NYC" ~ "New York City",
#                            area=="Richmand" ~ "Staten Island",
#                            TRUE ~ area)) |> 
#   inner_join(xwalkny |> select(unifips, uniname), by="uniname") 
# count(mtamrt2, unifips, uniname, area)
# 
# mtamrt3 <- mtamrt2 |> 
#   mutate(year=ifelse(month(date)==12, year(date) + 1, year(date)),
#          yeartype="DecNov",
#          src="mtamrt") |> 
#   group_by(uniname, name, year) |> 
#   mutate(n=n()) |> 
#   ungroup()
# count(mtamrt3, n)
# 
# mtamrt4 <- mtamrt3 |> 
#   filter(n==12) |> 
#   select(-n, -area)
# count(mtamrt4, name)
# count(mtamrt4, uniname)
# glimpse(mtamrt4)
# 
# mtamrt5 <- mtamrt4 |> 
#   group_by(unifips, uniname, name, year, yeartype, src) |> 
#   summarise(value=sum(value), .groups="drop")
# glimpse(mtamrt5)
# count(mtamrt5, year)  
# summary(mtamrt5)
# 
# mtamrt <- mtamrt5


# stack and save the allocators files and drop file-specific area names ----
names(mta)
names(pmt)
names(mft)
names(mrt)
names(rett)
names(sut)
# names(mtamrt)

keepvars <- c("unifips", "uniname", "name", "year", "yeartype", "src", "value")

# don't bother with mrt because we'll use values from mta
stack <- bind_rows(censuspop, pmt, mft, rett, sut, mta) |> 
  select(all_of(keepvars)) |> 
  filter(year %in% 2015:2021) |> 
  rename(allocator=name)

count(stack, unifips, uniname)
count(stack, allocator)
count(stack, year)
count(stack, yeartype)
count(stack, src)

saveRDS(stack, here::here("data", "allocation", "allocators.rds"))


# create MTA allocation data with collapsed NYC ---------------------------
allocators <- readRDS(here::here("data", "allocation", "allocators.rds"))

glimpse(allocators)
count(allocators, unifips, uniname)
count(allocators, allocator)

alloc1 <- allocators |> 
  left_join(xwalkny |> select(unifips, mta, nyc, nyccollapse), by = "unifips") |> 
  filter(mta | nyc | nyccollapse, # unifips %in% c(constants$mtafips, constants$totnycfips, "36xx2"),
         year %in% 2016:2022,
         allocator != "density")
count(alloc1, unifips, uniname)

# collapse nyc
allocnyc <- alloc1 |> 
  filter(nyc | nyccollapse) |> # get the NYC totals as well as boros
  # drop details if we have the nyc total
  group_by(allocator, year, yeartype, src) |> 
  filter(!constants$totnycfips %in% unifips) |> 
  summarise(n=n(), value=sum(value), .groups="drop") |> 
  mutate(unifips="3651000", uniname="New York City") |> 
  select(-n) # drop n after inspecting it

# allocnyc |> select(unifips, uniname) |> distinct()

alloc2 <- alloc1 |> 
  filter(!unifips %in% constants$nycfips, uniname!="nycxrichmond") |> 
  bind_rows(allocnyc) |> 
  select(-c(mta, nyc, nyccollapse))

alloc2
count(alloc2, unifips, uniname)
count(alloc2, year)
count(alloc2, allocator)

# fill in missing values by carrying forward and also carrying backward (rett, sut to 2016)
# , allocator, src, yeartype
stubs <- alloc2 |> 
  select(uniname, unifips) |> 
  distinct() |> 
  expand_grid(year=2016:2022, alloc2 |> select(allocator, src, yeartype) |> distinct())
stubs |> filter(str_detect(allocator, "urban"))
summary(stubs)

alloc3 <- stubs |> 
  left_join(alloc2 |> select(unifips, uniname, allocator, src, yeartype, year, value), 
            by = c("uniname", "unifips", "allocator", "src", "yeartype", "year")) |> 
  mutate(missval=ifelse(is.na(value), TRUE, FALSE)) # flag, so we know what we fill in
summary(alloc3)


# nyconly <- c("urban_value")
nyconly <- c("urban_value", "autorental_mtaaid", "taxicab_mtaaid")
alloc4 <- alloc3 |> 
  # set counties to zero if they cannot have a value
  mutate(value=ifelse(str_detect_any(allocator, nyconly) &
                        !uniname=="New York City",
                      0,
                      value)) |> 
  group_by(uniname, unifips, allocator) |> 
  # carry forward
  arrange(year) |> 
  fill(value, .direction="down") |> 
  # carry backward
  arrange(desc(year)) |> 
  fill(value, .direction="down") |> 
  ungroup() |> 
  arrange(uniname, unifips, allocator, year)
summary(alloc4)
alloc4 |> filter(is.na(value))
alloc4 |> filter(value==0)

alloc5 <- alloc4 |> 
  rename(allocval=value) |> 
  group_by(allocator, year) |> 
  mutate(allocshare=allocval / sum(allocval),
         allocshare=ifelse(is.na(allocshare), 0, allocshare)) |> 
  # we will have na shares where sum of mrt revenue value was zero -- all of 2016
  ungroup() |> 
  # put population on the file
  left_join(alloc4 |> 
              filter(allocator=="pop") |> 
              mutate(pop=value) |> 
              select(unifips, uniname, year, pop), 
            by = c("uniname", "unifips", "year"))

summary(alloc5)
count(alloc5, unifips, uniname)
count(alloc5, allocator)
count(alloc5, src)
count(alloc5, allocator, src)
count(alloc5, year)
alloc5 |> filter(is.na(allocval))
alloc5 |> filter(is.na(allocshare))


saveRDS(alloc5, here::here("data", "allocation", "allocators_mta.rds"))


# tmp <- Sys.getenv()
# names(tmp)
# str_subset(names(tmp), "KEY")
# str_subset(names(tmp), "API")


