
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
qcew1 <- readRDS(here::here("data", "qcew", "qcew_alloc.rds"))
rett1 <- readRDS(here::here("data", "dtf", "rett.rds"))
sut1 <- readRDS(here::here("data", "dtf", "sut.rds"))
tsp1 <- readRDS(here::here("data", "dtf", "salesyear", "tsp_alloc.rds"))

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


## dtf taxable sales and purchases ------------------------------------------

tsp <- tsp1 |> 
  mutate(src="dtf_txblsales",
         yeartype="sfy")

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


## nyc-only allocation dummy -----------------------------------------------
# this dummy allows us to take a tax and allocte it entirely to nyc
nyc <- tibble(year=2016:2022,
               unifips="3651000",
               uniname="New York City",
               src="constructed",
               yeartype="any",
               name="nyconly",
               value=100)
nyc


## qcew allocation info ----------------------------------------------------
# get wtd wages for surcharge, and tpu wages for utility franchise tax
qcew1

qcew <- qcew1 |>
  select(unifips, uniname, year, type=name, private, tpu, insure, privxtpu, surchwtd=wtdqcew) |> 
  pivot_longer(cols=c(private, tpu, insure, privxtpu, surchwtd)) |> 
  mutate(prefix="qcew",
         src="qcew",
         yeartype="cy") |> 
  unite(name, prefix, name, type)
qcew

## vehicle license and registration values ----
# here are percentage shares taken from the 2019 MTA study of the 2016 tax year
# from the file called "Ratio Calculations.xlsx", tab called "MTA Aid"

vl <- read_csv(
  "area, vlicenses_2011, vlicenses_2016
nyc,	49.41,	50.79
nas,	14.48,	14.08
suf,	15.89,	15.43
west,	9.36,	9.17
put,	1.12,	1.10
dut,	3.08,	2.96
rock,	3.01,	2.93
org,	3.65, 3.54")

vr <- read_csv(
  "area, vregistrations_2011, vregistrations_2016
nyc,	34.90,	36.12
nas,	17.31,	17.00
suf,	21.59,	21.33
west,	11.56,	11.31
put,	1.60,	1.54
dut,	4.26,	4.10
rock,	3.76,	3.69
org,	5.03,	4.92
")

anames <- c("nyc", "nas", "suf", "west", "put", "dut", "rock", "org")
unames <- c("New York City", "Nassau", "Suffolk", "Westchester", "Putnam",
            "Dutchess", "Rockland", "Orange")
cbind(anames, unames)

vlicregs1 <- left_join(vl, vr, by="area") |> 
  mutate(uniname=factor(area, levels=anames, labels=unames)) |> 
  left_join(xwalkny |> select(unifips, uniname), by="uniname")
vlicregs1

vlicregs2 <- vlicregs1 |> 
  select(-area) |> 
  mutate(src="mtastudy", yeartype="cy") |> 
  pivot_longer(cols=starts_with("v"), names_to = "vname") |> 
  separate(vname, c("name", "year")) |> 
  mutate(year=as.integer(year))

vlicregs <- vlicregs2 |> 
  pivot_wider() |> 
  mutate(vlicregs=vlicenses + vregistrations) |> 
  pivot_longer(cols=starts_with("v"))


# stack and save the allocators files and drop file-specific area names ----
names(mta)
names(pmt)
names(mft)
names(mrt)
names(qcew)
names(rett)
names(sut)
names(tsp)
names(vlicregs)
# names(mtamrt)

keepvars <- c("unifips", "uniname", "name", "year", "yeartype", "src", "value")

# don't bother with mrt because we'll use values from mta
stack <- bind_rows(censuspop, mft, mta, nyc, pmt, qcew, rett, sut, tsp, vlicregs) |> 
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

## collapse nyc counties into an nyc record ----
## get the info needed for collapsing
alloc1 <- allocators |> 
  left_join(xwalkny |> select(unifips, mta, nyc, nyccollapse), by = "unifips") |> 
  filter(mta | nyc | nyccollapse, # unifips %in% c(constants$mtafips, constants$totnycfips, "36xx2"),
         year %in% 2016:2022,
         allocator != "density")
count(alloc1, unifips, uniname)

# create the collapsed nyc record
allocnyc <- alloc1 |> 
  filter(nyc | nyccollapse) |> # get the NYC totals as well as boros
  # drop details if we have the nyc total
  group_by(allocator, year, yeartype, src) |> 
  filter(!constants$totnycfips %in% unifips) |> 
  summarise(n=n(), value=sum(value), .groups="drop") |> 
  mutate(unifips="3651000", uniname="New York City") |> 
  select(-n) # drop n after inspecting it
# allocnyc |> select(unifips, uniname) |> distinct()

# add the collapsed nyc record to the main data and remove nyc county records
alloc2 <- alloc1 |> 
  filter(!unifips %in% constants$nycfips, uniname!="nycxrichmond") |> 
  bind_rows(allocnyc) |> 
  select(-c(mta, nyc, nyccollapse))

alloc2
count(alloc2, unifips, uniname)
count(alloc2, year)
count(alloc2, allocator)

## fill in missing values ----
# by carrying forward and also carrying backward (rett, sut to 2016)
# , allocator, src, yeartype
stubs <- alloc2 |> 
  select(uniname, unifips) |> 
  distinct() |> 
  expand_grid(year=2016:2022, alloc2 |> select(allocator, src, yeartype) |> distinct())
stubs |> filter(str_detect(allocator, "urban"))
stubs |> filter(str_detect(allocator, "nyconly"))
summary(stubs)

alloc3 <- stubs |> 
  left_join(alloc2 |> select(unifips, uniname, allocator, src, yeartype, year, value), 
            by = c("uniname", "unifips", "allocator", "src", "yeartype", "year")) |> 
  mutate(missval=ifelse(is.na(value), TRUE, FALSE)) # flag, so we know what we fill in
summary(alloc3)


# nyconly <- c("urban_value")
nyconly <- c("urban_value", "autorental_mtaaid", "taxicab_mtaaid", "nyconly")
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


