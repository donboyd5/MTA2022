
# https://www.bls.gov/developers/api_r.htm
# includes ----
source(here::here("r", "libraries.r"))
source(here::here("r", "constants.r"))

#libraries ----
# devtools::install_github("mikeasilva/blsAPI")
# library(devtools)
# library(blsAPI)
# library(jsonlite)
# library(tidyjson)
# install.packages("blscrapeR") # for area titles
devtools::install_github("keberwein/blscrapeR") # for area titles, get rid of it
library(blscrapeR)

# various codes ----
# owner codes
# 0	Total Covered
# 1	Federal Government
# 2	State Government
# 3	Local Government
# 4	International Government
# 5	Private
# 8	Total Government
# 9	Total U.I. Covered (Excludes Federal Government)

# "own_code","own_title"
# 0,"Total Covered"
# 1,"Federal Government"
# 2,"State Government"
# 3,"Local Government"
# 4,"International Government"
# 5,"Private"
# 8,"Total Government"
# 9,"Total U.I. Covered (Excludes Federal Government)"

areacodes <- blscrapeR::area_titles
mtacodes <- areacodes |> filter(area_fips %in% constants$mtafips) |> 
  mutate(aname=str_remove(area_title, " County, New York"))
mtacodes
nycfips <- c("36005", "36047", "36061", "36081", "36085")


# download files ----
# http://data.bls.gov/cew/data/api/2017/1/industry/10.csv
# http://data.bls.gov/cew/data/api/2021/a/industry/10.csv

# https://data.bls.gov/cew/doc/titles/industry/industry_titles.htm
# 6111 NAICS 6111 Elementary and secondary schools

## all local govt inds ----
allindsurl <- "http://data.bls.gov/cew/data/api/2021/a/industry/10.csv"
allindsa <- read_csv(allindsurl)
allinds <- allindsa |> 
  filter(own_code==3, str_sub(area_fips, 1, 2)=="36") |> 
  select(area_fips:avg_annual_pay) |> 
  right_join(mtacodes, by = join_by(area_fips)) |> 
  mutate(type="localtot")

## k12 education -- we need to remove this ----
k12url <- "http://data.bls.gov/cew/data/api/2021/a/industry/6111.csv"
k12a <- read_csv(k12url)
k12 <- k12a |> 
  filter(own_code==3, str_sub(area_fips, 1, 2)=="36") |> 
  select(area_fips:avg_annual_pay) |> 
  right_join(mtacodes, by = join_by(area_fips)) |> 
  mutate(type="k12")

## combine files ----
mta <- bind_rows(allinds, k12)

# check employment
mtacodes

emp <- mta |> 
  filter(area_fips %in% nycfips) |> 
  select(area_fips, aname, type, emp=annual_avg_emplvl) |> 
  pivot_wider(names_from = type, values_from = emp) |> 
  janitor::adorn_totals()
emp

nylocemp <- allindsa |> 
  filter(own_code==3, str_sub(area_fips, 1, 2)=="36") |>
  select(area_fips, agglvl_code, size_code, year, emp=annual_avg_emplvl) |> 
  janitor::adorn_totals() # note this sums agglvl_code

nylocemp |> 
  select(area_fips, agglvl_code, emp) |> 
  arrange(desc(emp))

mtawide1 <- mta |> 
  # filter(!area_fips %in% nycfips) |> 
  select(area_fips, aname, type, wages=total_annual_wages, emp=annual_avg_emplvl) |> 
  pivot_wider(names_from = type, values_from = c(emp, wages))

mtawide2 <- mtawide1 |> 
  mutate(county=ifelse(area_fips %in% nycfips, "New York City", aname)) |> 
  select(-aname, -area_fips) |> 
  summarise(across(contains("_"), sum), .by="county") |> 
  mutate(wages_localxk12=wages_localtot - wages_k12,
         pmtnow=.0034 * wages_localxk12,
         pmtproposed=.0050 * wages_localxk12,
         pmtincrease=pmtproposed - pmtnow)
  
mtawide2

# get property tax data ----
# https://data.ny.gov/api/views/iq85-sdzs/rows.csv?accessType=DOWNLOAD&sorting=true
pturl <- "https://data.ny.gov/api/views/iq85-sdzs/rows.csv?accessType=DOWNLOAD&sorting=true"
ptfn <- "Real_Property_Tax_Rates_Levy_Data_By_Municipality__Beginning_2004.csv"
ptpath <- here::here("data", "proptax", ptfn)
download.file(pturl, ptpath, mode="wb")

ptax1 <- read_csv(ptpath)
glimpse(ptax1)

ptax2 <- ptax1 |> 
  select(fye=`Fiscal Year Ending`, ryear=`Roll Year`, 
         swis=`Swis Code`, muni=Municipality, county=County,
         schcode=`School Code`, schname=`School Name`, valtype=`Type of Value on which Tax Rates are applied`, 
         cntylevy=`County Tax Levy`, 
         munilevy=`Municipality Tax Levy`,
         sdlevy=`School District Tax Levy`) |> 
  filter(fye==2022)

count(ptax2, county)  
mtanames <- c("New York City", 
              "Dutchess", "Nassau", "Orange", "Putnam", "Rockland", "Suffolk", "Westchester")

ptax3 <- ptax2 |> 
  filter(county %in% mtanames)
count(ptax3, county)

ptaxsum <- ptax3 |> 
  mutate(totlevy=cntylevy + munilevy + sdlevy) |> 
  summarise(across(contains("levy"),
                   sum), .by = county)
ptaxsum

combined <- mtawide2 |> 
  left_join(ptaxsum, by = join_by(county)) |> 
  mutate(# totlevy=ifelse(county == "New York City", NA_real_, totlevy),
         pmtinc_pctptax=pmtincrease / totlevy)

tab <- combined |> 
  # filter(county != "New York City") |> 
  select(county, wages_localxk12, pmtincrease, totlevy, pmtinc_pctptax) |> 
  gt() |> 
  tab_header(
    title = "Local govt non-K12 PMT increase as % of TOTAL local property taxes, INCLUDING school tax",
    subtitle = "$ millions. Based on 2021 QCEW and 2022 FY property tax levies"
  ) |> 
  cols_label(wages_localxk12="Local govt wages excluding K12 wages",
             pmtinc_pctptax = html("LG PMT increase % of TOTAL local property taxes")) |> 
  fmt_currency(columns=c(wages_localxk12, totlevy, pmtincrease),
             scale=1e-6,
             decimals=3) |> 
  fmt_percent(columns=pmtinc_pctptax, decimals=2) 
tab
  


