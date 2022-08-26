

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


# get data ----------------------------------------------------------------

qdf <- readRDS(here::here("data", "qcew", "qcew_mta.rds"))
beadf <- readRDS(here::here("data", "bea", "bea_mta.rds"))
pmtdf <- readRDS(here::here("data", "dtf", "pmt_collections.rds"))
wagesize <- readRDS(here::here("data", "susb", "wagesize.rds"))

# use QCEW wages to calculate the pmt wage base as percent of total wages ----

# we need total, fed, state, local and private wages, and also local k12 
total <- expression(own==0)  # all totals
fslp <- expression(agglev==71) # fed state local private totals
localk12 <- expression(own==3 & agglev==76 & ind=="6111")
keep <- expression(eval(total) | eval(fslp) | eval(localk12))

# CAUTION!! we do not have k12 wages until 2015!!

pmt_qcewbase1 <- qdf |> 
  filter(eval(keep)) |> 
  filter(year>=2015) |> 
  select(year, mtasub, fips, area, own, ownf, agglev, agglevf, ind, indf, wages)
pmt_qcewbase1

# flip wages and compute base
pmt_qcewbase2 <- pmt_qcewbase1 |> 
  mutate(vname=case_when(own==0 ~ "wage_tot",
                         own==1 ~ "wage_fed",
                         own==2 ~ "wage_state",
                         own==3 & agglev==71 ~ "wage_local",
                         own==5 ~ "wage_private",
                         own==3 & agglev==76 & ind=="6111" ~ "wage_k12",
                         TRUE ~ "ERROR")) |> 
  select(year, mtasub, fips, area, vname, wages) |>
  pivot_wider(names_from = vname, values_from = wages) |> 
  mutate(pmt_gross_qcewbase=wage_private + wage_state + wage_local - wage_k12)

pmt_qcewbase2 |> 
  filter(year==2020) |> 
  mutate(pct=pmt_gross_qcewbase / wage_tot)


# bring in wages LT $1.2m in 2019, from SUSB ---------------------------------------

pmt_qcewbase3 <- pmt_qcewbase2 |> 
  left_join(wagesize |> select(fips, pctelt20), by = "fips") |> 
  mutate(smallwages=wage_private * pctelt20,
         pmt_qcewbase=pmt_gross_qcewbase - smallwages)

pmt_qcewbase3 |> 
  filter(year==2020) |> 
  mutate(pctgross=pmt_gross_qcewbase / wage_tot,
         pctnet=pmt_qcewbase / wage_tot) |> 
  select(year, fips, area, pctgross, pctnet)

# use BEA earnings data to calculate self-employment tax base -------------
pmt_earnbase <- beadf |> 
  filter(year>=2015) |>  # to be compatible with qcew year
  select(fips, area, year, vname, value) |> 
  pivot_wider(names_from = vname)


# payroll tax base -------------------------------------------------------
pmt_base1 <- pmt_earnbase |>
  select(fips, year, earnings_bea=earnings, wages_bea=wages, supplements_bea=supplements,
         propinc_bea=propinc, farminc_bea=farminc, nfpi_bea=nfpi) |> 
  left_join(pmt_qcewbase3,
            by=c("fips", "year"))
glimpse(pmt_base1)

pmt_base2 <- pmt_base1 |> 
  mutate(mtasub=get_mtasub(fips),
         nonwage_bea=earnings_bea - wages_bea,
         wages_pmt=wages_bea * pmt_qcewbase / wage_tot,
         nonwage_pmt=propinc_bea * propinc_taxable_ratio,
         taxbase_pmt=wages_pmt + nonwage_pmt) |> 
  select(fips, year, mtasub, area, 
         ends_with("_bea"),
         starts_with("wage_"), # qcew
         pmt_gross_qcewbase, smallwages, pmt_qcewbase,
         ends_with("_pmt"))
glimpse(pmt_base2)

saveRDS(pmt_base2, here::here("data", "pmtbase.rds"))


df <- readRDS(here::here("data", "pmtbase.rds"))

