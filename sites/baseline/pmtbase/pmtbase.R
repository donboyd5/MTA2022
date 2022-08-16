

# libraries ---------------------------------------------------------------

source(here::here("r", "libraries.r"))
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
  mutate(pmt_qcewbase=wage_private + wage_state + wage_local - wage_k12)

pmt_qcewbase2 |> 
  filter(year==2020) |> 
  mutate(pct=pmt_qcewbase / wage_tot)

# |> 
#   janitor::adorn_totals() |> 
#   mutate(pmtbase=wage_private + wage_state + wage_local - wage_k12,
#          basepct=pmtbase / wage_tot)


# use BEA earnings data to calculate self-employment tax base -------------
pmt_earnbase <- beadf |> 
  filter(year>=2015) |>  # to be compatible with qcew year
  select(fips, area, year, vname, value) |> 
  pivot_wider(names_from = vname)


# payroll tax base -------------------------------------------------------
pmt_base1 <- pmt_earnbase |>
  select(-area) |> 
  left_join(pmt_qcewbase2,
            by=c("fips", "year"))

pmt_base2 <- pmt_base1 |> 
  mutate(mtasub=get_mtasub(fips),
         nonwage=earnings - wages,
         pmtwages=wages * pmt_qcewbase / wage_tot,
         pmtnonwage=propinc * propinc_taxable_ratio,
         pmtbase=pmtwages + pmtnonwage) |> 
  select(fips, year, mtasub, area, earnings, wages, nonwage, pmtbase, pmtwages, pmtnonwage)

pmt_base2 |> 
  group_by(year) |> 
  summarise(across(c(earnings, wages, nonwage, pmtbase, pmtwages, pmtnonwage), sum), .groups="drop") |> 
  mutate(pct=pmtbase / earnings,
         pctwages=pmtwages / wages,
         pctnonwage=pmtnonwage / nonwage,
         dbase=earnings - pmtbase,
         dwages=wages - pmtwages,
         dnonwage=nonwage - pmtnonwage,
         tax=pmtbase * .0034)
  





# OLD BELOW HERE ----------------------------------------------------------


pmtbase2 |> 
  mutate(area=str_remove(area, " County, New York")) |> 
  gt() |> 
  tab_header(
    title = "BLS QCEW wages and PMT tax base, 2020, $ millions",
    subtitle = "Calculated base = private + state + local - k12"
  ) |> 
  fmt_number(
    columns = c(starts_with("wage"), pmtbase),
    decimals=1,
    scale_by=1e-6,
    suffixing = FALSE
  ) |> 
  fmt_percent(
    columns=basepct,
    decimals=1
  )




qdf |> 
  filter(fips=="36079", own==3, str_sub(ind, 1, 3)=="611", wages!=0) |> 
  select(year, fips, area, own, ownf, agglev, agglevf, ind, indf, wages)


# filter((own==0) |
#          (own==1 & agglev==71) |
#          (own==2 & agglev==71) |
#          (own==5 & agglev==71) |
#          (own==3), wages!=0)



# get BEA data ------------------------------------------------------------
df4

qbea1 <- pmtbase2 |> 
  filter(year=="2020") |> 
  select(fips, area, wage_tot, pmtbase, basepct) |> 
  left_join(df4 |> filter(year==2020) |> rename(fips=geofips), by = "fips") |> 
  mutate(rgn=ifelse(fips %in% nycfips, "NYC", "Suburbs")) |> 
  select(fips, geoname, rgn, wage_tot, pmtbase, earnings, wages, supplements, propinc, farminc, nfpi)
qbea1

subtots <- qbea1 |> 
  group_by(rgn) |> 
  summarise(across(wage_tot:nfpi, sum))

tots <- qbea1 |> 
  summarise(across(wage_tot:nfpi, sum)) |> 
  mutate(rgn="Total")
tots

(qbea2 <- bind_rows(qbea, subtots, tots))

qbea3 <- qbea2 |> 
  select(rgn, fips, area, everything()) |> 
  mutate(pmtpct=pmtbase / wage_tot,
         wages_adj=wages * pmtpct,
         earnings_adj=wages_adj + propinc * 831.7 / 1130.8,
         adjpct=earnings_adj / earnings,
         tax=earnings_adj * 0.0034) |> 
  arrange(rgn, area)
qbea3




# temp <- qdf |> 
#   filter(year==2020, own==0) |> 
#   select(year, fips, area, ownf, wages)
# # writeClipboard(temp) # must be vector
# 
# write.xlsx(temp, here::here("analysis", "temp.xlsx"))

# nycounty <- qdf |> 
#   filter(year==2020, fips=="36061") |> 
#   filter((own==0) |
#            (own==1 & agglev==71) |
#            (own==2 & agglev==71) |
#            (own==5 & agglev==71) |
#            (own==3), wages!=0)
# write.xlsx(nycounty, here::here("analysis", "nycounty.xlsx"))


qdf |> 
  filter(year==2020) |> 
  filter(ind==10, agglev %in% 70:71) |> 
  group_by(own, ownf) |> 
  summarise(wages=sum(wages)) |> 
  mutate(tax=wages * .0034)
