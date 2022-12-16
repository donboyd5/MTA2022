
# use bea wages and nonwage income as our universe 
# use qcew and susb to get shares of that universe
# excluded from tax:
#    agency or instrumentality of the United States, United Nations, interstate or international-with-Canada public corporation, fed-chartered credit union
#    K12 institution, libraries (excluded in 2011)
# lower tax rate:
#   payroll less than $1.75m annual (based on quarters)
# self employed excluded if:
#   annual net earnings from self-employment in MCTD <= $50k (raised to $50k from $10k in 2011)
# revenue lost due to 2011 changes (schools, to be offset in state budget

# 2011 changes subject to state offset
#   employer payroll threshold for taxation	$2,500 in a quarter raised to	$312,500
#     ($10k annual raised to $1.25m)
#   employer low rate instituted:
#     $312.5k-$375k / qtr 0.11% (from 0.34%) -- ($1.25m-$1.5m annual)
#     $375k-$437.5k / qtr 0.23% (from 0.34%) -- ($1.5m-$1.75m annual)
#     I have been treating the rate below $1.75m as if it is 0.17% effective rate
#     I don't really have data for this


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


# ONETIME SUSB employment and wages by firm's number of employees ----------------------------------
# get wage share by firm size by county by 2-digit naics
fn <- "county_3digitnaics_2019.xlsx"
df1 <- read_excel(here::here("data", "susb", fn), skip=2, n_max=Inf)
glimpse(df1)

df2 <- df1 |> 
  setNames(c("stfips", "stname",  "cntyfips", "county", "naics", "naicsdesc",
             "entsize", "nfirms", "nestabs", "emp", "empnoise", "wages", "wagenoise")) |> 
  mutate(fips=paste0(stfips, cntyfips)) |> 
  filter(fips %in% constants$mtafips)
glimpse(df2)
summary(df2)

# Noise Flag	Description
# G	Low noise applied to cell value (less than 2%).
# H	Medium noise applied to cell value (2% or more but less than 5%).
# J	High noise applied to cell value (5% or more).
count(df2, empnoise) |> mutate(pct=n / sum(n))
count(df2, wagenoise) |> mutate(pct=n / sum(n))
count(df2, naics, naicsdesc)

firmsize <- df2 |> 
  filter(fips %in% constants$mtafips, nchar(naics)==2 | str_detect(naics, "-")) |>
  mutate(naics=ifelse(naics=="--", "10", naics),
         sizef=case_when(str_sub(entsize, 1, 2)=="1:" ~ "total",
                         str_sub(entsize, 1, 2)=="2:" ~ "elt20",
                         str_sub(entsize, 1, 2)=="3:" ~ "e2099",
                         str_sub(entsize, 1, 2)=="4:" ~ "e100499",
                         str_sub(entsize, 1, 2)=="5:" ~ "e500p",
                         TRUE ~ "ERROR"
         ),
         sizef=factor(sizef, levels=c("total", "elt20", "e2099", "e100499", "e500p"))) |> 
  select(fips, stname, county, naics, naicsdesc, entsize, sizef, nfirms, nestabs, emp, wages) |> 
  pivot_longer(cols=c(nfirms, nestabs, emp, wages), names_to = "variable") |>
  group_by(fips, stname, county, naics, naicsdesc, variable) |>
  mutate(pct=value / value[sizef=="total"]) |> 
  ungroup()
# ,  pct2=value / sum(value[sizef!="total"])
# ,  pct2=ifelse(sizef=="total", NA_real_, pct2)
firmsize

firmsize |> filter(fips=="36027", naics=="22")

firmsize_wide <- firmsize |> 
  select(-entsize) |> 
  pivot_longer(cols=c(value, pct), names_to = "measure") |> 
  pivot_wider(names_from = sizef)
glimpse(firmsize_wide)

firmsize_wide |> 
  filter(county=="Bronx", naics=="23", measure=="pct")

saveRDS(firmsize_wide, here::here("data", "susb", "firmsize_wide.rds"))


# IMPORTANT: calculate potential PRIVATE wage loss due to small size --------------
fsize <- readRDS(here::here("data", "susb", "firmsize_wide.rds"))
glimpse(fsize)
count(fsize, county)
count(fsize, naics, naicsdesc)
count(fsize, variable)
count(fsize, measure)

avgfirmsize <- fsize |> 
  filter(naics=="10", variable %in% c("wages","nfirms"), measure=="value") |>
  select(variable, fips, total:e500p) |> 
  summarise(across(everything(), sum), .by=c(variable, fips)) |> 
  pivot_longer(-c(variable, fips)) |> 
  pivot_wider(names_from = variable) |> 
  mutate(avgfirmpayroll=wages / nfirms * 1000)

avgfirmsize |> 
  filter(name=="elt20") |> 
  arrange(desc(avgfirmpayroll))

avgfirmsize |> 
  filter(name=="e2099") |> 
  arrange(desc(avgfirmpayroll))
names(avgfirmsize)

wageloss <- avgfirmsize |> 
  select(fips, name, wages, avgfirmpayroll) |> 
  pivot_wider(values_from = c(wages, avgfirmpayroll)) |> 
  mutate(wages_excluded=wages_elt20,
         lowpct=(pmin(1.75e6, avgfirmpayroll_e2099) - avgfirmpayroll_elt20) / (avgfirmpayroll_e2099 - avgfirmpayroll_elt20),
         wages_lowrate=wages_e2099 * lowpct,
         wagepct_private_excluded=wages_excluded / wages_total,
         wagepct_private_lowrate=wages_lowrate / wages_total)


# get multiple needed data sources ----------------------------------------------------------------

qdf <- readRDS(here::here("data", "qcew", "qcew_mta.rds")) # annual qcew wages in the mta region, through 2021
beadf <- readRDS(here::here("data", "bea", "bea_mta.rds")) # annual bea income components, mta region, through 2021
pmtdf <- readRDS(here::here("data", "dtf", "pmt_collections.rds")) # monthly dtf pmt revenue components
firmsize_wide <- readRDS(here::here("data", "susb", "firmsize_wide.rds")) # 2019 SUSB nfirms, estabs, emp, wages by # employees, 2019
xwalkny <- readRDS(here::here("data", "xwalks", "xwalkny.rds")) # codes for NY counties

# checks
count(qdf, fips, area, mtasuburb_county)

# calculate QCEW wage wage base before size adjustment ----

# we need total, fed, state, local and private wages, and also local k12 
total <- expression(own==0)  # all totals
fslp <- expression(agglev==71) # fed state local private totals
localk12 <- expression(own==3 & agglev==76 & ind=="6111")
keep <- expression(eval(total) | eval(fslp) | eval(localk12))

# CAUTION!! we do not have k12 wages until 2015!!

pmt_qcewbase <- qdf |> 
  filter(eval(keep)) |> 
  filter(year>=2015) |> 
  select(year, mtasuburb_county, fips, area, own, ownf, agglev, agglevf, ind, indf, wages)
glimpse(pmt_qcewbase)
count(pmt_qcewbase, year)

# the following is ok -- in 2015, indf was Elementary and secondary schools
#   and in 2016+ indf was NAICS 6111 Elementary and secondary schools
count(pmt_qcewbase, ind, indf) 
tmp <- pmt_qcewbase |> filter(ind=="6111")


# flip wages and get fed and k12 shares
# count(pmt_qcewbase2, vname)
pmt_qcewbase_fedk12 <- pmt_qcewbase |> 
  mutate(vname=case_when(own==0 ~ "wage_tot",
                         own==1 ~ "wage_fed",
                         own==2 ~ "wage_state",
                         own==3 & agglev==71 ~ "wage_local",
                         own==5 ~ "wage_private",
                         own==3 & agglev==76 & ind=="6111" ~ "wage_k12",
                         TRUE ~ "ERROR")) |> 
  select(year, mtasuburb_county, fips, area, vname, wages) |>
  pivot_wider(names_from = vname, values_from = wages) |> 
  mutate(k12_share=wage_k12 / wage_tot,
         fed_share=wage_fed / wage_tot)

# bring in small wage loss and exclusion ----------------------------------------------------
glimpse(pmt_qcewbase_fedk12)
pmt_qcewbase_fedk12_small <- pmt_qcewbase_fedk12 |> 
  left_join(wageloss |> 
              select(c(fips, starts_with("wagepct_"))),
            by = join_by(fips)) |> 
  mutate(smallemp_excluded_share=wagepct_private_excluded * wage_private / wage_tot,
         smallemp_lowrate_share=wagepct_private_lowrate * wage_private / wage_tot) |> 
  select(-starts_with("wagepct_"))
glimpse(pmt_qcewbase_fedk12_small)



# use BEA earnings data to calculate self-employment tax base -------------
pmt_earnbase <- beadf |> # ends 2021
  filter(year>=2015) |>  # to be compatible with qcew year
  select(fips, area, year, vname, value) |> 
  pivot_wider(names_from = vname) |> 
  rename(earnings_bea=earnings, wages_bea=wages, supplements_bea=supplements,
         propinc_bea=propinc, farminc_bea=farminc, nfpi_bea=nfpi) |> 
  mutate(nonwage_bea=earnings_bea - wages_bea)

glimpse(pmt_earnbase)
count(pmt_earnbase, year)


# payroll tax base -------------------------------------------------------
pmt_base1 <- pmt_earnbase |>
  select(fips, year, wages_bea, propinc_bea) |> 
  left_join(pmt_qcewbase_fedk12_small,
            by = join_by(fips, year))
glimpse(pmt_base1)
count(pmt_base1, year)

pmt_base2 <- pmt_base1 |> 
  mutate(wages_beak12=wages_bea * k12_share,
         wages_beafed=wages_bea * fed_share,
         wages_beasmallemp_excluded=wages_bea * smallemp_excluded_share,
         wages_beasmallemp_lowrate=wages_bea * smallemp_lowrate_share,
         # this next line is key
         wages_beapmt=wages_bea - wages_beasmallemp_excluded - 
           wages_beak12 - wages_beafed,
         wages_beafullrate=wages_beapmt - wages_beasmallemp_lowrate,
         nonwage_beapmt=propinc_bea * propinc_taxable_ratio,
         taxbase_beapmt=wages_beapmt + nonwage_beapmt) |> 
  select(fips, year, mtasuburb_county, area, 
         starts_with("wage_"), # qcew
         contains("share"),
         starts_with("wages_bea"),
         starts_with("nonwage_bea"),
         ends_with("_beapmt"))
glimpse(pmt_base2)
count(pmt_base2, fips, area, mtasuburb_county)
count(pmt_base2, year) # through 2021

saveRDS(pmt_base2, here::here("data", "pmtbase_updated.rds"))



