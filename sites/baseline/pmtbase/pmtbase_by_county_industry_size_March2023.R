
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


# caution - avgfirmpayroll is avgpayroll of THE PORTION OF a firm in the county-industry



# get multiple needed data sources ----------------------------------------------------------------

qdf <- readRDS(here::here("data", "qcew", "qcew_mta.rds")) # annual qcew wages in the mta region, through 2021
beadf <- readRDS(here::here("data", "bea", "bea_mta.rds")) # annual bea income components, mta region, through 2021
# pmtdf <- readRDS(here::here("data", "dtf", "pmt_collections.rds")) # monthly dtf pmt revenue components
# firmsize_wide <- readRDS(here::here("data", "susb", "firmsize_wide.rds")) # 2019 SUSB nfirms, estabs, emp, wages by # employees, 2019
fsize <- readRDS(here::here("data", "susb", "firmsize_wide.rds")) # by county and industry (3 digit) and firm # employees
# the # employees buckets sum to the total in each row
# the variables for each county-industry are nfirms, pct, nestabs, pct, emp, pct, and wages, pct
# geographies are not additive nor are industries because same firm can be in multiple counties or industries
glimpse(fsize)
count(fsize, county)
count(fsize, naics, naicsdesc)
count(fsize, variable)
count(fsize, measure)

# xwalkny <- readRDS(here::here("data", "xwalks", "xwalkny.rds")) # codes for NY counties

# checks
count(qdf, fips, area, mtasuburb_county)
count(qdf, ind, indf)

# calculate QCEW wage  base before size adjustment ----
# let's keep 1, 2, and 3-digit industries for 2021
pmtbase1 <- qdf |> 
  filter(year==2021,
         fips %in% constants$mtafips,
         nchar(ind) <= 4)

## collapse NYC ----
pmtbase2 <- pmtbase1 |> 
  mutate(fips=ifelse(nyc_county, "36xxx", fips),
         area=ifelse(nyc_county, "New York City", area)) |> 
  summarise(across(c(emp, wages, estabs), sum),
            .by=c(year, fips, own, ownf, area, agglev, agglevf, ind, indf))

# get local government info to add to pmtbase2 ----
localgovts1 <- pmtbase2 |> 
  filter(own==3, 
         ind %in% c("10", "611", "6111", "61111"),
         emp > 0)
# this is a superset of what we need - now get the best we can

# where possible use NAICS 6111 Elementary and secondary schools for k12
# but where not use NAICS 611 Educational services

localgovts2 <- localgovts1 |> 
  mutate(has6111=sum(ind=="6111") > 0,
         etype=case_when(ind=="10" ~ "local",
                         ind=="6111" ~ "k12",
                         ind=="611" & !has6111 ~ "k12", # k12 proxy if we don't have 6111
                         ind=="611" & has6111 ~ "dontuse",
                         TRUE ~ "ERROR"),
         .by="fips")


# for Putnam we have NAICS 611 Educational services but not
#   NAICS 6111 Elementary and secondary schools
# so we give them the share of neighboring counties
localgovts3 <- localgovts2 |> 
  filter(etype != "dontuse") |> 
  select(year, fips, area, etype, emp, wages, estabs) |> 
  pivot_longer(cols=c(emp, wages, estabs), names_to = "vtype") |> 
  pivot_wider(names_from = etype) |> 
  mutate(putnam_neighbor=str_detect_any(area, c("Dutchess", "Orange", "Westchester")),
         k12share=k12 / local,
         nbrshare=mean(k12share[putnam_neighbor]),
         .by=vtype) |> 
  mutate(k12putnam=ifelse(str_detect(area, "Putnam"), nbrshare * local, NA_real_)) |> 
  mutate(k12=ifelse(str_detect(area, "Putnam"), pmin(k12putnam, k12), k12))


# determine small employer exclusion at 3-digit level ----
privates1 <- pmtbase2 |> 
  filter(own==5, 
         ind=="10" | agglev=="75",
         emp > 0)

# calculate the residual 
privresidual1 <- privates1 |> 
  mutate(
    # employment
   etot=emp[ind=="10"],
   eother=sum(emp[ind!="10"]),
   eresidual=etot - eother,
   eresidpct=eresidual / etot,
   # wages
   wtot=wages[ind=="10"],
   wother=sum(wages[ind!="10"]),
   wresidual=wtot - wother,
   wresidpct=wresidual / wtot,
   # establishments
   esttot=estabs[ind=="10"],
   estother=sum(estabs[ind!="10"]),
   estresidual=esttot - estother,
   estresidpct=estresidual / esttot,
   .by = fips)

privresidual2 <- privresidual1 |> 
  summarise(emp=first(eresidual),
            wages=first(wresidual),
            estabs=first(estresidual),
            .by=c(fips, area, year)) |> 
  mutate(ind="xxx", indf="residual")


privates2 <- bind_rows(privates1 |> select(year, fips, area, ind, indf, emp, wages, estabs),
                      privresidual2) |> 
  mutate(own=5, ownf="Private")

# checks
privates2 |>
  mutate(group=ifelse(ind=="10", "total", "detail")) |> 
  summarise(emp=sum(emp), wages=sum(wages), estabs=sum(estabs),
            .by=c(fips, area, group))


## now bring in size breakdown for privates ----
count(fsize, fips, county) # all 12
count(fsize, naics, naicsdesc)
naicsvals <- count(fsize, naics, naicsdesc)

## prep the fsize data by collapsing nyc ----
fsize_prep1 <- fsize |> 
  filter(measure=="value", variable %in% c("emp", "wages", "nestabs")) |> 
  mutate(area=ifelse(fips %in% constants$nycfips, "New York City", county),
         fips=ifelse(fips %in% constants$nycfips, "36xxx", fips),
         variable=ifelse(variable=="nestabs", "estabs", variable)) |> 
  # fill missing with zero even though we know it's not correct
  mutate(across(elt20:e500p, ~ replace_na(., 0)),
         sum=rowSums(across(elt20:e500p))) |> 
  relocate(sum, .after=total)
  
count(fsize_prep1, fips, area, county)  

## collapse NYC, recognizing that we will overstate the number of firms as there is double counting ----
# but wages will be ok
fsize_prep2 <- fsize_prep1 |> 
  summarise(across(total:e500p, ~ sum(.x, na.rm=TRUE)), .by=c(fips, area, naics, naicsdesc, variable)) |> # collapses NYC
  mutate(sum = rowSums(across(elt20:e500p), na.rm=TRUE))

## get average wages for every group -- adds records ----
fsize_prep3 <- fsize_prep2 |> 
  pivot_longer(cols=total:e500p) |> 
  pivot_wider(names_from = variable) |> 
  mutate(avgwage=wages * 1000 / emp) |> 
  pivot_longer(cols=estabs:avgwage, names_to = "variable") |> 
  pivot_wider()


# split into smaller employment groups based on CBP and uniform assumptions ----
fn <- "cbp20co.txt"

df1 <- read_csv(here::here("data", "cbp", fn))

df2 <- df |> 
  mutate(fips=paste0(fipstate, fipscty)) |> 
  filter(fips %in% constants$mtafips)

fn <- "CBP2020.CB2000CBP-Data.csv"
# df1 <- read_csv(here::here("data", "cbp", fn), 
#                 col_names = FALSE,
#                 col_types = cols(.default = col_character()))

df1 <- read_csv(here::here("data", "cbp", fn), 
                col_types = cols(.default = col_character()))


defs <- df1 |> 
  filter(row_number() ==1) |> 
  pivot_longer(cols=everything(),
               names_to = "vname",
               values_to = "desc") |> 
  mutate(vnum=row_number()) |> 
  relocate(vnum)

df2 <- df1 |> 
  filter(row_number() !=1) |> 
  lcnames() |> 
  mutate(sumlev=str_sub(geo_id, 1, 3),
         rectype=case_when(sumlev=="040" ~ "state", 
                           sumlev=="050" ~ "county",
                           TRUE ~ "ERROR"),
         fips=str_extract_after_last(geo_id, "US")) |> 
  select(rectype, sumlev, fips, name, naics=naics2017, naicsdesc=naics2017_label,
         lfo, lfo_label,
         empsize=empszes, empsizef=empszes_label, year, estabs=estab,
         wages=payann, emp) |> 
  mutate(across(c(estabs, wages, emp), 
                as.numeric)) 

# we have estabs, wages, emp for the state (and also lfo) by sizes but only estabs by size for counties
# so attribute state emp and wages shares to counties, controlling to ensure we hit the totals

count(df2, empsize, empsizef)

nys1 <- df2 |> filter(rectype=="state")
cnty <- df2 |> filter(rectype=="county")
skim(cnty)

nys2 <- nys1 |> 
  filter(lfo=="001") |> 
  select(-c(rectype, sumlev, name, year, lfo, lfo_label)) |> 
  mutate(empestabs_nys=emp / estabs,
         avgwage_nys=wages * 1000 / emp) |> 
  mutate(avgwageratio_nys=avgwage_nys / avgwage_nys[empsize=="001"],
         .by=naics) |> 
  mutate(across(c(estabs, wages, emp),
         ~ .x / .x[empsize=="001"],
         .names = "{.col}_share"),
         .by=naics)

# get cartesian product so that we have every state industry and size, for every county
indsize <- nys2 |> 
  select(naics, naicsdesc, empsize, empsizef) |> 
  distinct()

areas <- cnty |> 
  select(fips, name) |> 
  distinct()

base <- crossing(areas, indsize)

## create a complete county file with all counties, all industries, all sizes ----
cnty_imputed1 <- base |> 
  left_join(cnty |> 
              select(fips, naics, empsize, estabs, wages, emp),
            by = join_by(fips, naics, empsize)) |> 
  left_join(nys2 |> select(naics, empsize, contains("_")),
            by = join_by(naics, empsize))

## ensure that county-industry details # eestablishments add to total establishments for every group ----
# use the nys info to help fill in
cnty_imputed2 <- cnty_imputed1 |> 
  mutate(grouptot=ifelse(empsize=="001", TRUE, FALSE)) |> 
  mutate(fillneed=estabs[grouptot] - sum(estabs[!grouptot], na.rm=TRUE),
         fillin=ifelse(!grouptot, fillneed * estabs_share, NA_real_),
         ifillin=as.integer(fillin),
         ifillin=ifelse(grouptot, sum(ifillin, na.rm=TRUE), ifillin),
         # dump truing-up fillin in the first employment category
         trueup=fillneed[grouptot] - ifillin[grouptot],
         # final fillin
         ffillin=ifelse(row_number()==2, ifillin + trueup, ifillin),
         estabs2=ifelse(grouptot, estabs, naz(estabs) + ffillin),
         .by=c(fips, naics)) |> 
  relocate(fillneed, fillin, ifillin, trueup, ffillin, estabs2, .before=estabs)

# check - do we every have a situation where sum of estabs2 not = estabs[grouptot]
check <- cnty_imputed2 |> 
  group_by(fips, name, naics, naicsdesc) |> 
  summarise(estabs_tot=estabs[grouptot],
            estabs_sum=sum(estabs[!grouptot], na.rm=TRUE),
            estabs2_tot=estabs2[grouptot],
            estabs2_sum=sum(estabs2[!grouptot], na.rm=TRUE),
            .groups="drop"
            )

check |> 
  filter(estabs2_tot != estabs_tot |
           estabs2_sum != estabs_tot)
# good, details ALWAYS sum to totals


## now that establishments are filled in, construct employment details that sum to total for each group ----
cnty_imputed3 <- cnty_imputed2 |> 
  select(-c(fillneed, fillin, ifillin, ffillin, estabs, trueup)) |> 
  rename(estabs=estabs2) |> 
  # get establishment detail
  mutate(emp1=ifelse(!grouptot, estabs * empestabs_nys, NA_real_),
         emp1sum=sum(emp1, na.rm=TRUE),
         emp2=emp1 * emp[grouptot] / emp1sum[grouptot],
         emp2=ifelse(grouptot, sum(emp2[!grouptot]), emp2),
         .by=c(fips, naics)) |>  
  relocate(emp1, emp1sum, emp2, .after=emp)

# check -- is total employment always equal to what we expected?
check <- cnty_imputed3 |> 
  filter(grouptot) |> 
  filter(emp != emp2) |> 
  select(1:6, estabs, wages, emp, emp2) |> 
  mutate(diff=emp - emp2) |> 
  arrange(desc(abs(diff)))

# all good

## get group average wages then calc detailed avg wages to hit total wages ----
#   when adjusted, avg wages * emp = total wages
cnty_imputed4 <- cnty_imputed3 |> 
  select(-c(emp, emp1, emp1sum)) |> 
  rename(emp=emp2) |> 
  mutate(avgwage=wages * 1000 / emp,
         avgwage1=ifelse(!grouptot, avgwage[grouptot] * avgwageratio_nys, NA_real_),
         wages1=ifelse(grouptot, NA_real_, avgwage1 * emp / 1000),
         wages1sum=sum(wages1, na.rm=TRUE),
         avgwage2=avgwage1 * wages[grouptot] / wages1sum[grouptot],
         wages2=ifelse(!grouptot, emp * avgwage2 / 1000, NA_real_),
         wages2=ifelse(grouptot, sum(wages2[!grouptot], na.rm=TRUE), wages2),
         avgwage3=wages2 * 1000 / emp,
         .by=c(fips, naics)) |> 
  relocate(avgwage, avgwage1, wages1, wages1sum, avgwage2, wages2, avgwage3, .after=wages)

# check: are wages and avg wages ever inconsistent with expected totals?
check <- cnty_imputed4 |> 
  filter(grouptot) |> 
  select(1:9, emp, wages2, avgwage3) |> 
  mutate(diff1=wages2 - wages, diff2=avgwage - avgwage3) |> 
  arrange(desc(abs(diff1)))

# chautauqua all other industries off by $76k, the rest look good
# 36013 Chautauqua County, New York 99 Industries not classified 001 All establishments 4 76
# it has 0 emp -- maybe someday force it to have 2 employees?

cnty_imputed <- cnty_imputed4 |>
  select(fips, name, naics, naicsdesc, empsize, empsizef, estabs, wages=wages2, avgwage=avgwage3, grouptot) |> 
  mutate(estpayroll=wages * 1000 / estabs) |> 
  relocate(estpayroll, .before=grouptot)

saveRDS(cnty_imputed, here::here("data", "cbp", "cbp_filledin.rds"))



# now make up subranges with interpolation --------------------------------

cbp1 <- readRDS(here::here("data", "cbp", "cbp_filledin.rds"))
skim(cbp1)
count(cbp1, empsize, empsizef)
cbp1 |> filter(fips %in% constants$mtafips, naics=="00")

cbp1 |> filter(fips %in% constants$mtafips, naics=="00", empsize=="241") # 2049
cbp1 |> filter(fips %in% constants$mtafips, naics=="00", empsize=="242") # 5099
cbp1 |> filter(fips %in% constants$mtafips, naics=="00", empsize=="251") # 100249

cbp1 |> filter(fips %in% constants$mtafips, naics=="00", fips=="36059") # 2049

# fill in 5099 and 100-249 and 250499 by 25
# simple buckets uniform even though we know wrong, for estabs and emp, keep avgwages at group constant??



  

xlsx::write.xlsx(cnty_imputed2 |> filter(row_number() <= 50), file = here::here("check.xlsx"))
  






## split certain employment groups into smaller groups ----
# based on BED data https://www.bls.gov/news.release/cewfs.nr0.htm
# shares of group shown relative to its larger group




e2049_firmshare <- .75
e2049_empshare <- .57
# e100249 / e100499	firms 76.5%	emp 59.1%
e100249_firmshare <- .77
e100249_empshare <- .59

esize_splits <- read_csv("
original, new, estabshare, empshare
e2099, e2049, .75, 
e2099, e5099, .25
e100499, e100249, .77
e100499, e250e499, .23
")

fsize_prep4 <- fsize_prep3 |> 
  mutate(e2049=case_when(variable=="estabs" ~ e2049_firmshare * e2099, # based on shares at link above
                         variable=="avgwage" ~ e2099,
                         variable=="emp" ~ e2049_empshare * e2099,
                         # wages will have the same share as employment because we assume same avgwage
                         variable=="wages"~ e2049_empshare * e2099),
         e5099=ifelse(variable=="avgwage", e2099, e2099 - e2049),
         
         e100249=case_when(variable=="estabs" ~ e100249_firmshare * e100499, # based on shares at link above
                         variable=="avgwage" ~ e100499,
                         variable=="emp" ~ e100249_empshare * e100499,
                         # wages will have the same share as employment because we assume same avgwage
                         variable=="wages"~ e100249_empshare * e100499),
         e250499=ifelse(variable=="avgwage", e100499, e100499 - e100249)) |> 
  relocate(e2049, e5099, .after=e2099) |> 
  relocate(e100249, e250499, .after=e100499) |> 
  # fill missing with zero to deal with sum=0 case
  mutate(across(sum:e500p, ~ replace_na(., 0)))

# check
xlsx::write.xlsx(fsize_prep4, file = here::here("check.xlsx"))
skim(fsize_prep4)


# save the firm size data because they are KNOWN - then we'll gues --------
saveRDS(fsize_prep4, here::here("sites", "baseline", "pmtbase", "firmsize_byemp.rds"))
tmp <- fsize_prep4


# make the final firm-size prep file ----
firmemp <- read_csv("
egroup, firmemp
elt20, 10
e2049, 35
e5099, 75
e100249, 175
e250499, 375
e500p, 600")
firmemp

fsize_prep <- fsize_prep4 |> 
  # drop the two groups we just disaggregated and inspected, as their new details will always sum to them
  select(-c(e2099, e100499)) |> 
  # guess at average firm payroll in each group by using avgwage and midpoint employees
  pivot_longer(cols=total:e500p, names_to = "egroup") |> 
  pivot_wider(names_from = variable) |> 
  left_join(firmemp, by = join_by(egroup)) |> 
  mutate(avgfirmqpayroll=firmemp * avgwage / 4)

tmp <- fsize_prep |> filter(egroup=="e2049")
tmp <- fsize_prep |> filter(egroup=="e5099")

saveRDS(fsize_prep, here::here("sites", "baseline", "pmtbase", "firmsize_synthetic.rds"))
  

# now that we have the firmsize groups we want, calculate shares of total ----

fsize_pcts <- fsize_prep4 |> 
  # drop the two groups we just disaggregated and inspected, as their new details will always sum to them
  select(-c(e2099, e100499)) |> 
  # now calc percents
  mutate(across(elt20:e500p, ~ naz(.x) / sum, .names="{.col}_pct")) |> 
  # deal with edge cases
  mutate(elt20_pct=ifelse(sum==0, 1, elt20_pct),
         across(e2099_pct:e500p_pct, ~ ifelse(sum==0, 0, .x)))
skim(fsize_prep2)
  

## prep the privates data ----
privprep1 <- privates2 |>
  filter(ind != "10") |> 
  mutate(naics2=str_sub(ind, 1, 2),
         naics=case_when(naics2 %in% as.character(31:33) ~ "31-33",
                         naics2 %in% as.character(44:45) ~ "44-45",
                         naics2 %in% as.character(48:49) ~ "48-49",
                         naics2 %in% naicsvals$naics ~ naics2,
                         TRUE ~ "99"))

tmp <- count(privprep1, naics, ind, indf) |> 
  left_join(naicsvals |> select(naics, naicsdesc), by = join_by(naics))

# now make it long so that we can have employees and wages and estabs
privpreplong <- privprep1 |> 
  select(year, fips, area, own, ownf, ind, indf, naics, emp, wages, estabs) |> 
  pivot_longer(cols=c(emp, wages, estabs), names_to = "variable")


## merge privates data and wages by size ----
priv_size1 <- privpreplong |> 
  # select(-area) |> 
  left_join(fsize_prep2 |> 
              select(fips, naics, variable, contains("_pct")),
            by = join_by(fips, naics, variable))

priv_size2 <- priv_size1 |> 
  mutate(across(elt20_pct:e500p_pct, ~ .x * value,
                .names="{.col}_val"))


priv_size3 <- priv_size2 |>
  pivot_longer(-c(year, fips, area, own, ownf, ind, indf, naics, variable),
               names_to = "size") |> 
  filter(str_detect(size, "pct_val")) |> 
  mutate(size=str_remove(size, "_pct_val"))



# put the files together ----
# check that we are on safe grounds as long as our industries add up to ind 10 for each county and owner code
tmp <- pmtbase2 |> 
  filter(ind=="10") |> 
  select(fips, area, own, wages) |> 
  pivot_wider(names_from = own, values_from = wages) |> 
  mutate(sum=rowSums(across(`1`:`5`)),
         diff=sum - `0`)
# good, diffs are zero
  
fedstate <- pmtbase2 |> 
  filter(ind=="10", own %in% 1:2) |> 
  mutate(rectype=case_when(own==1 ~ "federal",
                           own==2 ~ "state",
                           TRUE ~ "ERROR"),
         size="total",
         indf="all") |> 
  select(year, fips, area, ind, indf, rectype, size, emp, wages, estabs)

local <- localgovts3 |> 
  select(year, fips, area, vtype, localtot=local, localk12=k12) |> 
  mutate(rectype="local",
         size="total",
         indf="all",
         localnonk12=localtot - localk12) |> 
  select(-localtot) |> # just keep components, not total
  pivot_longer(names_to = "ind", starts_with("local")) |> 
  pivot_wider(names_from = vtype) |> 
  select(year, fips, area, ind, indf, rectype, size, emp, wages, estabs)

privates <- priv_size3 |> 
  select(year, fips, area, ind, indf, size, variable, value) |> 
  pivot_wider(names_from = variable) |> 
  mutate(rectype="private") |> 
  select(year, fips, area, ind, indf, rectype, size, emp, wages, estabs)

stack <- bind_rows(fedstate, local, privates)
# check stack
glimpse(stack)
skim(stack)
count(stack, rectype)

# verify that totals by type match the base data by owner
basecheck <- pmtbase2 |> 
  filter(ind=="10") |> 
  summarise(across(c(emp, wages, estabs), sum), .by=c(own, ownf))
basecheck

newcheck <- stack |> 
  summarise(across(c(emp, wages, estabs), sum), .by=c(rectype))
newcheck
#  good!

saveRDS(stack, here::here("sites", "baseline", "pmtbase", "pmt_qcew_size.rds"))


# use BEA earnings data to calculate self-employment tax base -------------
pmt_earnbase1 <- beadf |> # ends 2021
  filter(year==2021) |>  # to be compatible with qcew year
  select(fips, area, year, vname, value) |> 
  pivot_wider(names_from = vname) |> 
  rename(earnings_bea=earnings, wages_bea=wages, supplements_bea=supplements,
         propinc_bea=propinc, farminc_bea=farminc, nfpi_bea=nfpi) |> 
  mutate(nonwage_bea=earnings_bea - wages_bea)

glimpse(pmt_earnbase1)
count(pmt_earnbase1, year)
count(pmt_earnbase1, fips, area)

## collapse to NYC ----
pmt_earnbase2 <- pmt_earnbase1 |> 
  # note that order of the mutates matters
  mutate(area=ifelse(fips %in% constants$nycfips, "New York City", area),
         fips=ifelse(fips %in% constants$nycfips, "36xxx", fips)) |> 
  summarise(propinc_bea=sum(propinc_bea), .by=c(year, fips, area))


## slim down and adjust the bea earnings and save ----
pmt_earnbase3 <- pmt_earnbase2 |> 
  mutate(nonwage_beapmt=propinc_bea * propinc_taxable_ratio)

saveRDS(pmt_earnbase3, here::here("sites", "baseline", "pmtbase", "pmt_bea_nonwage.rds"))


# put wages and self employment economic data together and save ----
wages <- readRDS(here::here("sites", "baseline", "pmtbase", "pmt_qcew_size.rds"))
areanames <- wages |> 
  select(fips, area) |> 
  distinct() |> 
  mutate(area=str_remove(area, ", New York"))

nonwage <- readRDS(here::here("sites", "baseline", "pmtbase", "pmt_bea_nonwage.rds"))
sum(nonwage$nonwage_beapmt) / 1e3

wages
nonwage2 <- nonwage |> 
  select(year, fips, area, nonwage=nonwage_beapmt) |> 
  mutate(ind="10", indf="all", rectype="nonwage", size="total",
         nonwage=nonwage * 1000)

stack <- bind_rows(wages,
                   nonwage2) |> 
  select(-area) |> 
  left_join(areanames,
            by = join_by(fips)) |> 
  relocate(area, .after=fips)
  
count(stack, rectype, area)
saveRDS(stack, here::here("sites", "baseline", "pmtbase", "pmt_econbase.rds"))


# calibrate to revenue ----
econbase <- readRDS(here::here("sites", "baseline", "pmtbase", "pmt_econbase.rds"))
pmtrev1 <- readRDS(here::here("data", "dtf", "pmt_collections.rds")) # monthly dtf pmt revenue components

avgwages <- econbase |> 
  filter(rectype=="private") |> 
  mutate(avgwage=wages / emp)
skim(avgwages)
payroll <- avgwages |> 
  summarise(awage_mean=mean(avgwage, na.rm = TRUE),
            awage_p25=p25(avgwage, na.rm=TRUE),
            awage_mdn=median(avgwage, na.rm=TRUE),
            awage_p75=p75(avgwage, na.rm=TRUE),
            .by=size) |> 
  mutate(emin=case_when(size=="elt20" ~ 1,
                        size=="e2099" ~ 20,
                        size=="e100499" ~ 100,
                        size=="e500p" ~ 500),
         qprmin=awage_mean * emin / 4)

# |> 
#   mutate(share0011)
payroll

# get 2021 revenue by type
taxrev2021 <- pmtrev1 |> 
  filter(year(date)==2021) |> 
  summarise(across(c(totpmt, local_mtapmt, local_mtapmtpitnet), sum))

# calc tax on econ data
count(econbase, rectype, size)
count(econbase |> filter(rectype=="local"), ind)
ht(econbase)

# assumed payroll
# payroll quarterly thresholds 
# <= 312500 0
# > 312500 <= 375000 0.0011
# > 375000 <= 437500 0.0023
# > 437500 
payroll

taxecon1 <- econbase |> 
  mutate(etr=case_when(rectype=="federal" ~ 0,
                           rectype=="state" ~ 0.0034,
                           ind=="localk12" ~ 0,
                           ind=="localnonk12" ~ 0.0034,
                           size=="elt20" ~ 0,
                           size=="e2099" ~ 0.0011 * 0.75 + 0.0023 * .25, # assumes 75%
                           size=="e100499" ~ 0.0034,
                           size=="e500p" ~ 0.0034,
                           rectype=="nonwage" ~ 0.0034,
                           TRUE ~ NA_real_),
         wagetax=wages * etr,
         nonwagetax=nonwage * etr)
count(taxecon1, etr)

taxecon <- taxecon1 |> 
  summarise(across(c(wagetax, nonwagetax), ~ sum(.x, na.rm=TRUE) / 1e6)) |> 
  mutate(tottax=wagetax + nonwagetax)
taxecon

calib <- taxecon |> 
  bind_cols(taxrev2021 |> 
              select(wagerev=local_mtapmt,
                     nonwagerev=local_mtapmtpitnet,
                     totrev=totpmt) |> 
              mutate(across(everything(), ~ .x / 1000))) |> 
  mutate(wagecalib=wagerev / wagetax,
         nonwagecalib=nonwagerev / nonwagetax,
         totcalib=totrev / tottax) # multiply econ data by these factors to get a calibrated tax base
calib


# now calibrate the data
rateshares <- read_csv(
"rectype"  
)

f <- function(rectype, ind, size){
  if(rectype=="federal"){
    rate0011_baseshare <- 0
    rate0023_base <- 0
    rate0034_base <- 0
    fullrate_base <- 0
  }
  tibble(rectype, rate0011_base, rate0023_base, rate0034_base, fullrate_2021)
}

df <- tibble(rectype=c("federal", "state", "federal"),
             ind=c("10", "10", "10"),
             size=c("all", "all", "all"))

f(df$rectype, df$ind, df$size)

getfed <- function(rectype, 
                   baseshare0000, baseshare0011, baseshare0023, baseshare0034){
  tibble(rectype, baseshare0000, baseshare0011, baseshare0023, baseshare0034) |> 
    mutate(baseshare0000=5)
}

# getfed <- function(df){
#   df |> 
#     mutate(baseshare0034=ifelse(rectype %in% ))
# }


getshares <- function(df){
  if(df$rectype=="state"){
    df <- df |> mutate(baseshare0034=1)
  }
  df
}

calibrated <- econbase |> 
  mutate(wages_calib=wages * calib$wagecalib,
         nonwage_calib=nonwage * calib$nonwagecalib) |> 
  mutate(baseshare0000=0,
         baseshare0011=0,
         baseshare0023=0,
         baseshare0034=0) |> 
  rowwise() |> 
  mutate(getshares(pick(everything()))) |> 
  ungroup()
           
  mutate(ifelse(rectype=="federal", ))
  # mutate(getfed(rectype, 
  #               baseshare0000, baseshare0011, baseshare0023, baseshare0034))
  mutate(getfed(pick(everything())))
  mutate(fullrate_2021=case_when(
    rectype=="federal" ~ 0,
    rectype=="state" ~ 1,
    ind=="localk12" ~ 0,
    ind=="localnonk12" ~ 1,
    size=="elt20" ~ 0,
    size=="e2099" ~ 0.0011 * 0.3 + 0.0023 * .7,
                                 size=="e100499" ~ 0.0034,
                                 size=="e500p" ~ 0.0034,
                                 rectype=="nonwage" ~ 0.0034,
                                 TRUE ~ NA_real_)),
         etr_2021=case_when(rectype=="federal" ~ 0,
                    rectype=="state" ~ 0.0034,
                    ind=="localk12" ~ 0,
                    ind=="localnonk12" ~ 0.0034,
                    size=="elt20" ~ 0,
                    size=="e2099" ~ 0.0011 * 0.3 + 0.0023 * .7,
                    size=="e100499" ~ 0.0034,
                    size=="e500p" ~ 0.0034,
                    rectype=="nonwage" ~ 0.0034,
                    TRUE ~ NA_real_))

check <- calibrated |> 
  mutate(wagetax=wages_calib * etr_2021,
         nonwagetax=nonwage_calib * etr_2021) |> 
  summarise(wagetax=sum(wagetax, na.rm=TRUE),
            nonwagetax=sum(nonwagetax, na.rm=TRUE)) |> 
  mutate(tottax=wagetax + nonwagetax)
check
taxrev2021

saveRDS(calibrated, here::here("data", "pmtbase_calibrated_2023-03-23.rds"))



