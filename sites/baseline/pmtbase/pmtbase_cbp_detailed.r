

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


# split into smaller employment groups based on CBP and uniform assumptions ----


# get cbp data ------------------------------------------------------------
fn <- "CBP2020.CB2000CBP-Data.csv" # all NYS counties, plus NYS total
df1 <- read_csv(here::here("data", "cbp", fn), 
                col_types = cols(.default = col_character()))




## get variable names and labels ----
defs <- df1 |> 
  filter(row_number() ==1) |> 
  pivot_longer(cols=everything(),
               names_to = "vname",
               values_to = "vdesc") |> 
  mutate(vnum=row_number()) |> 
  relocate(vnum) |> 
  filter(!is.na(vdesc))
ht(defs)


## mark county and state records ----
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

# df2 |> filter(fips=="36027", naics=="00", empsize=="001") # dutchess check

## split counties and nys out, prepare to impute emp and wages to counties by existing emp size groups ----
# we have estabs, wages, emp for the state (and also lfo legal form org) by sizes but only estabs by size for counties
# so attribute state emp and wages shares to counties, controlling to ensure we hit the totals
count(df2, empsize, empsizef)




## nys - enhance with calcs that help with imputation ----
nys1 <- df2 |> filter(rectype=="state")
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

# prep the counties and collapse nyc ----
cnty1 <- df2 |> filter(rectype=="county")
skim(cnty1) # note only 1 unique value for lfo

cnty2 <- cnty1 |> 
  select(fips, name, naics, naicsdesc, empsize, empsizef, estabs, wages, emp) |> 
  mutate(area=ifelse(fips %in% constants$nycfips, "New York City", name),
         fips2=ifelse(fips %in% constants$nycfips, "36xxx", fips))
count(cnty2 |> filter(fips %in% constants$nycfips), fips2, fips, name, area)

# cnty2 |> filter(fips=="36027", naics=="00", empsize=="001") # dutchess check


## fix the one case where we find wages but no employment ----
cnty2 |> filter(emp==0 &  wages>0 & estabs>0) # fips 36013, naics 99
cnty2 |> filter(naics=="99" &  wages>0)

cnty3 <- cnty2 |> 
  mutate(emp=ifelse(emp==0 &  wages>0 & estabs>0,
                    estabs, # assume 1 worker per establishment in this oddball case
                    emp))
cnty3 |> filter(naics=="99" &  wages>0)

cnty <- cnty3 |> 
  mutate(fips=fips2,
         area=str_remove(area, ", New York")) |> 
  summarise(across(c(estabs, wages, emp),
                   ~ sum(.x, na.rm = TRUE)), 
            .by = c(fips, area, naics, naicsdesc, empsize, empsizef))
count(cnty, fips, area)

# cnty |> filter(fips=="36027", naics=="00", empsize=="001") # dutchess check

## get cartesian product of inds, empsizes, and counties, so that we have every industry and size in the state, for every county ----
# define bounds for establishments
count(nys2, empsize, empsizef)
# empsize empsizef                                        n
# 1 001     All establishments                             21
# 2 210     Establishments with less than 5 employees      21
# 3 220     Establishments with 5 to 9 employees           21
# 4 230     Establishments with 10 to 19 employees         21
# 5 241     Establishments with 20 to 49 employees         21
# 6 242     Establishments with 50 to 99 employees         19
# 7 251     Establishments with 100 to 249 employees       20
# 8 252     Establishments with 250 to 499 employees       18
# 9 254     Establishments with 500 to 999 employees       18
# 10 260     Establishments with 1,000 employees or more    15

empsize1 <- read_csv(
"empsize, lb, ub
001, 0, Inf
210, 0, 4
220, 5, 9
230, 10, 19
241, 20, 49
242, 50, 99
251, 100, 249
252, 250, 499
254, 500, 999
260, 1000, Inf")
empsize1

empsize <- nys2 |> 
  select(empsize, empsizef) |> 
  distinct() |> 
  left_join(empsize1, by = join_by(empsize))
empsize


indsize <- nys2 |> 
  select(naics, naicsdesc, empsize, empsizef) |> 
  distinct()

areas <- cnty |> 
  select(fips, area) |> 
  distinct()

base <- crossing(areas, indsize)

## create a complete county file with all counties (nyc as 1 county), all industries, all sizes ----
# it will have plenty of NAs
cnty_imputed1 <- base |> 
  left_join(cnty |> 
              select(fips, naics, empsize, estabs, wages, emp),
            by = join_by(fips, naics, empsize)) |> 
  left_join(nys2 |> select(naics, empsize, contains("_")),
            by = join_by(naics, empsize))

## ensure that county-industry details for # establishments add to total establishments for every group ----
# use the nys info to help fill in
cnty_imputed2 <- cnty_imputed1 |> 
  mutate(grouptot=ifelse(empsize=="001", TRUE, FALSE)) |> 
  mutate(# determine fillneed -- number of detailed establishments we must create
         fillneed=estabs[grouptot] - sum(estabs[!grouptot], na.rm=TRUE),
         
         # initial noninteger estimate of fillin amount by size group, based on nys share
         fillin=ifelse(!grouptot, fillneed * estabs_share, NA_real_),
         ifillin=as.integer(fillin),
         ifillin=ifelse(grouptot, sum(ifillin, na.rm=TRUE), ifillin), # determine total of integer fillin
         
         # figure "truein up" fillin -- we'll dump it into the first employment category
         trueup=fillneed[grouptot] - ifillin[grouptot],
         # final fillin
         ffillin=ifelse(row_number()==2, ifillin + trueup, ifillin), # first empsize category
         
         estabs2=ifelse(grouptot, estabs, naz(estabs) + ffillin),
         .by=c(fips, naics)) |> 
  relocate(fillneed, fillin, ifillin, trueup, ffillin, estabs2, .before=estabs)

# cnty_imputed2 |> filter(fips=="36027", naics=="00", empsize=="001") # dutchess check

# check - do we ever have a situation where sum of estabs2 not = estabs[grouptot]
check <- cnty_imputed2 |> 
  summarise(estabs_tot=estabs[grouptot],
            estabs_sum=sum(estabs[!grouptot], na.rm=TRUE),
            estabs2_tot=estabs2[grouptot],
            estabs2_sum=sum(estabs2[!grouptot], na.rm=TRUE),
            .by=c(fips, area, naics, naicsdesc))

check |> 
  filter(estabs2_tot != estabs_tot |
           estabs2_sum != estabs_tot)
# good, details ALWAYS sum to totals


## now that establishments are filled in, construct employment details that sum to total for each group ----
# base this initially on empestabs_nys which is average statewide employment per establishment in each emp size group
# then scale to hit the total for an area-industry and calculate average employment per establishment
cnty_imputed3 <- cnty_imputed2 |> 
  select(-c(fillneed, fillin, ifillin, ffillin, estabs, trueup)) |> 
  rename(estabs=estabs2) |> 
  # get establishment detail
  mutate(emp1=ifelse(!grouptot, estabs * empestabs_nys, NA_real_),
         emp1sum=sum(emp1, na.rm=TRUE),
         emp2=emp1 * emp[grouptot] / emp1sum[grouptot],
         emp2=ifelse(grouptot, sum(emp2[!grouptot]), emp2),
         avgemp2=emp2 / estabs,
         .by=c(fips, naics)) |>  
  relocate(emp1, emp1sum, emp2, avgemp2, .after=emp)

# check -- is total employment always equal to what we expected?
check <- cnty_imputed3 |> 
  filter(grouptot) |> 
  filter(emp != emp2) |> 
  select(1:6, estabs, wages, emp, emp2) |> 
  mutate(diff=emp - emp2) |> 
  arrange(desc(abs(diff)))
# good

# check - do we have employment that exceeds bounds for an empsize group?? ----
# sometimes the average employment calculated above will be outside bounds of
# the empsize group so we will want to reassign
check <- cnty_imputed3 |> 
  select(fips:emp, emp2, avgemp2) |> 
  left_join(empsize |> 
              select(empsize, lb, ub),
            by = join_by(empsize)) |> 
  mutate(avgemp2=ifelse(is.na(avgemp2),
                        0,
                        avgemp2),
         bad=avgemp2 > 0 & (avgemp2 < lb | avgemp2 > ub),
         bad2=sum(bad),
         .by=c(fips, naics)) |> 
  filter(bad2 > 0)
# yes, this occurs frequently!!

# reassign employment size groups ----
## CAUTION!! This moves establishments out of groups that are real in the data !!!!

f <- function(emp, estabs, empsize){
  avgemp <- emp / estabs
  empsize <- case_when(empsize=="001" ~ "001",
                       is.na(avgemp) ~ empsize,
                       avgemp < 5 ~ "210",
                       avgemp < 10 ~ "220",
                       avgemp < 20 ~ "230",
                       avgemp < 50 ~ "241",
                       avgemp < 100 ~ "242",
                       avgemp < 250 ~ "251",
                       avgemp < 500 ~ "252",
                       avgemp < 1000 ~ "254",
                       TRUE ~ "260")
  return(empsize)
}

check <- cnty_imputed3 |> 
  select(fips:emp, emp2, avgemp2) |> 
  mutate(empsize2=f(emp2, estabs, empsize)) |> 
  left_join(empsize |> 
              select(empsize, lb, ub),
            by = join_by(empsize)) |> 
  mutate(diff=empsize2!=empsize) |> 
  relocate(empsize, empsizef, .after=ub)

cnty_imputed4 <- cnty_imputed3 |> 
  select(-c(emp, emp1, emp1sum)) |> 
  rename(empsize0=empsize,
         empsizef0=empsizef,
         emp=emp2) |> 
  mutate(empsize=f(emp, estabs, empsize0)) |> # reassign the employment group
  left_join(empsize, by = join_by(empsize))

check <- cnty_imputed4 |> 
  mutate(changed=sum(empsize != empsize0), .by=c(fips, naics))  |> 
  mutate(avgemp=emp / estabs) |> 
  filter(changed>0) |> 
  select(fips, area, naics, naicsdesc, estabs, emp, empsize, empsizef, empsize0, empsizef0, avgemp, changed)

# cnty_imputed4 |> filter(fips=="36027", naics=="00", empsize=="001") # dutchess check


## collapse by new establishment size groups ----
cnty_imputed5 <- cnty_imputed4 |> 
  filter(emp > 0) |> 
  summarise(across(c(estabs, emp, wages),
                   ~ sum(.x, na.rm=TRUE)),
                   .by=c(fips, area, naics, naicsdesc, empsize, empsizef, grouptot)) |> 
  mutate(avgemp=emp / estabs)

## finally, calculate average wages that hit total wages and follow the nys pattern
cnty_imputed6 <- cnty_imputed5 |> 
  mutate(avgwage=wages * 1000 / emp) |> 
  left_join(nys2 |> 
              select(naics, empsize, avgwageratio_nys),
            by = join_by(naics, empsize)) |> 
  mutate(avgwage2=ifelse(!grouptot, 
                         avgwage[grouptot] * avgwageratio_nys, # tentatively give each size group the statewide relationship to overall avg wage
                         NA_real_),
         wages2=avgwage2 * emp / 1000,
         wages2=ifelse(grouptot, sum(wages2, na.rm=TRUE), wages2),
         .by=c(fips, naics)) |> 
  relocate(wages2, .after = wages)

check <- cnty_imputed6 |> 
  filter(grouptot) |> 
  mutate(ratio=wages2 / wages,
         avgwage2=wages2 / emp * 1000) |> 
  arrange(desc(abs(ratio - 1)))

# adjust average wages
cnty_imputed7 <- cnty_imputed6 |> 
  mutate(avgwage3=ifelse(!grouptot, avgwage2 * wages[grouptot] / wages2[grouptot], NA_real_),
         wages3=avgwage3 * emp / 1000,
         wages3=ifelse(grouptot, sum(wages3, na.rm=TRUE), wages3),
         .by=c(fips, naics)) |> 
  relocate(wages2, wages3, .after=wages)

cnty_imputed7 |> filter(fips=="36027", naics=="00", empsize=="001") # dutchess check

# wrap it up and final checks ----
cnty_imputed <- cnty_imputed7 |> 
  select(fips, area, naics, naicsdesc, empsize, empsizef, grouptot, estabs, emp, wages=wages3, avgemp) |> 
  mutate(avgwage=wages * 1000 / emp,
         avgpayroll=wages * 1000 / estabs) |> 
  left_join(empsize |> select(empsize, emplb=lb, empub=ub),
            by = join_by(empsize))


cnty_imputed |> filter(fips=="36027", naics=="00", empsize=="001") # dutchess check

# verify that esablishment, emp, wages sums = totals
check <- cnty_imputed |> 
  summarize(across(c(estabs, emp, wages),
                   ~ sum(.x, na.rm=TRUE)),
            .by=c(fips, area, naics, naicsdesc, grouptot))
count(check, fips, area, naics, naicsdesc) |> filter(n!=2) # good, we have the right counts

check |> 
  mutate(across(c(estabs, emp, wages),
                ~ abs(.x[grouptot] / .x[!grouptot] - 1)),
                .by=c(fips, naics)) |> 
  filter(estabs >0 | emp > 0 | wages>0)
  
saveRDS(cnty_imputed, here::here("data", "cbp", "cbp_filledin.rds"))



# now make up subranges with interpolation --------------------------------
# payroll breaks now -- if below then ;
# 312,500 x 4 = $1.250m ; exempt
# 375,000 x 4 = $1.500m ; 0.11%
# 437,500 x 4 = $1.750m ; 0.23%
# Inf         = Inf     ; 0.34%
# desired breaks: $3.5m, $5m

cbp1 <- readRDS(here::here("data", "cbp", "cbp_filledin.rds"))
glimpse(cbp1)
skim(cbp1)
count(cbp1, empsize, empsizef)

keepfips <- c(constants$mtafips, "36xxx")

cbp1 |> filter(fips %in% keepfips, naics=="00") |> ht()

cbp1 |> filter(fips %in% keepfips, naics=="00", empsize=="241") # 2049
cbp1 |> filter(fips %in% keepfips, naics=="00", empsize=="242") # 5099
cbp1 |> filter(fips %in% keepfips, naics=="00", empsize=="251") # 100249

cbp1 |> filter(fips %in% constants$mtafips, naics=="00", fips=="36059") # 2049

# how does avgpayroll breakdown now??
egroups <- cbp1 |> 
  select(empsize, empsizef, emplb, empub) |> 
  distinct() |> 
  # mutate(clb=str_pad(emplb, side="left", width=4, pad="0"))
  mutate(egroup=paste0("e", emplb, "_", empub))
egroups

cbp2 <- cbp1 |> 
  filter(fips %in% keepfips) |> 
  left_join(egroups |> select(empsize, egroup), by=join_by(empsize)) 
count(cbp2, naics, naicsdesc)
  
cbp2 |> 
  summarise(p10=pany(avgpayroll, .1),
            p25=p25(avgpayroll),
            median=median(avgpayroll),
            p75=p75(avgpayroll),
            p90=pany(avgpayroll, .9),
            .by=c(empsize, egroup))

cbp2 |> 
  filter(naics=="00") |> 
  summarise(p10=pany(avgpayroll, .1),
            p25=p25(avgpayroll),
            median=median(avgpayroll),
            p75=p75(avgpayroll),
            p90=pany(avgpayroll, .9),
            .by=c(empsize, egroup))

# desired breaks: $3.5m, $5m
# to get these we'll need to break down:
#   e50_99, e100_249 -- let's breakdown by 10
newlbs <- seq(50, 240, 10)
newubs <- newlbs + 9
newegroups <- paste0("e", newlbs, "_", newubs)
buckets <- tibble(newemplb=newlbs,
                newempub=newubs,
                newegroup=newegroups) |> 
  mutate(egroup=case_when(newemplb >= 50 & newempub <= 99 ~ "e50_99",
                          newemplb >= 100 & newempub <= 249 ~ "e100_249",
                          TRUE ~ "ERROR")) |> 
  select(egroup, newegroup, newemplb, newempub)
           
buckets

# force estabs to be correct
cbpsub1 <- cbp2 |> 
  filter(egroup %in% unique(buckets$egroup)) |> 
  right_join(buckets,
            join_by(egroup),
            relationship = "many-to-many") |> 
  mutate(avgemp2=(newempub + newemplb) / 2)


fopt <- function(estabs, emp, avgemp){
  # solve for establishments that keep avgemp fixed while hitting employment and establishment targets
  
  # estabs is the # of estabs for the big group
  # emp is the # of employees in the big group
  # avgemp is the midpoint of group employment
  
  esttot <- estabs[1] # our target for total establishments
  emptot <- emp[1] # target for total employment
  est0 <- rep(esttot / length(avgemp), length(avgemp)) # initial guess at establishments
  
  obj <- function(estabs){
    # objective function
    error <- (sum(estabs) - esttot)^2 
    return(error)
  }
  
  gineq <- function(estabs){
    # inequality constraints
    c(sum(estabs * avgemp) - emptot,
      emptot - sum(estabs * avgemp))
  }
  
  res <- nloptr(x0=est0,
                eval_f=obj,
                eval_g_ineq=gineq,
                lb=rep(0, length(avgemp)),
                ub=rep(esttot, length(avgemp)),
                opts = list("algorithm"="NLOPT_LN_COBYLA",
                            maxeval=100,
                            "xtol_rel"=1.0e-8))
  
  if(res$objective > 1) print(res$objective)
  
  return(res$solution)
}
  
# x <- fopt(tmp$estabs, tmp$emp, tmp$avgemp1)
# x
# str(x)

cbpsub2 <- cbpsub1 |> 
  mutate(estabs2=fopt(estabs, emp, avgemp2), # find target-hitting number of establishments
         .by=c(fips, naics, egroup))

check <- cbpsub2 |> 
  summarise(emp=first(emp),
            estabs=first(estabs),
            empsum=sum(avgemp2 * estabs2),
            estsum=sum(estabs2),
            .by=c(fips, area, naics, naicsdesc, egroup)) |> 
  mutate(adiff1=abs(empsum / emp -1),
         adiff2=abs(estsum / estabs -1))
# good, close enough

# now that we have establishments and employment, we need to adjust average wages to hit total wages
cbpsub3 <- cbpsub2 |> 
  select(fips, area, naics, naicsdesc, grouptot,
         empsize,
         egroup1=egroup, egroup2=newegroup, emplb1=emplb, empub1=empub,
         emplb2=newemplb, empub2=newempub,
         estabs1=estabs, estabs2, 
         avgemp1=avgemp, avgemp2,
         emp1=emp,
         wages1=wages,
         avgwage1=avgwage,
         avgpayroll1=avgpayroll) |> 
  mutate(emp2=estabs2*avgemp2)

cbpsub4 <- cbpsub3 |> 
  mutate(wages1sum=sum(avgwage1*emp2) / 1000,
         avgwage2=avgwage1,
         wages2=emp2 * avgwage2 / 1000,
         avgpayroll2=ifelse(estabs2!=0, # 2 small instances
                            wages2 *1000 / estabs2,
                            0),
         .by=c(fips, naics, egroup1)) |> 
  relocate(wages1sum, .after=wages1)


# what does avgpayroll look like
check <- cbpsub4 |> 
  summarise(p10=pany(avgpayroll2, .1),
            p25=p25(avgpayroll2),
            median=median(avgpayroll2),
            p75=p75(avgpayroll2),
            p90=pany(avgpayroll2, .9),
            .by=c(egroup2))

check <- cbpsub4 |> 
  filter(naics=="00") |> 
  summarise(p10=pany(avgpayroll2, .1),
            p25=p25(avgpayroll2),
            median=median(avgpayroll2),
            p75=p75(avgpayroll2),
            p90=pany(avgpayroll2, .9),
            .by=c(egroup2))

check <- cbpsub4 |> 
  filter(naics=="00")

# xlsx::write.xlsx(check, file = here::here("check.xlsx"))


# save the breakdown group ----
saveRDS(cbpsub4 |> select(-wages1sum), here::here("data", "cbp", "cbp_detailed.rds"))


# FINAL combination of detailed group and other groups for naics 00 ----

# first, the original cleaned up data
cbpfin1 <- readRDS(here::here("data", "cbp", "cbp_filledin.rds"))
glimpse(cbpfin1)
names(cbpfin1)
# fips, area, naics, naicsdesc, empsize, empsizef, grouptot, estabs, emp, wages, avgemp, avgwage, avgpayroll, emplb, empub, 

egroups
cbpfin1a <- cbpfin1 |>
  filter(fips %in% c(constants$mtafips, "36xxx"), naics=="00") |> 
  rename(emplb1=emplb, empub1=empub) |> 
  left_join(egroups |> select(empsize, egroup1=egroup),
            by = join_by(empsize)) |> 
  filter(!egroup1 %in% c("e50_99", "e100_249")) |> 
  mutate(egroup2=egroup1, emplb2=emplb1, empub2=empub1)
count(cbpfin1a, empsize, egroup1, emplb1, empub1)

cbpfin1b <- cbpfin1a |>
  select(-c(empsize, empsizef))


## now the adjusted data ----
cbpfin2 <- readRDS( here::here("data", "cbp", "cbp_detailed.rds"))
glimpse(cbpfin2)
count(cbpfin2, egroup1)
count(cbpfin2, fips, area)
ns(cbpfin2)

cbpfin2a <- cbpfin2 |> 
  filter(naics=="00") |> 
  select(fips, area, naics, naicsdesc, grouptot, egroup1, egroup2, emplb1, empub1, emplb2, empub2,
         estabs=estabs2, emp=emp2, wages=wages2, avgemp=avgemp2, avgwage=avgwage2,
         avgpayroll=avgpayroll2)

symdiff(names(cbpfin1b), names(cbpfin2a))
stack <- bind_rows(cbpfin1b, 
                   cbpfin2a)
names(stack)
count(stack, naics, naicsdesc)
# count(stack, emplb1, empub1, )
skim(stack)

saveRDS(stack, here::here("data", "cbp", "cbp_synthetic.rds"))


stack <- readRDS(here::here("data", "cbp", "cbp_synthetic.rds"))

stack |> 
  filter(grouptot)

stack |> 
  filter(fips=="36027") |> 
  summarize(across(c(estabs, emp, wages), ~ sum(.x, na.rm = TRUE)),
            .by=c(fips, area, naics, grouptot))



# Dutchess check ----
# correct naics 00 totals from web are
# estabs wages emp  
#  7,412	5,006,793	98,011
  
# from their raw data I have same thing
  
# from my raw I have:
# 7412  4919976.2 98011.000
#  + 4919976.2 / 5006793
# 98.3%  what did I do?
  
fn <- "CBP2020.CB2000CBP-Data.csv" # all NYS counties, plus NYS total
df1 <- read_csv(here::here("data", "cbp", fn), 
                  col_types = cols(.default = col_character()))
dutchzip <- df1 |> 
    filter(str_detect(NAME, "Dutchess"),
           NAICS2017=="00") 
  
  
raw <- cbpfin1 |> 
    filter(fips=="36027", naics=="00")
  
  










# OLD put it all together ----
# swap out old details for new more-detailed data
## get unadjusted data, keep mta ----
cbpfin1 <- readRDS(here::here("data", "cbp", "cbp_filledin.rds"))
glimpse(cbpfin1)
names(cbpfin1)
# fips, area, naics, naicsdesc, empsize, empsizef, grouptot, estabs, emp, wages, avgemp, avgwage, avgpayroll, emplb, empub, 

# bring in egroup
egroups
cbpfin1a <- cbpfin1 |>
  filter(fips %in% c(constants$mtafips, "36xxx")) |> 
  left_join(egroups |> select(empsize, egroup),
            by = join_by(empsize))
count(cbpfin1a, empsize, egroup, emplb, empub)

cbpfin1b <- cbpfin1a |>
  filter(!egroup %in% c("e50_99", "e100_249")) |> 
  select(fips, area, naics, naicsdesc, egroup, emplb, empub, estabs, emp, wages, avgemp, avgwage, avgpayroll)


## now the adjusted data ----
cbpfin2 <- readRDS( here::here("data", "cbp", "cbp_detailed.rds"))
glimpse(cbpfin2)
count(cbpfin2, egroup1)
count(cbpfin2, fips, area)
ns(cbpfin2)

cbpfin2a <- cbpfin2 |> 
  select(fips, area, naics, naicsdesc, egroup1, egroup2, emplb=emplb2, empub=empub2,
         estabs=estabs2, emp=emp2, wages=wages2, avgemp=avgemp2, avgwage=avgwage2,
         avgpayroll=avgpayroll2)

symdiff(names(cbpfin1b), names(cbpfin2a))
stack <- bind_rows(cbpfin1b |> 
                     mutate(egroup1=egroup, egroup2=egroup) |> 
                     select(-egroup), 
                   cbpfin2a)

# original for the two groups:
(mtawages_raw <- sum(cbpfin1a$wages, na.rm=TRUE)) # 411,314,339
sum(cbpfin1b$wages, na.rm=TRUE) # 351,123,851  just the 2 emp groups
# adjusted for the two groups
sum(cbpfin2a$wages, na.rm=TRUE) # 222,335,222


count(cbpfin1b, egroup)

compvals <- cbpfin1b <- cbpfin1a |>
  filter(egroup %in% c("e50_99", "e100_249")) |> 
  select(fips, area, naics, naicsdesc, egroup1=egroup, emplb, empub, estabs, emp, wages, avgemp, avgwage, avgpayroll)

comp <- bind_rows(compvals  |> mutate(src="original"),
                  cbpfin2a |> mutate(src="adjusted"))
count(comp, egroup1)
cbpfin2a

check <- comp |> 
  filter(naics=="00", egroup1=="e100_249", fips=="36027")

check <- comp |> 
  filter(naics=="00")

check |> 
  summarise(across(c(estabs, emp, wages), 
                   ~ sum(.x, na.rm=TRUE)),
            .by=c(fips, area, egroup1, src)) |> 
  arrange(fips, egroup1, src)

check <- comp |> 
  summarise(across(c(estabs, emp, wages), 
                   ~ sum(.x, na.rm=TRUE)),
            .by=c(fips, area, naics, naicsdesc, egroup1, src)) |> 
  arrange(fips, egroup1, naics, src) |> 
  pivot_longer(cols=c(estabs, emp, wages)) |> 
  pivot_wider(names_from = src) |> 
  mutate(apdiff=abs(adjusted / original -1)) |> 
  arrange(desc(apdiff))
# great, all are close enough

# now check that industries come close to totals by county
comp |> 
  filter(src=="original") |> 
  mutate(group=ifelse(naics=="00", "total", "industry")) |> 
  summarise(wages=sum(wages, na.rm=TRUE),
            .by=c(fips, area, group)) |> 
  pivot_wider(names_from = group, values_from = wages) |> 
  mutate(ratio=industry / total)
# how could they be so different - let's look at Dutchess

check <- comp |> 
  filter(src=="original", egroup1=="e50_99", fips=="36027")

xlsx::write.xlsx(check, file = here::here("check.xlsx"))

# based on my checking, I do not yet trust the industry data but the naics 00 data look good

