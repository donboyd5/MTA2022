

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


# quick data check -- emp, estabs, wages in mta region --------------------
glimpse(df2)

check1 <- df2 |> 
  filter(lfo=="001", naics=="00", fips %in% c("36", constants$mtafips)) |> 
  select(rectype, fips, name, empsize, empsizef, estabs, wages, emp)

check2 <- check1 |> 
  mutate(egroup2=case_when(empsize=="001" ~ "1: Total",
                           empsize %in% c("210", "220", "230") ~ "2: < 20",
                           empsize %in% c("241", "242") ~ "3: 20-99",
                           empsize %in% c("251", "252") ~ "4: 100-499",
                           empsize %in% c("254", "260", "262", "263", "271", "273") ~ "5: 500+",
                                    TRUE ~ "error")) |> 
  summarise(across(c(estabs, emp, wages), sum),
            .by=c(rectype, egroup2)) |> 
  mutate(avgpayroll=wages * 1000 /estabs,
         avgpay=wages * 1000 / emp,
         avgempest=emp / estabs) |> 
  arrange(rectype, egroup2)
  
check2



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
  relocate(fillneed, fillin, ifillin, trueup, ffillin, estabs2, .before=estabs) |> 
  mutate(wages=naz(wages), emp=naz(emp)) # to be safe

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


## now that establishments are filled in, simultaneously select average emp/estab and avgwage to hit emp total and wage total while staying in bounds ----
base <- cnty_imputed2 |> 
  filter(naics=="00", estabs > 0) |>  # for now all industries later come back to this
  filter(fips %in% c(constants$mtafips, "36xxx")) |> 
  select(-c(fillneed, fillin, ifillin, ffillin, estabs, trueup)) |> 
  rename(estabs=estabs2) |> 
  left_join(empsize |> select(empsize, lb, ub), by = join_by(empsize))


# opt separate functions -----------------------------------------------------------


obj <- function(ratios){
  sum((ratios - 1)^2)
}
# obj(c(avgwage0, empest0))
# obj(c(.8, 1.3))


getew <- function(ratios){
  varlen=length(ratios)/2
  elen=(varlen + 1):length(ratios)
  wlen=1:varlen
  ratio_empest <- ratios[elen]
  ratio_avgwage <- ratios[wlen]
  
  empest <- empest0 * ratio_empest
  empvec <- empest * estabs
  
  avgwage <- avgwage0 * ratio_avgwage
  wagevec <- empvec * avgwage / 1000
  return(list(empvec=empvec, wagevec=wagevec))
}

g <- function(ratios){
  ew <- getew(ratios)
  
  emp <- sum(ew$empvec)
  wages <- sum(ew$wagevec)
  
  empestabs <- ew$empvec / estabs
  
  
  inequal <- c(emp - emptarget,
               emptarget - emp,
               wages - wagetarget,
               wagetarget - wages,
               eelb - empestabs,
               empestabs - eeub
  )
  return(inequal)
}

g(ratios0)


# create a test problem ----
test <- base |> 
  filter(fips=="36119", naics=="00", estabs > 0) |> 
  mutate(avgwage0=wages[grouptot] * 1000 / emp[grouptot] * avgwageratio_nys,
         empest0=emp[grouptot] / estabs[grouptot] * empestabs_nys / empestabs_nys[grouptot]) |> 
  select(fips, area, naics, empsize, empsizef, lb, ub, estabs, wages, emp, avgwage0, empest0, empestabs_nys, grouptot)
test

test |> 
  group_by(grouptot) |> 
  summarise(emp=sum(empest0* estabs),
            wages=sum(empest0 * estabs * avgwage0 / 1000))
test

(wagetarget <- test |> filter(row_number()==1) |> pull(wages))
(emptarget <- test |> filter(row_number()==1) |> pull(emp))
(estabs <- test |> filter(row_number()>1) |> pull(estabs))
(avgwage0 <- test |> filter(row_number()>1) |> pull(avgwage0))
(empest0 <- test |> filter(row_number()>1) |> pull(empest0))
(eelb <- test |> filter(row_number()>1) |> pull(lb))
(eeub <- test |> filter(row_number()>1) |> pull(ub))
ratios0 <- rep(1, length(c(empest0, avgwage0)))
g(ratios0)

cbind(eelb,
      empest0,
      eeub) 

# djb <- getew(ratios0)
# sum(djb$empvec)
# sum(djb$wagevec)

# make our unknowns c(avgemp, avgwages)
# make 
res <- nloptr(x0=ratios0,
              eval_f=obj,
              eval_g_ineq=g,
              lb=rep(0, length(ratios0)),
              ub=rep(100, length(ratios0)),
              opts = list("algorithm"="NLOPT_LN_COBYLA",
                          maxeval=500,
                          "xtol_rel"=1.0e-8))
str(res)
res$solution
(ew <- getew(res$solution))

sum(ew$empvec); emptarget
sum(ew$wagevec); wagetarget
cbind(eelb,
      ew$empvec / estabs,
      eeub)



# opt combined function -----------------------------------------------------------

fopt <- function(groupdata){
   
  # get employment and wages by employment size group
  # group data must have:
  #    group wages, emp, and estabs total in the first record
  #    initial average wages that we want to stay close to
  #    initial employment per establishment that we want to stay close to
  #    lower and upper bounds for employment per establishment in each emp size group
  groupdata <- groupdata |> 
    mutate(avgwage0=wages[grouptot] * 1000 / emp[grouptot] * avgwageratio_nys,
           empest0=emp[grouptot] / estabs[grouptot] * empestabs_nys / empestabs_nys[grouptot])
  
  
  # extract variables we need to have in the calling environment
  wagetarget <- groupdata |> filter(row_number()==1) |> pull(wages)
  emptarget <- groupdata |> filter(row_number()==1) |> pull(emp)
  estabs <- groupdata |> filter(row_number()>1) |> pull(estabs)
  avgwage0 <- groupdata |> filter(row_number()>1) |> pull(avgwage0)
  empest0 <- groupdata |> filter(row_number()>1) |> pull(empest0)
  eelb <- groupdata |> filter(row_number()>1) |> pull(lb)
  eeub <- groupdata |> filter(row_number()>1) |> pull(ub)
  ratios0 <- rep(1, length(c(empest0, avgwage0)))
  
  
  getew <- function(ratios){
    # function that receives vector with ratios of avgwage and emp per estab (empest)
    # and returns adjusted avgwage and empest vectors in a list
    varlen=length(ratios)/2
    elen=(varlen + 1):length(ratios)
    wlen=1:varlen
    ratio_empest <- ratios[elen]
    ratio_avgwage <- ratios[wlen]
    
    empest <- empest0 * ratio_empest
    empvec <- empest * estabs
    
    avgwage <- avgwage0 * ratio_avgwage
    wagevec <- empvec * avgwage / 1000
    return(list(empvec=empvec, wagevec=wagevec))
  }
  
  obj <- function(ratios){
    # objective function of the ratios to minimize
    sum((ratios - 1)^2)
  }
  
  gineq <- function(ratios){
    # inequality constraints -- each difference should be >= 0
    ew <- getew(ratios)
    
    emp <- sum(ew$empvec)
    wages <- sum(ew$wagevec)
    
    empestabs <- ew$empvec / estabs
    
    
    inequal <- c(emp - emptarget,
                 emptarget - emp,
                 wages - wagetarget,
                 wagetarget - wages,
                 eelb - empestabs,
                 empestabs - eeub
    )
    return(inequal)
  }
  
  res <- nloptr(x0=ratios0,
                eval_f=obj,
                eval_g_ineq=gineq,
                lb=rep(0, length(ratios0)),
                ub=rep(100, length(ratios0)),
                opts = list("algorithm"="NLOPT_LN_COBYLA",
                            maxeval=500,
                            "xtol_rel"=1.0e-8))

  ew <- getew(res$solution)
  # convert the vectors to a dataframe 
  
  groupdata$wages[2:nrow(groupdata)] <- ew$wagevec
  groupdata$emp[2:nrow(groupdata)] <- ew$empvec
  return(groupdata)
}




# optimization round 1 ----

base2 <- base |> 
  group_by(fips, naics) |> 
  group_modify(~ fopt(.x)) |> 
  ungroup() |> 
  mutate(avgwage=wages * 1000 / emp,
         empest=emp / estabs,
         avgpayroll=wages * 1000 / estabs)

# a few checks on the results

base2 |> 
  summarise(across(c(estabs, emp, wages), 
                   sum),
            .by=c(fips, naics, grouptot))

base2 |> 
  filter(!grouptot) |> 
  mutate(empestabs=emp / estabs) |> 
  select(fips, area, naics, lb, empestabs, ub) |> 
  mutate(bad=empestabs < lb | empestabs > ub)

base2 |> 
  mutate(avgwage=wages * 1000 / emp,
         wratio=avgwage / avgwage0, 
         empest=emp / estabs,
         eratio=empest / empest0) |> 
  select(fips, area, naics, empsize, empsizef, avgwage, avgwage0, wratio, empest, empest0, eratio)


saveRDS(base2, here::here("data", "cbp", "cbp_filledin.rds"))



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

cbp1 |> filter(fips %in% keepfips, naics=="00", empsize=="241") # 2049
cbp1 |> filter(fips %in% keepfips, naics=="00", empsize=="242") # 5099
cbp1 |> filter(fips %in% keepfips, naics=="00", empsize=="251") # 100249

cbp1 |> filter(fips %in% constants$mtafips, naics=="00", fips=="36059") # 2049

# how does avgpayroll breakdown now??
egroups <- cbp1 |> 
  select(empsize, empsizef, lb, ub) |> 
  distinct() |> 
  # mutate(clb=str_pad(emplb, side="left", width=4, pad="0"))
  mutate(egroup=paste0("e", lb, "_", ub))
egroups

cbp2 <- cbp1 |>
  select(-contains("_nys"), -contains("_share")) |> 
  left_join(egroups |> select(empsize, egroup), by=join_by(empsize))

cbp2 |> 
  summarise(p10=pany(avgpayroll, .1),
            p25=p25(avgpayroll),
            median=median(avgpayroll),
            p75=p75(avgpayroll),
            p90=pany(avgpayroll, .9),
            .by=c(empsize, egroup))

# desired breaks: $3.5m, $5m
# to get these we'll need to break down:
#   e20_49, e50_99, e100_249 -- let's breakdown by 10
sublbs <- seq(20, 240, 10)
sububs <- sublbs + 9
subegroups <- paste0("e", sublbs, "_", sububs)
buckets <- tibble(sublb=sublbs,
                  subub=sububs,
                  subegroup=subegroups) |> 
  mutate(egroup=case_when(sublb >= 20 & subub <= 49 ~ "e20_49",
                          sublb >= 50 & subub <= 99 ~ "e50_99",
                          sublb >= 100 & subub <= 249 ~ "e100_249",
                          TRUE ~ "ERROR")) |> 
  select(egroup, subegroup, sublb, subub)
           
buckets

# create a subset for just the egroups we're going to split and 
cbpsub1 <- cbp2 |> 
  filter(!grouptot) |>  # to be safe
  filter(egroup %in% unique(buckets$egroup)) |> 
  right_join(buckets,
            join_by(egroup),
            relationship = "many-to-many") |> 
  # create initial values for a new round of optimization, for estabs, empest, and average wages
  mutate(iestabs=(subub + 1 - sublb) / (ub + 1 - lb) * estabs, # the groups are integer but we need to go to the next up
         iempest=(empest - lb) / (ub + 1 - lb) * (subub + 1 - sublb) + sublb,
         iavgwage=avgwage) # we know avgwage should rise with estab size but no easy way to estimate


# optimization round 2 ----------------------------------------------------


foptr2 <- function(groupdata){
  
  # get employment and wages by employment size group
  # group data must have:
  #    group wages, emp, and estabs total in the first record
  #    initial average wages that we want to stay close to
  #    initial employment per establishment that we want to stay close to
  #    lower and upper bounds for employment per establishment in each emp size group
  # groupdata <- groupdata |> 
  #   mutate(avgwage0=wages[grouptot] * 1000 / emp[grouptot] * avgwageratio_nys,
  #          empest0=emp[grouptot] / estabs[grouptot] * empestabs_nys / empestabs_nys[grouptot])
  
  
  # extract variables we need to have in the calling environment
  wagetarget <- groupdata |> filter(row_number()==1) |> pull(wages)
  emptarget <- groupdata |> filter(row_number()==1) |> pull(emp)
  estabstarget <- groupdata |> filter(row_number()==1) |> pull(estabs)
  
  empest0 <- groupdata |> pull(iempest)
  avgwage0 <- groupdata |> pull(iavgwage)
  estabs0 <- groupdata |> pull(iestabs)
  
  eelb <- groupdata |> pull(sublb)
  eeub <- groupdata |> pull(subub)
  ratios0 <- rep(1, length(c(avgwage0, empest0, estabs0)))
  
  getew <- function(ratios){
    # function that receives vector with ratios of avgwage and emp per estab (empest)
    # and returns adjusted avgwage, empest, and estabs vectors in a list
    varlen=length(ratios) / 3
    wlen=1:varlen
    elen=(varlen + 1):(2 * varlen)
    estlen <- (2 * varlen + 1): length(ratios)
    
    ratio_avgwage <- ratios[wlen]
    ratio_empest <- ratios[elen]
    ratio_estabs <- ratios[estlen]
    
    estabsvec <- estabs0 * ratio_estabs
    
    empest <- empest0 * ratio_empest
    empvec <- empest * estabsvec
    
    avgwage <- avgwage0 * ratio_avgwage
    wagevec <- empvec * avgwage / 1000
    return(list(empvec=empvec, wagevec=wagevec, estabsvec=estabsvec))
  }
  
  obj <- function(ratios){
    # objective function of the ratios to minimize
    sum((ratios - 1)^2)
  }
    
  gineq <- function(ratios){
    # inequality constraints -- each difference should be >= 0
    ew <- getew(ratios)
    
    emp_sum <- sum(ew$empvec)
    wages_sum <- sum(ew$wagevec)
    estabs_sum <- sum(ew$estabsvec)
    
    empestabs <- ew$empvec / ew$estabsvec
    
    inequal <- c(emp_sum - emptarget,
                 emptarget - emp_sum,
                 wages_sum - wagetarget,
                 wagetarget - wages_sum,
                 estabstarget - estabs_sum,
                 estabs_sum - estabstarget,
                 
                 eelb - empestabs,
                 empestabs - eeub
    )
    return(inequal)
  }

  res <- nloptr(x0=ratios0,
                eval_f=obj,
                eval_g_ineq=gineq,
                lb=rep(0, length(ratios0)),
                ub=rep(100, length(ratios0)),
                opts = list("algorithm"="NLOPT_LN_COBYLA",
                            maxeval=1000,
                            "xtol_rel"=1.0e-8))
  
  ew <- getew(res$solution)
  # convert the vectors to a dataframe 
  
  gd2 <- groupdata |> 
    mutate(wages_opt=ew$wagevec,
           emp_opt=ew$empvec,
           estabs_opt=ew$estabsvec)
  return(gd2)
}



cbpres <- cbpsub1 |> 
  group_by(fips, naics, egroup) |> 
  group_modify(~ foptr2(.x)) |> 
  ungroup()
# |> 
#   mutate(avgwage=wages * 1000 / emp,
#          empest=emp / estabs,
#          avgpayroll=wages * 1000 / estabs)

# check values
cbpres |> 
  group_by(fips, area, naics, egroup) |> 
  summarise(wages=first(wages),
            wages_opt=sum(wages_opt),
            emp=first(emp),
            emp_opt=sum(emp_opt),
            estabs=first(estabs),
            estabs_opt=sum(estabs_opt))

# clean the data up and save ----

cbpres2 <- cbpres |> 
  select(fips, area, naics, egroup, lb, ub, subegroup, sublb, subub, estabs=estabs_opt, emp=emp_opt, wages=wages_opt)
saveRDS(cbpres2, here::here("data", "cbp", "cbp_detailed.rds"))


# FINAL combination of detailed group and other groups for naics 00 ----

newgroup_data <- readRDS(here::here("data", "cbp", "cbp_detailed.rds"))
newgroups <- newgroup_data |> pull(egroup) |> unique()


oldgroup_data1 <- readRDS(here::here("data", "cbp", "cbp_filledin.rds"))
oldgroup_data2 <- oldgroup_data1 |> 
  mutate(egroup=paste0("e", lb, "_", ub)) |> 
  filter(!egroup %in% newgroups, !grouptot)|> 
  mutate(subegroup=egroup, sublb=lb, subub=ub) |> 
  select(fips, area, naics, egroup, lb, ub, subegroup, sublb, subub, estabs, emp, wages)

# combine the data and calculate subgroup summaries
combo1 <- bind_rows(oldgroup_data2, newgroup_data)
sum(combo1$wages) # 462483132 good, almost same as raw dat4a
skim(combo1)

combosums <- combo1 |> 
  summarise(across(c(estabs, emp, wages), sum), 
            .by=c(fips, area, naics)) |> 
  mutate(egroup="all", subegroup="all")

combo2 <- bind_rows(
  combo1 |> mutate(grouptot=FALSE),
  combosums |> mutate(grouptot=TRUE)) |> 
  arrange(fips, area, naics, desc(grouptot), lb) |> 
  mutate(empest=emp / estabs,
         avgwage=wages * 1000 / emp,
         avgpayroll=wages * 1000 / estabs) |> 
  relocate(grouptot, .before=egroup)

saveRDS(combo2, here::here("data", "cbp", "cbp_synthetic.rds"))


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
  
  





#########
xlsx::write.xlsx(check, file = here::here("check.xlsx"))

# based on my checking, I do not yet trust the industry data but the naics 00 data look good

