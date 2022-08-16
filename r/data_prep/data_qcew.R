
# codes -------------------------------------------------------------------
# QCEW Ownership Codes for NAICS Coded Data
# Code	Ownership Title
# 0	Total Covered
# 5	Private
# 4	International Government
# 3	Local Government
# 2	State Government
# 1	Federal Government
# 8	Total Government
# 9	Total U.I. Covered (Excludes Federal Government)


# libraries ---------------------------------------------------------------

source(here::here("r", "libraries.r"))
source(here::here("r", "constants.r"))

# get mta counties, all years 1990-2021 -----------------------------------
(files <- list.files(qdir, full.names = TRUE))

f <- function(year){
  pre <- "E:\\data\\BLSData\\qcew\\annual_area\\"
  post <- "_annual_by_area.zip"
  zfile <- paste0(pre, year, post)
  print(zfile)
  csvnames <- unzip(zipfile = zfile, list = TRUE) |> lcnames()
  nycsv <- csvnames |> 
    filter(str_detect(name, ", New York") |
             str_detect(name, "36000 New York -- Statewide.csv"))
  
  f2 <- function(file) {
    print(file)
    read_csv(unz(zfile, file), col_types = cols(.default = col_character()), n_max=Inf) |> 
      mutate(datasource=file)
  }
  df <- map_dfr(nycsv$name, f2) # each year, read the relevant files
  df
}

df <- map_dfr(1990:2021, f)
glimpse(df)
ns(df)
count(df, year)
saveRDS(df, path(qdir, "nyraw.rds"))

# count(tmp, year)
# tmp |> filter(str_detect(name, "Unknown"))
# tmp |> filter(str_detect(name, "Statewide"))
# 1990-2000 do not have the unknown or unidentified file
# 2021.annual 36999 Unknown Or Undefined, New York.csv
# 2021.annual 36000 New York -- Statewide.csv


# clean the NY raw data ---------------------------------------------------
df <- readRDS(path(qdir, "nyraw.rds"))
count(df, area_title)

df2 <- df %>%
  filter(area_fips %in% constants$mtafips) |> 
  select(year, fips=area_fips, area=area_title, own=own_code, ownf=own_title,
         agglev=agglvl_code, agglevf=agglvl_title,
         ind=industry_code, indf=industry_title,
         emp=annual_avg_emplvl, wages=total_annual_wages, wagestxbl=taxable_annual_wages,
         contrib=annual_contributions, avgweeklywage=annual_avg_wkly_wage, avgpay=avg_annual_pay) |> 
    # conversion to numeric based on analysis further below
    mutate(across(c(own, agglev, emp, wages, wagestxbl, contrib, avgweeklywage, avgpay),
                  as.numeric),
           mtasub=get_mtasub(fips))
glimpse(df2)  
count(df2, area)
saveRDS(df2, path(qdir, "mtaraw.rds"))


# create mta qcew in project directory ----
df <- readRDS(path(qdir, "mtaraw.rds"))
glimpse(df) # we have all data for the mta region

qcew_mta <- df |> 
  mutate(year=as.integer(year)) |> 
  select(-c(wagestxbl, contrib, avgweeklywage, avgpay))
glimpse(qcew_mta)

saveRDS(qcew_mta, here::here("data", "qcew", "qcew_mta.rds"))




# EXPERIMENTAL ------------------------------------------------------------


## mta base will have just what we need ----
mtabase <- df |> 
  select(-c(wagestxbl, contrib, avgweeklywage, avgpay))
glimpse(mtabase)

## create nyc and mta totals ----
subtots <- mtabase |>
  group_by(mtasub, year, own, ownf, agglev, agglevf, ind, indf) |> 
  summarise(across(c(emp, wages), sum), .groups="drop")

tots <- subtots |>
  group_by(year, own, ownf, agglev, agglevf, ind, indf) |> 
  summarise(across(c(emp, wages), sum), .groups="drop") |> 
  mutate(mtasub="allmta")

qcew_mta <- bind_rows(mtabase |> mutate(rectype="county"),
                      subtots |> mutate(rectype="subtot"),
                      tots |> mutate(rectype="total"))

count(qcew_mta, mtasub)
qcew_mta |> 
  filter(year==2020, own==0) |> 
  group_by(rectype, mtasub) |> 
  summarise(emp=sum(emp), wages=sum(wages), .groups="drop")

