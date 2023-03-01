
# libraries ---------------------------------------------------------------

source(here::here("r", "libraries.r"))
source(here::here("r", "libraries_ts.r"))

source(here::here("r", "functions.r"))


# constants ---------------------------------------------------------------

source(here::here("r", "constants.r"))
dst <- here::here("data", "state_taxes")



# clean state taxes to be allocated ---------------------------------------
# https://openbudget.ny.gov/openbudgetdata/nysdob-revenue.xlsx?FundGroup=All+Funds&FPCategory=&DetailReceipt=
dobtax <- "nysdob-revenue.xlsx"
tax1 <- read_excel(path(dst, dobtax))
glimpse(df1)

staxes1 <- tax1 |> 
  select(fgroup=1, fpcat=2, source=3, 
         fy2019=`2018-19 Actuals`,
         fy2020=`2019-20 Actuals`,
         fy2021=`2020-21 Actuals`,
         fy2022=`2021-22 Actuals`
  ) |> 
  mutate(group=case_when(fpcat=="Personal income tax" |
                           source %in% c("ECET", "Pass-through entity tax")
                         ~ "pit",
                         source=="Sales and use taxes"      ~ "sut",
                         fpcat=="User taxes"                 ~ "user_other",
                         source=="Corporation franchise tax" ~ "cft",
                         fpcat=="Business taxes"             ~ "business_other",
                         source=="Estate & gift taxes"       ~ "egt",
                         source=="Real estate transfer tax"  ~ "rett",
                         fpcat=="Other taxes"                ~ "other_other"
  ),
  fpcat2=case_when(fpcat=="Personal income tax" |
                     source %in% c("ECET", "Pass-through entity tax") ~ "pit",
                   fpcat %in% "User taxes" ~ "user",
                   fpcat %in% "Business taxes" ~ "business",
                   fpcat %in% "Other taxes" ~ "other")) |> 
  relocate(fpcat2, group, .after = fpcat) |> 
  arrange(fpcat2, desc(fy2021)) |> 
  select(-fgroup, -fpcat)

staxes <- staxes1 |> 
  summarise(across(starts_with("fy"), sum),
            .by=group)

# get new allocator data ---------------------------------------------------------------
xwalkny <- readRDS(here::here("data", "xwalks", "xwalkny.rds"))

## census pop ----
# censuspop1 <- readRDS(here::here("data", "census", "censuspop.rds"))
# 
# censuspop <- censuspop1 |> 
#   filter(!str_starts(name, "density")) |> 
#   left_join(xwalkny |> select(unifips, uniname, censusname), by="censusname") |> 
#   mutate(src="census", yeartype="cy") |> 
#   select(unifips, uniname, year, src, yeartype, name, value)
# glimpse(censuspop)
# summary(censuspop) # 2010-2019
# count(censuspop, unifips, uniname) # 64: NYS 36000, 62, NYC 3651000 
# count(censuspop, name)

## taxable sales and purchases ----

# tsp <- tsp1 |> 
#   mutate(src="dtf_txblsales",
#          yeartype="sfy")
# count(tsp, unifips, uniname)

## PIT county of residence ----
# fn <- "Total_Income_And_Tax_Liability_By_Place_Of_Residence__Beginning_Tax_Year_1999.csv"
# url <- "https://data.ny.gov/api/views/nacg-rg66/rows.csv?accessType=DOWNLOAD&sorting=true"



# url <- "https://data.ny.gov/Government-Finance/Personal-Income-Tax-Filers-Summary-Dataset-1-Major/73iw-kuxv/data"
url <- "https://data.ny.gov/api/views/73iw-kuxv/rows.csv?accessType=DOWNLOAD&sorting=true"
pitfn <- "Personal_Income_Tax_Filers__Summary_Dataset_1_-_Major_Items_by_Liability_Status_and_Place_of_Residence__Beginning_Tax_Year_2015.csv"
download.file(url, path(dst, fn), mode="wb")

pit1 <- vroom(path(dst, pitfn))
glimpse(pit1)

pit2 <- pit1 |> 
  select(taxyear=1, 
         liabstatus=2,
         resstatus=3,
         por=4,
         county=5,
         stname=6,
         country=7, 
         notes=8,
         nret=`Number of Returns`,
         fedagi=`Federal Amount of NY Adjusted Gross Income`,
         nyagi=`New York State Amount of NY Adjusted Gross Income`,
         liab=`Tax Liability`)
summary(pit2)
count(pit2, liabstatus)
# 1 All Returns   804
# 2 Non-Taxable   804
# 3 Taxable       804

count(pit2, resstatus)
# 1 All Filers               18
# 2 Full-Year Nonresident  1188
# 3 Full-Year Resident     1188
# 4 Part-Year Resident       18
pit2 |> filter(resstatus=="All Filers")
pit2 |> filter(resstatus=="Part-Year Resident")


pitnyfyr <- pit2 |> 
  filter(liabstatus=="All Returns",
         resstatus=="Full-Year Resident",
         !county %in% c("All Counties Outside of New York City", "All Places"),
         !str_detect(por, "New York City - ")) |> # drop the boroughs
  mutate(county=case_when(county=="Other Places- NYS Resident" ~ "other",
                          county=="All New York City" ~ "New York City",
                          TRUE ~ county),
         mta=county %in% c("New York City", "Nassau", "Suffolk", 
                           "Dutchess", "Orange","Putnam", "Rockland", "Westchester")) |> 
  mutate(totliabfyr=sum(liab), .by=taxyear) |> 
  arrange(taxyear) |> 
  select(-por, -resstatus, -liabstatus)


count(pitnyfyr, county) # 59: 57 + NYC + other
count(pitnyfyr |> filter(mta), county)
count(pitnyfyr, por, county)

liabtots <- pit2 |> 
  filter(liabstatus=="All Returns",
         resstatus=="All Filers") |> 
  select(taxyear, totliab=liab)

pitalloc <- pitnyfyr |> 
  left_join(liabtots, by = join_by(taxyear)) |> 
  mutate(respct=liab / totliabfyr,
         allpct=liab / totliab)

pitshares <- pitalloc |> 
  # filter(mta) |> 
  select(taxyear, county, liab, totliabfyr, totliab, respct, allpct) |> 
  left_join(xwalkny |> 
              mutate(county=uniname) |> 
              select(unifips, uniname, county), 
            join_by(county))

pit_allocators <- pitshares |> 
  select(year=taxyear, unifips, uniname, pitres=respct, pitall=allpct) |> 
  pivot_longer(cols=c(pitres, pitall), names_to = "allocator")



## county gdp ----
gdp1 <- vroom(path(r"(E:\data\BEAData)", "CountyGDP_2023-02-20.csv"), skip=3)

gdp2 <- gdp1 |> 
  lcnames() |> 
  pivot_longer(-c(geofips, geoname, linecode, description),
               names_to = "year") |> 
  mutate(value=as.numeric(value))
skim(gdp2)

gdp3 <- gdp2 |> 
  filter(geofips!="36000") |> 
  filter(!str_detect(description, "Addenda"), linecode==2) |> 
  mutate(geoname=ifelse(geofips %in% constants$nycfips,
                        "New York City",
                        geoname),
         geofips=ifelse(geofips %in% constants$nycfips,
                        "3651000",
                        geofips)) |> 
  summarise(value=sum(value, na.rm=TRUE), .by=c(geofips, geoname, linecode, description, year))

gdp4 <- gdp3 |> 
  rename(unifips=geofips) |> 
  left_join(xwalkny |> 
             select(unifips, uniname), 
           join_by(unifips))

gdp_allocators <- gdp4 |> 
  mutate(allocator="privgdp", year=as.numeric(year)) |> 
  select(unifips, uniname, year, allocator, value)



## directly allocated state tax revenue ----



# allocators --------------------------------------------------------------
allocators_old <- readRDS(here::here("data", "allocation", "allocators.rds"))
count(allocators_old, allocator)
count(allocators_old, unifips, uniname)

allocators_old2 <- allocators_old |> 
  filter(!unifips %in% c("36000", constants$nycfips, "36xx1", "36xx2", "36xx9"))
count(allocators_old2, allocator)
count(allocators_old2, unifips, uniname)
summary(allocators_old2)


allocators_old3 <- allocators_old2 |> 
  filter(allocator %in% c("mft", "pop", "txblsales"))
count(allocators_old3, allocator, year)
count(allocators_old3, yeartype)
count(allocators_old3, uniname, unifips, year) |> 
  pivot_wider(names_from = year, values_from = n) # all through 2019

# now make new allocators ----
glimpse(allocators_old3)
glimpse(pit_allocators)
glimpse(gdp_allocators)

allocators1 <- 
  bind_rows(allocators_old3 |> 
              select(unifips, uniname, allocator, year, value),
            gdp_allocators)

count(allocators1, allocator, year) |> 
  pivot_wider(names_from = year, values_from = n)

allocators2 <- allocators1 |> 
  mutate(allocpct=value / sum(value),
            .by=c(year, allocator)) |> 
  # THIS IS IMPORTANT SO WE USE PREVIOUSLY CALC'D SHARES
  bind_rows(pit_allocators |> 
              mutate(allocpct=value))

allocators <- allocators2 |> 
  left_join(xwalkny |> 
              select(unifips, mta=mta_all),
            join_by(unifips))
skim(allocators)  
count(allocators, allocator)

allocators |> 
  filter(mta, year==2019, allocator!="mft") |> 
  select(unifips, uniname, allocator, allocpct) |> 
  pivot_wider(names_from = allocator, values_from = allocpct)
  

# allocation --------------------------------------------------------------
recipe_pitres <- read_csv(
  "group, allocator
cft, privgdp
business_other, pop
egt, privgdp
rett, privgdp
other_other, pop
pit, pitres
sut, txblsales
user_other, pop
")
recipe_pitres
recipe_pitall <- recipe_pitres |> 
  mutate(allocator=ifelse(group=="pit", "pitall", allocator))
recipe <- bind_rows(
  recipe_pitres |> mutate(recipe="pitres"),
  recipe_pitall |> mutate(recipe="pitall")
)
recipe

# allocator `2015` `2016` `2017` `2018` `2019` `2020` `2021`
# 1 mft           58     58     58     58     58     58     58
# 2 pitall        59     59     59     59     59     59     NA
# 3 pitres        59     59     59     59     59     59     NA
# 4 pop           58     58     58     58     58     NA     NA
# 5 privgdp       58     58     58     58     58     58     58
# 6 txblsales     58     58     58     58     58     58     58

allocation1 <- staxes |> 
  select(group, fy2021) |> 
  left_join(recipe, by = join_by(group), multiple = "all") |> 
  left_join(allocators |> 
              filter(year==2019) |> 
              select(mta, unifips, uniname, allocator, allocpct),
            by = join_by(allocator), multiple = "all") |> 
  mutate(taxamount=fy2021 * allocpct)

# get total taxes by county by recipe
cotots <- allocation1 |> 
  summarise(taxamount=sum(taxamount), .by=c(recipe, mta, unifips, uniname))

cotots |> 
  summarise(taxamount=sum(taxamount), .by=recipe)

staxes |> 
  select(group, fy2021) |> 
  janitor::adorn_totals()
# group    fy2021
# cft  4954.100
# business_other  3838.297
# egt  1537.437
# rett   948.856
# other_other     9.728
# pit 54969.900
# user_other 16147.450
# Total 82405.768

cotots |> 
  # filter(mta) |> 
  select(recipe, unifips, uniname, taxamount) |> 
  pivot_wider(names_from = recipe, values_from = taxamount) |> 
  janitor::adorn_totals()

cotots |> 
  mutate(share=taxamount / sum(taxamount), .by=recipe) |> 
  filter(mta) |> 
  select(recipe, unifips, uniname, share) |> 
  pivot_wider(names_from = recipe, values_from = share) |> 
  janitor::adorn_totals()



allocation2


