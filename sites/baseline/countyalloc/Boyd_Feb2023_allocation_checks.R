source(here::here("r", "libraries.r"))
source(here::here("r", "constants.r"))
source(here::here("r", "functions.r"))

alloctotals_fn <- "090922 Subsidies for DB_djb.xlsx"

qcew1 <- readRDS(here::here("data", "qcew", "qcew_alloc.rds"))

qcew1 <- readRDS(here::here("data", "qcew", "qcew_alloc_feb2023.rds"))
names(qcew1)

df1 <- qcew1 |> 
  select(unifips, uniname, year, name, private, tpu, insure, privxtpuins, sumvals, wtdqcew, wtdqcew2)

df2 <- df1 |> 
  filter(unifips %in% constants$nycfips) |> 
  summarise(across(c(private, tpu, insure, privxtpuins, wtdqcew, wtdqcew2), sum),
                   .by=c(year, name)) |> 
  mutate(unifips="3651000", uniname="New York City") |> 
  bind_rows(df1 |> filter(!unifips %in% constants$nycfips)) |> 
  mutate(across(c(private, tpu, insure, privxtpuins, wtdqcew, wtdqcew2),
                ~ .x / sum(.x),
                .names = "{.col}_share"),
         .by=c(year, name))
  
df2 |> filter(year==2021, name=="wages")
df2 |> filter(year==2021, name=="emp")

df2 |> 
  filter(unifips=="3651000", name=="wages") |> 
  select(name, year, contains("share")) |> 
  arrange(name)

# county gdp ----
library(BEAData)
data(package="BEAData")

library(bea.R)
bea_apikey <- "21F782AD-56A6-439D-B3D5-9A592F020E26"
beaSets(beaKey = bea_apikey)
beaParams(beaKey = bea_apikey, "Regional")
tmp <- beaSearch("gross domestic product", beaKey = bea_apikey)

# CountyGDP_2023-02-20.csv
# I downloaded the data from BEA interactive and saved to this file
df1 <- vroom(path(r"(E:\data\BEAData)", "CountyGDP_2023-02-20.csv"), skip=3)

df2 <- df1 |> 
  lcnames() |> 
  pivot_longer(-c(geofips, geoname, linecode, description),
               names_to = "year") |> 
  mutate(value=as.numeric(value))
skim(df2)

df3 <- df2 |> 
  filter(geofips %in% constants$mtafips) |> 
  filter(!str_detect(description, "Addenda"))
count(df3, geoname)
skim(df3)

df4 <- df3 |> 
  mutate(geoname=ifelse(geofips %in% constants$nycfips,
                        "New York City",
                        geoname),
         geofips=ifelse(geofips %in% constants$nycfips,
                        "3651000",
                        geofips)) |> 
  summarise(value=sum(value, na.rm=TRUE), .by=c(geofips, geoname, linecode, description, year)) |> 
  mutate(mta=sum(value), .by=c(linecode, description, year)) |> 
  mutate(sharemta=value / mta, .by=c(geofips, geoname, linecode, description, year)) |> 
  mutate(shareprivate=value / value[linecode==2], 
         .by=c(geofips, geoname, year))


nycshares <- df4 |> 
  filter(geofips=="3651000", year==2021) |> 
  arrange(desc(sharemta))

df4 |> 
  filter(geofips=="3651000", year==2021) |> 
  arrange(desc(sharemta))

df4 |> 
  filter(geofips=="3651000", linecode==2) |> 
  arrange(desc(sharemta))


# taxable sales and purchases check ----

tsp1 <- readRDS(here::here("data", "dtf", "salesyear", "tsp_alloc.rds"))
count(tsp1, unifips, uniname)
count(tsp1, name, yeartype)

check <- tsp1 |> 
  filter(unifips %in% c(constants$mtafips, "3651000")) |> 
  mutate(share=value / sum(value), .by=year, tax="taxable sales") |> 
  select(-name, -yeartype)
check


# motor fuel tax check ----

mft1 <- readRDS(here::here("data", "dtf", "mft.rds"))
mft2 <- mft1 |> 
  rename(year=fyear) |> 
  inner_join(xwalkny |> select(unifips, uniname, county=dtf_mft), by="county") |> 
  mutate(src="dtf_mft",
         yeartype="sfy")

count(mft2, unifips, uniname, county) # nyc as a whole
ht(mft2)

check <- mft2 |> 
  filter(unifips %in% c(constants$mtafips, "3651000")) |> 
  mutate(tax="mft",
         share=value / sum(value), .by=year) |> 
  select(-name, -yeartype) |> 
  arrange(unifips, year)
check


# pmt check ---------------------------------------------------------------
pmt1 <- readRDS(here::here("data", "pmtbase.rds"))

pmt2 <- pmt1 |>
  rename(unifips=fips) |> 
  left_join(xwalkny |> select(unifips, uniname), by="unifips") |> 
  # get only the vars we are 100% sure we might ever use for allocation
  select(unifips, uniname, year, earnings_bea, wages_bea, wages_pmt, nonwage_pmt, taxbase_pmt) |> 
  mutate(src="pmtbase",
         yeartype="cy") |>
  pivot_longer(cols=-c(unifips, uniname, year, yeartype, src))

count(pmt2, unifips, uniname)
count(pmt2, yeartype)
count(pmt2, src)

pmt3 <- pmt2 |> 
  filter(unifips %in% constants$mtafips) |> 
  mutate(uniname=ifelse(unifips %in% constants$nycfips,
                   "New York City", uniname),
         unifips=ifelse(unifips %in% constants$nycfips,
                        "3651000", unifips)) |> 
  summarise(value=sum(value), .by=c(year, unifips, uniname, name)) |> 
  mutate(value=value / sum(value), .by=c(year, name))
  
pmt3


# mrt ----
mta1 <- readRDS(here::here("data", "mta", "mta_alloc.rds"))  
count(mta1, yeartype)

check <- mta1 |> 
  filter(name %in% c("mrt1_value_accrual", "mrt2_value_accrual")) |> 
  mutate(uniname=ifelse(uniname %in% c("nycxrichmond", "Staten Island"),
                         "New York City",
                         uniname)) |> 
  summarise(value=sum(value), .by=c(uniname, year, name)) |> 
  mutate(sharemta=value / sum(value), .by=c(year, name))
  
  


