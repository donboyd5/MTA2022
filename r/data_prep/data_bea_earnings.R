
source(here::here("r", "libraries.r"))
source(here::here("r", "constants.r"))

# get BEA data ------------------------------------------------------------

df <- read_csv(here::here("data", "bea", "CAINC5N_NY_2001_2020.csv"))
glimpse(df)

# define the lines we need to keep from the bea data
# 35 50 60 70 71 72
codes <- tribble(
  ~line, ~vname,
  35, "earnings",
  50, "wages",
  60, "supplements",
  70, "propinc", 
  71, "farminc",
  72, "nfpi"
)

bea_mta <- df |> 
  lcnames() |> 
  filter(geofips %in% constants$mtafips,
         linecode %in% codes$line) |> 
  select(fips=geofips, area=geoname, line=linecode, description, `2001`:`2020`) |> 
  pivot_longer(-c(fips, area, line, description), names_to = "year") |> 
  mutate(year=as.integer(year),
         vname=factor(line, levels = codes$line, labels=codes$vname),
         value=as.numeric(value))

count(bea_mta, year)
count(bea_mta, fips, area)
count(bea_mta, line, vname, description)

saveRDS(bea_mta, here::here("data", "bea", "bea_mta.rds"))
