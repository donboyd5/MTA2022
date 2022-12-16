
source(here::here("r", "libraries.r"))
source(here::here("r", "libraries_ts.r"))
source(here::here("r", "constants.r"))
source(here::here("r", "functions.r"))

dsalesyear <- here::here("data", "dtf", "salesyear")

df1 <- readRDS(path(dsalesyear, "tsp_detail.rds"))
glimpse(df1)

df2 <- df1 |> 
  filter(govt=="NY STATE", styear=="2021 - 2022") |> 
  summarise(n=n(), txblsales=sum(txblsales) / 1e6, .by=c(naics, fnaics)) |> 
  mutate(tax=txblsales * .04)

