
# setup -------------------------------------------------------------------


source(here::here("r", "libraries.r"))
source(here::here("r", "libraries_ts.r"))
source(here::here("r", "constants.r"))
source(here::here("r", "functions.r"))


dcbp <- here::here("data", "cbp")
zcbp <- "CBP2020.CB2000CBP_2023-02-01T173400.zip"
fncbp <- "CBP2020.CB2000CBP-Data.csv"

fncbp2 <- "cbp20co.zip"




# auto rental tax ---------------------------------------------------------

## get data ----------------------------------------------------------------
# df1 <- read_csv(unz(path(dcbp, zcbp), fncbp))
# names(df1)

# get xwalk of county names, etc. ----
xwalkny <- readRDS(here::here("data", "xwalks", "xwalkny.rds"))


df2 <- read_csv(unz(path(dcbp, fncbp2), "cbp20co.txt"))
names(df2)
# AP              N       Total Annual Payroll ($1,000) with Noise

glimpse(df2)
skim(df2)

# 532111 Passenger Car Rental
ncodes <- c("532///", "5321//", "53211/", "532111")

df3 <- df2 |> 
  mutate(censusfips=paste0("36", fipscty)) |> 
  filter(fipstate=="36", censusfips %in% constants$mtafips) |> 
  filter(str_trim(naics) %in% ncodes) |> 
  select(fipstate, fipscty, censusfips, naics, ap) |> 
  left_join(xwalkny |> select(censusfips, uniname), by = join_by(censusfips))
glimpse(df3)
count(df3, naics)
count(df3, fipscty, uniname)

# estimate the putnam share
ncodes2 <- c("532///", "532111")
df4 <- df3 |> 
  filter(naics %in% ncodes2) |> 
  mutate(naics=factor(naics, levels=ncodes2, labels=c("rental", "passcars"))) |> 
  pivot_wider(names_from = naics, values_from = ap) |> 
  mutate(pct=passcars / rental,
         putnampct=ifelse(uniname=="Putnam",
                          mean(pct[uniname %in% c("Dutchess", "Orange", "Rockland", "Westchester")]),
                          NA_real_),
         passcars=ifelse(uniname=="Putnam", putnampct * rental, passcars),
         nyc=censusfips %in% constants$nycfips,
         nyctot=sum(passcars[nyc]),
         mtatot=sum(passcars),
         nyc_share=ifelse(nyc, nyctot / mtatot, NA_real_),
         county_share=passcars / mtatot)

xlsx::write.xlsx(df4, here::here("sites", "baseline", "countyalloc", "carrental.xlsx"))
# write_excel(df4, here::here("sites", "baseline", "countyalloc", "carrental.xlsx"))

# petroleum business tax ----


