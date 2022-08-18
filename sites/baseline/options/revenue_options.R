
# libraries ---------------------------------------------------------------

source(here::here("r", "libraries.r"))
source(here::here("r", "constants.r"))
source(here::here("r", "functions.r"))



# locations ---------------------------------------------------------------
fn <- "2019 Revenue Sources.xlsx"



# get data ----------------------------------------------------------------

df <- read_excel(here::here("data", "usdot", fn))
glimpse(df)

