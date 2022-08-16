

# locations ---------------------------------------------------------------




# libraries ---------------------------------------------------------------

source(here::here("r", "libraries.r"))


# download ----------------------------------------------------------------

url <- "https://data.ny.gov/api/views/2vni-8tmb/rows.csv?accessType=DOWNLOAD&sorting=true"
fn <- "New_York_State__Local__and_Local_Purpose_Taxes_and_Fees_Collected_Monthly_by_the_Department_of_Taxation_and_Finance__Beginning_April_1996.csv"

fpath <- file.path(here::here("data", "DTF", fn))

download.file(url, destfile=fpath, mode="wb")


# get data ----------------------------------------------------------------

df <- read_csv(fpath)
glimpse(df)

df2 <- df |> 
  setNames(c("fyear", "collmonth", "taxtype", "tax", "value", "fymonth", "typesort", "taxsort"))

tmp <- count(df2, taxtype, tax)
tmp |> 
  filter(str_detect(tax, coll("Mortgage", ignore_case = TRUE)))

tmp |> 
  filter(str_detect(taxtype, coll("Local", ignore_case = TRUE)))
