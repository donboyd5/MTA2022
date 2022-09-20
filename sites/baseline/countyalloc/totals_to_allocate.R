
# run this code BEFORE creating allocators -- 


# libraries and constants -------------------------------------------------

source(here::here("r", "libraries.r"))
source(here::here("r", "libraries_ts.r"))
source(here::here("r", "constants.r"))
source(here::here("r", "functions.r"))


# constants ---------------------------------------------------------------

# fn <- "090922 Subsidies for DB.xlsx"
alloctotals_fn <- "090922 Subsidies for DB_djb.xlsx"


# get raw allocation data; will use for (1) value-based allocators, (2) alloc totals for details, and (3) for splits --------

alloc_raw1 <- read_excel(here::here("data", "mta", alloctotals_fn),
                  sheet="subsidies_djb",
                  range="A9:V116")

alloc_raw2 <- alloc_raw1 |> 
  pivot_longer(cols=c(starts_with("accrual"), starts_with("cash")),
               names_to = "measyear") |> 
  separate(measyear, into=c("measure", "year")) |> 
  mutate(year=as.integer(year),
         value=as.numeric(value)) |> 
  filter(!is.na(year)) |> 
  select(rownum, rowtype, revgroup, revitem, location, vname, columnid, measure, year, value)
glimpse(alloc_raw2)
count(alloc_raw2, rownum) |> ht()
count(alloc_raw2, measure)
count(alloc_raw2, year)
count(alloc_raw2, location)
count(alloc_raw2, vname)

saveRDS(alloc_raw2, here::here("data", "allocation", "alloc_raw.rds"))


# get and save base totals and split values to allocate --------------------------------------------------------------
## get base total subsidies data from mta (before splitting mmtoa and mtaaid) ----
alloc_raw <- readRDS(here::here("data", "allocation", "alloc_raw.rds"))  
glimpse(alloc_raw)

# base totals do not have mmtoa and mtaaid split into details
base_totals <- alloc_raw |> 
  filter(!is.na(vname)) |> 
  select(vname, columnid, measure, year, value) |> 
  filter(year %in% 2016:2022)
base_totals  

base_totals |> 
  filter(year==2021, measure=="accrual")

base_totals |> select(vname) |> distinct()
saveRDS(base_totals, here::here("data", "allocation", "base_totals.rds"))  


## get mmtoa and mtaaid splits from my sheet in same file --------------------------

splits1 <- read_excel(here::here("data", "mta", alloctotals_fn),
                  sheet="MMTOA_MTAAid",
                  range="B7:M17")

# calculate the shares
splits2 <- splits1 |> 
  select(-c(suthh_mmtoa, suttot_mmtoa)) |>  # we do NOT want to allocate these
  mutate(yearend=as.integer(yearend),
         across(contains("_mmtoa") & !contains("total"),
                ~ .x / total_mmtoa),
         across(contains("_mtaaid") & !contains("total"),
                ~ .x / total_mtaaid))
splits2

splits3 <- splits2 |> 
  rename(year=yearend) |> 
  select(!contains("total")) |> 
  pivot_longer(-year, names_to = "vname", values_to = "fundshare") |> 
  mutate(revgroup=case_when(str_detect(vname, "mmtoa") ~ "mmtoa",
                            str_detect(vname, "mtaaid") ~ "mtaaid",
                            TRUE ~ "ERROR"))

# check
splits3 |> 
  group_by(year, revgroup) |> 
  summarise(sum=sum(fundshare))

saveRDS(splits3, here::here("data", "allocation", "taxsplits.rds"))  


## split the allocation totals for mmtoa and mtaaid into individual taxes -----------------------
base_totals <- readRDS(here::here("data", "allocation", "base_totals.rds"))
taxsplits <- readRDS(here::here("data", "allocation", "taxsplits.rds"))  


# define the totals we want to split into individual taxes
splitvars <- c("mmtoa", "mtaaid")

split_totals1 <- base_totals |>
  # get the totals, all years, and create allocated totals
  filter(vname %in% splitvars) |> 
  rename(revgroup=vname, totvalue=value) |> 
  # inner join to get the splits for each total
  inner_join(taxsplits, by = c("revgroup", "year")) |> 
  mutate(value=totvalue * fundshare)
# do some checks
split_totals1
count(split_totals1, revgroup, vname)
count(split_totals1, revgroup, columnid)
split_totals1 |> 
  group_by(revgroup, year, measure) |> 
  summarise(totvalue=first(totvalue), sumvalue=sum(value)) |> 
  mutate(diff=sumvalue - totvalue) |> 
  filter(diff != 0)

## save the split totals for later use ----
saveRDS(split_totals1, here::here("data", "mta", "split_totals.rds"))

## combine split totals with base totals values ----
alloc_detail <- bind_rows(base_totals,
                          split_totals1 |> select(vname, columnid, measure, year, value))

saveRDS(alloc_detail, here::here("data", "mta", "alloc_detail.rds"))


# save value-based allocators (mrt1, mrt2, urban, ...) to a file, with unicodes -------------------------------
## mrt1, mrt2, urban  ----
alloc_raw <- readRDS(here::here("data", "allocation", "alloc_raw.rds"))    
xwalkny <- readRDS(here::here("data", "xwalks", "xwalkny.rds"))

values_mrturban1 <- alloc_raw |> 
  select(-vname) |> 
  filter(rowtype %in% c("detail", "subdetail"),
         revgroup %in% c("mrt1", "mrt2", "urban")) |>
  mutate(location=case_when(location=="NYC" ~"New York City",
                            location=="Richmond" ~ "Staten Island",
                            TRUE ~ location)) |> 
  rename(uniname=location) |> 
  left_join(xwalkny |> select(unifips, uniname), by = "uniname")
glimpse(values_mrturban1)
count(values_mrturban1, rownum)
count(values_mrturban1, unifips, uniname)
count(values_mrturban1, rowtype, revgroup)

# we want unifips, uniname, year, src, yeartype, name, value
values_mrturban2 <- values_mrturban1 |> 
  mutate(src=paste0("mta_", measure),
         yeartype="cy",
         name=paste0(revgroup, "_value_", measure)) |> 
  select(unifips, uniname, year, src, yeartype, name, value)


## now taxicab_value autorental_value, which are nyc only ----
values_splits1 <- readRDS(here::here("data", "mta", "split_totals.rds"))

# we want unifips, uniname, year, src, yeartype, name, value
count(values_splits1, vname)
values_splits2 <- values_splits1 |> 
  filter(str_detect_any(vname, c("autorental", "taxicab"))) |> 
  mutate(unifips="3651000",
         uniname="New York City",
         src="mta_ais_sharing",
         yeartype="cy",
         name=vname) |>
  select(unifips, uniname, year, src, yeartype, name, value)

## rationalize, combine, and save the two sets of value-based data ----
mta_alloc <- bind_rows(values_mrturban2, values_splits2)
saveRDS(mta_alloc, here::here("data", "mta", "mta_alloc.rds"))    
  





