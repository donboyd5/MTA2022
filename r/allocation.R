

# libraries and constants -------------------------------------------------

source(here::here("r", "libraries.r"))
source(here::here("r", "libraries_ts.r"))
source(here::here("r", "constants.r"))
source(here::here("r", "functions.r"))


# constants ---------------------------------------------------------------

# fn <- "090922 Subsidies for DB.xlsx"
alloctotals_fn <- "090922 Subsidies for DB_djb.xlsx"

# set up allocation MOVE TO ANOTHER FILE -------------------------------------------------------

recipes1 <- read_excel(here::here("data", "mta", alloctotals_fn),
                      sheet="recipes",
                      range="A4:C23") |> 
  filter(!is.na(vname))
recipes1

recipes <-  recipes1 |> 
  rename(allocator=allocator1) |> 
  filter(!is.na(allocator))
recipes

toallocate <- readRDS(here::here("data", "allocation", "alloc_detail.rds"))  

mtashares <- readRDS(here::here("data", "allocation", "allocators_mta.rds"))


# allocation --------------------------------------------------------------
recipes

glimpse(toallocate)
count(toallocate, vname)
recipes |> filter(vname=="autorental_mtaaid")
toallocate |> filter(vname=="autorental_mtaaid")
mtashares |> filter(allocator=="autorental_mtaaid")

# check availabilities
setdiff(recipes$vname, unique(toallocate$vname)) # good - all vnames with recipes are in the data
setdiff(unique(recipes$allocator), unique(mtashares$allocator))
# we need "taxicab_value"    "autorental_value" -- they go to NYC


result1 <- recipes |> 
  # filter(vname=="autorental_mtaaid") |> 
  filter(allocator != "split") |> 
  select(-src) |> 
  left_join(toallocate |> select(-columnid) |> rename(totvalue=value), by="vname") |> 
  left_join(mtashares |> select(unifips, uniname, year, allocator, allocshare, pop),
            by=c("allocator", "year")) |> 
  mutate(allocated=totvalue * allocshare)
summary(result1)

# add summary records
mctd_sums <- result1 |> 
  group_by(vname, allocator, measure, year) |> 
  summarise(totvalue=first(totvalue),
            allocated=sum(allocated, na.rm=TRUE),
            pop=sum(pop),
            .groups="drop") |> 
  mutate(uniname="MTA total")

suburb_sums <- result1 |> 
  filter(!str_detect(uniname, "New York City")) |> 
  group_by(vname, allocator, measure, year) |> 
  summarise(totvalue=first(totvalue),
            allocated=sum(allocated, na.rm=TRUE),
            pop=sum(pop),
            .groups="drop") |> 
  mutate(uniname="Suburban")

result2 <- bind_rows(result1 |> mutate(rectype="detail"),
                     mctd_sums |> mutate(rectype="mta"),
                     suburb_sums |> mutate(rectype="suburbs"),
                     result1 |> 
                       filter(str_detect(uniname, "New York City")) |> 
                       mutate(rectype="nyc")) |> 
  mutate(allocpc=allocated / pop) |> 
  group_by(vname, measure, year) |> 
  mutate(ipcmta=allocpc / allocpc[rectype=="mta"],
         ipcsuburbs=allocpc / allocpc[rectype=="suburbs"]) |> 
  ungroup()

result2

# checks ----
summary(result2)
count(result2, rectype)
count(result2, unifips, uniname)
count(result2, measure)
count(result2, year) # 2022 is different

## totals ----
tots <- result2 |> 
  filter(rectype=="detail") |> 
  group_by(vname, measure, year) |> 
  summarise(totvalue=first(totvalue), sumvalues=sum(allocated, na.rm=TRUE),
            .groups="drop") |> 
  mutate(diff=sumvalues - totvalue, pdiff=diff / totvalue)
tots |> filter(diff != 0)  # good, differences are miniscule

## laugh test, specific taxes ----
glimpse(result2)
count(result2, vname)
result2 |> 
  filter(measure=="accrual", year==2021) |> 
  select(vname, allocator, measure, year, totvalue) |> 
  distinct() |> 
  arrange(desc(totvalue))

tax <- "pmt"
tax <- "fransurcharge_mmtoa"
tax <- "sut_mmtoa"
tax <- "pbt"
tax <- "urban"
tax <- "mrt1"
tax <- "mrt2"
tax <- "urban"
result2 |> 
  # filter(rectype=="detail") |> 
  filter(rectype!="nyc") |> 
  filter(year==2021, measure=="accrual", vname==tax) |> 
  arrange(desc(allocpc))



# get the 2019 study for comparison ----

study1 <- read_excel(here::here("data", "mta", alloctotals_fn),
                       sheet="Big Summary",
                       range="A95:J121",
                     col_types = "text")


study2 <- study1 |> 
  filter(row_number() %in% c(17, 18, 21, 22, 23, 24, 25, 26)) |> 
  pivot_longer(-1) |> 
  setNames(c("studyname", "studyarea", "value")) |> 
  mutate(year=2016,
         vname=case_when(str_detect(studyname, "MRT") ~ "mrt1and2",
                         str_detect(studyname, "Urban") ~ "urban",
                         str_detect(studyname, "Sales and Use") ~ "sut_mmtoa",
                         str_detect(studyname, "Franchise") ~ "franchise_mmtoa",
                         str_detect(studyname, "Business Surcharge") ~ "fransurcharge_mmtoa",
                         str_detect(studyname, "PBT") ~ "pbt",
                         str_detect(studyname, "PMT") ~ "pmt",
                         str_detect(studyname, "MTA Aid") ~ "mtaaid",
                         TRUE ~ "other"))
count(study2, studyname, vname)
count(study2, studyarea)


snames <- c("Dutch.", "Orange", "Rock.", "Putnam", "Westch.", "Nassau", "Suffolk", "NYC", "Total")
uninames <- c("Dutchess", "Orange", "Rockland", "Putnam", "Westchester", "Nassau", "Suffolk", "New York City", "MCTD")
unifips <- xwalkny$unifips[match(uninames, xwalkny$uniname)]
cbind(snames, uninames, unifips)

study3 <- study2 |> 
  mutate(uniname=factor(studyarea, levels=snames, labels=uninames),
         unifips=factor(uniname, levels=uninames, labels=unifips),
         value=as.numeric(value),
         year=as.integer(2016))
study3
count(study3, studyarea, uniname, unifips)
count(study3, studyname, vname)





