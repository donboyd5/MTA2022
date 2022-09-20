

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

xwalkny <- readRDS(here::here("data", "xwalks", "xwalkny.rds"))
toallocate <- readRDS(here::here("data", "allocation", "alloc_detail.rds"))  
mtashares <- readRDS(here::here("data", "allocation", "allocators_mta.rds"))

# toallocate |> filter(vname=="autorental_mtaaid")

# allocation --------------------------------------------------------------
recipes

glimpse(toallocate)
count(toallocate, vname)
recipes |> filter(vname=="autorental_mtaaid")
toallocate |> filter(vname=="autorental_mtaaid")
mtashares |> filter(allocator=="autorental_mtaaid_accrual")

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

tmp <- mtashares |> select(unifips, uniname, year, allocator, allocshare, pop)
tmp |> filter(allocator=="autorental_mtaaid", unifips=="3651000")
result1 |> filter(vname=="autorental_mtaaid", unifips=="3651000")

# add summary records
mctd_sums <- result1 |> 
  group_by(vname, allocator, measure, year) |> 
  summarise(totvalue=first(totvalue),
            allocated=sum(allocated, na.rm=TRUE),
            pop=sum(pop),
            .groups="drop") |> 
  mutate(unifips="36xx1", uniname="MCTD")

suburb_sums <- result1 |> 
  filter(!str_detect(uniname, "New York City")) |> 
  group_by(vname, allocator, measure, year) |> 
  summarise(totvalue=first(totvalue),
            allocated=sum(allocated, na.rm=TRUE),
            pop=sum(pop),
            .groups="drop") |> 
  mutate(unifips="36xx3", uniname="Suburban")

result2 <- bind_rows(result1 |> mutate(rectype="detail"),
                     mctd_sums |> mutate(rectype="mta"),
                     suburb_sums |> mutate(rectype="suburban"),
                     result1 |> 
                       filter(str_detect(uniname, "New York City")) |> 
                       mutate(rectype="nyc")) |> 
  mutate(allocpc=allocated / pop) |> 
  group_by(vname, measure, year) |> 
  mutate(ipcmta=allocpc / allocpc[rectype=="mta"],
         ipcsuburbs=allocpc / allocpc[rectype=="suburban"]) |> 
  ungroup() |> 
  arrange(measure, vname, year, unifips)

result2

# checks ----
summary(result2)
count(result2, rectype)
count(result2, unifips, uniname)
count(result2, measure)
count(result2, year)

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
tax <- "autorental_mtaaid"
result2 |> 
  # filter(rectype=="detail") |> 
  filter(rectype!="nyc") |> 
  filter(year==2021, measure=="accrual", vname==tax) |> 
  arrange(desc(allocpc))
count(result2, unifips, uniname)

saveRDS(result2, here::here("results", "allocation_results.rds"))






# get the 2019 study for comparison ----
alloctotals_fn <- "090922 Subsidies for DB_djb.xlsx"
mtashares <- readRDS(here::here("data", "allocation", "allocators_mta.rds"))

study1 <- read_excel(here::here("data", "mta", alloctotals_fn),
                       sheet="Big Summary",
                       range="A95:J121",
                     col_types = "text")


study2 <- study1 |> 
  filter(row_number() %in% c(17, 18, 21, 22, 23, 24, 25, 26)) |> 
  pivot_longer(-1) |> 
  setNames(c("studyname", "studyarea", "allocated")) |> 
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
         allocated=as.numeric(allocated) * 1e6,
         year=as.integer(2016)) |> 
  group_by(vname) |> 
  mutate(totvalue=sum(allocated[uniname != "MCTD"])) |> 
  ungroup()

#  prepare population 
mtapop <- mtashares |> 
  filter(allocator=="pop", year==2016) |> 
  select(unifips, pop) |> 
  janitor::adorn_totals("row", name="36xx1") |> 
  as_tibble()
mtapop

study4 <- study3 |> 
  left_join(mtapop, by = "unifips") |> 
  group_by(vname) |> 
  mutate(allocpc=allocated / pop,
         ipcmta=allocpc / allocpc[uniname=="MCTD"]) |> 
  ungroup()

study4
count(study4, studyarea, uniname, unifips)
count(study4, studyname, vname)
saveRDS(study4, here::here("data", "mta", "study2016data.rds"))


# compare current results to mta study ------------------------------------
study <- readRDS(here::here("data", "mta", "study2016data.rds"))
result2

prepres <- result2 |> 
  filter(year==2016, measure=="accrual", rectype %in% c("detail", "mta"), !is.na(totvalue)) |> 
  mutate(src="analysis") |> 
  # collapse to the categories in the old study
  mutate(vname=case_when(str_detect(vname, "mrt") ~ "mrt1and2",
                         str_detect(vname, "mtaaid") ~ "mtaaid",
                         str_detect(vname, "pbt") ~ "pbt",
                         TRUE ~ vname)) |> 
  group_by(year, unifips, uniname, pop, measure, src, rectype, vname) |> 
  summarise(totvalue=sum(totvalue),
            allocated=sum(allocated), .groups="drop") |> 
  group_by(year, measure, src, vname) |> 
  mutate(allocpc=allocated / pop,
         ipcmta=allocpc / allocpc[uniname=="MCTD"]) |> 
  ungroup()
prepres


comp1 <- bind_rows(study |> 
                     mutate(src="study"),
                  prepres) |> 
  select(src, vname, unifips, uniname, pop, totvalue, allocated, allocpc, ipcmta)
comp1

tmp <- comp1 |> filter(vname=="autorental_mtaaid")

comp1 |> 
  filter(uniname=="MCTD") |> 
  select(src, vname, comp=totvalue) |> 
  pivot_wider(names_from = src, values_from = comp) |> 
  mutate(diff=analysis - study,
         pdiff=diff / study)

# totals: 
# mrt1and2 good
# mtaaid good
# urban is not in the study??
# sut_mmtoa is close enough -- I'm $6m greater, 0.9%
# franchise I am $11m lower, 25%
# surcharge I am $12m higher, 1.5%
# pbt I am 0.8% higher (assuming mmtoa is combined)
# pmt I am $321m lower, or 19%, think MTA includes pmt offset

count(comp1, vname)
# allocpc, ipcmta
comp1 |> 
  filter(vname=="fransurcharge_mmtoa") |> 
  select(vname, src, uniname, unifips, comp=ipcmta) |> 
  pivot_wider(names_from = src, values_from = comp) |> 
  mutate(diff=analysis - study,
         pdiff=diff / study)

# per capita
# mrt1and2 identical
# franchise_mmtoa indexed - my nyc much higher, counties lower -- allocation
# fransurcharge_mmtoa indexed - my nyc much higher, counties lower -- allocation
# mtaaid - my NYC 27% higher, counties much lower
# pbt - my nyc 7% lower, most counties higher, Rockland much lower
# pmt indexed my nyc higher, counties lower
# sut my nyc 5% higher, Westchester, Putname, Orange, Dutches, considerably lower

# priorities for allocation
#   pbt BIG
#   sut BIG
#   fransurcharge_mmtoa BIG
#   mtaaid


