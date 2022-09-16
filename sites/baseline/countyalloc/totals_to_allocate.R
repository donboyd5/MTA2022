

# libraries and constants -------------------------------------------------

source(here::here("r", "libraries.r"))
source(here::here("r", "libraries_ts.r"))
source(here::here("r", "constants.r"))
source(here::here("r", "functions.r"))


# get totals --------------------------------------------------------------

# fn <- "090922 Subsidies for DB.xlsx"
fn <- "090922 Subsidies for DB_djb.xlsx"

df1 <- read_excel(here::here("data", "mta", fn),
                  sheet="subsidies_djb",
                  range="A9:u116")
glimpse(df1)
df1
  	  
idvars <- c("rownum", "rowtype", "revgroup", "revitem", "location", "columnid")
df2 <- df1 |> 
  select(-`...14`) |> 
  pivot_longer(-all_of(idvars)) |> 
  separate(name, into=c("measure", "year")) |> 
  mutate(year=as.integer(year),
         value=as.numeric(value))
df2  

df2 |> 
  filter(year==2016, measure=="accrual") |> 
  group_by(revgroup, rowtype) |> 
  summarise(n=n(), value=sum(value, na.rm = TRUE))

saveRDS(df2, here::here("data", "toallocate_raw.rds"))  

# create collapsed NYC records
toallocate_raw <- readRDS(here::here("data", "toallocate_raw.rds"))
nyc <- toallocate_raw |> 
  filter(revitem %in% c("nycxrichmond", "Richmond")) |> 
  arrange(measure, year, revgroup, revitem) |> 
  group_by(measure, year, revgroup, location) |> 
  summarise(value=sum(value), rownum=mean(rownum), .groups="drop") |> 
  mutate(revitem="NYC", rowtype="detail")

toallocate <- toallocate_raw |>
  bind_rows(nyc) |>
  # put group totals on each record
  group_by(measure, year, revgroup, rowtype) |> 
  mutate(groupsum=sum(value, na.rm=TRUE),
         groupsum=ifelse(groupsum==0, NA_real_, groupsum),
         valshare=ifelse(revgroup %in% c("mrt1", "mrt2") &
                           rowtype=="detail",
                         value / groupsum,
                         NA_real_)) |> 
  ungroup() |> 
  arrange(measure, year, revgroup, rowtype, rownum)
saveRDS(toallocate, here::here("data", "toallocate.rds"))  

# toallocate |> filter(measure=="accrual", year==2019, revgroup=="mrt1", rowtype=="detail")


# get recipes -------------------------------------------------------------
fn <- "090922 Subsidies for DB_djb.xlsx"

recipes1 <- read_excel(here::here("data", "mta", fn),
                  sheet="recipes",
                  range="A2:F109")

saveRDS(recipes1, here::here("data", "recipes.rds"))



# set up allocation -------------------------------------------------------
toallocate <- readRDS(here::here("data", "toallocate.rds"))
recipes <- readRDS(here::here("data", "recipes.rds"))
allocators <- readRDS(here::here("data", "allocators.rds"))



# prepare allocators ------------------------------------------------------
glimpse(allocators)
count(allocators, unifips, uniname)
alloc1 <- allocators |> 
  filter(unifips %in% c(constants$mtafips, constants$totnycfips),
         year %in% 2016:2022,
         allocator != "density")
count(alloc1, unifips, uniname)

allocnyc <- alloc1 |> 
  filter(unifips %in% c(constants$nycfips, constants$totnycfips)) |> # get the NYC totals as well as boros
  # drop details if we have the nyc total
  group_by(allocator, year, yeartype, src) |> 
  filter(!constants$totnycfips %in% unifips) |> 
  summarise(n=n(), value=sum(value), .groups="drop") |> 
  mutate(unifips="3651000", uniname="New York City") |> 
  select(-n) # drop n after inspecting it

alloc2 <- alloc1 |> 
  filter(!unifips %in% constants$nycfips) |> 
  bind_rows(allocnyc)
count(alloc2, unifips, uniname)

count(alloc2, year)

# fill in missing values by carrying forward
stubs <- alloc2 |> 
  select(uniname, unifips, allocator, src, yeartype) |> 
  distinct() |> 
  expand_grid(year=2016:2022)

alloc3 <- stubs |> 
  left_join(alloc2, by = c("uniname", "unifips", "allocator", "src", "yeartype", "year")) |> 
  mutate(missval=ifelse(is.na(value), TRUE, FALSE))
summary(alloc3)

alloc4 <- alloc3 |> 
  group_by(uniname, unifips, allocator) |> 
  arrange(year) |> 
  fill(value, .direction="down") |> 
  ungroup() |> 
  arrange(uniname, unifips, allocator, year)

alloc5 <- alloc4 |> 
  rename(allocval=value) |> 
  group_by(allocator, year) |> 
  mutate(allocshare=allocval / sum(allocval))
summary(alloc5)  


# prepare final allocation data ----------------------------------------------------------
count(alloc4, uniname, unifips, allocator) |> filter(n!=7)
count(alloc4, allocator)

# prepare allocation ------------------------------------------------------
prep1 <- recipes |> 
  filter(!is.na(recipe1)) |> 
  select(rownum, allocator=recipe1) |> 
  left_join(toallocate, by="rownum") |> 
  unite(rev, revgroup, revitem, remove=FALSE)
count(prep1, allocator)


allocation1 <- prep1 |> 
  mutate(allocator=ifelse(allocator=="gas", "pop", allocator)) |> 
  left_join(alloc5, by = c("allocator", "year")) |> 
  mutate(allocshare=ifelse(!is.na(valshare), valshare, allocshare))
glimpse(allocation1)

allocation2 <- allocation1 |> 
  mutate(allocated=value * allocshare)

allocation2 |> 
  filter(measure=="accrual", year==2016, revitem=="mmtoa") |> 
  select(rownum, unifips, uniname, allocator, value, allocshare, allocated)





# OLD BELOW HERE ----------
# locations <- c("NYC-LIRR", "NYC-M/N", "NASSAU", "SUFFOLK", "WESTCHESTER", "PUTNAM", "DUTCHESS", "ORANGE", "ROCKLAND")
df2 <- df1 |> 
  setNames(c("idcolumn", paste0("accrual_", 2016:2022), "junk", paste0("cash_", 2016:2022))) |> 
  select(-junk) |>
  filter(!is.na(idcolumn)) |> 
  mutate(rn=row_number(),
         revcode=as.numeric(str_sub(idcolumn, 1, 6)),
         revname=str_remove(idcolumn, paste0(revcode, " - ")),
         item=case_when(str_detect(revname, "NYC-LIRR") ~ "NYC-LIRR",
                            str_detect(revname, "NYC-M/N") ~ "NYC-M/N",
                            str_detect(revname, "RICHMOND") ~ "Richmond",
                            str_detect(revname, "NASSAU") ~ "Nassau",
                            str_detect(revname, "SUFFOLK") ~ "Suffolk",
                            str_detect(revname, "WESTCHESTER") ~ "Westchester",
                            str_detect(revname, "PUTNAM") ~ "Putnam",
                            str_detect(revname, "DUTCHESS") ~ "Dutchess",
                            str_detect(revname, "ORANGE") ~ "Orange",
                            str_detect(revname, "ROCKLAND") ~ "Rockland",
                            TRUE ~ NA_character_),
         revname=str_remove(revname, paste0(" - ", str_to_upper(item))),
         revgroup=case_when(str_detect(idcolumn, "MMTOA") ~ "mmtoa",
                            str_detect(idcolumn, "PBT") ~ "pbt",
                            str_detect(idcolumn, "MRT 1") | str_detect(idcolumn, "MRT-1") ~ "mrt1",
                            str_detect(idcolumn, "MRT 2") | str_detect(idcolumn, "MRT-2") ~ "mrt2",
                            str_detect(idcolumn, "PBT") ~ "PBT",
                            TRUE ~ NA_character_))

df2 |> select(rn, idcolumn, revcode, revname, item)


# 440001 - STATION MAINT - NYC-LIRR
# 440002 - STATION MAINT - NYC-M/N
# 440003 - STATION MAINT - NASSAU
# 440004 - STATION MAINT - SUFFOLK
# 440005 - STATION MAINT - WESTCHESTER
# 440006 - STATION MAINT - PUTNAM
# 440007 - STATION MAINT - DUTCHESS
# 440008 - STATION MAINT - ORANGE
# 440009 - STATION MAINT - ROCKLAND


df3 <- df2 |> 
  mutate(location=case_when(str_detect_any(revname, locations) ~ "xxx",
                            TRUE ~ NA_character_))

df3 <- df2 |> 
  pivot_longer(-revcode) |> 
  separate(name, into=c("measure", "year")) |> 
  mutate(year=as.integer(year)) |> 
  filter(value != "YearTotal") |> 
  mutate(value=as.numeric(value))
glimpse(df2)
tmp <- count(df2, revcode)

df3 <- 
