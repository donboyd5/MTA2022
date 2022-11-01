

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
                      range="A4:D23") |> 
  filter(!is.na(vname))
recipes1
recipes1 |> filter(recipe1 != recipe2)

recipes <-  recipes1 |> 
  pivot_longer(cols=starts_with("recipe"),
               names_to = "recipe",
               values_to = "allocator") |> 
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

# check availabilities
setdiff(unique(recipes$vname), unique(toallocate$vname)) # good - all vnames with recipes are in the data
setdiff(unique(recipes$allocator), unique(mtashares$allocator))
# we need "taxicab_value"    "autorental_value" -- they go to NYC


result1 <- recipes |> 
  filter(allocator != "split") |> 
  select(-src) |> 
  left_join(toallocate |> select(-columnid) |> rename(totvalue=value), by="vname") |> 
  left_join(mtashares |> select(unifips, uniname, year, allocator, allocshare, pop),
            by=c("allocator", "year")) |> 
  mutate(allocated=totvalue * allocshare,
         rectype="detail")
summary(result1)


# create summary records ----

mctd_sums <- result1 |> 
  group_by(recipe, vname, allocator, measure, year) |> 
  summarise(totvalue=first(totvalue),
            allocated=sum(allocated, na.rm=TRUE),
            pop=sum(pop),
            .groups="drop") |> 
  mutate(unifips="36xx1",
         uniname="MCTD",
         rectype="mta")

suburb_sums <- result1 |> 
  filter(!str_detect(uniname, "New York City")) |> 
  group_by(recipe, vname, allocator, measure, year) |> 
  summarise(totvalue=first(totvalue),
            allocated=sum(allocated, na.rm=TRUE),
            pop=sum(pop),
            .groups="drop") |> 
  mutate(unifips=NA_character_, uniname="Suburban", rectype="suburban")

li_sums <- result1 |> 
  filter(uniname %in% c("Nassau", "Suffolk")) |> 
  group_by(recipe, vname, allocator, measure, year) |> 
  summarise(totvalue=first(totvalue),
            allocated=sum(allocated, na.rm=TRUE),
            pop=sum(pop),
            .groups="drop") |> 
  mutate(unifips=NA_character_, uniname="Long Island", rectype="li")

westhud_sums <- result1 |> 
  filter(uniname %in% c("Orange", "Rockland")) |> 
  group_by(recipe, vname, allocator, measure, year) |> 
  summarise(totvalue=first(totvalue),
            allocated=sum(allocated, na.rm=TRUE),
            pop=sum(pop),
            .groups="drop") |> 
  mutate(unifips=NA_character_, uniname="West of Hudson", rectype="wh")

easthud_sums <- result1 |> 
  filter(uniname %in% c("Dutchess", "Putnam", "Westchester")) |> 
  group_by(recipe, vname, allocator, measure, year) |> 
  summarise(totvalue=first(totvalue),
            allocated=sum(allocated, na.rm=TRUE),
            pop=sum(pop),
            .groups="drop") |> 
  mutate(unifips=NA_character_, uniname="East of Hudson", rectype="eh")

metronorth_sums <- result1 |> 
  filter(uniname %in% c("Orange", "Rockland", "Dutchess", "Putnam", "Westchester")) |> 
  group_by(recipe, vname, allocator, measure, year) |> 
  summarise(totvalue=first(totvalue),
            allocated=sum(allocated, na.rm=TRUE),
            pop=sum(pop),
            .groups="drop") |> 
  mutate(unifips=NA_character_, uniname="Metro North", rectype="mn")


# create the combined geography file ----
combo1 <- bind_rows(result1,
                    mctd_sums,
                    suburb_sums,
                    li_sums,
                    westhud_sums,
                    easthud_sums,
                    metronorth_sums)
count(combo1, rectype, unifips, uniname)

# contstruct aggregated variables ----
combo1

agg_f <- function(aggname, vnames){
  agg <- combo1 |> 
    filter(vname %in% vnames) |> 
    group_by(recipe, rectype, measure, year, unifips, uniname, pop) |> 
    summarise(totvalue=sum(totvalue, na.rm=TRUE), allocated=sum(allocated, na.rm=TRUE), .groups="drop") |> 
    mutate(vname=aggname, allocator="combined", allocshare=allocated / totvalue)
  agg
}

count(combo1, vname)

mrt_agg <- agg_f("mrt", c("mrt1", "mrt2"))
pbt_agg <- agg_f("pbtall", c("pbt", "pbt_mmtoa"))
mmtoa_agg <- agg_f("mmtoa", c("franchise_mmtoa", "fransurcharge_mmtoa", "pbt_mmtoa", "sut_mmtoa"))
mtaaid_agg <- agg_f("mtaaid", c("autorental_mtaaid", "licensereg_mtaaid", "taxicab_mtaaid"))

optaxes <- c("pmt", 
             "mrt1", "mrt2",
             "pbt",
             "franchise_mmtoa", "fransurcharge_mmtoa", "pbt_mmtoa", "sut_mmtoa",
             "autorental_mtaaid", "licensereg_mtaaid", "taxicab_mtaaid",
             "urban") # don't include mansion or sutinternet
optax_agg <- agg_f("optaxsum", optaxes)

comboagg1 <- bind_rows(combo1, mrt_agg, pbt_agg, mmtoa_agg, mtaaid_agg, optax_agg)

# check whether the optaxagg is correct
check <- comboagg1 |> 
  filter(vname %in% c(optaxes, "optaxsum"),
         measure=="accrual",
         rectype=="detail",
         year==2017, 
         recipe=="recipe1") |> 
  select(vname, unifips, uniname, allocated) |> 
  mutate(allocated=allocated / 1e6) |> 
  pivot_wider(names_from = vname, values_from = allocated) |> 
  mutate(check= rowSums(across(-c(unifips, uniname, optaxsum))))

# calculate variables ----

comboagg2 <- comboagg1 |>
  group_by(recipe, measure, vname, year) |>
  mutate(allocpc=allocated / pop,
         ipcmta=allocpc / allocpc[rectype=="mta"],
         pctofmta=allocated / allocated[rectype=="mta"]) |>
  ungroup() |>
  # place vars in order of most to least aggregated
  select(recipe, measure, rectype, unifips, uniname,
         vname, allocator, year, pop, totvalue, allocshare, allocated, allocpc, ipcmta, pctofmta) |> 
  arrange(recipe, measure, rectype, unifips, uniname, vname, allocator, year)

# quick check
comboagg2 |> 
  filter(recipe=="recipe1", measure=="accrual", vname=="mrt", year==2021) |> 
  select(unifips, uniname, allocated, allocpc, ipcmta, pctofmta)

# put potential sort orders on the file ----
glimpse(comboagg2)
count(comboagg, rectype, unifips, uniname)

## geoorder ----
comboagg3 <- comboagg2 |> 
  filter(rectype!="nyc") |> 
  mutate(rectype_order=case_when(rectype=="detail" ~ 1,
                                 rectype=="mta" ~ 2,
                                 rectype=="suburban" ~ 3,
                                 rectype=="li" ~ 4,
                                 rectype=="mn" ~ 5,
                                 rectype=="eh" ~ 6,
                                 rectype=="wh" ~ 7,
                                 TRUE ~ -99),
         detail_order=case_when(rectype=="detail" &
                                  uniname=="New York City" ~ 9,
                                rectype=="detail" &
                                  uniname!="New York City" ~ 1,
                                TRUE ~ 1),
         geo_order=10 * rectype_order + detail_order)

count(comboagg3, rectype_order, detail_order, uniname)
count(comboagg3, geo_order, uniname)

## varsort orders ----
count(comboagg3, vname)

# put each vector in the order desired for printing
optax_detail <- c("pmt", 
             "mrt1", "mrt2",
             "urban",
             "pbt",
             "franchise_mmtoa", "fransurcharge_mmtoa", "pbt_mmtoa", "sut_mmtoa",
             "autorental_mtaaid", "licensereg_mtaaid", "taxicab_mtaaid",
             "optaxsum") # don't include mansion or sutinternet

optax_summary <- c("pmt", 
                  "mrt",
                  "urban",
                  "pbtall",
                  "franchise_mmtoa", "fransurcharge_mmtoa", "sut_mmtoa",
                  "autorental_mtaaid", "licensereg_mtaaid", "taxicab_mtaaid",
                  "optaxsum") # don't include mansion or sutinternet

tottax_detail <- c(optax_detail, "mansion", "sutinternet")
tottax_summary <- c(optax_summary, "mansion", "sutinternet")

comboaggvar1 <- comboagg3 |> 
  mutate(foptax_detail=factor(vname, levels=optax_detail),
         foptax_summary=factor(vname, levels=optax_summary),
         ftottax_detail=factor(vname, levels=tottax_detail),
         ftottax_summary=factor(vname, levels=tottax_summary))

count(comboaggvar1, foptax_detail)
count(comboaggvar1, foptax_summary)
count(comboaggvar1, ftottax_detail)
count(comboaggvar1, ftottax_summary)

saveRDS(comboaggvar1, here::here("results", "allocation_results.rds"))


# combo1 |> 
#   filter(recipe=="recipe1", rectype=="detail", measure=="accrual", year==2017,
#          unifips=="36059", str_detect(vname, "mrt"))
# 
# mrt_agg |> 
#   filter(recipe=="recipe1", rectype=="detail", measure=="accrual", year==2017,
#          unifips=="36059", str_detect(vname, "mrt"))

# tmp <- readRDS(here::here("results", "allocation_results.rds"))
# saveRDS(tmp, here::here("results", "allocation_results_2022-09-21.rds"))

# checks ----
# summary(result2)
# count(result2, rectype)
# count(result2, unifips, uniname)
# count(result2, measure)
# count(result2, year)

## totals ----
res <- readRDS(here::here("results", "allocation_results.rds"))


tots <- res |> 
  filter(recipe=="recipe1", rectype=="detail") |> 
  group_by(vname, measure, year) |> 
  summarise(totvalue=first(totvalue), sumvalues=sum(allocated, na.rm=TRUE),
            .groups="drop") |> 
  mutate(diff=sumvalues - totvalue, pdiff=diff / totvalue)
tots |> filter(diff != 0)  # good, differences are miniscule

## laugh test, specific taxes ----
result2 <- comboaggvar1
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


