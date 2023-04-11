

# setup -------------------------------------------------------------------


source(here::here("r", "libraries.r"))
source(here::here("r", "libraries_ts.r"))
source(here::here("r", "constants.r"))
source(here::here("r", "functions.r"))


# constants ---------------------------------------------------------------

pdir <- here::here("sites", "baseline", "pmtbase")



# get data ----------------------------------------------------------------

# Beginning with tax year 2015, the Department of Taxation and Finance
# (hereafter “the Department”) began producing a new annual population data
# study file to provide more comprehensive statistical information on New York
# State personal income tax returns. The data are from full‐year resident,
# nonresident, and part‐year resident returns filed between January 1 and
# December 31 of the year after the start of the liability period (hereafter
# referred to as the “processing year”). The four datasets display major income
# tax components by tax year. This includes the distribution of New York
# adjusted gross income and tax liability by county or place of residence, as
# well as the value of deductions, exemptions, taxable income and tax before
# credits by size of income. In addition, three of the four datasets include all
# the components of income, the components of deductions, and the
# addition/subtraction modifications. Caution: The current datasets are based on
# population data. For tax years prior to 2015, data were based on sample data.
# Data customers are advised to use caution when drawing conclusions comparing
# data for tax years prior to 2015 and subsequent tax years. Further details are
# included in the Overview.


# Personal Income Tax Filers, Summary Dataset 2 - Major Items and Income & Deduction Components by Place of Residence and Broad Income Range: Beginning Tax Year 2015
# Personal Income Tax Filers, Summary Dataset 3 - Statewide Major Items and Income & Deduction Components by Liability Status and Detail Income Range: Beginning Tax Year 2015
# Personal Income Tax Filers, Summary Dataset 4 - County-level Major Items and Income & Deduction Components by Wide Income Range: Beginning Tax Year 2015


# dataset 2 by POR and broad income range ----
url2 <- "https://data.ny.gov/api/views/f3t7-zvbx/rows.csv?accessType=DOWNLOAD&sorting=true"
ds2 <- read_csv(url2)
glimpse(ds2)
skim(ds2)
# 68 counties, 16 income ranges
# gain & loss from capsupp, rent/roy/partner/est/trusts, gain/loss bus/farm, but no numbers of returns
count(ds2, `NY Adjusted Gross Income Range (Fed.Col)`) # 50-60, 60-75, 75-100
count(ds2, County)

# dataset 3 statewide, detailed income range ----
url3 <- "https://data.ny.gov/api/views/rt8x-r6c8/rows.csv?accessType=DOWNLOAD&sorting=true"
ds3 <- read_csv(url3)
glimpse(ds3)
skim(ds3)
# gain & loss from cappsup, rrpet, busfarminc; number and amount
count(ds3, `NY Adjusted Gross Income Range (Fed.Col)`) # more detail on upper end, $50-100k by $5k

# dataset 4 county, detailed income range ----
url4 <- "https://data.ny.gov/api/views/qjqv-zrwt/rows.csv?accessType=DOWNLOAD&sorting=true"
ds4 <- read_csv(url4)
glimpse(ds4)
skim(ds4)
# gain & loss from cappsup, rrpet, businc, amount and number
count(ds4, `NY Adjusted Gross Income Range (Fed.Col)`) # 50-75, 75-100


# ds3 - statewide, where is netse by size? ----
glimpse(ds3)
cbind(1:ncol(ds3), names(ds3))
ds3.1 <- ds3 |> 
  select(1:4, 6, 11, 12, 21, 24, 25:26, 35:42, 89)
glimpse(ds3.1)
ds3.2 <- ds3.1 |> 
  select(taxyear=`Tax Year`,
         taxstatus=`Tax Liability Status`,
         resstatus=`NYS Residency Status`,
         agirange=`NY Adjusted Gross Income Range (Fed.Col)`,
         numret=`Number of Returns`,
         nyagi=`New York State Amount of NY Adjusted Gross Income`,
         nwages=`Number with Wage and Salary Income`,
         wages=`Wage and Salary Income`,
         ngrrpet=`Number with Gain Rent,Royalties,Prtnrshp,Estates,Trusts Income`,
         grrpet=`Gain from Rent,Royalties,Prtnrshp,Estates,Trusts Income`,
         nlrrpet=`Number with Loss from Rent,Royalties,Prtnrshp,Estates,Trusts Income`,
         lrrpet=`Loss from Rent,Royalties,Prtnrshp,Estates,Trusts Income`,
         ngbusinc=`Number with Gain from Business & Farm Income`,
         gbusinc=`Gain from Business & Farm Income`,
         nlbusinc=`Number with Loss from Business & Farm Income`,
         lbusinc=`Loss from Business & Farm Income`,
         ysort=`Sort on Income Range`)
skim(ds3.2)

count(ds3.2, taxyear) # 2015-2020
count(ds3.2, taxstatus)
# taxstatus       n
#   1 All Returns   672
# 2 Non-Taxable   672
# 3 Taxable       672
count(ds3.2, resstatus)
# resstatus                 n
#   1 All Filers              504
# 2 Full-Year Nonresident   504
# 3 Full-Year Resident      504
# 4 Part-Year Resident      504

## subset ----
ds3f1 <- ds3.2 |> 
  filter(taxstatus=="All Returns", resstatus=="All Filers")

tabbase1 <- ds3f1 |> 
  filter(taxyear==2020) |> 
  arrange(ysort) |> 
  select(ysort, agirange, numret, nyagi, ngrrpet, grrpet, nlrrpet, lrrpet, ngbusinc, gbusinc, nlbusinc, lbusinc)

# collapse tabbase1
count(tabbase1, ysort, agirange)
 
tabbase2 <- tabbase1 |> 
  mutate(# oysort=ysort, # original
         agirange=case_when(ysort %in% 1:10 ~ "Under $50,000",
                            ysort %in% 24:27 ~ "$1,000,000 and Over",
                            TRUE ~ agirange),
         ysort=case_when(ysort %in% 1:10 ~ 1,
                         ysort %in% 24:27 ~ 24,
                         TRUE ~ ysort)) |> 
  summarise(across(numret:lbusinc, sum),
            .by=c(ysort, agirange))

saveRDS(tabbase2, path(pdir, "ds3base.rds"))



# table 1 basic data -- 
tab <- tabbase2 |> 
  mutate(lrrpet=-lrrpet, lbusinc=-lbusinc) |> 
  gt() |> 
  cols_hide(ysort) |> 
  tab_header(
    title = html("Statewide potential self employment income by AGI group in 2020 (DTF dataset 3)"),
    subtitle = html("Amounts are in $ millions")
  ) |>
  # cols_label(
  #   egroup2="Employment size group",
  #   emplb2="Employment lower bound",
  #   empub2="Employment upper bound",
  #   mta="MTA SIMPLE MEAN (CAUTION)"
  # ) |> 
  # fmt_percent(columns=-c(fips, area, sum),
  #            decimals=1) |> 
  tab_spanner(columns = c(ngrrpet, nlrrpet, ngbusinc, nlbusinc),
              label="Numbers with gain or loss") |> 
  tab_spanner(columns = c(grrpet, lrrpet, gbusinc, lbusinc),
              label="Amount of gain or loss ($ millions)") |> 
  fmt_number(columns=c(numret, starts_with("ng"), starts_with("nl")),
             decimals=0) |> 
  fmt_number(columns=c(starts_with("ny"),
                       starts_with("g"),
                       starts_with("l")),
             scale=1e-6,
             decimals=0) |> 
  fmt_number(columns=c(starts_with("ny"),
                       starts_with("g"),
                       starts_with("l")),
             rows=c(1, nrow(.data)),
             pattern = "${x}",
             scale=1e-6,
             decimals=0) |> 
  #gt_theme_guardian() |> 
  tab_style(
    style = cell_fill(color = "grey96"),
    locations = cells_body(rows = even(1:nrow(tabbase2))) # even(1:nrow(.data))
  )
  # gt_theme_guardian()

tab



# table 2 averages -- 
tab <- tabbase2 |> 
  mutate(lrrpet=-lrrpet, lbusinc=-lbusinc) |> 
  mutate(grrpetavg=grrpet / ngrrpet,
         lrrpetavg=lrrpet / nlrrpet,
         gbusincavg=gbusinc / ngbusinc,
         lbusincavg=lbusinc / nlbusinc) |> 
  select(agirange, numret, nyagi, contains("avg")) |> 
  gt() |> 
  tab_header(
    title = html("Statewide potential self employment income by AGI group in 2020 (DTF dataset 3)"),
    subtitle = html("Average per return with gain or loss, $")
  ) |>
  fmt_number(columns=numret,
             decimals=0) |> 
  fmt_number(columns=-c(agirange, numret),
             decimals=0) |> 
  fmt_number(columns=c(),
             rows=c(1, nrow(.data)),
             pattern = "${x}",
             decimals=0) |> 
  tab_style(
    style = cell_fill(color = "grey96"),
    locations = cells_body(rows = even(1:nrow(tabbase2))) # even(1:nrow(.data))
  )
# gt_theme_guardian()

tab

# ds4 - county ----
glimpse(ds4)
skim(ds4)
# gain & loss from cappsup, rrpet, businc, amount and number
count(ds4, `NY Adjusted Gross Income Range (Fed.Col)`) # 50-75, 75-100
count(ds4, County)
# 

cbind(1:ncol(ds4), names(ds4))
ds3.1 <- ds3 |> 
  select(1:4, 6, 11, 12, 21, 24, 25:26, 35:42, 92)
glimpse(ds3.1)
ds4.2 <- ds4 |> 
  select(taxyear=`Tax Year`,
         resstatus=`NYS Residency Status`,
         agirange=`NY Adjusted Gross Income Range (Fed.Col)`,
         numret=`Number of Returns`,
         nyagi=`New York State Amount of NY Adjusted Gross Income`,
         nwages=`Number with Wage and Salary Income`,
         wages=`Wage and Salary Income`,
         ngrrpet=`Number with Gain Rent,Royalties,Prtnrshp,Estates,Trusts Income`,
         grrpet=`Gain from Rent,Royalties,Prtnrshp,Estates,Trusts Income`,
         nlrrpet=`Number with Loss from Rent,Royalties,Prtnrshp,Estates,Trusts Income`,
         lrrpet=`Loss from Rent,Royalties,Prtnrshp,Estates,Trusts Income`,
         ngbusinc=`Number with Gain from Business & Farm Income`,
         gbusinc=`Gain from Business & Farm Income`,
         nlbusinc=`Number with Loss from Business & Farm Income`,
         lbusinc=`Loss from Business & Farm Income`,
         ysort=`Sort on Income Range`)
skim(ds3.2)



saveRDS(xxx, path(pdir, "ds4base.rds"))



