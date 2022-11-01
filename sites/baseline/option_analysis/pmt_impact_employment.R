

# libraries ---------------------------------------------------------------
source(here::here("r", "libraries.r"))
source(here::here("r", "constants.r"))
source(here::here("r", "functions.r"))


# locations ---------------------------------------------------------------

qdir <- r"(E:\data\BLSData\qcew)"
dldir <- path(qdir, "quarterly_single") # r"(E:\data\BLSData\qcew\quarterly_single)"
docdir <- path(qdir, "documentation")

# constants ---------------------------------------------------------------


# get selected documentation ----------------------------------------------
areas <- read_csv(path(docdir, "area-titles-csv.csv"),
                  col_types = cols(.default = col_character())) |> 
  mutate(area_fips=str_pad(area_fips, width=5, side="left", pad = "0"))

inds <- read_csv(path(docdir, "industry-titles-csv.csv"),
                 col_types = cols(.default = col_character()))


# define counties of interest ---------------------------------------------

# counties adjacent to MTA
adjacent <- read_csv("
nonmta, nonmtast, nonmtafips
Fairfield, CT, 09001
Litchfield, CT, 09005

Bergen, NJ, 34003
Essex, NJ, 34013
Hudson, NJ, 34017
Middlesex, NJ, 34023
Passaic, NJ, 34031
Sussex, NJ, 34037
Union, NJ, 34039

Columbia, NY, 36021
Sullivan, NY, 36105
Ulster, NY, 36111

Pike, PA, 42103")

# areas |> filter(str_detect(area_title, "Ulster County, New York"))
# areas |> filter(area_fips %in% adjacent$nonmtafips)
saveRDS(adjacent, here::here("data", "qcew", "adjacent.rds"))

pairs1 <- read_csv("
mta, nonmta, nonmtast, nonmtafips
Dutchess, Litchfield, CT, 09005
Orange,	Passaic, NJ, 34031
Orange,	Pike, PA, 42103
Orange,	Sussex, NJ, 34037
Orange,	Ulster, NY, 36111
Putnam, Fairfield, CT, 09001
Richmond, Middlesex, NJ, 34023
Rockland,	Bergen, NJ, 34003
Rockland,	Passaic, NJ, 34031
Westchester, Fairfield, CT, 09001")


# initial data prep -------------------------------------------------------

# qraw has private sector all-industries employment for CT, NJ, NY, PA
df1 <- readRDS(here::here("data", "qcew", "qraw.rds"))

df1 |> filter(area_fips=="36001") |> ht() # Albany county
count(df1, disclosure_code)
df1 |> filter(disclosure_code=="N")

## put area and industry names on the file ----
df2 <- df1 |>
  mutate(stfips=str_sub(area_fips, 1, 2),
         across(c(agglvl_code, year, qtr, qtrly_estabs), as.integer),
         across(c(starts_with("month"), total_qtrly_wages), as.numeric),
         date=yq2(year, qtr)) |> 
  left_join(areas, by = "area_fips") |> 
  left_join(stcodes |> select(stfips, stabbr), by="stfips")

# df2 |> filter(area_fips=="36000", year==2019, qtr==1) # NYS

## get quarterly mean employment, spruce up names ----
empbase <- df2 |>
  select(stabbr, afips=area_fips, area=area_title, agglvl=agglvl_code,
         date, 
         estabs=qtrly_estabs, wages=total_qtrly_wages,
         starts_with("month")) |> 
  mutate(emp = rowMeans(across(starts_with("month"), na.rm=TRUE)),
         avgwage=wages * 4 / emp,
         atype=case_when(str_detect(area, "Statewide") ~ "state",
                         str_detect(area, "County") ~ "county",
                         TRUE ~ "other"),
         area=paste0(str_remove(area, " -- Statewide"), " State"),
         area=str_extract_before_first(area, ","))

count(empbase, atype)

empbase |> filter(atype=="other")
count(df4 |> filter(atype=="other"), area)

empbase |> filter(afips %in% constants$mtafips)
saveRDS(empbase, here::here("data", "qcew", "empbase.rds"))


# Panel plot: MTA and neighboring non-MTA counties ------------------------------------

# empbase |> 
#   filter(fips=="36027") |> 
#   ggplot(aes(date, emp)) +
#   geom_line()

# get trend revenue for each county

keepcos <- c(constants$mtafips, pairs1$nonmtafips)
count(empbase |> filter(afips %in% keepcos), afips, area)

cotrend <- empbase |> 
  filter(afips %in% keepcos) |> 
  group_by(area) |> 
  # get trend
  dplyr::arrange(date) %>% # BE SURE DATA HAVE BEEN SORTED BY DATE WITHIN GROUPING VARS!!!
  do(cbind(., stldf(.$emp, 4))) |> 
  # mutate(empsa=sa(emp, freq=4, s.window = 9)) |>
  mutate(iemp=emp / emp[date=="2009-04-01"],
         ietrend=trend / trend[date=="2009-04-01"]) |> 
  ungroup()


# Westchester trend and actual data
px <- cotrend |> 
  filter(afips=="36027") |> 
  select(date, emp, trend) |> 
  pivot_longer(cols=-date) |> 
  mutate(namef=factor(name, levels=c("emp", "trend"), labels=c("actual", "trend"))) |> 
  # mutate(namef=ifelse(name=="emp", "actual", "trend")) |> 
  ggplot(aes(date, value, colour=namef, size=namef)) +
  geom_line() +
  scale_size_manual(values=c(0.5, 1.5)) +
  geom_point(size=0.5) +
  scale_x_date(name=NULL, 
               date_breaks = "5 years",
               date_labels = "%Y") +
  scale_y_continuous(name="# employed",
                     breaks=seq(0, 1e6, 10e3)) +
  scale_colour_manual(values=c("grey30", "blue")) +
  ggtitle("Example: Actual and trend private-sector employment, Westchester County") +
  theme_bw() +
  theme(legend.title=element_blank())
px

ggsave(here::here("sites", "baseline", "results", "example_trend.png"),
       px,
       width=10, height=8,
       scale=1)



df1 <- readRDS(here::here("data", "qcew", "qraw.rds"))
glimpse(df1)
count(df1, disclosure_code)
count(df1, agglvl_code)

tmp <- areas |> select(area_fips, area_title) |> distinct()

df1 |> filter(area_fips=="36001") |> ht()

df2 <- df1 |> 
  select(-c(disclosure_code)) |> 
  mutate(across(c(agglvl_code, year, qtr, qtrly_estabs), as.integer),
         across(c(starts_with("month"), total_qtrly_wages), as.numeric),
         date=yq2(year, qtr)) |> 
  select(area_fips, agglvl_code, date, estabs=qtrly_estabs, wages=total_qtrly_wages,
         starts_with("month"))

df3 <- df2 |> 
  pivot_longer(starts_with("month")) |> 
  group_by(area_fips, agglvl_code, date, estabs) |> 
  summarize(emp=mean(value, na.rm=TRUE), .groups = "drop") |> 
  mutate(stfips=str_sub(area_fips, 1, 2))


# check
df3 |> 
  group_by(area_fips, date) |> 
  summarise(n=n(), .groups = "drop") |> 
  filter(n > 1)

df4 <- df3 |> 
  left_join(areas, by = "area_fips") |> 
  left_join(stcodes |> select(stfips, stabbr), by="stfips") |> 
  select(stabbr, fips=area_fips, area=area_title, date, estabs, emp) |> 
  mutate(area=ifelse(str_detect(area, "Statewide"), "NYS", area),
         area=str_extract_before_first(area, ","))




df4 |> 
  filter(fips=="36119") |> 
  ggplot(aes(date, emp)) +
  geom_line()

df5 <- df4 |> 
  filter(!is.na(area)) |> 
  group_by(area) |> 
  # get trend
  dplyr::arrange(date) %>% # BE SURE DATA HAVE BEEN SORTED BY DATE WITHIN GROUPING VARS!!!
  do(cbind(., stldf(.$emp, 4))) |> 
  # mutate(empsa=sa(emp, freq=4, s.window = 9)) |>
  mutate(iemp=emp / emp[date=="2009-04-01"],
         ietrend=trend / trend[date=="2009-04-01"]) |> 
  ungroup()

df5

# Westchester trend and actual data
px <- cotrend |> 
  filter(afips=="36119") |> 
  select(date, emp, trend) |> 
  pivot_longer(cols=-date) |> 
  mutate(namef=factor(name, levels=c("emp", "trend"), labels=c("actual", "trend"))) |> 
  # mutate(namef=ifelse(name=="emp", "actual", "trend")) |> 
  ggplot(aes(date, value, colour=namef, size=namef)) +
  geom_line() +
  scale_size_manual(values=c(0.5, 1.5)) +
  geom_point(size=0.5) +
  scale_x_date(name=NULL, 
               date_breaks = "5 years",
               date_labels = "%Y") +
  scale_y_continuous(name="# employed",
                     breaks=seq(0, 1e6, 10e3)) +
  scale_colour_manual(values=c("grey30", "blue")) +
  ggtitle("Example: Actual and trend private-sector employment, Westchester County") +
  theme_bw() +
  theme(legend.title=element_blank())
  
ggsave(here::here("sites", "baseline", "results", "example_trend.png"),
       px,
       width=10, height=8,
       scale=1)



# Define neighbors
# neighbors
qfips <- areas |> 
  select(area_fips, area_title) |>
  mutate(stfips=str_sub(area_fips, 1, 2)) |> 
  left_join(stcodes |> select(stfips, stabbr), by="stfips") |> 
  filter(stabbr %in% c("CT", "NJ", "NY", "PA"))


area <- "Litchfield"
qfips[str_detect(qfips$area_title, area), ]

pairs1 <- read_csv("
mta, nonmta, nonmtast, nonmtafips
Dutchess, Litchfield, CT, 09005
Orange,	Passaic, NJ, 34031
Orange,	Pike, PA, 42103
Orange,	Sussex, NJ, 34037
Orange,	Ulster, NY, 36111
Putnam, Fairfield, CT, 09001
Richmond, Middlesex, NJ, 34023
Rockland,	Bergen, NJ, 34003
Rockland,	Passaic, NJ, 34031
Westchester, Fairfield, CT, 09001")

pairs <- pairs1 |> 
  mutate(pairnum=row_number(),
         mtafips=constants$mtacos_to_fips(pairs1$mta)) |> 
  relocate(pairnum, .before = mta)
pairs

plong <- pairs |> 
  # select(-nonmtast) |> 
  pivot_longer(contains("fips"), names_to="rectype", values_to = "fips") |> 
  mutate(rectype=str_remove(rectype, "fips"),
         nonmta=ifelse(nonmtast != "NY", paste0(nonmta, ", ", nonmtast), nonmta),
         name=ifelse(rectype=="mta", 
                     paste0(mta, " (MTA)"),
                     paste0(nonmta, " (non-MTA)"))) |> 
  select(pairnum, rectype, fips, name)
plong


int <- lubridate::interval(ymd("2006-04-01"), ymd("2012-04-01"))

df6 <- plong |> 
  left_join(df5, by="fips")

pf <- function(pairnum){
  df6 |>
    filter(pairnum==!!pairnum) |> 
    filter(date %within% int) |> 
    group_by(name) |> 
    mutate(itrend=trend / trend[date == "2009-04-01"] * 100) |> 
    ungroup() |> 
    arrange(rectype, name) |> 
    mutate(name=fct_reorder(name, rectype, .fun = first)) |> 
    ggplot(aes(date, itrend, colour=name, size=name)) +
    geom_line() +
    geom_point(size=0.5) +
    scale_colour_manual(values=c("blue", "darkgreen")) +
    scale_size_manual(values=c(1.25, .75)) +
    geom_vline(xintercept = as.Date("2009-04-01")) +
    geom_hline(yintercept = 100) +
    # ggtitle(title=NULL,
    #         subtitle=NULL) +
    scale_x_date(name=NULL, 
                 date_breaks = "1 year",
                 date_labels = "%Y") +
    scale_y_continuous((name="April-June 2009=100"),
                       breaks=seq(50, 200, 1)) +
    theme_bw() +
    theme(legend.title=element_blank())
}


# pf(3)

p1 <- 
  (pf(1) + pf(2) + pf(3)) /
  (pf(4) + pf(5) + pf(6)) /
  (pf(7) + pf(8) + pf(9))

# p1 <- 
#   (pf(1) + pf(2)) /
#   (pf(4) + pf(5)) /
#   (pf(6) + pf(7)) /
#   (pf(8) + pf(9))

p <- p1  + 
  plot_annotation(title ="Private sector employment in an MTA county and a neighboring non-MTA county",
                  subtitle="Trend values: 3 years before and after payroll mobility tax, indexed to start of PMT",
                  caption="U.S. Bureau of Labor Statistics, Quarterly Census of Employment and Wages")
p
ggsave(here::here("sites", "baseline", "results", "pmt_impacts_facets.png"),
       p,
       width=16, height=8,
       scale=1.0)


# layout ----
# https://www.bls.gov/cew/about-data/downloadable-file-layouts/quarterly/naics-based-quarterly-layout.htm
# columns 1-21 most important
# NOTE: "single" files do not include fields 9-13

# QCEW CSV Quarterly Files Field Table
# Field
# Number	Field
# Name	Data
# Type	Maximum
# Length	Field
# Description
# 1	area_fips	Text	5	5-character FIPS code
# 2	own_code	Text	1	1-character ownership code
# 3	industry_code	Text	6	6-character industry code (NAICS, SuperSector)
# 4	agglvl_code	Text	2	2-character aggregation level code
# 5	size_code	Text	1	1-character size code
# 6	year	Text	4	4-character year
# 7	qtr	Text	1	1-character quarter (always A for annual)
# 8	disclosure_code	Text	1	1-character disclosure code (either ' '(blank) or 'N' not disclosed)
# 9	area_title	Text	80	Multi-character area title associated with the area's FIPS code (Excluded from singlefile)
# 10	own_title	Text	80	Multi-character ownership title associated with the ownership code (Excluded from singlefile)
# 11	industry_title	Text	80	Multi-character industry title associated with the industry code (Excluded from singlefile)
# 12	agglvl_title	Text	80	Multi-character aggregation title associated with the agglvl code (Excluded from singlefile)
# 13	size_title	Text	80	Multi-character size title associated with the size code (Excluded from singlefile)
# 14	qtrly_estabs	Numeric	8	Count of establishments for a given quarter
# 15	month1_emplvl	Numeric	9	Employment level for the first month of a given quarter
# 16	month2_emplvl	Numeric	9	Employment level for the second month of a given quarter
# 17	month3_emplvl	Numeric	9	Employment level for the third month of a given quarter
# 18	total_qtrly_wages	Numeric	15	Total wages for a given quarter
# 19	taxable_qtrly_wages	Numeric	15	Taxable wages for a given quarter
# 20	qtrly_contributions	Numeric	13	Quarterly contributions for a given quarter
# 21	avg_wkly_wage	Numeric	8	Average weekly wage for a given quarter
# 22	lq_disclosure_code	Text	1	1-character location-quotient disclosure code (either ' '(blank) or 'N' not disclosed
# 23	lq_qtrly_estabs	Numeric	8	Location quotient of the quarterly establishment count relative to the U.S. (Rounded to the hundredths place)
# 24	lq_month1_emplvl	Numeric	8	Location quotient of the employment level for the first month of a given quarter relative to the U.S. (Rounded to the hundredths place)
# 25	lq_month2_emplvl	Numeric	8	Location quotient of the employment level for the second month of a given quarter relative to the U.S. (Rounded to the hundredths place)
# 26	lq_month3_emplvl	Numeric	8	Location quotient of the employment level for the third month of a given quarter relative to the U.S. (Rounded to the hundredths place)
# 27	lq_total_qtrly_wages	Numeric	8	Location quotient of the total wages for a given quarter relative to the U.S. (Rounded to the hundredths place)
# 28	lq_taxable_qtrly_wages	Numeric	8	Location quotient of the total taxable wages for a given quarter relative to the U.S. (Rounded to the hundredths place)
# 29	lq_qtrly_contributions	Numeric	8	Location quotient of the total contributions for a given quarter relative to the U.S. (Rounded to the hundredths place)
# 30	lq_avg_wkly_wage	Numeric	8	Location quotient of the average weekly wage for a given quarter relative to the U.S. (Rounded to the hundredths place)
# 31	oty_disclosure_code	Text	1	1-character over-the-year disclosure code (either ' '(blank) or 'N' not disclosed)
# 32	oty_qtrly_estabs_chg	Numeric	8	Over-the-year change in the count of establishments for a given quarter
# 33	oty_qtrly_estabs_pct_chg	Numeric	8	Over-the-year percent change in the count of establishments for a given quarter (Rounded to the tenths place)
# 34	oty_month1_emplvl_chg	Numeric	9	Over-the-year change in the first month's employment level of a given quarter
# 35	oty_month1_emplvl_pct_chg	Numeric	8	Over-the-year percent change in the first month's employment level of a given quarter (Rounded to the tenths place)
# 36	oty_month2_emplvl_chg	Numeric	9	Over-the-year change in the second month's employment level of a given quarter
# 37	oty_month2_emplvl_pct_chg	Numeric	8	Over-the-year percent change in the second month's employment level of a given quarter (Rounded to the tenths place)
# 38	oty_month3_emplvl_chg	Numeric	9	Over-the-year change in the third month's employment level of a given quarter
# 39	oty_month3_emplvl_pct_chg	Numeric	8	Over-the-year percent change in the third month's employment level of a given quarter (Rounded to the tenths place)
# 40	oty_total_qtrly_wages_chg	Numeric	15	Over-the-year change in total quarterly wages for a given quarter
# 41	oty_total_qtrly_wages_pct_chg	Numeric	8	Over-the-year percent change in total quarterly wages for a given quarter (Rounded to the tenths place)
# 42	oty_taxable_qtrly_wages_chg	Numeric	15	Over-the-year change in taxable quarterly wages for a given quarter
# 43	oty_taxable_qtrly_wages_pct_chg	Numeric	8	Over-the-year percent change in taxable quarterly wages for a given quarter (Rounded to the tenths place)
# 44	oty_qtrly_contributions_chg	Numeric	13	Over-the-year change in quarterly contributions for a given quarter
# 45	oty_qtrly_contributions_pct_chg	Numeric	8	Over-the-year percent change in quarterly contributions for a given quarter (Rounded to the tenths place)
# 46	oty_avg_wkly_wage_chg	Numeric	8	Over-the-year change in average weekly wage for a given quarter
# 47	oty_avg_wkly_wage_pct_chg	Numeric	8	Over-the-year percent change in average weekly wage for a given quarter (Rounded to the tenths place)
# Last Modified Date: September 24, 2020

