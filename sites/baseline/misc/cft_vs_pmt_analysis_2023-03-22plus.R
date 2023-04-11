
# libraries and constants -------------------------------------------------

source(here::here("r", "libraries.r"))
source(here::here("r", "libraries_ts.r"))
source(here::here("r", "constants.r"))
source(here::here("r", "functions.r"))



# get and save corporate profits and wages -------------------------------------


# U.S. Bureau of Economic Analysis, Total wages and salaries, BLS
# [BA06RC1A027NBEA], retrieved from FRED, Federal Reserve Bank of St. Louis;
# https://fred.stlouisfed.org/series/BA06RC1A027NBEA, March 10, 2023.

# U.S. Bureau of Economic Analysis, Corporate business: Profits before tax
# (without IVA and CCAdj) [A446RC1Q027SBEA], retrieved from FRED, Federal
# Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/A446RC1Q027SBEA,
# March 10, 2023.

# U.S. Bureau of Economic Analysis, Compensation of Employees: Wages and Salary
# Accruals [WASCUR], retrieved from FRED, Federal Reserve Bank of St. Louis;
# https://fred.stlouisfed.org/series/WASCUR, March 10, 2023.

# U.S. Bureau of Economic Analysis, Gross Domestic Product: Chain-type Price
# Index [GDPCTPI], retrieved from FRED, Federal Reserve Bank of St. Louis;
# https://fred.stlouisfed.org/series/GDPCTPI, March 10, 2023.

# all are quarterly
pbtx <- fredr(series_id = "A446RC1Q027SBEA")
wages <- fredr(series_id = "WASCUR")
price <- fredr(series_id = "GDPCTPI")

stack <- bind_rows(
  pbtx |> mutate(vname="profits"),
  wages |> mutate(vname="wages")) |> 
  left_join(price |> select(date, gdppi=value), by = join_by(date)) |> 
  mutate(rvalue=value * 129.502 / gdppi)

saveRDS(stack, here::here("sites", "baseline", "misc", "pbtxwages.rds"))


# get and save cft and pmt data ----------------------------------------------------------------

taxmonthly <- readRDS(here::here("data", "dtf", "taxmonthly.rds"))
glimpse(taxmonthly)
skim(taxmonthly)
tmp <- count(taxmonthly, type, tax, vname)

xldir <- r"(G:\My Drive\Consulting\MTA revenue\DataForMTA\PMT_analysis)"
xlsx::write.xlsx(taxmonthly, file = path(xldir, "taxmonthly.xlsx"))

tmp <- taxmonthly |> 
  filter(vname=="local_mtasurch")

tmp2 <- tmp |>
  mutate(qdate=floor_date(date, "quarter")) |> 
  summarize(value=sum(value, na.rm=TRUE), .by=qdate) |> 
  mutate(valya=lag(value, 4),
         pchya=value / valya - 1) 

tmp2 |> 
  ggplot(aes(qdate, pchya)) +
  geom_line()


#.  cft ---------------------------------------------------------
cftbank1 <- taxmonthly |> 
  filter(vname %in% c("bus_cft", "bus_bank"))
skim(cftbank1)
# 1996-04-01
# rates
# https://fiscalpolicy.org/state-corporate-tax-cut-would-cost-new-york-1-2-billion-in-annual-revenue

# 1996	9.225%
# 1997-99	9.000%
# 1999-00	8.500%
# 2000-01	8.000%
# 2001-05	7.500%
# 2006	7.500%
# 2007-2015	7.100%
# 2016-2021	6.500% – Cuomo corporate tax cut
# 2021-2023	7.250% – 2021 corporate tax increase
cftbank1a <- cftbank1 |> 
  arrange(date, type, desc(tax), typesort) |> 
  summarise(across(c(value, value_original),
                   ~ sum(.x, na.rm=TRUE)),
                   .by=c(date, sfy, typesort),
            tax=first(tax)) |> 
  mutate(taxsort=1, vname="bus_cftbank")


cftbank2 <- cftbank1a |> 
  mutate(year=year(date),
         rate=case_when(year==1996 ~ 9.225,
                        year %in% 1997:1999 ~ 9,
                        year %in% 1999:2000 ~ 8.5,
                        year %in% 2000:2001 ~ 8.0, # ???
                        year %in% 2002:2005 ~ 7.5,
                        year %in% 2006 ~ 7.5,
                        year %in% 2007:2015 ~ 7.1,
                        year %in% 2016:2021 ~ 6.5,
                        year %in% 2021:2023 ~ 7.25,
                        TRUE ~ NA_real_),
         value_adj = rate / 7.25 * value)

## pmt ---------------------------------------------------------
pmt1 <- taxmonthly |> 
  filter(vname %in% c("local_mtapmt", "local_mtapmtpitnet"))
count(pmt1, type, tax, vname)

# collapse by tax and month
pmt2 <- pmt1 |>
  mutate(tax="pmt") |> 
  summarise(value=sum(value, na.rm=TRUE),
            .by=c(date, tax))

## combine and save ----
glimpse(cftbank2)
glimpse(pmt2)

cftpmt1 <- bind_rows(cftbank2 |> 
                       select(date, value=value_adj) |> mutate(tax="cftbank"),
                     pmt2)

cftpmt1unadj <- bind_rows(cft2 |> 
                       select(date, value=value) |> mutate(tax="cftbank"),
                     pmt2)

cftpmt <- bind_rows(cftpmt1 |> 
                      mutate(valtype="cftbank_adjusted"),
                    cftpmt1unadj |> 
                      mutate(valtype="unadjusted"))
skim(cftpmt)


saveRDS(cftpmt, here::here("sites", "baseline", "misc", "cftbankpmt.rds"))

# plot with cft and pmt data ----------------------------------------------

cftpmt <- readRDS(here::here("sites", "baseline", "misc", "cftbankpmt.rds"))
skim(cftpmt)
count(cftpmt, tax)

cftpmtq <- cftpmt |> 
  filter(date <= "2022-12-01") |>
  mutate(qdate=floor_date(date, "quarter")) |> 
  summarise(value=sum(value),
            .by=c(qdate, tax, valtype)) |> 
  arrange(qdate) |> 
  mutate(pch=value / lag(value, 4) - 1,
         .by=c(tax, valtype))

cftpmta <- cftpmt |> 
  filter(date <= "2021-12-01") |>
  mutate(year=year(date)) |> 
  summarise(value=sum(value),
            .by=c(year, tax, valtype)) |> 
  arrange(year) |> 
  mutate(pch=value / lag(value) - 1,
         .by=c(tax, valtype))

# quarterly plot ----
years <- 1996:2022
(recdf <- recdata(years))

cols2 <- c('#7fc97f','#beaed4')
# cols2 <- c("lightblue", "green")

capt1 <- "Source: N.Y. Department of Taxation and Finance (https://data.ny.gov/Government-Finance/New-York-State-Local-and-Local-Purpose-Taxes-and-F/2vni-8tmb)"
capt2 <- "Notes:  Recession periods are shaded."
capt3 <- "Corporate franchise and bank tax crudely adjusted for rate changes."
capt3 <- NULL
capt4 <- "% change truncated at +/- 75% to exclude corporate franchise outliers."
capt <- paste0(capt1, "\n", capt2, " ", capt3, " ", capt4)

pdataq <- cftpmtq |> 
  filter(year(qdate) %in% years, valtype=="unadjusted") |>
  filter(!(tax=="pmt" & qdate < "2011-01-01")) |>
  filter(!(tax=="pmt" & qdate >= "2022-10-01")) |> 
  mutate(tax=factor(tax, levels=c("cftbank", "pmt"),
                    labels=c("Corporate franchise (incl bank)", "Payroll mobility"))) 

p <- pdataq |> 
  ggplot(aes(qdate, pch, fill=tax)) +
  geom_band(recdf, period="monthly", fill="grey90") +
  geom_bar (stat="identity", position = position_dodge2(preserve="single")) +
  scale_fill_manual(values=cols2) +
  geom_hline(yintercept = c(-.1, .1), linetype="dashed") +
  scale_x_date(name=NULL, 
               breaks = "1 years", date_labels = "%Y", 
               # limits=as.Date(c("1997-01-01", "2022-10-01")),
               limits=as.Date(c("2011-01-01", "2022-10-01")),
               expand=c(0,0)) +
  scale_y_continuous(name="% change",
                     breaks=seq(-1, 1, .1),
                     labels=percent_format(accuracy = 1),
                     limits=c(-.75, .75),
                     expand=c(0,0)) +
  theme_bw() +
  legend_notitle +
  ggtitle("NYS corporate franchise tax and MTA payroll mobility tax",
          "year over year % change of quarterly values") +
  labs(caption=capt) +
  caption_left

p


# annual plot ----
years <- 1996:2022
(recdf <- recdata(years))

cols2 <- c('#7fc97f','#beaed4')
# cols2 <- c("lightblue", "green")

capt1 <- "Source: N.Y. Department of Taxation and Finance (https://data.ny.gov/Government-Finance/New-York-State-Local-and-Local-Purpose-Taxes-and-F/2vni-8tmb)"
capt2 <- "Notes:  Recession periods are shaded."
capt3 <- "Corporate franchise tax crudely adjusted for rate changes."
capt4 <- "% change truncated at +/- 75% to exclude corporate franchise outliers."
capt <- paste0(capt1, "\n", capt2, " ", capt3, " ", capt4)

pdataa <- cftpmta |> 
  filter(year %in% years) |>
  filter(!(tax=="pmt" & year < 2011)) |>
  filter(!(tax=="pmt" & year >= 2022)) |> 
  mutate(tax=factor(tax, levels=c("cft", "pmt"),
                    labels=c("Corporate franchise", "Payroll mobility"))) 

p <- pdataa |> 
  filter(year >= 2011) |> 
  ggplot(aes(year, pch, fill=tax)) +
  # geom_band(recdf, period="monthly", fill="grey90") +
  geom_bar (stat="identity", position = position_dodge2(preserve="single")) +
  scale_fill_manual(values=cols2) +
  geom_hline(yintercept = c(-.1, .1), linetype="dashed") +
  # scale_x_date(name=NULL, 
  #              breaks = "1 year", date_labels = "%Y", 
  #              limits=as.Date(c("1997-01-01", "2022-10-01")),
  #              expand=c(0,0)) +
  scale_y_continuous(name="% change",
                     breaks=seq(-1, 1, .1),
                     labels=percent_format(accuracy = 1),
                     # limits=c(-.75, .75),
                     expand=c(0,0)) +
  theme_bw() +
  legend_notitle +
  ggtitle("NYS corporate franchise tax and MTA payroll mobility tax",
          "year over year % change of quarterly values") +
  labs(caption=capt) +
  caption_left

p

# end plot ----

# end plot ----

cft3 |>
  ggplot(aes(qdate, pch)) +
  geom_line(colour="blue") +
  geom_point(colour="blue") +
  geom_hline(yintercept = c(-.1, .1), linetype="dashed") +
  scale_y_continuous(limits=c(-.5, .5)) +
  geom_hline(yintercept = 0) +
  theme_bw()




# make quarterly
cft3 <- cft2 |> 
  filter(date <= "2022-12-01") |>
  mutate(qdate=floor_date(date, "quarter")) |> 
  select(qdate, value, value_adj) |> 
  summarise(value=sum(value),
            value_adj=sum(value_adj), .by=c(qdate)) |> 
  arrange(qdate) |> 
  mutate(pch=value_adj / lag(value_adj, 4) - 1)


years <- 1996:2022
(recdf <- recdata(years))

cft3 |>
  ggplot(aes(qdate, pch)) +
  geom_line(colour="blue") +
  geom_point(colour="blue") +
  geom_hline(yintercept = c(-.1, .1), linetype="dashed") +
  scale_y_continuous(limits=c(-.5, .5)) +
  geom_hline(yintercept = 0) +
  theme_bw()


# get pmt and cft ---------------------------------------------------------
pmtcft1 <- taxmonthly |> 
  filter(vname %in% c("local_mtapmt", "local_mtapmtpitnet") |
           vname %in% c("bus_cft"))
count(pmtcft1, type, tax, vname)

# collapse by tax and month
pmtcft2 <- pmtcft1 |> 
  mutate(taxcat=case_when(vname=="bus_cft" ~ "cft",
                          vname %in% c("local_mtapmt", "local_mtapmtpitnet") ~ "pmt",
                          TRUE ~ "ERROR")) |> 
  summarise(value=sum(value, na.rm=TRUE),
            .by=c(date, taxcat))

# make quarterly
pmtcft3 <- pmtcft2 |> 
  filter(date <= "2022-12-01") |>
  mutate(qtr=quarter(date),
         date2=floor_date(date, "quarter")) |> 
  select(date=date2, tax=taxcat, value) |> 
  summarise(value=sum(value), .by=c(tax, date))

# cft
pmtcft3 |>
  filter(tax=="cft") |> 
  arrange(date) |> 
  mutate(pch=value / lag(value, 4) - 1) |> 
  ggplot(aes(date, pch)) +
  geom_line(colour="blue") +
  geom_point(colour="blue") +
  scale_y_continuous(limits=c(-.5, .5)) +
  geom_hline(yintercept = 0) +
  theme_bw()

qpmtcft3 |> 
  filter(date>="2010-01-01",
         date <= "2022-07-01") |> 
  mutate(value=ifelse(year(date) < 2010 & tax=="pmt",
                      NA_real_, value)) |> 
  ggplot(aes(date, value)) +
  geom_line(colour="blue") +
  geom_point(colour="blue") +
  facet_wrap(~tax, ncol=1, scales="free_y") +
  theme_bw()


# make annual
pmtcft3 <- pmtcft2 |> 
  filter(year(date) %in% 2010:2021) |> 
  mutate(year=year(date)) |> 
  select(year, tax=taxcat, value) |> 
  summarise(value=sum(value), .by=c(tax, year))

pmtcft3 |> 
  ggplot(aes(year, value)) +
  geom_line(colour="blue") +
  geom_point(colour="blue") +
  facet_wrap(~tax, ncol=1, scales="free_y") +
  theme_bw()
  

# plot profits and wages --------------------------------------------------


stack1 <- readRDS(here::here("sites", "baseline", "misc", "pbtxwages.rds"))

skim(stack1)

stack2 <- stack1 |>
  mutate(pch=rvalue / lag(rvalue, 4) - 1,
         .by=vname) |> 
  filter(year(date) %in% 2000:2022) |> 
  # calculate % above or below average for a period
  mutate(meanrvalue=mean(rvalue, na.rm=TRUE),
         pdrvalue=rvalue / mean(rvalue, na.rm=TRUE) - 1,
         .by=vname)

stack3 <- stack2 |> 
  select(date, vname, value, gdppi, rvalue, meanrvalue, pch, pdrvalue) |> 
  pivot_wider(names_from=vname,
              values_from=c(value, gdppi, rvalue, meanrvalue, pch, pdrvalue))

write_csv(stack3, here::here("sites", "baseline", "misc", "profits_wages_for_Steve_2023-03-16.csv"))

stack3 |> 
  ggplot(aes(date)) +
  geom_line(aes(y=pdrvalue_wages)) +
  geom_line(aes(y=pdrvalue_profits))

stack3 |> 
  ggplot(aes(date)) +
  geom_line(aes(y=pch_wages)) +
  geom_line(aes(y=pch_profits))


# 
# geom_band <- function(recdf, period="annual", fill="grey", alpha=.5, ...) {
#   # for recession bands and other bands
#   # recdf is a recession data frame with columns:
#   #  peak_decimal  (e.g., 1980.417 for May 1980 (1980 = 5/12))
#   #  trough_decimal
#   # it can be created with the recdata function, which takes a vector of
#   #   recession years
#   if(period=="annual"){
#     recdf$peak_values <- recdf$peak_decimal
#     recdf$trough_values <- recdf$trough_decimal
#   } else{
#     recdf$peak_values <- recdf$peak
#     recdf$trough_values <- recdf$trough
#   }
#   ggplot2::geom_rect(
#     mapping=aes(xmin = peak_values,
#                 xmax = trough_values),
#     data=recdf,
#     ymin = -Inf, ymax = Inf,
#     fill = fill,
#     alpha = alpha, # larger alpha is darker rectangle
#     ...,
#     inherit.aes = FALSE, show.legend = FALSE)
# }


stack <- readRDS(here::here("sites", "baseline", "misc", "pbtxwages.rds"))

years <- 2000:2022
(recdf <- recdata(years))
stack |> 
  filter(year(date) %in% years) |> 
  # calculate % above or below average for a period
  mutate(pdrvalue=rvalue / mean(rvalue, na.rm=TRUE) - 1,
         .by=vname) |> 
  mutate(vname=factor(vname, levels=c("profits", "wages"),
                      labels=c("Corporate profits", "Employer payroll"))) |> 
  ggplot(aes(date, pdrvalue)) +
  geom_line(colour="blue", linewidth=1) +
  geom_hline(yintercept = 0) +
  geom_band(recdf, period="monthly", fill="lightblue") +
  scale_x_date(name=NULL, breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(name="% above or below period average",
                     breaks=seq(-1, 1, .1),
                     labels=percent_format(accuracy = 1)) +
  facet_wrap(~vname, ncol=1) +
  theme_bw() +
  ggtitle("U.S. Corporate profits and employer payroll, adjusted for inflation",
          "% above or below period average, 2000-2022") +
  labs(caption="Source: U.S. Bureau of Economic Analysis\nNote:Recession periods are shaded") +
  caption_left

