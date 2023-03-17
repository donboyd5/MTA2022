
# libraries and constants -------------------------------------------------

source(here::here("r", "libraries.r"))
source(here::here("r", "libraries_ts.r"))
source(here::here("r", "constants.r"))
source(here::here("r", "functions.r"))


# get data ----------------------------------------------------------------

taxmonthly <- readRDS(here::here("data", "dtf", "taxmonthly.rds"))
glimpse(taxmonthly)
skim(taxmonthly)
count(taxmonthly, type, tax, vname)


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

pmtcft3 |> 
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
  


# look at corporate profits and wages -------------------------------------


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

