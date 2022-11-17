

# libraries ---------------------------------------------------------------

source(here::here("r", "libraries.r"))
source(here::here("r", "libraries_ts.r"))
source(here::here("r", "constants.r"))
source(here::here("r", "functions.r"))

# library(slider)
# library(RcppRoll)


# check -------------------------------------------------------------------

pmtdf <- readRDS(here::here("data", "dtf", "pmt_collections.rds"))
pmtdf

tmp <- pmtdf |> 
  filter(date>"2009-11-01") |> # this value seems wrong
  arrange(date) |> 
  select(date, local_mtapmt, local_mtapmtpitnet, totpmt) |> 
  mutate(cumsum=slide_dbl(totpmt, sum, .before = 11, .complete = TRUE),
         cumsum2=roll_sum(totpmt, n=12, fill=NA, align="right"))


tmp |> 
  filter(year(date)>=2014) |> 
  ggplot(aes(date, cumsum)) +
  geom_line(color="blue") +
  geom_point(color="blue") +
  scale_y_continuous(name="Rolling 12-month sum", breaks=seq(0, 2e6, 50e3), labels=number_format(accuracy=1, scale=1e-3)) +
  scale_x_date(name=NULL, breaks="3 months", date_labels = "%Y-%b", expand=expansion(mult = c(0.02, .02))) +
  theme(axis.text.x = element_text(angle = -90, vjust = 0, hjust=0.5)) +
  ggtitle("PMT revenue, 12 month sums")


df <- pmtdf |> 
  filter(date>"2009-11-01") |> # this value seems wrong
  arrange(date) |> 
  select(date, local_mtapmt, local_mtapmtpitnet, totpmt) |> 
  mutate(date=yearmonth(date)) |> 
  as_tsibble(index = date) 

mod <- df |> 
  model(arima = ARIMA(log(totpmt))) 
mod
report(mod)



fc <- mod |> 
  forecast(h = "2 years")

str(fc)

fc |> 
  autoplot(filter(pmtdf, year(date) > 2010), level = NULL) +
  scale_x_date(name=NULL) +
  theme_bw()


df2 <- df |> 
  select(date, totpmt) |> 
  bind_rows(fc |> as_tibble() |> select(date, .mean)) |>
  mutate(hfc=ifelse(is.na(.mean), "history", "forecast"),
         value=ifelse(is.na(.mean), totpmt, .mean))
glimpse(df2)  

fyfc <- df2 |> 
  as_tibble() |> 
  mutate(fy=year(date)) |> 
  group_by(fy) |> 
  summarise(n=n(), value=sum(value))
fyfc

fyfc2 <- df2 |> 
  as_tibble() |> 
  mutate(fy=ifelse(month(date)==12,
                   year(date) + 1,
                   year(date))) |> 
  group_by(fy) |> 
  summarise(n=n(), value=sum(value))
fyfc2

p <- df2 |> 
  mutate(date=as.Date(date), hfc=factor(hfc, levels=c("history", "forecast"))) |> 
  filter(year(date)>=2015, date<="2023-12-01") |> 
  ggplot(aes(date, value, colour=hfc)) +
  geom_line(linewidth=1) +
  geom_point(size=0.5) +
  scale_colour_manual(values=c("blue", "red")) +
  ggtitle("Total Payroll Mobility Tax (wages + self-employment), monthly data",
          subtitle="History plus simple ARIMA forecast through 2023") +
  scale_x_date(name=NULL, breaks="3 months", date_labels = "%Y-%b", expand=expansion(mult = c(0.02, .02))) +
  scale_y_continuous(name="$ millions", breaks=seq(0, 10e6, 25e3), labels=number_format(accuracy=1, scale=1e-3)) +
  theme_bw(base_size = 18) +
  theme(axis.text.x = element_text(angle = -90, vjust = 0, hjust=0.5)) +
  theme(legend.title=element_blank())
p


