
pmtdf |> 
  filter(year(date)==2021)

df <- pmtdf |> 
  filter(date>"2009-11-01") |> # this value seems wrong
  arrange(date) |> 
  select(date, totpmt) |> 
  mutate(date=yearmonth(date)) |> 
  as_tsibble(index = date) 

mod <- df |> 
  model(arima = ARIMA(log(totpmt))) 
# mod
# report(mod)

fc <- mod |> 
  forecast(h = "2 years")

mod <- df |> 
  model(arima = ARIMA(log(totpmt))) 

fc |>
  autoplot(filter(pmtdf, year(date) > 2010), level = NULL) +
  # scale_x_date(name=NULL) +
  theme_bw()

sz <- 1
df |> 
  filter(year(date) > 2015) |> 
  autoplot(totpmt, size=sz, colour="blue") +
  # autolayer(fc |> filter(year(date) <= 2022), size=sz, level=seq(10,90,by=10),
  #           color = 'red') +
  autolayer(fc |> filter(year(date) <= 2022), data=df, size=sz, colour="darkred", show_gap = FALSE) +
  geom_point(size=0.75) +
  scale_x_yearmonth(name=NULL, breaks="3 months", expand=expansion(mult = c(0.01, .005))) +
  scale_y_continuous(name="$ millions", breaks=seq(0, 1e6, 25e3), labels=number_format(scale=1e-3), limits=c(0, NA)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = -90, vjust = 0, hjust=0.5))
# +  theme(legend.position="none")
  # guides(col= guide_legend(title= "Confidence\ninterval"))

fc |> unnest()
str(fc$totpmt)


aus_production %>% 
  autoplot(Beer) + 
  autolayer(fc)