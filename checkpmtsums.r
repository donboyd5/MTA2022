

library(slider)
pmtdf <- readRDS(here::here("data", "dtf", "pmt_collections.rds"))
pmtdf

tmp <- pmtdf |> 
  filter(date>"2009-11-01") |> # this value seems wrong
  arrange(date) |> 
  select(date, local_mtapmt, local_mtapmtpitnet, totpmt) |> 
  mutate(cumsum=slide_dbl(totpmt, sum, .before = 11, .complete = TRUE))

tmp |> 
  filter(year(date)>=2014) |> 
  ggplot(aes(date, cumsum)) +
  geom_line(color="blue") +
  geom_point(color="blue") +
  scale_y_continuous(breaks=seq(0, 2e6, 50e3), )



