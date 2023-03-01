
taxcalc2 <- function(newrate, policy){
  baserate <- .0034
  baselowrate <- .0017
  # newlowrate <- baselowrate + (newrate - baserate) # OLD: increase lowrate by rate change
  newlowrate <- newrate / baserate * baselowrate  # proportionate increase
  
  tax <- taxbase |> 
    select(year, fips, area, contains("_adj")) |> 
    mutate(tax_wagefull=wages_beafullrate_adj * newrate,
           tax_wagelow=wages_beasmallemp_lowrate_adj * newlowrate,
           tax_wage=tax_wagefull + tax_wagelow,
           
           tax_nonwage=nonwage_beapmt_adj * newrate,
           
           tax_pmt=tax_wage + tax_nonwage,
           
           # calculate tax losses from the 2011 law:
           # impact of the low rate for small employers above the minimum
           # we lose the full rate minus the lowrate
           # previous code was newrate -  baselowrate, WRONG!! fixed 2023-02-23
           # it should have been newrate - newlowrate rather than baselow rate
           
           # so when newrate was 0.0034 (the total current rate) and
           # baselowrate was 0.0017, the calc loss was 0.0017, which was correct
           
           # but when newrate was 0.00479 (to raise $700 million)
           # baselowrate was still 0.0017 but new low rate would be
           # 0.0017 * 0.00479 / 0.0034 = 0.002395
           # so calc loss was (0.00479 - 0.0017) = 0.00309
           # but calc loss should have been 0.00479 - 0.002395 = 0.00240
           # so calc loss was 309 / 240 -1 too high, or 28.75% too high
           # calc loss was $140.6 million but should have been $108.996
           # so calc loss was $31.7m too high, this carried through to total loss
           taxloss_smallemplowrate_2011=
             wages_beasmallemp_lowrate_adj * (newrate - newlowrate),
           
           # impact of excluding all wages of very small employers complete untaxed
           # we lose the full rate on excluded wages
           taxloss_smallempexcluded_2011=
             wages_beasmallemp_excluded_adj * newrate, 
           
           # full rate for k12 because we lose all their wages
           taxloss_k12_2011=wages_beak12_adj * newrate, 
           
           taxloss_total_2011=
             taxloss_smallemplowrate_2011 + 
             taxloss_smallempexcluded_2011 + 
             taxloss_k12_2011
    ) |> 
    summarise(across(c(contains("_adj"), contains("tax_"), contains("taxloss_")),
                     sum),
              .by=c(year, fips, area)) |> 
    mutate(policy=policy, rate=newrate)
  
  tax
  
}

tax_current2 <- taxcalc2(newrate=.0034, policy="current")
tax_nooffset2 <- taxcalc2(newrate=.005, policy="nooffset")

stack <- bind_rows(tax_current2, tax_nooffset2)

stackwide <- stack |> 
  select(year, fips, area, policy, tax_pmt) |> 
  pivot_wider(names_from = policy, values_from = tax_pmt) |> 
  mutate(change=nooffset - current)

write_csv(stackwide, here::here("output.csv"))


ns(stack)
details <- stack |> 
  filter(year==2021) |> 
  select(year, fips, area, policy, tax_wage, tax_nonwage, tax_pmt) |> 
  mutate(area=ifelse(fips %in% constants$nycfips,
                       "New York City",
                     area),
         policy=ifelse(policy=="nooffset", "newrate", policy)) |> 
  summarise(across(starts_with("tax_"), sum), .by=c(policy, area)) |> 
  mutate(order=row_number(), .by=policy)

totals <- details |> 
  summarise(across(starts_with("tax_"), sum), .by=c(policy)) |> 
  mutate(order=9, area="Total")

full <- bind_rows(details, totals) |> 
  pivot_longer(cols=starts_with("tax_")) |> 
  pivot_wider(names_from = policy) |> 
  mutate(change=newrate - current) |> 
  arrange(name, order)
full

  
details <- stack |> 
  filter(year==2021) |> 
  select(year, fips, area, policy, tax_wage, tax_nonwage, tax_pmt) |> 
  mutate(area=ifelse(fips %in% constants$nycfips,
                     "New York City",
                     area),
         policy=ifelse(policy=="nooffset", "newrate", policy)) |> 
  summarise(across(starts_with("tax_"), sum), .by=c(policy, area)) |> 
  mutate(order=row_number(), .by=policy) |> 
  pivot_longer(cols=starts_with("tax_"), names_to = "name") |>
  pivot_wider(names_from = policy) |> 
  mutate(change=newrate - current) |> 
  pivot_longer(cols=c(change, current, newrate), names_to = "calculation")|> 
  pivot_wider(names_from = calculation) |> 
  relocate(change, .after=newrate)
details

totals <- details |> 
  summarise(across(c(current, newrate, change), sum), .by=c(name)) |> 
  mutate(order=9, area="Total")
totals

full <- bind_rows(details, totals) |>
  mutate(name=factor(name, levels=c("tax_wage", "tax_nonwage", "tax_pmt"))) |> 
  mutate(across(c(current, newrate, change),
                ~ .x / .x[area=="Total"], 
                .names = "{.col}_share"),
         .by=name) |> 
  arrange(name, order)
full

# |> 
#   pivot_wider(names_from = calculation)
# details  
  





