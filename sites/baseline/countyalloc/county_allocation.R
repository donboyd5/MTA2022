
# create a county-level file with allocation variables

# libraries ---------------------------------------------------------------

source(here::here("r", "libraries.r"))
source(here::here("r", "libraries_ts.r"))
source(here::here("r", "constants.r"))
source(here::here("r", "functions.r"))


# constants ---------------------------------------------------------------
# BEA proprietors' income is much larger than IRS self-employment income
# in the BEA documentation for non-farm proprietors' income,
# https://www.bea.gov/resources/methodologies/nipa-handbook/pdf/chapter-11.pdf
# Table 11.3—Relation of Nonfarm Proprietors’ Income in the National Income and Product
# Accounts to Corresponding Measures as Published by the Internal Revenue Service
# has estimates for 2015 of a relevant IRS concept ($831.7b) and NFPI ($1,130.8b)
# I use that ratio to adjust propinc
propinc_taxable_ratio <- 831.7 / 1130.8




# get previously prepared allocators data ---------------------------------


