
constants <- list()

# locations ---------------------------------------------------------------
constants$qdir <- r"(E:\data\BLSData\qcew\annual_area\)"
constants$mtaout <- r"(G:\My Drive\Consulting\MTA revenue\DataForMTA\)"

# plot items ---------------------------------------------------------------
legend_none <- theme(legend.position = "None")
legend_notitle <- theme(legend.title = element_blank())
caption_left <- theme(plot.caption = element_text(hjust = 0))

# constants ---------------------------------------------------------------

# MCTD includes the counties of:
#   New York (Manhattan)
#   Bronx
#   Kings (Brooklyn)
#   Queens
#   Richmond (Staten Island)
#   Rockland
#   Nassau
#   Suffolk
#   Orange
#   Putnam
#   Dutchess
#   Westchester
# these constants are ordered and NOT aligned with each other
# constants$mtacos <- c("New York", "Bronx", "Kings", "Queens", "Richmond", "Rockland", 
#             "Nassau", "Suffolk", "Orange", "Putnam", "Dutchess", "Westchester")
# constants$mtafips <- c(36005, 36027, 36047, 36059, 36061, 36071, 36079, 36081, 36085, 36087, 36103, 36119) |> as.character()
# constants$totnycfips <- "3651000"
# constants$mtafips_to_county <- function(fips) constants$mtacos[match(fips, constants$mtafips)]
# constants$mtacos_to_fips <- function(county) constants$mtafips[match(county, constants$mtacos)]

# constants$nycfips <- c(36005, 36047, 36061, 36081, 36085) |> as.character()
# constants$nyccos <- c("New York", "Bronx", "Kings", "Queens", "Richmond")
# constants$nyboros <- c("Manhattan", "Bronx", "Brooklyn", "Queens", "Staten Island")
# constants$boro_to_county <- function(boro) constants$nyccos[match(boro, constants$nyboros)]
# constants$county_to_boro <- function(county) constants$nyboros[match(county, constants$nyccos)]
# constants$fips_to_boro <- function(fips) constants$nyboros[match(fips, constants$nycfips)]
# constants$boro_to_county(c("Staten Island", "Brooklyn"))
# constants$county_to_boro(c("Kings", "Richmond"))
# constants$fips_to_boro(c("36085", "36005"))




