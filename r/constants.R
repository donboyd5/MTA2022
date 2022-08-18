
constants <- list()

# locations ---------------------------------------------------------------
constants$qdir <- r"(E:\data\BLSData\qcew\annual_area\)"


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
constants$mtacos <- c("New York County", "Bronx", "Kings", "Queens", "Richmond", "Rockland", 
            "Nassau", "Suffolk", "Orange", "Putnam", "Dutchess", "Westchester")
constants$mtafips <- c(36005, 36027, 36047, 36059, 36061, 36071, 36079, 36081, 36085, 36087, 36103, 36119) |> as.character()

constants$nycfips <- c(36005, 36047, 36061, 36081, 36085) |> as.character()7
constants$nyccos <- c("New York County", "Bronx", "Kings", "Queens", "Richmond")


