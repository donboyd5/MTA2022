
source(here::here("r", "libraries.r"))
source(here::here("r", "constants.r"))

url <- "https://www2.census.gov/programs-surveys/susb/tables/2019/county_3digitnaics_2019.xlsx"



# ONETIME get data --------------------------------------------------------

download.file(url, here::here("forMTA", path_file(url)), mode="wb")


# construct file ----------------------------------------------------------

susb1 <- read_excel(here::here("forMTA", path_file(url)))
glimpse(susb1)
head(susb1)
susb1[2, ] |> as.character()

susb2 <- susb1 |> 
  select(stfips=1, stname=2, cofips=3, coname=4, 
         naics=5, naicstitle=6, entsize=7, 
         nfirms=8, nestabs=9, emp=10, empnoise=11,
         payannk=12, paynoise=13) |> 
  filter(row_number() >= 3) |> 
  mutate(across(c(nfirms, nestabs, emp, payannk), as.numeric))

susb3 <- susb2 |> 
  filter(stfips=="36") |> 
  filter(paste0("36", cofips) %in% constants$mtafips) |> 
  mutate(osort=row_number(), nyc=paste0("36", cofips) %in% constants$nycfips)

susb4 <- susb3 |> 
  select(-c(stfips, stname, empnoise, paynoise)) |> 
  mutate(ppfirm=payannk * 1000 / nfirms,
         ppemp=payannk * 1000 / emp,
         pctfirms=nfirms / nfirms[entsize=="1: Total"],
         pctpay=payannk / payannk[entsize=="1: Total"], 
         .by=c(cofips, coname, naics, naicstitle)) |> 
  arrange(desc(nyc), cofips, naics, naicstitle, entsize) |> 
  relocate(osort, nyc)

xlsx::write.xlsx(susb4, here::here("forMTA", "Census_susb2019_firm_size_MTA_counties.xlsx"))


outpath <- here::here("forMTA", "Census_susb2019_firm_size_MTA_counties.xlsx")

# create and save workbook ------------------------------------------------

wb <- createWorkbook()
## Add worksheets
addWorksheet(wb, "notes")
addWorksheet(wb, "susbdata")

# prepare notes
note1 <- "This workbook contains data on businesses in the MTA region by county, NAICS code, and enterprise size."
note2 <- "The original source canb be found at: https://www2.census.gov/programs-surveys/susb/tables/2019/county_3digitnaics_2019.xlsx"
note3 <- "The data are from the Census Bureau's Statistics of U.S. Businesses site (https://www.census.gov/programs-surveys/susb.html)."
note4 <- "The Census Bureau constructed these data from County Business Patterns data. The original source note is below."
note5 <- "SOURCE: 2019 County Business Patterns.  For information on confidentiality protection, sampling error, nonsampling error, and definitions, see http://www.census.gov/programs-surveys/susb/technical-documentation/methodology.html and http://www.census.gov/programs-surveys/susb/about/glossary.html. The Census Bureau has reviewed this data product for unauthorized disclosure of confidential information and has approved the disclosure avoidance practices applied. (Approval ID: CBDRB-FY22-117)"

notes <- c(note1, note2, note3, note4, note5)

for(i in 1:length(notes)){
  writeData(wb, sheet = "notes", x=notes[i], startCol=1, startRow=i, rowNames = FALSE, colNames = FALSE)
}


# write the data

cheads <- c("sort", "NYC indicator", "County FIPS", "County", "NAICS code", "NAICS Title", "Enterprise size",
            "# of firms", "# of establishments", "Total employment",
            "Annual payroll ($000)", "Average payroll per firm ($)", "Average payroll per employee",
            "# of firms as % of total firms", "Payroll as % of total payroll")
for(i in 1:length(cheads)){
  writeData(wb, sheet = "susbdata", x=cheads[i], startRow=1, startCol=i, rowNames = FALSE, colNames = FALSE)
}

first_data_row <- 3  
writeDataTable(wb, sheet = "susbdata", x=susb4, startRow=first_data_row - 1, startCol=1, rowNames = FALSE, colNames = TRUE)
freezePane(wb, sheet="susbdata", firstActiveRow = first_data_row, firstActiveCol = 8)


# add styling
num_cols <- 8:13
pct_cols <- 14:15
s <- createStyle(numFmt = "#,##0")
addStyle(wb, sheet = "susbdata", style = s, rows=first_data_row:10000, cols = num_cols, gridExpand = TRUE)
s <- createStyle(numFmt = "0.0%")
addStyle(wb, sheet = "susbdata", style = s, rows=first_data_row:10000, cols = pct_cols, gridExpand = TRUE)
setColWidths(wb, "susbdata", cols = 6, widths = 30)
setColWidths(wb, "susbdata", cols = 7, widths = 20)

s <- createStyle(
  # fontSize = 24, fontColour = rgb(1, 0, 0),
  textDecoration = c("bold"),
  wrapText = TRUE,
  halign = "center", valign = "center"
)
addStyle(wb, sheet = "susbdata", style = s, rows=1, cols = 1:15)

# openXL(wb)

saveWorkbook(wb, outpath, overwrite = TRUE)

