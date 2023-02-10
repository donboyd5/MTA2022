

qcew_api <- function(year=c(format(Sys.Date(), "%Y")), qtr="1", slice=NULL, sliceCode=NULL, ...){
  if (is.null("slice") | is.null("sliceCode")){
    message("Please specify a Slice and sliceCode. See function documentation for examples.")
  }
  
  year = as.numeric(year)
  
  # This API only keeps four years of data.
  if (year < as.numeric(format(Sys.Date(), "%Y")) - 4)
    warning("ERROR: The QCEW API only provides data for the last four years. Please ajust your arguments.")
  
  if (!is.character(year)) year <- as.character(year)
  if (!is.character(qtr)) qtr <- as.character(qtr)
  
  qtr <- tolower(qtr)
  
  slice.options <- c("industry", "area", "size")
  if (!is.character(slice)) slice <- as.character(slice)
  if (!is.character(sliceCode)) sliceCode <- as.character(sliceCode)
  if (!isTRUE(any(grepl(slice, slice.options)))){
    message("Please select slice as 'area', 'industry', or 'size'")
  }
  if (!is.numeric(year)){message("Please set a numeric year.")}
  if (slice=="area" & is.numeric(sliceCode) & !isTRUE(sliceCode %in% blscrapeR::area_titles$area_fips)){
    message("Invalid sliceCode, please check you FIPS code.")
  }
  if (slice=="industry" & is.numeric(sliceCode) & !isTRUE(sliceCode %in% blscrapeR::niacs$industry_code)){
    message("Invalid sliceCode, please check you NIACS code.")
  } 
  if (slice=="size" & is.numeric(sliceCode) & !isTRUE(sliceCode %in% blscrapeR::size_titles$size_code)){
    message("Invalid sliceCode, please enter an integer between 0 and 9.")
  }   
  baseURL <- "https://data.bls.gov/cew/data/api/"
  url <- paste0(baseURL, year, "/", qtr, "/", slice, "/", sliceCode, ".csv")
  
  out <- tryCatch(
    {
      message("Trying BLS servers...")
      temp <- tempfile()
      download.file(url, temp, quiet = TRUE)
      qcewDat <- read.csv(temp, fill=TRUE, header=TRUE, sep=",", stringsAsFactors=FALSE,
                          strip.white=TRUE)
      message("Payload successful.")
    },
    error=function(cond) {
      message(paste("URL does not seem to exist. Please check your parameters and try again.", url))
      return(NULL)
    },
    warning=function(cond) {
      message(paste("URL caused a warning. Please check your parameters and try again:", url))
      return(NULL)
    }
  ) %>% tibble::as_tibble()   
  return(qcewDat)
}

