#' Function to retrieve list of countries, 
#' available at Market Access DB
getmadbcountries <- function(url = "http://madb.europa.eu/madb/euTariffs.htm") {
  force(url)
    
  r <- httr::content(httr::GET(url))
  
  df <- do.call("rbind", 
          XML::xpathApply(r, 
                     "//form[@name='theform']//option[@value!='none']", 
                     function(x) data.frame(iso = XML::xmlAttrs(x), 
                                            name = XML::xmlValue(x), 
                                            stringsAsFactors = F)))
  row.names(df) <- NULL
  
  df
}

#' Function to retrieve descriptions of country-specific 
#' Harmonized System commodity codes from Market Access
#' Database of the European Commission.
#' 
#' @param country ISO3166 country code.
#' @param hs HS numeric code of length 4 or 6.
#' @return Dataframe with columns country, hs code and description.

getmadb <- function(country, hs) {
  
  
}


