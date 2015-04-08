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
#' @param license.accept User has to explicitly accept the license.
#'  FALSE by default.
#' @param drop.mfn Should a most favoured nation clause be dropped
#'  from resulting data.frame. TRUE by default.
#' @return Dataframe with columns country, hs code and description.

getmadb <- function(country, hs, license.accept = F, drop.mfn = T) {
  
  if(!license.accept) stop("You have to accept Mendel Verlag licence.")
  
  url <- "http://madb.europa.eu/madb/atDutyOverviewPubli.htm"
  
  r <- httr::POST(url, body = 
                    list(datacat_id = "AT",
                         hscode = as.character(hs),
                         keyword = "",
                         countries = toupper(country),
                         datasettype = "",
                         submit = "Search",
                         showregimes = "",
                         license = "Accept"),
                  encode = "form")
  
  df <- XML::readHTMLTable(httr::content(r), which = 1, stringsAsFactors = F)
  row.names(df) <- NULL
  
  df <- df[-1,] # First row contains NA only
  colnames(df) <- c("hs", "desc", "mfn") # R-friendly colanames
  if(drop.mfn) df <- df[,c("hs", "desc")] # Drop MFN if required
  df <- cbind(country = toupper(country), df, stringsAsFactors = F) # Add country column
  df$hs <- sapply(df$hs, function(x) { 
    if(x == "") return(NA)
    stringr::str_replace(x, "\\.", "")
  })
  
  df
}

