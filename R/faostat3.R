#' Function retrieves data from FAOSTAT3 FENIX API
#' 
#' JS source: https://gist.github.com/Kalimaha/80da85e6d7cbdbf92557
#' 
#' 
#' 
#' 

getfenix <- function(list1Codes = NULL,
                     list2Codes = NULL,
                     list3Codes = NULL,
                     list4Codes = NULL,
                     list5Codes = NULL,
                     list6Codes = NULL,
                     list7Codes = NULL,
                     nullValues = FALSE,
                     thousand   = "",
                     decimal    = ".",
                     decPlaces  = 2,
                     datasource = "faostatdb",
                     domainCode = "TM",
                     lang       = "E",
                     url = "http://faostat3.fao.org/wds/rest/procedures/data") {
  payload <- list(datasource = datasource,
                  domainCode = domainCode,
                  lang       = lang,
                  list1Codes = list1Codes,
                  list2Codes = list2Codes,
                  list3Codes = list3Codes,
                  list4Codes = list4Codes,
                  list5Codes = list5Codes,
                  list6Codes = list6Codes,
                  list7Codes = list7Codes,
                  thousand   = thousand,
                  decimal    = decimal,
                  decPlaces  = decPlaces,
                  limit      = 10000000)
  lnames <- names(payload)
  
  payload <- lapply(names(payload), function(lname) {
    x <- payload[[lname]]
    if(stringr::str_detect(lname, "^list[1-7]Codes$")) {
      if(is.null(x)) return(integer()) # Empty value has to be in [] after AJAXing
      return(paste0("\'", as.character(x), "\'")) # FENIX wants additional single quotes around values in arrays
    }
    jsonlite::unbox(x) # Removing [] around settings' values
  })
  
  names(payload) <- lnames
  payload <- as.character(jsonlite::toJSON(payload))
  resp <- httr::POST(url, body = list(payload = payload), encode = "form")
  t <- jsonlite::fromJSON(content(resp, as = "text"))
  tnames <- make.names(t[1,])
  t  <- as.data.frame(t[-1,], stringsAsFactors = F)
  colnames(t) <- tnames
  t <- t[, !(colnames(t) %in% c("NoRecords", "RecordOrder", "Year.Code",
                                "Var1Order", "Var2Order",
                                "Var3Order", "Var4Order",
                                "Var5Order"))]
  t$Value <- as.numeric(t$Value)
  t
}


# https://github.com/mkao006/FAOSTATpackage/blob/28bd0ba5606ea8570c0f33f919a83105de54c8ac/FAOSTAT/R/getFAO.R
# http://faostat3.fao.org/wds/api?db=faostatdb&select=A.AreaCode[FAOST_CODE],D.year[Year],D.value[Value]&from=data[D],element[E],item[I],area[A]&where=D.elementcode(2610),D.itemcode(15),D.domaincode('TM'),D.year(2011),JOIN(D.elementcode:E.elementcode),JOIN(D.itemcode:I.itemcode),JOIN(D.areacode:A.areacode)
# AreaCodeAreaCodeElementListCodeItemCodeItemNameEElementListNameEItemCodeAreaNameEAreaNameFElementCodeElementListNameFItemNameFItemNameSElementListNameSYearAreaNameSAreaNameCValueElementListNameCItemNameCItemNameAElementListNameAFlagAreaNameAAreaNameRLoadDateElementListNameRItemNameRItemLevelElementCodeDomainCodeAreaLevelAreaUNDPCodeGroupCodeElementNameEItemDescriptionEItemDescriptionFElementNameFElementListCodeAreaISO2CodeAreaISO3CodeNotesElementNameSItemDescriptionSElementNameCAreaM49CodeAreaWBCodeElementNameAElementNameRAreaStartYearAreaEndYearElementUnitNameEElementUnitNameFElementUnitNameSElementUnitNameCElementUnitNameAElementUnitNameRUnitCodeUnitNameEUnitNameFUnitNameSUnitNameCUnitNameAUnitNameRElementDescriptionEElementDescriptionFElementDescriptionS 


