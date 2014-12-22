
findCorrespondCode <- function(code, cortab = hs7map_hs_items) {
  if (length(code) > 1) return(unlist(lapply(code, findCorrespondCode)))
  return(NULL)
}

#' Convert coutrycodes from FAO to UN and vice versa
#' 
#' @import stringr
#' @importFrom countrycode countrycode 
areas <- function(value, from, to) {
  
  if(missing(value)) stop('Value is missing')
  if(length(value) > 1) return(do.call(rbind, lapply(value, areas, from = from, to = to)))
  
  # Character value
  if(!str_detect(value, "^[0-9]+$")) {
    
    if(missing(from) & missing(to)) {
      return(data.frame(country = countrycode(value, "country.name", "country.name"),
                        fao =  countrycode(value, "country.name", "fao"),
                        un = countrycode(value, "country.name", "un"),
                        stringsAsFactors = F))
    }
  }
  # Numeric value
  if(str_detect(value, "^[0-9]+$")) {
    if(missing(from) & missing(to)) {
      stop("Don't know how to convert. Please specify 'from' either 'to' argument")
    }
    return(data.frame(country = countrycode(value, from, "country.name"),
                      fao =  countrycode(value, from, "fao"),
                      un = countrycode(value, from, "un"),
                      stringsAsFactors = F))
  }
}


areas2 <- function(area, to) {
  if(length(area) > 1) return(unlist(lapply(area, areas2, to)))
  if(to == "fao") area <- 
    unctfaoareasmap$fao[unctfaoareasmap$unct == area]
  if(to == "unct") area <- 
    unctfaoareasmap$unct[unctfaoareasmap$fao == area]
  if(length(area) == 0) return(NA)
  area
}


#' Convert UN ComTrade country codeinto corresponding FAO area code.
#' @export
faoarea <- function(unct) {
  # TODO: Add China aggregation
  areas2(unct, "fao")
}

#' Convert FAO area code into corresponding UN ComTrade country code.
#' 
#' @section Waring:
#' It doesn't aggregate China's parts.
#' 
#' @export
unctarea <- function(fao) {
  areas2(fao, "unct")
}

#' Convert HS2007 code into corresponding FCL codes.
#' @export
hs2fcl <- function(hs) {
  unique(hsfclmap$fcl[hsfclmap$hs %in% hs])
}

#' Convert FCL code into corresponding HS2007 codes.
#' @export
fcl2hs <- function(fcl) {
  unique(hsfclmap$hs[hsfclmap$fcl %in% fcl])
}

#' Description of FCL-item
#' @export
descfcl <- function(item) {
  fcl[fcl$fcl %in% item,]
}

# It doesn't work correclty!!!
reltype <- function(d) {
  d <- d %>%
    select_(~fcl, ~hs) %>%
    group_by_(~fcl) %>%
    mutate_(hs_n = ~length(unique(hs))) %>%
    group_by_(~hs) %>%
    mutate_(items_n = ~length(unique(fcl))) %>%
    ungroup() %>%
    mutate_(reltype = 
              ~ifelse(hs_n == 1 & items_n == 1, "one-to-one",
                      ifelse(hs_n == 1 & items_n > 1, "one-hs-to-many-fcl",
                             ifelse(hs_n > 1 & items_n == 1, "many-hs-to-one-fcl",
                                    ifelse(hs_n > 1 & items_n > 1, "many-to-many",
                                           NA)))))
  d$reltype
}

#' List FCL codes in a correspondence group.
#' @export
fclingroup <- function(group) {
  sort(unique(hsfclmap$fcl[hsfclmap$group == group]))
}

#' List HS codes in a correspondence group.
#' @export
hsingroup <- function(group) {
  sort(unique(hsfclmap$hs[hsfclmap$group == group]))
}

#' Returns correspondence group for fcl code.
#' @export
fclgroup <- function(fcl) {
  if (length(fcl) > 1) return(unlist(lapply(fcl, fclgroup)))
  x <- unique(hsfclmap$group[hsfclmap$fcl == fcl])
  if (length(x) < 1) return(NA)
  if (length(x) > 1) {
    warning(paste("FCL code ", fcl, " is included in several groups: ",
                                   x, ". Return only first.", sep = ""))
    return(x[1]) 
  }
  x
}


#' Returns correspondence group for HS2007 code.
#' @export
hsgroup <- function(hs) {
  if (length(hs) > 1) return(unlist(lapply(hs, hsgroup)))
  x <- unique(hsfclmap$group[hsfclmap$hs == hs])
  if (length(x) < 1) return(NA)
  if (length(x) > 1) {
    warning(paste("HS code ", hs, " is included in several groups: ",
                  x, ". Return only first.", sep = ""))
    return(x[1]) 
  }
  x
}

#' Extract particular datasets from FAOSTAT3 data.
#' 
#' @param data. Data frame to take original data from. It should be downloaded 
#'   from faostat3.fao.org.
#' @param area Numeric. Codes of reporter countries from FAO classification.
#' @param year Numeric. Reported year.
#' @param partner Numeric. Codes of partner countries from FAO classification.
#'   All if omitted.
#' @param flow. Numeric. 1 - Import, 2 - Export. Both if omitted.
#' @param weightonly. Drop all except measured in tonnes. TRUE by default.
#' 
#' @import dplyr
#' @import stringr
#' @export

convertfao3 <- function(data, area, year, partner, 
                        flow = "all", weightonly = T) {

  if(missing(area) | missing(year)) stop("area or year parameters are not provided")
  
  if(flow != "all") {
    data <- data %>%
      filter_(~str_detect(Element, flow))
  }

  if(!missing(partner)) {
    data <- data %>%
      filter_(~Partner.Country.Code  == partner)
  }
    
  data <- data %>%
    filter_(~Reporter.Country.Code == area,
            ~Year                  == year)
  
  if(nrow(data) ==0) return(data.frame())
            
  
  data <- data %>%
    select_(area  = ~Reporter.Country.Code,
            pt    = ~Partner.Country.Code,
            el    = ~Element,
            fcl   = ~Item.Code,
            year  = ~Year,
            unit  = ~Unit,
            value = ~Value) %>%
    mutate_(flow  = ~str_extract(el, "^.{6}"), #Import/Export - first 6 symbols of el
           type   = ~ifelse(str_detect(unit, "1000 US\\$"), "value", "quan")) %>%
    select_(~-el)
  
  data_quan <- data %>%
    filter_(~type == "quan") %>%
    rename_(quan = ~value) %>%
    select_(quote(-type))
  
  data_value <- data %>%
    filter_(~type == "value") %>%
    select_(quote(-unit), quote(-type))
  
  data <- data_quan %>%
    inner_join(data_value, by = c("area", "pt", "fcl", "year", "flow"))
  
  if(weightonly) data <- data %>%
    filter_(~unit == "tonnes")
  
  data
}

#' Convert dataset gathered from UNCT API to comparable form.
#' 
#' @param data. Data frame returned from call getunct() with compact = F
#' @param area Numeric. Codes of reporter countries from UNCT classification.
#'  All if omitted.
#' @param year Numeric. Reported year. All if omitted.
#' @param partner Numeric. Codes of partner countries from UNCT classification.
#'   All if omitted.
#' @param flow. Numeric. 1 - Import, 2 - Export. Both if omitted.
#' @param weightonly. Drop all except measured in kg. TRUE by default.
#'
#' @import dplyr
#' @export
convertunctapi <- function(data, area, year, partner, 
                           flow = "all", weightonly = T) {
  if(any(!(c("rtCode", "ptCode", "rgDesc", "cmdCode", "yr",
             "qtDesc", "TradeQuantity", "TradeValue") %in% names(data)))) {
    stop("Unsupported data. Use getunct() with compact=F to get correct data.")
  }
  
  if(!missing(area)) data <- data %>%
    filter_(~rtCode == area)
  
  if(!missing(year)) data <- data %>%
    filter_(~yr == year)
  
  if(!missing(partner)) data <- data %>%
    filter_(~ptCode == partner)
  
  if(flow != "all") data <- data %>%
    filter_(~rgDesc == flow)
      
  data <- data %>%
    select_(areact = ~rtCode,
           ptct    = ~ptCode,
           flowct  = ~rgDesc,
           hs      = ~cmdCode,
           year    = ~yr,
           unitct  = ~qtDesc,
           quanct  = ~TradeQuantity,
           valuect = ~TradeValue)
  
  if(weightonly) data <- data %>%
    filter_(~unitct == "Weight in kilograms")
  
  data
}


#' Function to join dataframes with FAO and UN CT data.
#' 
#' @param faodata. Data frame returned by convertfao3()
#' @param ctdata. Data frame returned by convertunctapi()
#'
#' @import dplyr
#' @export
joincompared <- function(faodata, ctdata) {
  if(any(faodata$unit != "tonnes"))
    stop("Items other than weight in tonnes are not supported in FAO data")

  if(any(ctdata$unit != "Weight in kilograms"))
    stop("Items other than weight in kilograms are not supported in UN CT data")
  
  faodata <- faodata %>%
    mutate_(group = ~fclgroup(fcl)) %>%
    group_by_(~area, ~pt, ~year, ~flow, ~group, ~unit) %>%
    summarize_(quan =  ~sum(quan),
              value = ~sum(value)) %>%
    ungroup()
  
  ctdata <- ctdata %>%
    mutate_(area    = ~faoarea(areact),
            pt      = ~faoarea(ptct),
            flow    = ~flowct,
            quanct  = ~round(quanct / 1000),
            valuect = ~round(valuect / 1000),
            unit    = ~ifelse(unitct == "Weight in kilograms", 
                              "tonnes", NA),
            group   = ~hsgroup(hs)) %>%
    group_by_(~area, ~pt, ~year, ~flow, ~group, ~unit) %>%
    summarize_(quanct  = ~sum(quanct),
               valuect = ~sum(valuect)) %>%
    ungroup()
  
  data <- inner_join(faodata, ctdata, 
                     by = c("area", "pt", "year", "flow", "group", "unit")) %>%
    mutate_(qdiff = ~(quan - quanct),
            vdiff = ~(value - valuect))
  
  data
  
}
