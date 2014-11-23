## Function to read source file
readTradeFileFCL = function(file){
  tradeFCL = data.table(read.csv(file, stringsAsFactors = FALSE))
  setnames(tradeFCL,
           old = c("REPORT_AREA", "PARTNER_AREA", "ITEM_ORIG", "ITEM",
                   "FLOW", "source.orig_quantity",
                   "source.other_quantity", "source.value",
                   "valid.quantity", "valid.value"),
           new = c("faostatReportArea", "faostatPartnerArea",
                   "HS2007Code", "FCLcode", "flow",
                   "sourceOrigQuantity", "sourceOtherQuantity",
                   "sourceValue", "validQuantity", "validValue"))
  tradeFCL
}


## Function to determin relationship
keyRelationship = function(key1, key2, data){
  map1 = max(data[,
                  eval(parse(text = paste0("length(unique(", key1, "))"))),
                  by = key2]$V1)
  map2 = max(data[,
                  eval(parse(text = paste0("length(unique(", key2, "))"))),
                  by = key1]$V1)
  if(map1 == 1 & map2 == 1){
    relation = "one-to-one"
  } else if(map1 == 1 & map2 >= 1){
    relation = "one-to-many"
  } else if (map1 >= 1 & map2 == 1){
    relation = "many-to-one"
  } else {
    relation = "many-to-many"
  }
  cat("Key relationship is: ", relation, "\n")
}


## Function to read unsd to faostat country mapping
readComtradeFaostatCountryMapping = function(file){
  countryMap =
    data.table(read.csv(file = file, stringsAsFactors = FALSE))
  setnames(countryMap, old = c("rtCode", "AreaCode"),
           new = c("unsdPartnerArea", "faostatPartnerArea"))
  countryMap
}

## Function to map unsd country code to faostat country code.
mapComtradeCountry = function(comtradeData, countryMapping){
  mapped = merge(comtradeData, countryMapping,
                 by = "unsdPartnerArea", all = TRUE)
  mapped
}


## Function to compute aggregated china (357)
appendAggFclChina = function(data){
  chinaAgg.dt = data[faostatPartnerArea %in% c(41, 214),
                     list(sourceOrigQuantity = sum(sourceOrigQuantity),
                          sourceOtherQuantity = sum(sourceOtherQuantity),
                          sourceValue = sum(sourceValue),
                          validQuantity = sum(validQuantity),
                          validValue = sum(validValue)),
                     by = c("faostatReportArea", "HS2007Code", "FCLcode",
                            "flow")]
  chinaAgg.dt[, faostatPartnerArea := 357]
  rbind(data[!faostatPartnerArea %in% c(41, 214), ],
        chinaAgg.dt[, colnames(data), with = FALSE])
}


## Function to merge comtrade and source
mergeComtradeFcl = function(comtradeData, fclData){
  final = merge(comtradeData, fclData,
                by = c("faostatPartnerArea", "HS2007Code", "flow"),
                all = TRUE, allow.cartesian = TRUE)
  final[!is.na(flow), ]
}


## Function to ignore entries that are zero or missing
not0notMiss = function(x){
  x != 0 & !is.na(x)
}



## Function to create new aggregated classification category to map
## many-to-many into many-to-one
createAggregatedCode = function(data){
  dataCopy = copy(data)
  tmp = graph.data.frame(dataCopy[, list(HS2007Code, FCLcode)])
  dataCopy[, aggHS2007Code := ""]
  while(any(dataCopy$aggHS2007Code == "")){
    tmpCode = dataCopy[aggHS2007Code == "", HS2007Code][1]
    print(tmpCode)
    connectedCodes = V(tmp)[neighborhood(tmp, order = 100,
                                         nodes = as.character(tmpCode))[[1]]]$name
    HSgroupCode =
      paste(unique(unlist(dataCopy[as.character(HS2007Code) %in%
                                     connectedCodes, ]$HS2007Code)),
            collapse = "_")
    print(length(HSgroupCode))
    index = which(as.character(dataCopy$HS2007Code) %in%
                    connectedCodes)
    dataCopy[index, 
             aggHS2007Code := HSgroupCode]       
  }
  dataCopy[, aggHS2007Code]
}



## Function to aggregate the quantity
aggregateQuantity = function(data){
  dataCopy = copy(data)
  dataCopy[, c("aggOtherSourceQuantity", "aggComtradeQuantity") :=
             list(sum(unique(.SD[, list(HS2007Code, sourceOtherQuantity)])$sourceOtherQuantity, na.rm = TRUE),
                  sum(unique(.SD[, list(HS2007Code, comtradeQuantity)])$comtradeQuantity, na.rm = TRUE)),
           by = c("faostatPartnerArea", "flow", "aggHScode2007")]
  dataCopy[, c("HS2007Code", "sourceOtherQuantity",
               "comtradeQuantity") := NULL]
  unique(dataCopy)
}


## Function to compute the split ratio
computeSplit = function(data){
  data[, aggHScodeSplit :=
         validQuantity/round(aggOtherSourceQuantity/1000) *
         (round(aggOtherSourceQuantity/1000))/sum(validQuantity),
       by = c("faostatPartnerArea", "flow", "aggHScode2007")]
  data[, comtradeSplitQuantity :=
         round((aggComtradeQuantity * aggHScodeSplit)/1000)]
}


## Function to check the difference between the two
computeDifference = function(data){
  data[, validDifference :=
         comtradeSplitQuantity - validQuantity]
}




## Function to read comtrade file
readTradeFileComtrade = function(file){
  comtrade =
    data.table(unique(read.csv(file = file,
                               stringsAsFactors = FALSE)))
  setnames(comtrade,
           old = c("ItemCode", "AreaCode", "Flow", "Qty"),
           new = c("HS2007Code", "unsdPartnerArea", "flow",
                   "comtradeQuantity"))
  comtrade[, HS2007Code := as.numeric(gsub("x", "", HS2007Code))]
  comtrade = comtrade[!is.na(unsdPartnerArea), ]
  comtrade
}
