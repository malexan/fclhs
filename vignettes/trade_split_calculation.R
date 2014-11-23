########################################################################
## Title: trade work
## Date: 2014-07-28
## Note: This work is instructed by Adam Prakash.
########################################################################

library(igraph)
library(data.table)

source("functions.r")

## Read source file
mexicoFcl.dt = readTradeFileFCL("mexico_test_comp_3.csv")
mxcfcl <- mexicoFcl.dt

keyRelationship("HS2007Code", "FCLcode", mexicoFcl.dt)


## Read comtrade file
mexicoComtrade.dt =
    readTradeFileComtrade(file = "mexico_unsd_raw_new.csv")
mxcunsd <- mexicoComtrade.dt

## Read unsd to faostat country mapping
countryMapping.dt =
    readComtradeFaostatCountryMapping("comtradeCountryMapping.csv")
keyRelationship("unsdPartnerArea", "faostatPartnerArea",
                countryMapping.dt)



## map unsd country code to faostat country code
mexicoComtradeCountryMapped.dt =
    mapComtradeCountry(comtradeData = mexicoComtrade.dt,
                       countryMapping = countryMapping.dt)

## Add meta data about records which were originally NA
mexicoComtradeCountryMapped.dt$metaData <- as.character(
  mexicoComtradeCountryMapped.dt$metaData)
mexicoComtradeCountryMapped.dt[is.na(comtradeQuantity) & !is.na(flow),
                               metaData := "no quantity"]
# metaData's type is logic vector!!
# and it consists of NA only - ******* Should we remove the column? ****


## Create aggregate china and discrad China, main land (41) and Taiwan
## (214)
mexicoFclChina.dt = appendAggFclChina(mexicoFcl.dt)


## Merge comtrade and source data
mexicoFinal.dt =
    mergeComtradeFcl(comtradeData = mexicoComtradeCountryMapped.dt,
                     fclData = mexicoFclChina.dt)


## Check the difference between the two files
missInSource.dt =
    mexicoFinal.dt[!not0notMiss(sourceOtherQuantity) &
                   not0notMiss(comtradeQuantity), ]
## write.csv(missInSource.dt, file = "miss_in_source.csv", na = "",
##           row.names = FALSE)
missInComtrade.dt =
    mexicoFinal.dt[not0notMiss(sourceOtherQuantity) &
                   !not0notMiss(comtradeQuantity), ]
## write.csv(missInComtrade.dt, file = "miss_in_comtrade.csv", na = "",
##           row.names = FALSE)

# 10110 -> 010110
# 10619 -> 010619
# 120929

## Compute the trade dissemination based on transactions matched with
## source
mexicoMatchMariana.dt =
    mexicoFinal.dt[!is.na(FCLcode),
                   list(faostatPartnerArea, HS2007Code, flow,
                        comtradeQuantity, FCLcode, sourceOtherQuantity,
                        validQuantity)]
# ******** Who is Mariana? ***************

## Create aggregated code classification
mexicoMatchMariana.dt[, aggHScode2007 := createAggregatedCode(.SD),
                      by = c("faostatPartnerArea", "flow")]


## Aggregate the quantity
mexicoAggregated.dt = aggregateQuantity(mexicoMatchMariana.dt)


## Compute the split ratio
mexicoAggregated.dt = computeSplit(mexicoAggregated.dt)


## Check the difference
mexicoAggregated.dt = computeDifference(mexicoAggregated.dt)

         
## write.csv(mexicoAggregated.dt, file = "mexico_new_split_final.csv",
##           row.names = FALSE, na = "")

