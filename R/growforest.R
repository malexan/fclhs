#' Wrapper for CloudForest GO-lang RandomForest solution
#' 
#' 
#' \url{https://github.com/ryanbressler/CloudForest}
#' 
#' @import dplyr


growforest <- function(data, 
                       sample,
                       target,
                       nTrees    = 100, 
                       nSamples  = 0, 
                       nCores    = 1,
                       progress  = F,
                       selftest  = F,
                       rm.unused = T,
                       path      = "/home/sas/go/bin/",
                       workdir   = "tmp") {
  
  if(!file.exists(workdir)) dir.create(workdir)
  
  moment <-  format(Sys.time(), "%Y%m%d%H%M%S")
  fileprefix <- c(workdir, "/")
  
  trainfile <- paste0(fileprefix, "traindata", moment, ".arff")
  testfile  <- paste0(fileprefix, "testdata", moment, ".arff")
  rffile    <- paste0(fileprefix, "forest", moment, ".rf")
  predsfile <- paste0(fileprefix, "preds", moment, ".tsv")
  
  if(!missing(sample)) {
    
    train <- sample.int(nrow(data), sample)
    
    traindata <- data[train,]
    
    testdata <- data[-train,] 
    
    if(rm.unused) testdat <- testdata %>%
      filter_(~reporter %in% levels(traindata$reporter),
              ~partner  %in% levels(traindata$partner),
              ~hs       %in% levels(traindata$hs),
              ~fcl      %in% levels(traindata$fcl)) 
    
    foreign::write.arff(testdata, 
                        file = testfile)
  }
  
  if(missing(sample)) traindata <- data
  
  foreign::write.arff(traindata, 
                      file = trainfile)
  
  
  argums <- c(paste0("-train ", trainfile),
              ifelse(!missing(sample), paste0("-test ", testfile), ""),
              paste0("-target ", target),
              paste0("-rfpred ", rffile),
              paste0("-nTrees ", nTrees),
              paste0("-nCores ", nCores),
              paste0("-progress ", tolower(as.character(progress))),
              paste0("-selftest ", tolower(as.character(selftest))),
              paste0("-nSamples ", nSamples))
  
  system2(paste0(path, "growforest"), argums)
  
  apply_argums <- c(paste0("-fm ", testfile),
                    paste0("-rfpred ", rffile),
                    paste0("-preds ", predsfile))
  
  system2(paste0(path, "applyforest"), apply_argums)
  
  preds <- read.table(predsfile, header = F, sep = '\t')
  testdata$fcl.pred <- preds$V2
  
  testdata %>%
    mutate_(matched = ~as.numeric(fcl.pred == fcl)) %>%
    group_by_(~matched) %>%
    summarise_(n = ~n()) %>%
    mutate_(prop = ~n / sum(n))
  
}


applyforest <- function()