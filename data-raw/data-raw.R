# library(XLConnect)
# hs7 <- readWorksheetFromFile(file = 'export_hs_items_1414130966.xlsx',
#                       sheet = 1)


f <- rbind_all(apply(fcl_roots[1:189,], 1, function(x) {
  y <- x[['FCL.Item.code']]
  t <- x[['lngth']]
  parent = rep_len(y, t)
  t <- data.frame(parent, )
  t
}))

fcl_roots <- fcl_full %>%
  select(FCL.Item.code, root, rnk) %>%
  filter(root) %>%
  select(-root) %>%
  mutate(lngth = lead(rnk) - rnk)

f <- unlist(apply(fcl_roots[1:3,], 1, function(x) {
  force(x)
  rep(x[['FCL.Item.code']], each = x[['lngth']]) # Add root code to output!
}))





# system2("soffice", 
#         args = '--headless --convert-to xlsx --outdir sources sources/export_hs_items_1414408736.xls')

hsfclmap <- 
  readWorksheetFromFile(file = system.file("extdata", 
                                           "export_hs_items_1414130966.xlsx", 
                                           package = "fclhs"), 
                        sheet = 1, 
                        colTypes = c("character", # Read HS-code as string
                                     "character", 
                                     "character",
                                     "numeric",
                                     "character")) %>%
  select(hs = HS.2007.code, x = X., hstitle = HS.2007.title, 
         fcl = FCL.Item.code) %>%
  mutate(hschap = as.numeric(
    str_replace(hs, "^([0-9]{1,2})[0-9]{2}(\\..*$|$)", "\\1")),
    hshead = as.numeric(
      str_replace(hs, "^[0-9]{1,2}([0-9]{2})(\\..*$|$)", "\\1")),
    hssub = as.numeric(
      str_replace(hs, "^[0-9]{3,4}\\.([0-9]{0,2})", "\\1")),
    hs = formatC(round(as.numeric(hs) * 100, 0), 
                 width = 6, flag = "0", format = 'd')) %>%
  left_join(fcl_full, by = "fcl")

# It'll be usefull to rewrite in apply() style.
hsfclmap$group <- NA
for (i in hsfclmap$fcl) {
  fclingroup <- hs2fcl(fcl2hs(hs2fcl(fcl2hs(i))))
  groupname <- min(fclingroup)
  hsfclmap$group[hsfclmap$fcl %in% fclingroup] <- groupname
}

save(hsfclmap, file = "data/hsfclmap.rda")

# Convert original xls to xlsx because can't make XLConnect to read binary xls.
# hs7map_items_hs <- readWorksheetFromFile(file = 'sources//export_items_hs_1414408683.xlsx', sheet = 1)

fcl  <- readWorksheetFromFile('sources//export_items_1414495109.xlsx', 
                                   sheet = 1) %>%
  filter(!is.na(FCL.Item.code) & # Remove rows resulting from reding merged cells
           FCL.Item.code != 9999 )  %>% # Remove test record
  mutate(Group = ifelse(Group == "22 - PESTICIDES", # Errors in names of groups
                        "22 - FERTILIZERS", Group),  # in xls from fao.org
         Group = ifelse(Group == "21 - FERTILIZERS",
                        "21 - PESTICIDES", Group),
         fclgroup = as.numeric(str_extract(Group, "^[1-9][0-9]?")),
         fclgrouptitle = tolower(str_extract(Group, "(\\b[A-Z]+\\b ?)*$"))) %>%
  #          root = !is.na(Scientific.Name) # Idea was Sc.Name is root, 
  # but not for every case
  select(fcl = FCL.Item.code, fclgroup, fcltitle = FCL.Title,
         fclgrouptitle, fclscient = Scientific.Name, fcldef = Definition)

# fcl <- readRDS('data/fcl.rds')
save(fcl, file = 'data/fcl.rda')


unctfaoareasmap <- read.table(file = system.file("extdata", 
                                                 "comtradeCountryMapping.csv", 
                                                 package = "fclhs"),
                              header = T, sep = ",")
names(unctfaoareasmap) <- c("unct", "fao")

save(unctfaoareasmap, file = 'data/unctfaoareasmap.rda')

# http://faostat.fao.org/DesktopModules/Faostat/AreaGroups/DownloadAreaList.aspx?caseDownload=0&selectedLanguage=E
# http://faostat.fao.org/site/371/DesktopDefault.aspx?PageID=371

faoareanames <- read.table(file = system.file("extdata", 
                                           "FaostatAreaList.csv", 
                                           package = "fclhs"),
                        header = T, sep = ",", na.strings = "..")

save(faoareanames, file = 'data/faoareanames.rda')




data <- read.table(system.file("extdata", 
                                  "mxc_2011_2007_all_all.csv.gz", 
                                  package = "fclhs"), 
                      header = T,
                      sep = ',', stringsAsFactors = F,
                      nrows = 63582 # drop final line with timestamp
)

