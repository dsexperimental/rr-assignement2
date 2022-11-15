library(dplyr)
#library(ggplot2)
library(lubridate)
library(stringr)

set.seed(111)

## notes on data:
## https://www.ncdc.noaa.gov/stormevents/details.jsp
##
##  1950 - 1954 - only tornado
##  1955 - 1995 - only tornado, thunderstorm wind, hail 
##  1996 - present - 48 event types

###########################################
## FUNCTIONS
###########################################


##=====================================
## This function converts exponent strings to numerica exponents
##=====================================
## This converts exponent string values that are
## - a number of metric exponents - see code for selection
## - numeric strings (convert to numeric)
## - other - anything else is converted to an exponent of 0
## In addition a safety text is done that there are no single letter
## values that are not converted by the metric coefficients, with the
## assumption that theis coefficient should be added.
convertExpCol <- function(expStr) {
  expNum <- numeric(length(expStr))
  expNum <- NA
  
  expNum[str_detect(expStr,"[hH]")] = 2
  expNum[str_detect(expStr,"[kK]")] = 3
  expNum[str_detect(expStr,"[mM]")] = 6
  expNum[str_detect(expStr,"[bB]")] = 9
  expNum[str_detect(expStr,"[tT]")] = 12
  
  isnum <- str_detect(expStr,"[0-9]+")
  expNum[isnum] = as.numeric(expStr[isnum])
  
  ## SAFETY TEST --------
  ## Check for any remaining single letter exponents
  ## These are probably metric prefixes we left out
  singleLetterExpLeft <- sum(str_detect(expStr[is.na(expNum)],"[a-zA-Z]"))
  if(singleLetterExpLeft > 0) {
    warning("There are unaccounted letter values for exponent! Investigate")
  }
  ##--------------
  
  expNum[is.na(expNum)] = 0 
  
  expNum
}

## standardize string - lowercase and replace dash with space
stdString <- function(textVector) {
  str_replace(tolower(textVector),"-"," ")
}


## this function takes a vector of outerStrings and innerStrings and returns a matrix
## telling if a given outerString contains a given innerString. The outerStrings represent
## row indices and the innerStrings represent column indices
getContainedIn <- function(outerStrings,innerStrings) {
  inner_in_outer <- matrix(FALSE,nrow = length(outerStrings),ncol = length(innerStrings))
  dimnames(inner_in_outer) <- list(outerStrings,innerStrings)
  
  setRow <- function(outerString) {
    inner_in_outer[outerString,] <<- str_detect(outerString,regex(innerStrings,ignore_case=TRUE))
  }
  
  sapply(outerStrings,setRow)
  
  inner_in_outer
}

###########################################
## Load the Process Data
###########################################

##======================
## load the data file
##======================

##---------------
## Column names/classes - to assist in reading and verifying file
##---------------

## This is a list of column names for the data
colNames <- c("STATE__", "BGN_DATE", "BGN_TIME", "TIME_ZONE", "COUNTY", 
              "COUNTYNAME", "STATE", "EVTYPE", "BGN_RANGE", "BGN_AZI",
              "BGN_LOCATI", "END_DATE", "END_TIME", "COUNTY_END", "COUNTYENDN", 
              "END_RANGE", "END_AZI", "END_LOCATI", "LENGTH", "WIDTH", "F",
              "MAG", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", 
              "CROPDMG", "CROPDMGEXP", "WFO", "STATEOFFIC", "ZONENAMES", 
              "LATITUDE", "LONGITUDE", "LATITUDE_E", "LONGITUDE_", "REMARKS",
              "REFNUM") 

## place the column names you want to keep here
colToKeep <- c("BGN_DATE","EVTYPE","FATALITIES","INJURIES","PROPDMG",
               "PROPDMGEXP","CROPDMG","CROPDMGEXP","REMARKS")

## place the column classes here, to help loading
colToKeepClasses <- c("character","character","numeric","numeric","numeric",
                      "character","numeric","character","character")

## generate the col class vector
colClasses <- rep("NULL",length(colNames))
colClasses[match(colToKeep,colNames)] <- colToKeepClasses

##-----------------
## load the csv file
##-----------------
dat <- read.csv("repdata_data_StormData.csv.bz2",colClasses = colClasses)

## SAFETY TEST: verify the format is as expected
if(!identical(colToKeep,names(dat))) {
  error("File not loaded properly! Format may have changed since origianl analysis")
}


##-----------------
## modify columns and classes
##-----------------

## convert timefrom character class
dat$BGN_DATE <- mdy_hms(dat$BGN_DATE)

## we will only use data from 1996 and newer
dat <- filter(dat,BGN_DATE >= "1996-01-01")

## the analysis only examines data that contains damage, fatalities or injuries
dat <- filter(dat,((PROPDMG > 0) | (CROPDMG > 0) | (FATALITIES > 0) | (INJURIES > 0)) )

## standardize the format of the type labels
dat$EVTYPE <- stdString(dat$EVTYPE)

## add a column for damage amounts
## first convert character "exponent" columns to numerical values
dat$PEXP <- convertExpCol(dat$PROPDMGEXP)
dat$CEXP <- convertExpCol(dat$CROPDMGEXP)
dat$DMG <- dat$PROPDMG * (10^dat$PEXP) + dat$CROPDMG * (10^dat$CEXP)

##===========================
## load the types data
##===========================

##---------------------
## NWS types file
##---------------------

## official types from NWS
officialTypes <- fromJSON("officialTypes.json")

## convert the official types to the working format: lowercase, "-" removed
types <- stdString(officialTypes)

##---------------------
##load the type aliases
##---------------------

## type aliases
## - list names: NWS types
## - list values: aliases for these types 
typeAliases <- fromJSON("typeAliases.json")

## invert mapping to go from aliases to types
aliasToType <- list()
addTypeAlias <- function(type,aliases) {
  aliasToType[aliases] <<- type
}
dummy <- mapply(addTypeAlias,names(typeAliases),typeAliases,SIMPLIFY=FALSE)

##this is a vector with the aliases in it
aliases <- names(aliasToType)

## keywords - a list of types and aliases we will use to identify type from labels
keywords <- c(types,aliases)

##create mapping from keywords to types, to identify types from matched keywords
typeToType <- as.list(types)
names(typeToType) <- types
keywordToType <- c(typeToType,aliasToType)

##============================
## identify types based on labeling of data
##============================

##vector of labels from data set
labels <- unique(dat$EVTYPE)

## k_in_l: logical matrix with an entry TRUE telling if a key (column index) is
## in a label (row index)
k_in_l <- getContainedIn(labels,keywords)

## k_in_k: logical matrix with an entry TRUE telling if a keyword (column index) is
## contained in a keyword (row index)
k_in_k <- getContainedIn(keywords,keywords)

## adj_k_in_k: this is the same as k_in_k except we remove the diagonal entries
## which say a key contains itself
adj_k_in_K <- k_in_k
diag(adj_k_in_K) <- FALSE

## if a longer key contains a shorter key, any label that contains a longer
## key will also contain the shorter key. We want to discard the shorter key
## in this case. The equation below does this. Explanation:
## - k_in_l[label,long key] * adj_k_in_k[long key,short key]: This is true if
##   both a ("long") key contains a ("short") key and a label contains the 
##   ("long") key.
## - (k_in_l %*% adj_k_in_k): for a given label and key, this is true if any
##   other keys both contain this key and are contained in this label
## - k_in_l & !as.logical(k_in_l %*% adj_k_in_K): For any long key that is
##   contained in a label, this clears the flag that states on "short" key contained
##   in this "long" key is also contained in the label
fixed_k_in_l <- k_in_l & !as.logical(k_in_l %*% adj_k_in_K)


##get the list of keywords in each label
getKeywords <- function(innerKeyFlags) {
  keywords[innerKeyFlags]
}
labelKeywords <- apply(fixed_k_in_l,1,getKeywords)

## Here we make a map from the labels to the type
## if the label contains one keyword (type or alias), we associate it with that type
## it the label contains 0 or multiple keywords, we record the type as the 
## value of the label enclosed in double arrows, to flag a failed match.
getKeywordTypes <- function(keywords,label) {
  if(length(keywords) == 1) {
    type <- keywordToType[[keywords]]
  }
  else {
    type <- sprintf("<<%s>>",label)
  }
  type
}
labelToType <- mapply(getKeywordTypes,labelKeywords,label=names(labelKeywords),SIMPLIFY=FALSE)


## we use our label to Type mapping to add a "type" column to our data.
## this will either include a valid type name or the label enclosed in "<<" + ">>"
dat$type <- factor(unlist(labelToType[dat$EVTYPE]))

###########################################
## Analyze Data
###########################################

##===============================
## Get the total costs, fatalities and injuries
##===============================

## get totals
totalCost <- sum(dat$DMG)
totalFatalities <- sum(dat$FATALITIES)
totalInjuries <- sum(dat$INJURIES)

##===========================
## group the data by type
##===========================
groupedDat <- dat %>% group_by(type)

##===========================
## for each category (fatalities, injuries and damage), create a ordered
## data frame of totals by type
##===========================

fatalitiesDat <- groupedDat %>% 
  summarise(fatalities = sum(FATALITIES)) %>% 
  arrange(desc(fatalities))

injuriesDat <- groupedDat %>% 
  summarise(injuries = sum(INJURIES)) %>% 
  arrange(desc(injuries))

costDat <- groupedDat %>% 
  summarise(cost = sum(DMG)) %>% 
  arrange(desc(cost))


###########################################
## Analyze Data
###########################################

## we make a pie chart for each category of costs
## we will cut off values that are too small and place them in an 
## "other" type

##=================================
## Preprocessing for plotting
##=================================

## this function takees a data frame with two columns - a factor and a number
## it removes all rows that are smaller than a given fraction and replaces these
## with an "other" row
## NOTE: there should not be an existing category named "other"
addOtherCat <- function(fullDF,cutoffFrac) {
  #truncate the data frame
  totalValue <- sum(fullDF[[2]])
  redDF <- filter(fullDF,fullDF[[2]] / totalValue > cutOffFrac)
  
  ##get the "other" value and add it
  redTotalValue <- sum(redDF[[2]])
  otherValue <- totalValue - redTotalValue
  levels(redDF[[1]]) <- c(levels(redDF[[1]]),"other")
  newRow <- list("other",otherValue)
  names(newRow) <- names(redDF)
  redDF[length(redDF[[1]]) + 1,] <- newRow
  redDF
}

##this is the fractional value below which we wil place in the "other" type
cutOffFrac <- 0.007

## color palette
pal <- colorRampPalette(palette.colors(n=8,palette = "Pastel 2"))
otherColor <- rgb(230,230,230,maxColorValue=255)

# create modified data frames keeping only the major types
## which are data readable from the pie chart
redCostDat <- addOtherCat(costDat,cutOffFrac)
redFatalitiesDat <- addOtherCat(fatalitiesDat,cutOffFrac)
redInjuriesDat <- addOtherCat(injuriesDat,cutOffFrac)

##=================================
## Plotting
##=================================

##plots
pie(redCostDat$cost,
    labels=sprintf("%s $%.1fB",redCostDat$type,redCostDat$cost/1000000000),
    cex=0.75,init.angle=20,col=c(pal(length(redCostDat$type)-1),otherColor),
    main="Dollar Cost by Storm Type",
    sub = sprintf("Total Cost: $%.1fB",totalCost/1000000000))

pie(redFatalitiesDat$fatalities,
    labels=sprintf("%s (%.0d)",redFatalitiesDat$type,redFatalitiesDat$fatalities),
    cex=0.75,init.angle=20,col=c(pal(length(redFatalitiesDat$type)-1),otherColor),
    main="Fatalities by Storm Type",
    sub = sprintf("Total Fatalities: %.0d",totalFatalities))

pie(redInjuriesDat$injuries,
    labels=sprintf("%s (%.0d)",redInjuriesDat$type,redInjuriesDat$injuries),
    cex=0.75,init.angle=20,col=c(pal(length(redInjuriesDat$type)-1),otherColor),
    main="Injuries by Storm Type",
    sub = sprintf("Total Injuries: %.0d",totalInjuries))



################################################################################
## Appendix
################################################################################

##other analysis functions

sum(sapply(labelKeywords,length) == 0)


##===================
## This function generates the type files used in this analysis
##===================
generateTypeFiles <- function() {
  
  # write the NWS types file
  typeJson <- '["Tsunami", "Storm Surge/Tide", "High Surf", "Seiche", 
                "Rip Current", "Astronomical Low Tide", "Flash Flood", "Flood", 
                "Coastal Flood", "Lakeshore Flood", "Heavy Rain", "Hail", 
                "Lightning", "Hurricane (Typhoon)", "Tropical Depression", 
                "Tropical Storm", "Thunderstorm Wind", "High Wind", "Strong Wind", 
                "Dust Storm", "Tornado", "Funnel Cloud", "Waterspout", 
                "Dust Devil", "Blizzard", "Winter Storm", "Heavy Snow", 
                "Ice Storm", "Lake-Effect Snow", "Sleet", "Winter Weather", 
                "Avalanche", "Extreme Cold/Wind Chill", "Frost/Freeze", 
                "Cold/Wind Chill", "Excessive Heat", "Heat", "Dense Fog", 
                "Freezing Fog", "Debris Flow", "Wildfire", "Dense Smoke", 
                "Drought", "Volcanic Ash", "Marine Hail", "Marine High Wind", 
                "Marine Strong Wind", "Marine Thunderstorm Wind"]'
  writeLines(typeJson,"officialTypes.json")
  
  #write the type alias file used in this analysis
  typeAliasJson <- '{
    "cold/wind chill": ["cold", "wind chill", "windchill", "hypothermia/exposure", "hyperthermia/exposure"],
    "debris flow": ["landslide", "rock slide", "mud slide", "mudslide", "landslump"],
    "dense fog": ["fog"],
    "dust storm": ["blowing dust"],
    "extreme cold/wind chill": ["extreme cold", "extreme wind chill", "extreme windchill"],
    "freezing fog": ["glaze"],
    "frost/freeze": ["frost", "freeze"],
    "heat": ["unseasonably warm", "warm weather"],
    "heavy rain": ["torrential rainfall", "urban/sml stream fld", "hvy rain"],
    "heavy snow": ["excessive snow"],
    "high surf": ["heavy surf", "rough surf", "hazardous surf"],
    "high wind": ["non tstm wind", "non-tstm wind"],
    "hurricane (typhoon)": ["hurricane", "typhoon", "hurricane/typhoon"],
    "lake-effect snow": ["lake effect snow"],
    "storm surge/tide": ["storm surge"],
    "strong wind": ["gusty wind", "gradient wind", "wind damage", "wind"],
    "thunderstorm wind": ["tstm wind", "tstmw", "gustnado", "downburst", "microburst"],
    "tornado": ["landspout"],
    "wildfire": ["wild fire", "grass fire", "forest fire", "brush fire"],
    "winter weather": ["freezing rain", "light snowfall", "snow and ice", "late season snow", "light snow", "snow", "freezing drizzle", "rain/snow", "snow squall", "mixed precipitation", "blowing snow", "wintry mix", "mixed precip", "snow squalls", "falling snow/ice"]
  }'
  writeLines(typeAliasJson,"typeAliases.json")

}