library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr) #reg exp

set.seed(111)

##import the event type info
source("eventTypes.R")

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

## this returns a randomly selected subset roughly a frac of original length
getSubset <- function(df,frac) {
  df[as.logical(rbinom(nrow(df),size=1,prob=frac)),]
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
## ANALYSIS
###########################################

##=======================
## load file
##======================
loadData <- function() {
  source("colClasses.R")
  
  ##load all data
  allData <- read.csv("repdata_data_StormData.csv.bz2",colClasses = colClasses)
  
  if(!setequal(colToKeep,names(allData))) {
    #If this fails the data file must have changed columns
    error("File not loadeed properly!")
  }
  
  ##return data with damage, fatalities or injuries
  filter(allData,(PROPDMG > 0) | (CROPDMG > 0) | (FATALITIES > 0) | (INJURIES > 0))
}

dat <- loadData()

##=====================
## update columns and classes
##=====================

dat$BGN_DATE <- mdy_hms(dat$BGN_DATE)
dat$EVTYPE <- stdString(dat$EVTYPE)

##add a column for damage amounts
dat$PEXP <- convertExpCol(dat$PROPDMGEXP)
dat$CEXP <- convertExpCol(dat$CROPDMGEXP)

dat$DMG <- dat$PROPDMG * (10^dat$PEXP) + dat$CROPDMG * (10^dat$CEXP)

#datShort = getSubset(dat,.05)

##analyze based on data 1996 and later.
## long and short data
dat96 <- dat[dat$BGN_DATE >= "1996-01-01",]

#datShort96 <- datShort[datShort$BGN_DATE >= "1996-01-01",]

##===========================
## totals
##===========================
labels <- unique(dat96$EVTYPE)

length(labels)

k_in_l <- getContainedIn(labels,keywords) 
k_in_k <- getContainedIn(keywords,keywords)

adj_k_in_K <- k_in_k
diag(adj_k_in_K) <- FALSE


## this is the matrix telling if a given label contains a given keyword
## keeping only the longer keyword for any of its keyword that contain another shorter keyword.
fixed_k_in_l <- k_in_l & !as.logical(k_in_l %*% adj_k_in_K)

##THIS IS JUST A TEST
totals <- apply(fixed_k_in_l,1,sum)

labels[totals == 0]
labels[totals == 1]
labels[totals == 2]
labels[totals == 3]


##get a list of keywords for each label
getKeywords <- function(innerKeyFlags) {
  keywords[innerKeyFlags]
}
labelKeywords <- apply(fixed_k_in_l,1,getKeywords)

## get a single type for each label

getKeywordTypes <- function(keywords,label) {
  if(length(keywords) == 1) {
    type <- keywordToType[[keywords]]
  }
  else {
    type <- sprintf("<<%s>>",label)
  }
  type
}
labelTypeMap <- mapply(getKeywordTypes,labelKeywords,label=names(labelKeywords),SIMPLIFY=FALSE)


##add a type column, including the official types (in modified format) and "other"
dat96$type <- factor(unlist(labelTypeMap[dat96$EVTYPE]))

##=================================
## PLOTS
##=================================

groupedDat <- dat96 %>% group_by(type)

fatalitiesDat <- groupedDat %>% summarise(fatalities = sum(FATALITIES)) %>% 
  filter(fatalities > 0) %>%
  arrange(fatalities)

injuriesDat <- groupedDat %>% summarise(injuries = sum(INJURIES)) %>% 
  filter(injuries > 0) %>%
  arrange(injuries)
costDat <- groupedDat %>% summarise(cost = sum(DMG)) %>% 
  filter(cost > 0) %>%
  arrange(cost)

barplot(names.arg=costDat$type,height=costDat$cost,
        horiz=TRUE,las=1,cex.names=0.5,log="x")

barplot(names.arg=fatalitiesDat$type,height=fatalitiesDat$fatalities,
        horiz=TRUE,las=1,cex.names=0.5,log="x")

barplot(names.arg=injuriesDat$type,height=injuriesDat$injuries,
        horiz=TRUE,las=1,cex.names=0.5,log="x")

## I think I want to make pie charts
## the bar plot is just a repeat of the table and I can only
## use three figures.
## I should however limit the included values to significant contributors
## and group the rest in "other"
pie(costDat$cost,labels=costDat$type)

pie(fatalitiesDat$fatalities,labels=fatalitiesDat$type)

pie(injuriesDat$injuries,labels=injuriesDat$type)

## here is some quick and dirty code to add cost to label
##I also do some fine tuning to optimize the labels visible
pie(costDat$cost,labels=sprintf("%s $%.1fB",costDat$type,costDat$cost/1000000000))

pie(fatalitiesDat$fatalities,
    labels=sprintf("%s (%.0d)",fatalitiesDat$type,fatalitiesDat$fatalities),
    init.angle = -30,cex=0.75)


