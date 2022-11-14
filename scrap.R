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
getKeywordTypes <- function(keywords) {
  if(length(keywords) == 1) {
    type <- keywordToType[[keywords]]
  }
  else {
    type <- "other"
  }
  type
}
labelTypeMap <- lapply(labelKeywords,getKeywordTypes)





###########################################
## APPENDIX
###########################################







getEtf <- function(df,et,regTexts) {
  origCols <- ncol(df)
  
  addCol <- function(regText) {
    df[[regText]] <<- str_detect(et,regex(regText,ignore_case=TRUE))
  }
  
  sapply(regTexts,addCol)

  df$total <- apply(df[,(origCols+1):ncol(df)],1,sum)
  
  df
}



#make a dataframe with a col for types and a col for exact match flag
#other cols will tell if keywords are included
#and a col of total matched keywords
etf <- data.frame(et = et)
etf$exact <- et %in% lowerOffTypes
etf$alias <- aliasMap[et]

etf <- getEtf(etf,et,c(lowerOffTypes,aliases))

##exact
exactMatches <- sum(etf$exact)
aliasMatches <- sum(etf$alias != "NULL")
containsCount <- sum(etf$total > 0)
##unmatched
etf[etf$total == 0,"et"]
etf[(etf$total == 1)&(!etf$exact)&(etf$alias == "NULL"),"et"]
etf[(etf$total == 2)&(!etf$exact)&(etf$alias == "NULL"),"et"]
etf[(etf$total > 2)&(!etf$exact)&(etf$alias == "NULL"),"et"]

############################################################


## other stuff

etf$et[etf$thunderstorm] 

lowerUnmatched <- etf[etf$exact == FALSE,"et"]

lowerPresentTypes <- etf[etf$exact == TRUE,"et"]

lowerMissingTypes <- lowerOffTypes[!(lowerOffTypes %in% lowerPresentTypes)]
lowerPresentTypes2 <- lowerOffTypes[lowerOffTypes %in% lowerPresentTypes]


#scrap
getUsage <- function(et,regText) {
  et[str_detect(et,regex(regText,ignore_case=TRUE))]
}

getUsage(et,"Marine")

##JUNK
matchMat <- function(et,regTexts) {
  nc <- length(regTexts)
  nr <- length(et)
  m = matrix(logical(nr*nc),nrow=nr,ncol=nc)
  
  addCol <- function(regText) {
    str_detect(et,regex(regText,ignore_case=TRUE))
  }
  
  m = sapply(regTexts,addCol)
  
  match1 <- apply(m,1,function(row) regTexts[row][1])
  match2 <- apply(m,1,function(row) regTexts[row][2])
  match3 <- apply(m,1,function(row) regTexts[row][3])
  
  cbind(match1,match2,match3)
}


##===========================
## event types
##===========================

getum <- function(regText) {
 lowerUnmatched[str_detect(lowerUnmatched,regex(regText,ignore_case=TRUE))]
}
getum("thunderstorm")

##==============================
## bar plot
##==============================

##sample, needs refining
xxx = runif(48,100)
barplot(names.arg=offTypes,height=xxx,horiz=TRUE,las=1,cex.names=0.5)

##===========================
##look at data for inflation and other
##===========================



##scatter plot of all data
## noticeable change in 90s - see data sourcing
ggplot(dataShort,aes(BGN_DATE,log10(DMG))) + geom_point() + geom_smooth(method = "lm")

##before 92 - note the increasing cost trend (not infalation adjusted?)
ggplot(dataShort[dataShort$BGN_DATE < "1992-01-01",],aes(BGN_DATE,log(DMG))) + geom_point() + geom_smooth(method = "lm")

##after 1/1/96 - cost constant over time (inflation adjusted?)
ggplot(dataShort[dataShort$BGN_DATE > "1996-01-01",],aes(BGN_DATE,log10(DMG))) + geom_point() + geom_smooth(method = "lm")


##=========================
## test
##=========================

datShort <- dat[1:1000,]

##https://rstudio-pubs-static.s3.amazonaws.com/58957_37b6723ee52b455990e149edde45e5b6.html

convertExpCol <- function(df,targetCol,sourceCol) {
  df[[targetCol]] <- NA
  
  ##metric prefixes
  df$PEXP[str_detect(df$PROPDMGEXP,"[hH]")] = 2
  df$PEXP[str_detect(df$PROPDMGEXP,"[kK]")] = 3
  df$PEXP[str_detect(df$PROPDMGEXP,"[mM]")] = 6
  df$PEXP[str_detect(df$PROPDMGEXP,"[bB]")] = 9
  df$PEXP[str_detect(df$PROPDMGEXP,"[tT]")] = 12
  
  #check for any alpha values
  singleLetterExpLeft <- sum(str_detect(datShort$PROPDMGEXP[is.na(datShort$PEXP)],"[a-zA-Z]"))
  if(singleLetterExpLeft > 0) {
    warning("There are unaccounted letter values for exponent! Investigate")
  }
  
  ##number entries
  isnum <- str_detect(df$PROPDMGEXP,"[0-9]+")
  df$PEXP[isnum] = as.numeric(df$PROPDMGEXP[isnum])
  
  ##other - these are all 0 except "+', which is 1
  df$PEXP[str_detect(df$PROPDMGEXP,"+")] = 1
  df$PEXP[is.na(df$PEXP)] = 0 
  
  df
}






dataShort = data[as.logical(rbinom(nrow(data),size=1,prob=.01)),]

unique(dataShort$EVTYPE)

summary(dataShort$PROPDMG)


damDataShort <- filter(dataShort,(PROPDMG > 0) | (CROPDMG > 0) | (FATALITIES > 0) | (INJURIES > 0))
dim(damDataShort)

damData <- filter(data,(PROPDMG > 0) | (CROPDMG > 0) | (FATALITIES > 0) | (INJURIES > 0))



unique(damDataShort$EVTYPE)