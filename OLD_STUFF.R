library(jsonlite)

install.packages("jsonlite")

xxx <- toJSON(aliasMap,pretty=TRUE)

yyy <- fromJSON(xxx)



aliases <- names(aliasMap)


##===============================
## write types to file
##===============================


officialTypesJson <- toJSON(officialTypes,pretty=TRUE)

writeLines(officialTypesJson,"officialTypes.json")


##===============================
## write aliasToType to a file
##===============================
## - convert to typeAliases

## - write file

## convert aliasMap to typeAliases
typeAliases <- list()
addAlias <- function(type,alias) {
  if(type %in% names(typeAliases)) {
    typeAliases[[type]] <<- c(typeAliases[[type]],alias)
  }
  else {
    typeAliases[[type]] <<- alias
  }
  
}
mapply(addAlias,aliasMap,names(aliasMap))

##alphabetize the type list by key
typeAliases <- typeAliases[order(names(typeAliases))]


## convert to pretty json
typeAliasesJson <- toJSON(typeAliases,pretty=TRUE)

writeLines(typeAliasesJson,"typeAliases.json")

##===============================
## read aliases
##===============================

typeAliases <- fromJSON("typeAliases.json")

aliasToType <- list()
addTypeAlias <- function(type,aliases) {
  aliasToType[aliases] <<- type
}
mapply(addTypeAlias,names(typeAliases),typeAliases,SIMPLIFY=FALSE)

#####################



barplot(names.arg=costDat$type,height=costDat$cost,
        horiz=TRUE,las=1,cex.names=0.5,log="x")

barplot(names.arg=fatalitiesDat$type,height=fatalitiesDat$fatalities,
        horiz=TRUE,las=1,cex.names=0.5,log="x")

barplot(names.arg=injuriesDat$type,height=injuriesDat$injuries,
        horiz=TRUE,las=1,cex.names=0.5,log="x")

###########################################
## APPENDIX
###########################################


## subset

## this returns a randomly selected subset roughly a frac of original length
getSubset <- function(df,frac) {
  df[as.logical(rbinom(nrow(df),size=1,prob=frac)),]
}

#datShort = getSubset(dat,.05)
#datShort96 <- datShort[datShort$BGN_DATE >= "1996-01-01",]


###################################################


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