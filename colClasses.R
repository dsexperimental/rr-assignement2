## column name processing to redude load time/size
colNames <- c("STATE__", "BGN_DATE", "BGN_TIME", "TIME_ZONE", "COUNTY", 
              "COUNTYNAME", "STATE", "EVTYPE", "BGN_RANGE", "BGN_AZI",
              "BGN_LOCATI", "END_DATE", "END_TIME", "COUNTY_END", "COUNTYENDN", 
              "END_RANGE", "END_AZI", "END_LOCATI", "LENGTH", "WIDTH", "F",
              "MAG", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", 
              "CROPDMG", "CROPDMGEXP", "WFO", "STATEOFFIC", "ZONENAMES", 
              "LATITUDE", "LONGITUDE", "LATITUDE_E", "LONGITUDE_", "REMARKS",
              "REFNUM") 

##These are the column names I want to keep
colToKeep <- c("BGN_DATE","EVTYPE","FATALITIES","INJURIES","PROPDMG",
               "PROPDMGEXP","CROPDMG","CROPDMGEXP","REMARKS")

colToKeepClasses <- c("character","character","numeric","numeric","numeric",
                "character","numeric","character","character")

indexInColNames <- match(colToKeep,colNames)

## Generate the col classes - set all to NULL except the columns we want
colClasses <- rep("NULL",length(colNames))
colClasses[indexInColNames] <- colToKeepClasses
