##offcial weather type_sum

offTypes <- c(
  
  ##waves, tide
  "Tsunami",
  "Storm Surge/Tide",
  "High Surf",
  "Seiche",
  
  "Rip Current",
  
  "Astronomical Low Tide",
  
  ##flood
  "Flash Flood",
  "Flood",
  "Coastal Flood",
  "Lakeshore Flood",
  
  "Heavy Rain", # urban or small stream flooding should be classified here
  
  "Hail",
  
  "Lightning",
  
  ##rain, wind
  "Hurricane (Typhoon)", #Hurricane, Typhoon
  "Tropical Depression",
  "Tropical Storm",

  "Thunderstorm Wind", #(arising from convection within 30 minutes of lightning, >50knots or with fatality/injury/damage)  downburst, gustnado, tstm
  "High Wind", # (>35knots sustained or >50)
  "Strong Wind", # (less that high wind)
  
  "Dust Storm", #(wind that raises dust/sand reducing visibility)
  
  "Tornado", #(touching land) landspout
  "Funnel Cloud", #(not touching land, less violent than ronado)
  "Waterspout", #(touching water)
  "Dust Devil", #(does not reacdh clouds - ground effect)
  
  
  ##winter
  "Blizzard",
  "Winter Storm", #(precipitation event that meets criteria)
  "Heavy Snow",
  "Ice Storm",
  "Lake-Effect Snow",
  "Sleet",
  "Winter Weather", #(precipitation event)
  "Avalanche",
  
  #cold
  "Extreme Cold/Wind Chill",
  "Frost/Freeze",
  "Cold/Wind Chill",
  
  #heat
  "Excessive Heat",#(local rules distinguish excessive heat vs heat vs none)
  "Heat",
  
  #fog
  "Dense Fog", #(local guidelines for visbility distance - usually 1/4 mile)
  "Freezing Fog", #(freezes on contact and forms a glaze. can be much longer visibility than dens fog, maybe up to 6mi)
  
  "Debris Flow", #(landslide and other) landslide
  
  #fire
  "Wildfire",
  "Dense Smoke",
  
  "Drought",
  
  "Volcanic Ash",
  
  "Marine Hail",
  "Marine High Wind",
  "Marine Strong Wind",
  "Marine Thunderstorm Wind"
  
)

##these are the official types ocnverted to lowercase
lowerOffTypes <- tolower(offTypes)

##here are some aliases for official types
##the alias is the NAME in the list, the offical value is the VALUE
aliasMap = list()
aliasMap[["hurricane"]] <- "hurricane (typhoon)"
aliasMap[["typhoon"]] <- "hurricane (typhoon)"

aliasMap[["tstm wind"]] <- "thunderstorm wind"
aliasMap[["tstmw"]] <- "thunderstorm wind"
aliasMap[["gustnado"]] <- "thunderstorm wind"
aliasMap[["downburst"]] <- "thunderstorm wind"
aliasMap[["microburst"]] <- "thunderstorm wind"

aliasMap[["non tstm wind"]] <- "high wind"
aliasMap[["non-tstm wind"]] <- "high wind"

aliasMap[["gusty wind"]] <- "strong wind"
aliasMap[["gradient wind"]] <- "strong wind"
aliasMap[["wind damage"]] <- "strong wind"
aliasMap[["wind"]] <- "strong wind"

aliasMap[["blowing dust"]] <- "dust storm"

aliasMap[["landspout"]] <- "tornado"

aliasMap[["landslide"]] <- "debris flow"
aliasMap[["rock slide"]] <- "debris flow"
aliasMap[["mud slide"]] <- "debris flow"
aliasMap[["mudslide"]] <- "debris flow"
aliasMap[["landslump"]] <- "debris flow"
aliasMap[["landslide"]] <- "debris flow"

aliasMap[["wild fire"]] <- "wildfire"
aliasMap[["grass fire"]] <- "wildfire"
aliasMap[["forest fire"]] <- "wildfire"
aliasMap[["brush fire"]] <- "wildfire"

aliasMap[["extreme cold"]] <- "extreme cold/wind chill"
aliasMap[["extreme wind chill"]] <- "extreme cold/wind chill"
aliasMap[["extreme windchill"]] <- "extreme cold/wind chill"

aliasMap[["cold"]] <- "cold/wind chill"
aliasMap[["wind chill"]] <- "cold/wind chill"
aliasMap[["windchill"]] <- "cold/wind chill"
aliasMap[["hypothermia/exposure"]] <- "cold/wind chill"
aliasMap[["hyperthermia/exposure"]] <- "cold/wind chill"

aliasMap[["frost"]] <- "frost/freeze"
aliasMap[["freeze"]] <- "frost/freeze"

aliasMap[["unseasonably warm"]] <- "heat"
aliasMap[["warm weather"]] <- "heat"

aliasMap[["torrential rainfall"]] <- "heavy rain"
aliasMap[["urban/sml stream fld"]] <- "heavy rain" #noted in document
aliasMap[["hvy rain"]] <- "heavy rain"

aliasMap[["excessive snow"]] <- "heavy snow"

aliasMap[["lake effect snow"]] <- "lake-effect snow"

aliasMap[["freezing rain"]] <- "winter weather"
aliasMap[["light snowfall"]] <- "winter weather"
aliasMap[["snow and ice"]] <- "winter weather"
aliasMap[["late season snow"]] <- "winter weather"
aliasMap[["light snow"]] <- "winter weather"
aliasMap[["snow"]] <- "winter weather"
aliasMap[["freezing drizzle"]] <- "winter weather" 
aliasMap[["rain/snow"]] <- "winter weather" 
aliasMap[["snow squall"]] <- "winter weather"
aliasMap[["mixed precipitation"]] <- "winter weather"
aliasMap[["blowing snow"]] <- "winter weather"
aliasMap[["wintry mix"]] <- "winter weather" 
aliasMap[["mixed precip"]] <- "winter weather"
aliasMap[["snow squalls"]] <- "winter weather" 
aliasMap[["falling snow/ice"]] <- "winter weather"

aliasMap[["glaze"]] <- "freezing fog"

aliasMap[["fog"]] <- "dense fog"

aliasMap[["storm surge"]] <- "storm surge/tide"

aliasMap[["heavy surf"]] <- "high surf"
aliasMap[["rough surf"]] <- "high surf"
aliasMap[["hazardous surf"]] <- "high surf"





#----------
# included
#----------

#"wind" 
#"winds"
#"non-severe wind damage"
#"gusty winds"
#"gusty wind"
#"gradient wind"
#"gusty wind/rain"
#"unseasonal rain"
#"rain" 
#"gusty wind/hvy rain" 
#"wind damage" 

#"fog"

#---------
# omitted
#---------

#"dam break" - 2 - both flooding
 

#"coastalstorm" - 2
#"coastal storm" - mostly winter, some just rain - 4

#"thunderstorm" - 2 (1 tree limb: wind; one light snow)

#"ice roads" - 1 
#"ice on road" - 1
#"icy roads" - 17 (but there were numerous repeats)
#"black ice" - 1

#"high water" - 2 - b oth high water in creeks - heavy rain





##this is a vector with the aliases in it
aliases = names(aliasMap)


  