# Clean BGN$DATE column

library(dplyr)      # for easier data frame manipulation
library(stringr)    # for string manipulation

data <- storm_data
dim(data)
    # 902297 x 37


# Edit date column

data$BGN_DATE <- as.Date(data$BGN_DATE, "%m/%d/%Y")

# https://www.ncdc.noaa.gov/stormevents/details.jsp?type=eventtype
# Before 1Jan1996, only few event types were recorded,
# so we focus on events after 1Jan1996 !



# Filtering by date

data <- filter(data, data$BGN_DATE>="1996-01-01")
dim(data)
    # 653530 x 37




# Restrict our data to important columns and to observations where there is
# some fatalities or injury, or property or crop damage

data <- data %>% select(EVTYPE, FATALITIES, INJURIES, PROPDMG,
                        PROPDMGEXP, CROPDMG, CROPDMGEXP) %>%
    filter(FATALITIES>0 | INJURIES>0 | PROPDMG>0| CROPDMG>0)

dim(data)
    # 201318 x 7
    # This has already significantly shortened our database
    # to less than a fourth of its original number of rows.



# Checking if the data in all cols EXCEPT EVTYPE is clean

class(data$FATALITIES)
class(data$INJURIES)
    # both numeric
unique(data$FATALITIES)
unique(data$INJURIES)
    # both with non-negative values

class(data$PROPDMG)
class(data$CROPDMG)
    # both numeric
all(data$PROPDMG>=0)
all(data$CROPDMG>=0)
    # both with non-negative values

class(data$PROPDMGEXP)
class(data$CROPDMGEXP)
    # both character
unique(data$PROPDMGEXP)
    # "K" ""  "M" "B"
unique(data$CROPDMGEXP)
    # "K" ""  "M" "B"



# Edit EVTYPE column

# There should be 48 different event types.
# (see p.6 in documentation.pdf)

# There are a lot more types of events in the data frame:
length(unique(data$EVTYPE))
    # 222

data$EVTYPE <- tolower(data$EVTYPE)     # all in lowercase
length(unique(data$EVTYPE))
    # 186

data$EVTYPE <- str_trim(data$EVTYPE)    # remove extra spaces at beginning or end of string
data$EVTYPE <- gsub("\\s+", " ", data$EVTYPE)   # remove multiple spaces
length(unique(data$EVTYPE))
    # 181



# abbreviations
length(grep('tstm', data$EVTYPE))   # how many times does 'tstm' show up in dataset
    # 62383
length(grep('wnd', data$EVTYPE))
    # 0
length(grep('fld', data$EVTYPE))
    # 702
length(grep('cstl', data$EVTYPE))
    # 2
length(grep('strm', data$EVTYPE))
    # 0
length(grep('sml', data$EVTYPE))
    # 702


data$EVTYPE <- gsub('tstm', 'thunderstorm', data$EVTYPE)
#data$EVTYPE <- gsub('wnd', 'wind', data$EVTYPE)
data$EVTYPE <- gsub("fld", "flood", data$EVTYPE)
data$EVTYPE <- gsub("cstl", "coastal", data$EVTYPE)
#data$EVTYPE <- gsub("strm", "stream", data$EVTYPE)
data$EVTYPE <- gsub("sml", "small", data$EVTYPE)
data$EVTYPE <- gsub("hvy", "heavy", data$EVTYPE)

length(unique(data$EVTYPE))
    # 178



# typos
length(grep('floodg', data$EVTYPE))
    # 0

# data$EVTYPE <- gsub('floodg', "flood", data$EVTYPE)



# plurals
length(grep('winds', data$EVTYPE))
    # 77
length(grep('thunderstorms', data$EVTYPE))
    # 0
length(grep('currents', data$EVTYPE))
    # 239

data$EVTYPE <- gsub("winds", "wind", data$EVTYPE)
#data$EVTYPE <- gsub("thunderstorms", "thunderstorm", data$EVTYPE)
data$EVTYPE <- gsub("currents", "current", data$EVTYPE)


# Ambiguities

data$EVTYPE <- gsub("flooding", "flood", data$EVTYPE)

length(unique(data$EVTYPE))
# 171




# Further processing

official <- tolower(c('Astronomical Low Tide', 'Avalanche', 'Blizzard', 'Coastal Flood',
               'Cold/Wind Chill', 'Debris Flow', 'Dense Fog', 'Dense Smoke', 'Drought',
               'Dust Devil', 'Dust Storm', 'Excessive Heat', 'Extreme Cold/Wind Chill',
               'Flash Flood', 'Flood', 'Frost/Freeze', 'Funnel Cloud', 'Freezing Fog',
               'Hail', 'Heat', 'Heavy Rain', 'Heavy Snow', 'High Surf', 'High Wind',
               'Hurricane (Typhoon)', 'Ice Storm', 'Lake-Effect Snow', 'Lakeshore Flood',
               'Lightning', 'Marine Hail', 'Marine High Wind', 'Marine Strong Wind',
               'Marine Thunderstorm Wind', 'Rip Current', 'Seiche', 'Sleet',
               'Storm Surge/Tide', 'Strong Wind', 'Thunderstorm Wind', 'Tornado',
               'Tropical Depression', 'Tropical Storm', 'Tsunami', 'Volcanic Ash',
               'Waterspout', 'Wildfire', 'Winter Storm', 'Winter Weather'))

typeleft <- unique(data$EVTYPE)
write.table(typeleft, './evtypes_left.txt')
    # At this point we can just look at the whole list of different event types
    # we still have and work from there.

sum(typeleft %in% official)
    # 45 cats are the official ones

official[!(official %in% typeleft)]
    # the three that weren't there are:
    # "debris flow"         "hurricane (typhoon)" "sleet"

typeleft <- typeleft[!(typeleft %in% official)]
length(typeleft)
    # 126


# We start with the cases of the 3 missing official even types:
# "debris flow"         "hurricane (typhoon)" "sleet"

typeleft[grep('hurricane', typeleft)]
    # "hurricane"         "hurricane edouard" "hurricane/typhoon"
data$EVTYPE[grep("hurricane|^typhoon$", data$EVTYPE)] <- "hurricane (typhoon)"

typeleft <- typeleft[-grep('hurricane', typeleft)]
    # removes those 3 cases from typeleft
typeleft <- typeleft[typeleft != 'typhoon']
length(typeleft)
    # 122


# There is no occurrence of 'sleet', 'debris' or 'flow' in the types left
# so we replace them by other appropriate ones that do show up

typeleft[grep("(slides?|slump)$", typeleft)]
    # "mudslides"  "mudslide"   "mud slide"  "landslides" "rock slide" "landslide"

data$EVTYPE[grep("(slides?|slump)$", data$EVTYPE)] <- 'debris flow'

typeleft <- typeleft[-grep("(slides?|slump)$", typeleft)]
    # removes those 6 cases from typeleft
length(typeleft)
    # 116

data$EVTYPE[grep("(^mixed prec)|(^(light )?freezing rain$)", data$EVTYPE)] <- 'sleet'

typeleft <- typeleft[-grep("(^mixed prec)|(^(light )?freezing rain$)",typeleft)]


# Thunderstorm

typeleft[grep("thunderstorm", typeleft)]
# [1] "thunderstorm wind/hail"          "thunderstorm wind (g45)"         "thunderstorm wind 40"           
# [4] "thunderstorm wind 45"            "thunderstorm wind (41)"          "thunderstorm wind (g40)"        
# [7] "thunderstorm"                    "thunderstorm wind and lightning" "thunderstorm wind (g35)"        
# [10] "thunderstorm wind g45"           "non-thunderstorm wind"           "non thunderstorm wind"          

typeleft[grep("( g?[0-9]*| \\(g?[0-9]*\\))$", typeleft)]
# [1] "thunderstorm wind (g45)" "thunderstorm wind 40"    "thunderstorm wind 45"   
# [4] "thunderstorm wind (41)"  "thunderstorm wind (g40)" "thunderstorm wind (g35)"
# [7] "thunderstorm wind g45"

data$EVTYPE <- gsub("( g?[0-9]*| \\(g?[0-9]*\\))$", "", data$EVTYPE)
# all of these become "thunderstorm wind"
# [1] "thunderstorm wind (g45)" "thunderstorm wind 40"    "thunderstorm wind 45"   
# [4] "thunderstorm wind (41)"  "thunderstorm wind (g40)" "thunderstorm wind (g35)"
# [7] "thunderstorm wind g45"

typeleft <- typeleft[-grep("( g?[0-9]*| \\(g?[0-9]*\\))$", typeleft)]

data$EVTYPE[grep("^non(-| )thunderstorm wind$", data$EVTYPE)] <- "strong wind"
data$EVTYPE[grep("^thunderstorm( wind(/hail| and lightning))?$", data$EVTYPE)] <- "thunderstorm wind"

typeleft <- typeleft[grep("^non(-| )thunderstorm wind$", typeleft)]
typeleft <- typeleft[-grep("^thunderstorm( wind(/hail| and lightning))?$", typeleft)]

#typeleft[grep("thunderstorm", typeleft)]

data$EVTYPE[grep("burst$", data$EVTYPE)] <- "thunderstorm wind"

typeleft <- typeleft[-grep("burst$", typeleft)]


# High surf

typeleft[grep('surf', typeleft)]

data$EVTYPE[grep('surf', data$EVTYPE)] <- "high surf"

typeleft <- typeleft[-grep('surf', typeleft)]
typeleft[grep('high', typeleft)]

data$EVTYPE[grep('(^high (sw|se|wa))|(high tide$)', data$EVTYPE)] <- "high surf"

typeleft <- typeleft[-grep('(^high (sw|se|wa))|(high tide$)', typeleft)]


# Storm surge/tide

data$EVTYPE[grep("seas$|^(storm surge)$|^((coastal ?storm)|(wind and wave))$", data$EVTYPE)] <- "storm surge/tide"

typeleft <- typeleft[-grep("seas$|^(storm surge)$|^((coastal ?storm)|(wind and wave))$", typeleft)]


# Wind

data$EVTYPE[grep("^hypo|^extreme windchill$", data$EVTYPE)] <- "extreme cold/wind chill"
data$EVTYPE[grep("^(whirlwind|landspout)$", data$EVTYPE)] <- "tornado"
data$EVTYPE[grep("(^(gusty|gradient) wind)|(^wind$)|(wind damage)", data$EVTYPE)] <- "strong wind"
data$EVTYPE[grep("^((marine accident)|(rogue wave))$", data$EVTYPE)] <- "marine strong wind"
    # See info about this accident here:
    # storm_data[tolower(storm_data$EVTYPE) == 'marine accident',]
    # storm_data[tolower(storm_data$EVTYPE) == 'rogue wave',]

typeleft <- typeleft[-grep('^hypo|^extreme windchill$', typeleft)]
typeleft <- typeleft[-grep('^(whirlwind|landspout)$', typeleft)]
typeleft <- typeleft[-grep('(^(gusty|gradient) wind)|(^wind$)|(wind damage)', typeleft)]
typeleft <- typeleft[-grep("^((marine accident)|(rogue wave))$", typeleft)]


# Heavy Rain

data$EVTYPE[grep("^((torrential rainfall)|(unseasonal rain)|(rain)|(drowning))$", data$EVTYPE)] <- "heavy rain"

    # We include the 1 case of drowning, because it was due to heavy rain.
    # Read Remarks in:
    # storm_data[tolower(storm_data$EVTYPE) == 'drowning',]

typeleft <- typeleft[-grep("^((torrential rainfall)|((unseasonal )?rain)|(drowning))$", typeleft)]


# Cold

data$EVTYPE[grep('cold$', data$EVTYPE)] <- "extreme cold/wind chill"
data$EVTYPE[data$EVTYPE == "cold temperature" | data$EVTYPE == "cold weather" ] <- "extreme cold/wind chill"

typeleft <- typeleft[-grep('cold$', typeleft)]
typeleft <- typeleft[typeleft != "cold temperature" & typeleft != "cold weather" ]


# Flood

data$EVTYPE[grep('erosion|(tidal flood)', data$EVTYPE)] <- "coastal flood"
data$EVTYPE[grep("^(river|urban|(ice jam))", data$EVTYPE)] <- "flood"
data$EVTYPE[grep("/flood$", data$EVTYPE)] <- "flood"
data$EVTYPE[grep("^dam ", data$EVTYPE)] <- "flash flood"

typeleft <- typeleft[-grep('erosion|(tidal flood)', typeleft)]
typeleft <- typeleft[-grep("^(river|urban|(ice jam))",typeleft)]
typeleft <- typeleft[-grep("(/flood$)|(^dam )", typeleft)]


# Frost/Freeze

data$EVTYPE[grep('freez|(frost$)', data$EVTYPE)] <- "frost/freeze"

typeleft <- typeleft[-grep('freez|(frost$)', typeleft)]


# Fog

data$EVTYPE[grep('^fog$', data$EVTYPE)] <- "dense fog"
data$EVTYPE[grep('glaze', data$EVTYPE)] <- 'freezing fog'

typeleft <- typeleft[-grep('^(fog|glaze)$',typeleft)]


# Snow

data$EVTYPE <- gsub('^lake ','lake-', data$EVTYPE)
    # replacing 'lake effect snow' with 'lake-effect snow'

unique(data$EVTYPE[grep('snow', data$EVTYPE)])

snow <- grep('snow', data$EVTYPE)
data$EVTYPE[snow[! (snow %in% grep('^((heavy snow)|(lake-effect snow))$',data$EVTYPE))]] <- "heavy snow"

typeleft <- typeleft[-grep('snow', typeleft)]


# Ice storm

data$EVTYPE[grep('(^ice (o|r))|(^icy)|(ice$)', data$EVTYPE)] <- "ice storm"

typeleft <- typeleft[-grep('ice|icy', typeleft)]


# Heat

data$EVTYPE[grep('^((heat wave$)|(record heat$)|hyper)', data$EVTYPE)] <- "excessive heat"
data$EVTYPE[grep("warm", data$EVTYPE)] <- "heat"

typeleft <- typeleft[-grep('^((heat wave$)|(record heat$)|hyper)', typeleft)]
typeleft <- typeleft[-grep("warm", typeleft)]


# Wildfire

data$EVTYPE[grep(' fire$', data$EVTYPE)] <- "wildfire"

typeleft <- typeleft[-grep(' fire$', typeleft)]


# Winter weather

data$EVTYPE[grep('mix$', data$EVTYPE)] <- "winter weather"

typeleft <- typeleft[-grep('mix$', typeleft)]


# Dust storm

data$EVTYPE[grep("dust$", data$EVTYPE)] <- "dust storm"

typeleft <- typeleft[-grep("dust$", typeleft)]


# Hail

data$EVTYPE[grep("^small hail$", data$EVTYPE)] <- "hail"

typeleft <- typeleft[-grep('^small hail$', typeleft)]


# Other

sum(data$EVTYPE == 'other')
    # 34

data <- data[data$EVTYPE != "other",]   # remove other events

typeleft <- typeleft[typeleft != "other"]


length(typeleft)



typeleft <- unique(data$EVTYPE)
typeleft %in% official
length(typeleft)
typeleft



# unique( data$EVTYPE[grep('', data$EVTYPE)] )

# data$EVTYPE[grep('', data$EVTYPE)]

# typeleft[grep('', typeleft)]

# typeleft <- typeleft[-grep('', typeleft)]
