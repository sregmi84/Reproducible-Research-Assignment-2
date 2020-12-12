## Loading and pre-processing the data

### Download and unzip the data
setwd("~/Coursera/Reproducible Research/Week 4/Reproducible-Research-Assignment-2")
url1 <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
destfile1 <- "repdata_data_StormData.csv.bz2"

library(R.utils)
if(!file.exists(destfile1)) {
    download.file(url1, 
                  destfile = destfile1, 
                  method = "curl")
}

csvfileName <- gsub(".bz2","",destfile1,)
csvfileFullPath <- paste("./Data",csvfileName,sep="/")

if(!file.exists(csvfileFullPath)) {
    bunzip2(filename = destfile1, destname = csvfileFullPath,remove=FALSE,skip = TRUE)
}

#Read the data
stormDataFull<-read.csv(csvfileFullPath, header=TRUE, stringsAsFactors=FALSE) 
dim(stormDataFUll)
head(stormDataFull)
str(stormDataFull)
nrow(stormDataFull)
ncol(stormDataFull)

#Keep only essential columns
fieldsToKeep<-c("STATE__","STATE","BGN_DATE","EVTYPE","FATALITIES","INJURIES","PROPDMG", "PROPDMGEXP","CROPDMG","CROPDMGEXP","REFNUM")
stormData <- stormDataFull[fieldsToKeep]
dim(stormData)
head(stormData)
str(stormData)

#Create Date and Year variable
library(lubridate)
stormData$BGN_DATE <- with(stormData,as.Date(BGN_DATE, format="%m/%d/%Y"))   
stormData$BGN_YEAR <- with(stormData,year(BGN_DATE)) 
table(stormData$BGN_YEAR)
unique(stormData$EVTYPE)

#Remove Unknown State (XX)
unique(stormData[c("STATE__","STATE")])
stormDataClean <- subset(stormData,!(STATE=="XX"))

#Explore Data
library(dplyr)
test <- stormDataClean %>% group_by(BGN_YEAR,EVTYPE) %>% summarize(total=n())
with(test, plot(BGN_YEAR,total))
#Data was not recorded for all weather events in the years prior to 1996.


#Calculate Crop Damage and Property Damage based on exponents
unique(stormDataClean$PROPDMGEXP)
unique(stormDataClean$CROPDMGEXP)

library(dplyr)
stormDataClean <- stormDataClean %>%
    #Do nothing if exponent is blank or 0. If exponent is ? change value to 0.
    mutate(CROPDMG = ifelse(CROPDMGEXP == "?", 0, CROPDMG),
           CROPDMG = ifelse(CROPDMGEXP == "2", CROPDMG * 100, CROPDMG),
           CROPDMG = ifelse(CROPDMGEXP == "k", CROPDMG * 1000, CROPDMG),
           CROPDMG = ifelse(CROPDMGEXP == "K", CROPDMG * 1000, CROPDMG),
           CROPDMG = ifelse(CROPDMGEXP == "m", CROPDMG * 1000000, CROPDMG),
           CROPDMG = ifelse(CROPDMGEXP == "M", CROPDMG * 1000000, CROPDMG),
           CROPDMG = ifelse(CROPDMGEXP == "B", CROPDMG * 1000000000, CROPDMG)
           ) %>%
    #Do nothing if exponent is blank or 0 or +. If exponent is ? change value to 0. ALso - could mean blank or negative value which can't be right, so change to 0.
    mutate(PROPDMG = ifelse(PROPDMGEXP == "?", 0, PROPDMG),
           PROPDMG = ifelse(PROPDMGEXP == "-", 0, PROPDMG),
           PROPDMG = ifelse(PROPDMGEXP == "1", PROPDMG * 10, PROPDMG),
           PROPDMG = ifelse(PROPDMGEXP == "2", PROPDMG * 100, PROPDMG),
           PROPDMG = ifelse(PROPDMGEXP == "3", PROPDMG * 1000, PROPDMG),
           PROPDMG = ifelse(PROPDMGEXP == "4", PROPDMG * 10000, PROPDMG),
           PROPDMG = ifelse(PROPDMGEXP == "5", PROPDMG * 100000, PROPDMG),
           PROPDMG = ifelse(PROPDMGEXP == "6", PROPDMG * 1000000, PROPDMG),
           PROPDMG = ifelse(PROPDMGEXP == "7", PROPDMG * 10000000, PROPDMG),
           PROPDMG = ifelse(PROPDMGEXP == "8", PROPDMG * 100000000, PROPDMG),
           PROPDMG = ifelse(PROPDMGEXP == "h", PROPDMG * 100, PROPDMG),
           PROPDMG = ifelse(PROPDMGEXP == "H", PROPDMG * 100, PROPDMG),
           PROPDMG = ifelse(PROPDMGEXP == "K", PROPDMG * 1000, PROPDMG),
           PROPDMG = ifelse(PROPDMGEXP == "m", PROPDMG * 1000000, PROPDMG),
           PROPDMG = ifelse(PROPDMGEXP == "M", PROPDMG * 1000000, PROPDMG),
           PROPDMG = ifelse(PROPDMGEXP == "B", PROPDMG * 1000000000, PROPDMG))

#Read in Inflation data from BLS website
CPI <- read.csv("https://download.bls.gov/pub/time.series/cu/cu.data.1.AllItems", 
                stringsAsFactors = FALSE, header = TRUE, sep = "")

#Annual average "M13" is not available for early years so use the end of year "M12" values
CPI <- CPI %>%
    filter(series_id == "CUUR0000SA0", (year >= 1950 & year <= 2011), period == "M12") %>%
    mutate(multiplier = value[year == 2011]/value) %>%
    select(2, 6)

#Merge CPI data with the clean storm data
stormDataClean <- merge(x = stormDataClean, y = CPI, by.x = "BGN_YEAR", by.y="year", all.x = TRUE)

#Adjust Crop and Property Damage for Inflation using CPI data
stormDataClean$CROPDMGADJ <- with(stormDataClean, CROPDMG * multiplier)
stormDataClean$PROPDMGADJ <- with(stormDataClean, PROPDMG * multiplier)

#Calculate Total Inflation Adjusted Damage (Property + Crop)
stormDataClean$ALLDMGADJ <- with(stormDataClean, CROPDMGADJ + PROPDMGADJ)

#Group Similar Event Types into same category 
library(stringr)
stormDataClean <- stormDataClean[] %>% mutate(EVCATEGORY = str_to_lower(EVTYPE)) %>% 
       mutate(EVCATEGORY = str_trim(EVCATEGORY, side = "both")) %>%
       mutate(EVCATEGORY = str_replace_all(EVCATEGORY, pattern = "tstm", replacement = "thunderstorm"),
              EVCATEGORY = str_replace_all(EVCATEGORY, pattern = "\\s\\(g\\d*\\)", replacement = ""),
              EVCATEGORY = str_replace_all(EVCATEGORY, pattern = "^heat", replacement = "excessive heat"),
              EVCATEGORY = str_replace_all(EVCATEGORY, pattern = "wild/forest fire", replacement = "wildfire"),
              EVCATEGORY = str_replace_all(EVCATEGORY, pattern = "strong wind", replacement = "high wind"),
              EVCATEGORY = str_replace_all(EVCATEGORY, pattern = "winter weather", replacement = "winter storm"),
              EVCATEGORY = str_replace_all(EVCATEGORY, pattern = "^hurricane$", replacement = "hurricane/typhoon")) %>%
       mutate(EVCATEGORY = str_to_title(EVCATEGORY))

#Compute Top 10 Total Economic Damage (Crop and Property Damage)
economicDamage10 <- stormDataClean %>% group_by(EVCATEGORY) %>%
    summarize(TOTALDMGADJ = sum(ALLDMGADJ, na.rm = TRUE)) %>%
    arrange(desc(TOTALDMGADJ)) %>% top_n(10)

#Plot BarPlot
par(mfrow = c(1, 2), mar = c(12, 4, 3, 2), mgp = c(3, 1, 0), las=3, cex = 0.8, cex.main = 0.9)

barplot((economicDamage10$TOTALDMGADJ)/(1*1000000000), names.arg=economicDamage10$EVCATEGORY, col="blue", ylab= "Total Economic Damage($ billions)", 
        main="Top 10 Weather Events Causing Highest OVerall (Property + Crop) Damage Value\nAdjusted for Inflation")


#Compute Top 10 Fatalities and Injuries Causing Events
library(tidyr)
casualtyEvents10 <- stormDataClean %>% 
    gather(FATALITIES, INJURIES, key = CASUALTYTYPE, value = CASUALTYCOUNT) %>%
    filter( CASUALTYCOUNT > 0) %>%
    group_by(EVCATEGORY) %>%
    summarize(TOTALCASUALTYCOUNT = sum( CASUALTYCOUNT)) %>%
    arrange(desc(TOTALCASUALTYCOUNT)) %>%
    top_n(10, TOTALCASUALTYCOUNT) %>%
    select(1)

casualty10 <- stormDataClean %>%
    gather(FATALITIES, INJURIES, key = CASUALTYTYPE, value = CASUALTYCOUNT) %>%
    filter(CASUALTYCOUNT > 0, EVCATEGORY %in% casualtyEvents10$EVCATEGORY) %>%
    mutate(EVCATEGORY = factor(EVCATEGORY, casualtyEvents10$EVCATEGORY)) %>%
    group_by(CASUALTYTYPE,EVCATEGORY) %>%
    summarize(TOTALCASUALTYCOUNT = sum(CASUALTYCOUNT))

#Plot
library(ggplot2)
ggplot(casualty10, aes(x = reorder(EVCATEGORY,TOTALCASUALTYCOUNT), y = TOTALCASUALTYCOUNT, fill = CASUALTYTYPE)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    theme_light() +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
    labs(title = "Fatalities and Injuries by Event Type", 
         subtitle = "Top 10 Weather Events", 
         x = "Weather Event",
         y = "Total Number of Casualties")

