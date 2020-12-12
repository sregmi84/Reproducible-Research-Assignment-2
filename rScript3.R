if(!file.exists("./data/StormData.csv.bz2")){
    
    fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata/data/StormData.csv.bz2"
    destPath <- "./data"
    binData <- getBinaryURL(fileUrl, ssl.verifypeer=0L, followlocation=1L)
    destFileHandle <- file(destPath, open="wb")
    writeBin(binData,destFileHandle)
    close(destFileHandle)
}

rawdata<-read.csv("./StormData.csv", header=TRUE, stringsAsFactors=FALSE) 

dim(rawdata)

fields<-c("EVTYPE","FATALITIES","INJURIES","PROPDMG", "PROPDMGEXP","CROPDMG","CROPDMGEXP")
working<-rawdata[fields]

fatal <- aggregate(FATALITIES ~ EVTYPE, data = working, FUN = sum)
injury <- aggregate(INJURIES ~ EVTYPE, data = working, FUN = sum)

fatal10 <- fatal[order(-fatal$FATALITIES),][1:10, ]
injury10 <- injury[order(-injury$INJURIES),][1:10, ]

unique(working$PROPDMGEXP)
unique(working$CROPDMGEXP)

working$PROPEXP[working$PROPDMGEXP ==  "K"    ]  <-    1000
working$PROPEXP[working$PROPDMGEXP == "M"     ]   <-  1000000
working$PROPEXP[working$PROPDMGEXP == ""      ]   <-  1
working$PROPEXP[working$PROPDMGEXP == "B"     ]   <-  1000000000
working$PROPEXP[working$PROPDMGEXP == "m"     ]   <-  1000000
working$PROPEXP[working$PROPDMGEXP == "+"     ]   <-  0
working$PROPEXP[working$PROPDMGEXP == "0"     ]   <-  1
working$PROPEXP[working$PROPDMGEXP == "5"     ]   <-  100000
working$PROPEXP[working$PROPDMGEXP == "6"     ]   <-  1000000
working$PROPEXP[working$PROPDMGEXP == "?"     ]   <-  0
working$PROPEXP[working$PROPDMGEXP == "4"     ]   <-  10000
working$PROPEXP[working$PROPDMGEXP == "2"     ]   <-  100
working$PROPEXP[working$PROPDMGEXP == "3"     ]   <-  1000
working$PROPEXP[working$PROPDMGEXP == "h"     ]   <-  100
working$PROPEXP[working$PROPDMGEXP == "7"     ]   <-  10000000
working$PROPEXP[working$PROPDMGEXP == "H"     ]   <-  100
working$PROPEXP[working$PROPDMGEXP == "-"     ]   <-  0
working$PROPEXP[working$PROPDMGEXP == "1"     ]   <-  10
working$PROPEXP[working$PROPDMGEXP == "8"     ]   <-  100000000


working$CROPEXP[working$CROPDMGEXP ==  ""     ]   <-  1
working$CROPEXP[working$CROPDMGEXP == "M"     ]   <-  1000000
working$CROPEXP[working$CROPDMGEXP == "K"     ]   <-  1000
working$CROPEXP[working$CROPDMGEXP == "m"     ]   <-  1000000000
working$CROPEXP[working$CROPDMGEXP == "B"     ]   <-  1000000
working$CROPEXP[working$CROPDMGEXP == "?"     ]   <-  0
working$CROPEXP[working$CROPDMGEXP == "0"     ]   <-  1
working$CROPEXP[working$CROPDMGEXP == "k"     ]   <-  1000
working$CROPEXP[working$CROPDMGEXP == "2"     ]   <-  100

working$PROPDMGVAL <- working$PROPDMG * working$PROPEXP
working$CROPDMGVAL <- working$CROPDMG * working$CROPEXP

working$ALLDMGVAL <- working$PROPDMGVAL + working$CROPDMGVAL

propcropdmg <- aggregate(ALLDMGVAL ~ EVTYPE, data = working, FUN = sum)
propcropdmg10<-propcropdmg[order(-propcropdmg$ALLDMGVAL), ][1:10,]

par(mfrow = c(1, 2), mar = c(12, 4, 3, 2), mgp = c(3, 1, 0), las=3,cex = 0.8)
barplot(fatal10$FATALITIES, names.arg=fatal10$EVTYPE, ylim= c(0,8000),col="blue",ylab="Number of Fatalities", main=" Top 10 Events with Highest Fatalities")
barplot(injury10$INJURIES, names.arg=injury10$EVTYPE,ylim= c(0,90000), col="red", ylab="Number of Injuries", main=" Top 10 Events with Highest Injuries")

par(mfrow = c(1, 2), mar = c(12, 4, 3, 2), mgp = c(3, 1, 0), las=3,cex = 0.8, cex.main = 0.9)

barplot((propcropdmg10$ALLDMGVAL)/(1*1000000000), names.arg=propcropdmg10$EVTYPE, col="orange", ylab=" Cost of Property Damage($ billions)", main="Top 10 Events Causing Highest Property/Crop Damage Value")