setwd("~/Coursera RepRes Assignment 2")

library(R.utils)
library(data.table)

# Uncompress the file 
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", "repdata_data_StormData.csv.bz2")

bunzip2("repdata_data_StormData.csv.bz2", remove = FALSE)

# Read into R
data <- fread("repdata_data_StormData.csv")

library(pdftools)
library(tm)
library(stringr)
events <- pdf_text("https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf")

summary(events)
events <-Corpus(VectorSource(events))
events <- unlist(events[[6]][1])

events <- stripWhitespace(events)
events <- str_extract(events, pattern = "Astronomical.*Winter Weather")
events <- str_split(events, pattern = "\\s[A-Z]\\s")
events <- unlist(events)
events <- tolower(events)

events

data$PROPDMGEXP[data$REFNUM == 605943] <- "M"

library(dplyr)
library(lubridate)
library(stringdist)

clean_data <- data %>%
    mutate(BGN_DATE = mdy_hms(BGN_DATE)) %>%
    filter(STATE__ < 60, BGN_DATE >=ymd(19960101)) %>%
    select(1, 2, 7, 8, 23:28, 37)clean_data <- clean_data %>%
    mutate(EVTYPE = str_to_lower(EVTYPE),
           EVTYPE = str_trim(EVTYPE, side = "left"),
           EVTYPE = str_replace_all(EVTYPE, pattern = "tstm", replacement = "thunderstorm"),
           EVTYPE = str_replace_all(EVTYPE, pattern = "\\s\\(g\\d*\\)", replacement = ""),
           EVTYPE = str_replace_all(EVTYPE, pattern = "^heat", replacement = "excessive heat"),
           EVTYPE = str_replace_all(EVTYPE, pattern = "wild/forest fire", replacement = "wildfire"),
           EVTYPE = str_replace_all(EVTYPE, pattern = "strong wind", replacement = "high wind"),
           EVTYPE = str_replace_all(EVTYPE, pattern = "winter weather", replacement = "winter storm"),
           EVTYPE = str_replace_all(EVTYPE, pattern = "^hurricane$", replacement = "hurricane (typhoon)"))

clean_data <- clean_data %>%
    mutate(Eventcode = amatch(EVTYPE, events, maxDist = 5)) %>%
    mutate(Event = ifelse(is.na(Eventcode), EVTYPE, events[Eventcode]))

clean_data <- clean_data %>%
    mutate(year = year(BGN_DATE)) %>%
    mutate(CROPDMG = ifelse(CROPDMGEXP == "K", CROPDMG * 1000, CROPDMG),
           CROPDMG = ifelse(CROPDMGEXP == "M", CROPDMG * 1000000, CROPDMG),
           CROPDMG = ifelse(CROPDMGEXP == "B", CROPDMG * 1000000000, CROPDMG)) %>%
    mutate(PROPDMG = ifelse(PROPDMGEXP == "K", PROPDMG * 1000, PROPDMG),
           PROPDMG = ifelse(PROPDMGEXP == "M", PROPDMG * 1000000, PROPDMG),
           PROPDMG = ifelse(PROPDMGEXP == "B", PROPDMG * 1000000000, PROPDMG))

CPI <- read.csv("https://download.bls.gov/pub/time.series/cu/cu.data.1.AllItems", 
                stringsAsFactors = FALSE, header = TRUE, sep = "")
CPI <- CPI %>%
    filter(series_id == "CUUR0000SA0", (year >= 1996 & year <= 2011), period == "M13") %>%
    mutate(multiplier = value[year == 2011]/value) %>%
    select(2, 6)

library(ggplot2)
library(tidyr)

top10_f_i2 <- clean_data %>%
    gather(FATALITIES, INJURIES, key = Casualty, value = Number) %>%
    filter(Number > 0) %>%
    group_by(Event) %>%
    summarize(Total = sum(Number)) %>%
    arrange(Total) %>%
    top_n(10, Total) %>%
    mutate(Event = str_to_title(Event)) %>%
    select(1)

f_i2 <- clean_data %>%
    gather(FATALITIES, INJURIES, key = Casualty, value = Number) %>%
    mutate(Event = str_to_title(Event)) %>%
    filter(Number > 0, Event %in% top10_f_i2$Event) %>%
    mutate(Event = factor(Event, top10_f_i2$Event)) 

top10_damage <- clean_data %>%
    gather(PROPDMG, CROPDMG, key = Loss, value = Amount) %>%
    filter(Amount > 0) %>%
    left_join(CPI) %>%
    group_by(Event) %>%
    summarize(Total = sum(Amount * multiplier)) %>%
    arrange(Total) %>%
    top_n(10, Total) %>%
    mutate(Event = str_to_title(Event)) %>%
    select(1)

damage2 <- clean_data %>%
    gather(PROPDMG, CROPDMG, key = Loss, value = Amount) %>%
    mutate(Event = str_to_title(Event)) %>%
    filter(Amount > 0, Event %in% top10_damage$Event) %>%
    left_join(CPI) %>%
    mutate(Inf_Adj_Amount = (Amount/1000000000) * multiplier) %>%
    mutate(Event = factor(Event, top10_damage$Event)) 

ggplot(f_i2, aes(x = Event, y = Number, fill = Casualty)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    theme_light() +
    labs(title = "Fatalities and Injuries by Event Type", 
         subtitle = "Top 10 Weather Events from 1996 - 2011", 
         x = "Weather Event",
         y = "Number of Casualties")

ggplot(damage2, aes(x = Event, y = Inf_Adj_Amount, fill = Loss)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    theme_light() +
    labs(title = "Financial Losses (in billions $) by Event Type", 
         subtitle = "Top 10 Weather Events from 1996 - 2011, adjusted for inflation",
         x = "Weather Event",
         y = "Amount of Losses") 

damage2 %>%
    group_by(STATE) %>%
    summarise(Total_Costs = sum(Inf_Adj_Amount)) %>%
    arrange(desc(Total_Costs)) %>%
    top_n(10, Total_Costs) %>%
    mutate(Total_Costs = format(Total_Costs, digits = 2))

