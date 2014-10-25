# Effects of Severe Weather Events in the United states
Author: Eddy  
Date: Wednesday, October 25, 2014  

## Synopsis:
The purpose of this report is to inform government or municipal managers responsible for preparing for severe weather events.  The report uses data provided by the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database to identify events of greatest damage and harm.  The final assessment of this report shows that the events that cause the most damage with respect to properties and crops differ from those that cause the most fatalities as well as injuries.  The goal of this report is to provide analysis on data that would allow officials to make informed decisions and preparations for certain infrastructure or services based on expected events.
 
## Data:
The data used for this analysis come in the form of a comma-separated-value file compressed via the bzip2 algorithm to reduce its size.
The data can be downloaded from [Storm Data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2). The data must be unzipped using a utility such as bunzip2.
There is also documentation of the database available. This document explains how some of the variables are constructed and defined.
  [National Weather Service Storm Data Documentation](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf)
	[National Climatic Data Center Storm Events FAQ](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2FNCDC%20Storm%20Events-FAQ%20Page.pdf)
The events in the database start in the year 1950 and end in November 2011. In the earlier years of the database there are generally fewer events recorded, most likely due to a lack of good records. More recent years should be considered more complete
To reproduce this research, please ensure to set the working directory where the data resides.

Note: No code book was made available associated with this dataset.


```r
## set working directory
setwd("~/Data Science/Assignments/RepData_PA2")

## install required libraries
library(R.utils)
library(ggplot2)
library(dplyr)
library(plyr)
```

```
## -------------------------------------------------------------------------
## You have loaded plyr after dplyr - this is likely to cause problems.
## If you need functions from both plyr and dplyr, please load plyr first, then dplyr:
## library(plyr); library(dplyr)
## -------------------------------------------------------------------------
## 
## Attaching package: 'plyr'
## 
## The following objects are masked from 'package:dplyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
```

```r
library(stats)
## download file
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
filename <- "repdata-data-StormData.csv.bz2"
download.file(fileUrl, destfile=filename, method="curl")
## bunzip2(filename, exdir="data")

## read data
StormData <- read.csv("repdata-data-StormData.csv")
```

### Questions
The data analysis must address the following questions:

1.  Across the United States, which types of events (as indicated in the **`EVTYPE`** variable) are most harmful with respect to population health?
2.  Across the United States, which types of events have the greatest economic consequences?

### Data Processing:
I began by looking at the available columns data

```r
names(StormData)
```

```
##  [1] "STATE__"    "BGN_DATE"   "BGN_TIME"   "TIME_ZONE"  "COUNTY"    
##  [6] "COUNTYNAME" "STATE"      "EVTYPE"     "BGN_RANGE"  "BGN_AZI"   
## [11] "BGN_LOCATI" "END_DATE"   "END_TIME"   "COUNTY_END" "COUNTYENDN"
## [16] "END_RANGE"  "END_AZI"    "END_LOCATI" "LENGTH"     "WIDTH"     
## [21] "F"          "MAG"        "FATALITIES" "INJURIES"   "PROPDMG"   
## [26] "PROPDMGEXP" "CROPDMG"    "CROPDMGEXP" "WFO"        "STATEOFFIC"
## [31] "ZONENAMES"  "LATITUDE"   "LONGITUDE"  "LATITUDE_E" "LONGITUDE_"
## [36] "REMARKS"    "REFNUM"
```

The main columns that were required for this analysis were location, event type, injuries, and fatalities, and economic data.

I narrowed the tables down to the following seven columns:
8.  EVTYPE
23. FATALITIES
24. INJURIES
25. PROPDMG
26. PROPDMGEXP
27. CROPDMG
28. CROPDMGEXP


```r
StormData2 <- StormData[ ,c(8, 23:28)]
```

I normalized all of the cost values based on an assumption that the character values corresponded to the multiplication necessary to adjust values.  I did this by identifying the multiplication values within columns PROPRDMGEXP and CROPDMGEXP to only accept values for the following:

hundred:  h = 100
thousand: k = 1000
million:  m = 1000000
billion:  b = 1000000000


```r
## apply lower case to all values
StormData2$PROPDMGEXP <- tolower(StormData2$PROPDMGEXP)
StormData2$CROPDMGEXP <- tolower(StormData2$CROPDMGEXP)

mult <- function(x) {
        if (x == "h") x <- 100
        if (x == "k") x <- 1000
        if (x == "m") x <- 1000000
        if (x == "b") x <- 1000000000
        x
}

StormData2$PROPDMGEXP <- sapply(StormData2$PROPDMGEXP, mult)
StormData2$CROPDMGEXP <- sapply(StormData2$CROPDMGEXP, mult)
```
Now I multiply the **`PROPDMG`** with **`ROPDMGEXP`** and the **`CROPDMG`** with **`CROPDMGEXP`**.

```r
StormData2$PROPDMG <- as.numeric(StormData2$PROPDMG)
StormData2$PROPDMG <- StormData2$PROPDMG * as.numeric(StormData2$PROPDMGEXP)
```

```
## Warning: NAs introduced by coercion
```

```r
StormData2$CROPDMG <- as.numeric(StormData2$CROPDMG)
StormData2$CROPDMG <- StormData2$PROPDMG * as.numeric(StormData2$CROPDMGEXP)
```

```
## Warning: NAs introduced by coercion
```

```r
## convert NA values to 0
write.csv(StormData2,"StormData2.csv",na="0")

StormData3 <- read.csv("StormData2.csv")
## sub the data to columns needed
StormData3 <- StormData3[,c(2:5,7)]
```
Then I determined the event types in order to normalize the **`EVTYPE`** values.


Next I aggregated by the event type to determine total injuries, fatalities, and damages to property and crops

```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.1.1
```

```
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
harm <- aggregate(cbind(FATALITIES, INJURIES) ~ EVTYPE, data=StormData3, sum)
dmg <- aggregate(cbind(PROPDMG, CROPDMG) ~ EVTYPE, data=StormData3, sum)
dmg <- mutate(dmg, DAMAGE = (PROPDMG  + CROPDMG)/1000000)
dmg <- dmg[,c(1,4)]
names(dmg) <- c("EVENT", "DAMAGE")

## sort by DAMAGE cost decending
dmgsort <- dmg[order(-dmg$DAMAGE),]
## sort by FATALITIES decending
fatsort <- harm[order(-harm$FATALITIES),]
## sort by INJURIES decending
injsort <- harm[order(-harm$INJURIES),]
```
Looking at the top 30 events for damages, we can normalize the events to the top nine.  I chose to do this by pulling the rows and normalizing the events to the following nine values for EVENTS

*COMMON NAME*            
1  HURRICANE
2  FLOOD
3  TORNADOES, THUNDERSTORM, WIND, HAIL
4  WILDFIRE
5  ICE STORM
6  TROPICAL STORM
7  DROUGHT
8  HEAVY RAIN
9  HEAVY SNOW, BLIZZARD   
   


```r
HURRI <- dmgsort[c(1, 4, 5, 14, 19), ]
T1 <- sum(HURRI$DAMAGE)
FLOOD <- dmgsort[c(2, 3, 6, 22, 25, 27), ] 
T2 <- sum(FLOOD$DAMAGE)
TORNA <- dmgsort[c(7, 8, 10, 11, 13, 18, 20, 23, 26, 28, 29), ]
T3 <- sum(TORNA$DAMAGE)
WILDF <- dmgsort[c(9, 30), ] 
T4 <- sum(WILDF$DAMAGE)
ICEST <- dmgsort[c(12), ]
T5 <- sum(ICEST$DAMAGE)
TROST <- dmgsort[15, ]
T6 <- sum(TROST$DAMAGE)
DROUG <- dmgsort[16, ]
T7 <- sum(DROUG$DAMAGE)
HRAIN <- dmgsort[17, ]
T8 <- sum(HRAIN$DAMAGE)
HSNOW <- dmgsort[c(21,24), ]
T9 <- sum(HSNOW$DAMAGE)
```
I will now make a data frame for the **`EVENT`** and the **`DAMAGE`**.

```r
EVENT <- c("HURRICANE", "FLOOD", "TORNADO", "FIRE", "ICE STORM", "TROPICAL STORM", "DROUGHT", "HEAVY RAIN", "HEAVY SNOW")
DAMAGE <- c(T1, T2, T3, T4, T5, T6, T7, T8, T9)
dmgdf <- data.frame(EVENT, DAMAGE)
```
Looking at the top 30 events for fatalities, we can normalize the events to the top nine.  I chose to do this by pulling the rows and normalizing the events to the following nine values for EVENTS

*COMMON NAME*            
1  TORNADO
2  HEAT
3  FLOOD
4  LIGHTENING
5  WIND
6  RIP CURRENT
7  AVALANCHE
8  WINTER STORM
9  EXTREME COLD   

```r
TORNA <- fatsort[1, ]
F1 <- sum(TORNA$FATALITIES)
HEAT <- fatsort[c(2, 4, 13, 22), ] 
F2 <- sum(HEAT$FATALITIES)
FLOOD <- fatsort[3, ]
F3 <- sum(FLOOD$FATALITIES)
LIGHT <- fatsort[5, ] 
F4 <- sum(LIGHT$FATALITIES)
WIND <- fatsort[c(6, 9, 15, 18, 27), ]
F5 <- sum(WIND$FATALITIES)
RIPCU <- fatsort[c(8, 12), ]
F6 <- sum(RIPCU$FATALITIES)
AVALA <- fatsort[10, ]
F7 <- sum(AVALA$FATALITIES)
WINTE <- fatsort[c(11, 16, 19), ]
F8 <- sum(WINTE$FATALITIES)
EXTRE <- fatsort[c(14, 17, 22), ]
F9 <- sum(EXTRE$FATALITIES)
```
I will now make a data frame for the **`EVENT`** and the **`DAMAGE`**.

```r
EVENT <- c("TORNADO", "HEAT", "FLOOD", "LIGHTENING", "WIND", "RIP CURRENT", "AVALANCHE", "WINTER STORM", "EXTREME COLD")
FATALITIES <- c(F1, F2, F3, F4, F5, F6, F7, F8, F9)
fatdf <- data.frame(EVENT, FATALITIES)
```
Looking at the top 30 events for injuries, we can normalize the events to the top nine.  I chose to do this by pulling the rows and normalizing the events to the following nine values for EVENTS

*COMMON NAME*            
1  TORNADO
2  WIND
3  FLOOD
4  HEAT
5  LIGHTENING
6  ICE STORM
7  HAIL
8  BLIZZARD
9  HURRICANE


```r
TORNA <- injsort[1, ]
I1 <- sum(TORNA$INJURIES)
WIND <- injsort[c(2, 9, 13, 16, 25, 27), ] 
I2 <- sum(WIND$INJURIES)
FLOOD <- injsort[c(3, 8), ]
I3 <- sum(FLOOD$INJURIES)
HEAT <- injsort[c(4, 6, 24), ] 
I4 <- sum(HEAT$INJURIES)
LIGHT <- injsort[5, ]
I5 <- sum(LIGHT$INJURIES)
ICEST <- injsort[7, ]
I6 <- sum(ICEST$INJURIES)
HAIL <- injsort[10, ]
I7 <- sum(HAIL$INJURIES)
BLIZZ <- injsort[c(11, 14, 17, 21), ]
I8 <- sum(BLIZZ$INJURIES)
HURRI <- injsort[12, ]
I9 <- sum(HURRI$INJURIES)
```
I will now make a data frame for the **`EVENT`** and the **`INJURIES`**.

```r
EVENT <- c("TORNADO", "WIND", "FLOOD", "HEAT", "LIGHTENING", "ICE STORM", "HAIL", "BLIZZARD", "HURRICANE")
INJURIES <- c(I1, I2, I3, I4, I5, I6, I7, I8, I9)
injdf <- data.frame(EVENT, INJURIES)
```
Now I resort the results for each of the data frames

```r
## sort by DAMAGE cost decending
dmgsort <- dmgdf[order(-dmgdf$DAMAGE),]
## sort by FATALITIES decending
fatsort <- fatdf[order(-fatdf$FATALITIES),]
## sort by INJURIES decending
injsort <- injdf[order(-injdf$INJURIES),]
```
Now I will create data frames to of the three figures

```r
damage <- data.frame(dmgsort)
fatalities <- data.frame(fatsort)
injuries <- data.frame(injsort)
```
## Results and Analysis:

```r
damage
```

```
##            EVENT       DAMAGE
## 1      HURRICANE 5.909545e+12
## 2          FLOOD 5.120030e+12
## 3        TORNADO 5.908311e+09
## 4           FIRE 1.342822e+09
## 5      ICE STORM 5.808015e+08
## 6 TROPICAL STORM 2.524481e+08
## 7        DROUGHT 2.005862e+08
## 8     HEAVY RAIN 1.604681e+08
## 9     HEAVY SNOW 1.486613e+08
```

Table 1. Dollar value of damages to crops and properties by event

Table 1 indicates that property and crop damage cost impacts are highest for hurricanes, floods, and tornados.  This is most likely due to the long-term impacts associated with rebuilding and the process required to re-establish agriculture for a given area.
Note: A table was used here given the difference in range in cost by events.


```r
library(ggplot2)
f <- ggplot(fatalities, aes(x=EVENT, y=FATALITIES))
f <- f + geom_bar(stat="identity", position = "identity")
f <- f + labs(x="EVENT",
              y="FATALITIES",
              title="Fatalities by Event")
f <- f + theme(axis.text.x =
               element_text(size  = 10,
                            angle = 45,
                            hjust = 1,
                            vjust = 1))
f
```

![](StormAssessment_files/figure-html/unnamed-chunk-16-1.png) 
Figure 1. Fatalities by event

Figure 1 indicates that fatalities are caused most by tornados.  Second and third events causing fatalities are heat and wind associated events.  


```r
i <- ggplot(injuries, aes(x=EVENT, y=INJURIES))
i <- i + geom_bar(stat="identity", position = "identity")
i <- i + labs(x="EVENT",
              y="INJURIES",
              title="Injuries by Event")
i <- i + theme(axis.text.x =
               element_text(size  = 10,
                            angle = 45,
                            hjust = 1,
                            vjust = 1))
i
```

![](StormAssessment_files/figure-html/unnamed-chunk-17-1.png) 

Figure 2. Injuries by event

Figure 2 indicates that injuries are caused most by tornados.  Second and third events causing injuries are wind and heat, respectively.  Although tornados are difficult to predict making preparations challenging, heat and wind associated fatalities and injuries could be reduced with proper training and preparation.
