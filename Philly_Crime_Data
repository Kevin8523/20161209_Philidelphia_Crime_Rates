#Crime Data

#What I Learned
#Must change time from character to integer or it wont plot - Why?
#Converting to Posix for best results on DATE-TIME
#Aggregate function to group data 
#For barchart you must add stat ="idenity" in the geom_bar() and add the x and y in the aes
#How to group by seasonality or a particular word in a phrase
#Sorting data & Selecting to get top 10 for ggplot
#Barchart tricks in ggplot

# Questions:
# 1.	Overall trend in crimes for the whole period of time in the dataset. 
#     The granularity should be at the Day level. 
# 2.	Which are the most and the least dangerous hours in Philadelphia? 
# 3.	Is there any seasonality in the crime rate? 
# 4.	What are the top 10 crimes crime types? 
# 5.	Which police HQ is in the most need of strengthening? 


#Libraries 
library(ggplot2) # Visualization 
library(plyr) # Used for Splitting and Combining Data
library(dplyr)

#Import the dataset
setwd("C:\\Users\\Kevin\\")
getwd()

#Two datasets, one is a subset since the file is so huge
dataset <- read.csv("CrimeData.csv", stringsAsFactors = TRUE, na.string=c(""))
dataset2 <- read.csv("CrimeData2(SmallSample).csv", stringsAsFactors = TRUE, na.string=c(""))

#Analyzing the data
head(dataset,20)
tail(dataset,10)
str(dataset)
summary(dataset)
dim(dataset) #5 columns #2035062 columns

#Changing Columns into different types: Int to Factors | Handling Dates
is.factor(dataset$Dc_Dist) 
dataset$Dc_Dist <- factor(dataset$Dc_Dist) #Making Dc_Dist a factor
summary(dataset)
str(dataset)


head(dataset$Dispatch_Date_Time,10) #Format is YR-Month-Day Hour:Minute:Sec ####-##-## ##:##:##
tail(dataset$Dispatch_Date_Time,10)
as.POSIXct(dataset$Dispatch_Date_Time, format="%Y-%m-%d %H:%M:%S") #Check of Posix time
dataset$PosixTime <- as.POSIXct(dataset$Dispatch_Date_Time, format="%Y-%m-%d %H:%M:%S") #Conversion of Time
summary(dataset)
str(dataset)

#Rearrange Columns
dataset$Dispatch_Date_Time <- NULL #If you want to delete the unused old time
dataset <- dataset[,c(6,1,3,4,5,2)]
head(dataset,10)

# 1.	Overall trend in crimes for the whole period of time in the dataset. 
#     The granularity should be at the Day level. 

#Splitting the Date and time columns
dataset$Date <- as.Date(dataset$PosixTime) #Creates a date column
dataset$Time <- format(as.POSIXct(dataset$PosixTime) ,format = "%H")#Creates a time column: If needed add tz="EST" for a time zone. Did this by hour because of the case study
#dataset$Time <- strftime(dataset$PosixTime, format = '%H') #Alternatte way for time

#Aggregating Data - Count the number of crimes by day
#Creates a New Data frame
#You Could use plyr if you know how, but I did not
By_Date <- aggregate(dataset$Date, by = list(Date = dataset$Date), FUN = length)
str(By_Date)

#Rename the columns
colnames(By_Date) <- c("Date", "Total")

#Time-Series of Crime at the day level
ggplot(data=By_Date, aes(x=Date, y=Total,
                              color=Date)) + geom_line()


# 2.	Which are the most and the least dangerous hours in Philadelphia? 

#Aggregating Data - Count the number of crimes by Hour
By_Hour <- aggregate(dataset$Time, by = list(Hour = dataset$Time), FUN = length)
str(By_Hour)

#Rename the columns
colnames(By_Hour) <- c("Time", "Total")

#Convert Categorical Hours to Integers
By_Hour$Time <- as.integer(By_Hour$Time) #Must do this or the visual wont show
str(By_Hour)

#Time-Series of Crime at the day level
ggplot(data=By_Hour, aes(x=Time, y=Total)) + 
  geom_line(colour='Red') +
  ggtitle("Crime By Hour") +
  xlab("Hour of the Day") +
  ylab("Total Crimes")

# 3.	Is there any seasonality in the crime rate? 

#Create the months
dataset$Month <- strftime(dataset$Date, format = '%m') #Alternate way for extraction

#Aggregating Data - Count the number of months for season in buckets
By_Month <- aggregate(dataset$Month, by = list(Month = dataset$Month), FUN = length)
str(By_Month)

#Rename the columns
colnames(By_Month) <- c("Month", "Total")

#Convert Categorical Month to Integers
By_Month$Month <- as.integer(By_Month$Month) #Must do this or the visual wont show
str(By_Month)

#Bar Chart of Crime at the Month level
ggplot(data=By_Month, aes(x=Month, y=Total)) + 
  geom_bar(fill='Red', stat ="identity") +
  ggtitle("Crime By Month") +
  xlab("Month of the Year") +
  ylab("Total Crimes")


# 4.	What are the top 10 crimes crime types? 

summary(dataset)
head(dataset,25)
tail(dataset,25)

#Aggregating Data
By_Crime <- aggregate(dataset$Text_General_Code, by = list(Crime_type = dataset$Text_General_Code),
                      FUN = length )

#Rename the columns
colnames(By_Crime) <- c("Crime", "Total")

#Sort
By_Crime_Sort <- By_Crime[order(By_Crime$Total,decreasing = T),]

#Select Top 10 Crimes
top10crimes <- By_Crime_Sort[1:10,]
top10crimes

#Bar Chart by Crime
ggplot(data=top10crimes, aes(x=reorder(Crime,Total), y=Total)) + 
  geom_bar(aes(fill=Crime), stat ="identity") +
  coord_flip() +
  ggtitle("Crime By Category") +
  xlab("Crime by Category") +
  ylab("Total Crimes")
  
  
# 5.	Which police HQ is in the most need of strengthening? 

#Aggregating Data
By_hq <- aggregate(dataset$Dc_Dist, by = list(hq = dataset$Dc_Dist), FUN = length )

#Rename the columns
colnames(By_hq) <- c("HQ", "Total")

#Bar Chart by Dist
ggplot(data=By_hq, aes(x=reorder(HQ,-Total), y=Total)) + 
  geom_bar(fill = "blue", stat ="identity") +
  ggtitle("Crime By Dist") +
  xlab("Crime by Dist") +
  ylab("Total Crime")
