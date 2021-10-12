# DATA ON SURFACE TOUCH OBSERVATIONS #

# read all the csv files in the CSO/obs directory
filestoread<-list.files("data/CSO/obsdata/", pattern=".csv", recursive = T, full.names = T)
library(vroom)
Obsdata <- vroom(filestoread)

# format date
dates<-subset(Obsdata, select=c("Date_ymd", "Log_File_Name"))
dates<-dates[!duplicated(dates$Log_File_Name),]

# Code the surfaces into near field and far field
NFsurfaces<-c("Bed", "Patient", "Handrail", "Notes")
FFsurfaces<-c("Door", "Sink", "Window")
Obsdata$Loc<-ifelse(Obsdata$Behavior%in%NFsurfaces, "Near Field", "Far Field")

# summary of the number of touches for each observation time point 
library(dplyr)
typeoftouches<-Obsdata %>% 
  group_by(Log_File_Name, Loc) %>%
  summarise(touches=n())

# Spread the data wide
data_wide <- tidyr::spread(typeoftouches,Loc, touches)

# recode NA as 0 touches
data_wide$`Far Field`[is.na(data_wide$`Far Field`)]<-0
data_wide$`Near Field`[is.na(data_wide$`Near Field`)]<-0

# Calculate total touches and then use it to calculate percentage NF
data_wide$totaltouches<-data_wide$`Far Field`+data_wide$`Near Field`
data_wide$percnearfield<-(data_wide$`Near Field`/data_wide$totaltouches)*100

# Observation duration
time<-Obsdata %>% group_by(Log_File_Name) %>%
 summarise(timediff=max(Time_Absolute_hms)- min(Time_Absolute_hms),
           touches=n()) 

# join this to the wide data and the dates above
timetouches<-time%>%
  right_join(., data_wide, by="Log_File_Name")%>%
  right_join(., dates, by="Log_File_Name")

# Calculate the touches per hour
timetouches$timemins<-timetouches$timediff/60
timetouches$timehr<-timetouches$timediff/60/60
timetouches$NFtouchperhour<-timetouches$`Near Field`/as.numeric(timetouches$timehr)
timetouches$FFtouchperhour<-timetouches$`Far Field`/as.numeric(timetouches$timehr)

# SUmmary of the touches per hour in each zone
summary(timetouches$NFtouchperhour)
summary(timetouches$FFtouchperhour)

# visualise as density and calculate the value of the peak (NF)
d <- density(timetouches$NFtouchperhour)
plot(d)
which.max(density(timetouches$NFtouchperhour)$y)
density(timetouches$NFtouchperhour)$x[187]
# mode is 116 for NF
# min is 0
# max is 411

# visualise as density and calculate the value of the peak (FF)
d <- density(timetouches$FFtouchperhour)
which.max(density(timetouches$FFtouchperhour)$y)
density(timetouches$FFtouchperhour)$x[82]
plot(d)
# mode is 8.465296 for FF
# min is 0
# max is 360

# 
#summary(as.numeric(timetouches$timediff)/60)
#d <- density(as.numeric(timetouches$timediff))
#which.max(density(as.numeric(timetouches$timediff))$y)
#density(timetouches$FFtouchperhour)$x[90]
#plot(d)

#timetouches$Log_File_Name<-gsub("LogFocalBehaviour_Near field _Far field  ", "", timetouches$Log_File_Name)
#library(stringr) 
#timetouches$Log_File_Name<-str_sub(timetouches$Log_File_Name,1,8)#

#timetouches %>%
#  group_by(Log_File_Name) %>%
#  summarise(totaltime=sum(timediff))


#time<-c(72.35, 35.95, 33.91, 25.96)
#d <- density(as.numeric(time))

#timetouches %>%
#  group_by(Log_File_Name) %>%
 # summarise(totaltime=mean(percnearfield))


###############################################

# format time and hour of day
library(hms)
Obsdata$Time_Absolute_hms<-as_hms(Obsdata$Time_Absolute_hms)
Obsdata$Time_Absolute_h<-lubridate::hour(Obsdata$Time_Absolute_hms)
Obsdata$Loc<-as.factor(Obsdata$Loc)

# visualisation of fraction of touches per hour
library(ggplot2)

# for all days
ggplot(Obsdata, aes(x=Observation_Name, fill = Loc)) +
  geom_bar(position="fill", stat="count") + 
  theme(text = element_text(size=20))+
  xlab(" ")+
  ylab("Fraction of Touches")+
  ggtitle("Overall Fraction of touches in near and far field")

# by hour of day
ggplot(Obsdata, aes(x=Time_Absolute_h, fill = Loc)) +
  geom_bar(position="fill", stat="count") + 
  theme(text = element_text(size=20))+
  xlab("Hour of the day")+
  ylab("Fraction of Touches")+
  ggtitle("Overall Fraction of touches in near and far field, by hour of the day")

# for each day
ggplot(Obsdata, aes(x=as.factor(Time_Absolute_h), fill = Loc)) +
  geom_bar(position="fill",stat="count")+ 
  facet_grid(Date_ymd ~ .)+
  xlab("Hour of the day")+
  ylab("Fraction of Touches")+
  ggtitle("Touches in near and far field, by hour of the day, for each observation day")+
  theme(text = element_text(size=20))

# Visualisation of number of touches per hour
#library(dplyr)

# number of touches
#modeldata<-Obsdata %>% group_by_(.dots=c("Date_ymd","Time_Absolute_h", "Loc")) %>%  tally()
#tapply(modeldata$n, modeldata$Loc, summary)

#get mode of touches
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
#tapply(modeldata$n, modeldata$Loc, getmode)

######################

timetouches2<-subset(timetouches, select=c("Log_File_Name", "NFtouchperhour", "FFtouchperhour"))

# values to take for all day value
num2 <- ggplot(timetouches, aes(y=FFtouchperhour), group=Loc) + 
  geom_boxplot(aes(fill=Loc))+
  xlab("")+
  ylab("Number of touches per hour")+
  ggtitle("Overall number of touches per hour in near and far field")+
  theme(text = element_text(size=20))
num2


# value to take for 
num1 <- ggplot(modeldata, aes(x=as.factor(Time_Absolute_h), y=n)) + 
  geom_boxplot(aes(fill=Loc))+
  xlab("Hour of the day")+
  ylab("Number of touches per hour")+
  ggtitle("Number of touches per hour in near and far field for each observation day")+
  theme(text = element_text(size=20))
num1
