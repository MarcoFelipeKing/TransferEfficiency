# preprocessing touch data

# run the other file first
source("preprocessing_HSE_Christopher_microactivity.R")

# load packages
library(readxl)

# read the file
finalobservations<-read_excel("data/HSE_Christopher/tbl final observations.xls")
names(finalobservations)
str(finalobservations)

# A number of the variables are character - change

# The header was over two rows
finalobservations<-finalobservations[-1,]

# convert to numeric
finalobservations$Duurobs<-as.numeric(finalobservations$Duurobs)

### What does the Duration of observations look like?

# histogram
hist(finalobservations$Duurobs)

# density plot
plot(density(finalobservations$Duurobs))

# repeat as numeric for the other columns
finalobservations[,5:14] <- sapply(finalobservations[,5:14],as.numeric)

# 1. Figure out what cc means? Converted to NA

# contacts per hour has no information - delete
finalobservations$`Contacts/hour`<- NULL

# 2. Figure out the column names meaning

# They are mostly contained in the original excel sheet (in the bit that we took out above)
# Busyness was rated on a 4-point scale from 0 to 3 (0 = not busy; 1 = a little busy; 2 = moderately busy; 3 = very busy)

# something in the ID column, i.e. for understanding difference in industry
# Seperate the first bit out which looks unique

library(stringr)
finalobservations$RecordIDIND<-str_extract(finalobservations$RecordID, "[:alpha:]*")
finalobservations$RecordIDIND2<-str_extract(finalobservations$RecordID, "\\d+")
finalobservations$RecordIDIND3<-substring(finalobservations$RecordIDIND2, 1,2)
finalobservations$RecordIDIND4<-substring(finalobservations$RecordIDIND2, 3,4)
finalobservations$RecordIDIND2<-NULL


finalobservations$RecordIDIND5<-str_remove(finalobservations$RecordID, "[:alpha:]*")
finalobservations$RecordIDIND5<-str_remove(finalobservations$RecordIDIND5, "\\d+")


finalobservations$RecordID2<-paste0(finalobservations$RecordIDIND, finalobservations$RecordIDIND3)

# 'finalobservations' is the microactivity data but with the "no observations" value in Tskdescript taken out (141-55)


# merge with unique microactivity data

library(dplyr)
library(tidyr)
finalobservationsmicroactadjuni<-finalobservations %>%
  left_join(., microactadjuni, by="RecordID")


# make NA 0 touches
finalobservationsmicroactadjuni[, c(5:14, 20:25)][is.na(finalobservationsmicroactadjuni[, c(5:14, 20:25)])] <- 0


# create the total vars
finalobservationsmicroactadjuni$totF<-finalobservationsmicroactadjuni$FLH+
                                      finalobservationsmicroactadjuni$FTO+
                                      finalobservationsmicroactadjuni$FRH

finalobservationsmicroactadjuni$totNE<-finalobservationsmicroactadjuni$NELH+
  finalobservationsmicroactadjuni$NERH+
  finalobservationsmicroactadjuni$NETO

finalobservationsmicroactadjuni$totPO<-finalobservationsmicroactadjuni$POLH+
  finalobservationsmicroactadjuni$PORH+
  finalobservationsmicroactadjuni$POO

finalobservationsmicroactadjuni$totO<-finalobservationsmicroactadjuni$OLH+
  finalobservationsmicroactadjuni$ORH+
  finalobservationsmicroactadjuni$OTO

finalobservationsmicroactadjuni$totT<-finalobservationsmicroactadjuni$TLH+
  finalobservationsmicroactadjuni$TRH

finalobservationsmicroactadjuni$totSurf<-finalobservationsmicroactadjuni$SLH+
  finalobservationsmicroactadjuni$SRH

# mutate across the touch variables so that we can calculate contact rate (i.e. divide by observation time * 60)
finalobservationsmicroactadjuni<-finalobservationsmicroactadjuni %>%
  mutate(
    across(c(5:14, 20:25, 27:32),
           .fns = (~./Duurobs*60))) 

# Check !!
# SLH first obs is (33/32)*60 = 61.875
# yes! ok


#Q Is there a different distribution in touches for each industry

library(ggplot2)

ggplot(finalobservationsmicroactadjuni, aes(x=totSurf))+
  geom_histogram()+
  facet_wrap(~RecordID2)


finalobservationsmicroactadjuni %>% group_by(RecordID2) %>%
  summarise(TP0=mean(totPO, na.rm=T))

finalobservationsmicroactadjuni %>% group_by(RecordID2) %>%
  summarise(TP0=median(totSurf, na.rm=T))


# Compared the mean PO for healthcare and it corresponds to the ID - UK05
# let's make a dummy variable for non-healthcare (will figure out which is which later)
# so that we can see if any major differences 


# Q Is there a different distribution in touches between healthcare/non-healthcare

finalobservationsmicroactadjuni$healthcare<-ifelse(finalobservationsmicroactadjuni$RecordID2=="UK05", 1,0)

# total F (face)
model1<-lm(totF~ healthcare + busyness, finalobservationsmicroactadjuni)
summary(model1)

# total PO (peri-oral)
model2<-lm(totPO~ healthcare + busyness, finalobservationsmicroactadjuni)
summary(model2)

# total O (oral)
model3<-lm(totO~ healthcare + busyness, finalobservationsmicroactadjuni)
summary(model3)

# total T (tool)
model4<-lm(totT~ healthcare + busyness, finalobservationsmicroactadjuni)
summary(model4)

# total Surf (Surfaces)
model5<-lm(totSurf~ healthcare + busyness, finalobservationsmicroactadjuni)
summary(model5)


# more face touches by healthcare workers than other occupations
# No difference in oral or peri-oral touches by healthcare workers compared with other occupations (higher busyness was strongly associated with lower contacts)
# Perhaps less touching of tools by healthcare workers compared to other occupations
# Lower surface touches by healthcare workers compare to other occupations (no association with busyness) - is there a relationship between surface touches and patient touches

















