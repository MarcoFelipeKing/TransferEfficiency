# preprocessing touch data

# load packages
library(readxl)

# name of the sheets
sheetnames <-excel_sheets("data/HSE_Christopher/tblmicroactadj.xls")

# as far as I can see the difference between the sheets is that a total column has been created in sheet1 
# and then in sheet2 the visualisations have been created

# read the file
microactadj<-read_excel("data/HSE_Christopher/tblmicroactadj.xls", sheet= "Microacivity data orig")

# FLH	FRH	FTO	NELH	NERH	NETO - still unsure about these

# Left hand (LH)/ Right hand (RH)

# F = Forehead?

# TO = tools?

# NE = Nose/ears?

# Double check these!

names(microactadj) %in% names(finalobservations)

# 14 same variables and 7 not the same between the two datasets

# get a unique dataset for the microactivity data
microactadjuni<-cbind(microactadj[,c(! names(microactadj) %in% names(finalobservations)),], microactadj$RecordID)

colnames(microactadjuni)[8]<-"RecordID"



