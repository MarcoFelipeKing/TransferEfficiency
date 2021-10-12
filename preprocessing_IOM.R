# library
library(RODBC)

# Create the connection to the microsoft access database
mdbConnect<-odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=data/IOM/database/Transfer Efficiency Database V1.1.3.mdb")

# what tables do we have
tbls <- sqlTables(mdbConnect)

# main contacts
contacts <- sqlFetch(mdbConnect, "Contacts")
write.csv(contacts, "data/IOM/processeddata/IOMcontactsdata.csv", row.names=F)

# output the auxilliary data
for(i in c("ContactType", "Efficiency Type", "PhysicalState", "References", "Substances", "TE Analysis", "TransferEfficiencySource", "TransferType")){
outputAUX <- sqlFetch(mdbConnect, i)
write.csv(outputAUX, paste0("data/IOM/processeddata/auxdata/", i ,".csv"), row.names=F)
}

# close the database connection
odbcClose(mdbConnect)
