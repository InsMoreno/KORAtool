############################################################################
######                                                                 #####
######                          WOLF MONITORING                        #####   
######                               KORA                              #####
######                      Average of observed pups                   #####
######          a) all packs and b) only first reproductions           #####
######                                                                 #####
######                               ***                               #####
######                                                                 #####     
######                     Scripting by Inès Moreno                    #####
######                                                                 #####
############################################################################


## ---- Preparation                                                     ####
library(dplyr)
library(RMariaDB)


## ----- Import raw data                                                ####
## -- Link to KORA DB server 
# directory according to the computer to read DB
if (Sys.info()["sysname"] == "Darwin") {  # macOS
  ssl_ca_value <- "/etc/ssl/cert.pem"
} else if (Sys.info()["sysname"] == "Windows") {
  ssl_ca_value <- "C:/cacert.pem"
} else {
  # Handle other operating systems if needed
}

# Read DB


# Import pack table
query <- paste("SELECT * FROM WolfPack ;", sep= "")
rs = dbSendQuery(DB,query)
packTable<-dbFetch(rs)

## ---- Data sorting and calculations                                   ####
# Keep only the packs (not the pairs)
Pack_only <- packTable %>%
  dplyr::filter(type=="PACK")

# Keep only packs where there was observed pups (because if 0 pups observed there may not be a reproduction)
Pack_reprod <- packTable %>%
  dplyr::filter(totCubsObs != 0)

#Remove the year for the pack ID, because we have it in bioYear column, and we
#need to be able to identify when there was a change in the reproductive couple of a pack
Pack_reprod$packID <- sub("_.*", "", Pack_reprod$packID)

#Keep only the first reproduction of each pack (packID)
Pack_firstReprod <- Pack_reprod %>%
  group_by(packID) %>%
  filter(bioYear == min(bioYear))


#Now count the mean pup litter (obs pup) 
# Calculate the mean of totCubsObs for the first reproductions
MeanPupObs_1stRepr <- round(mean(Pack_firstReprod$totCubsObs, na.rm = TRUE),2)
# Calculate the mean of totCubsObs for all reproductions:
MeanPupObs <- round(mean(Pack_reprod$totCubsObs, na.rm = TRUE),2)

# Print the result
print(paste0("The average litter size (observed pups) for established wolf packs in Switzerland is ", MeanPupObs, ", while it is ",MeanPupObs_1stRepr, " for the first reproduction"))

