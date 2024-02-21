## Individualnachweise Genetik Wolf
## by Ursi Version 22-05-10
## adapted by Inès december 2023

#define working directory
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
print( getwd() )


#### Preparation ####
library(readr)
library(tidyverse)
library(readxl)
library(xlsx)
library(data.table)
library(dplyr)
library(tidyr)
library(RMariaDB)
library(sf)
library(lubridate)

# --- Import raw data ####
# --------- Link to KORA DB server ####
#Main
if (Sys.info()["sysname"] == "Darwin") {  # macOS
  ssl_ca_value <- "/etc/ssl/cert.pem"
} else if (Sys.info()["sysname"] == "Windows") {
  ssl_ca_value <- "C:/cacert.pem"
} else {
  # Handle other operating systems if needed
}

#Read DB


#Import Individual table
query <- paste("SELECT * FROM Individual v WHERE v.species = 'WOLF';", sep= "")
rs = dbSendQuery(DB,query)
Wolf_Individual<-dbFetch(rs)

#Import sample table
query <- paste("SELECT * FROM Sample ;", sep= "")
rs = dbSendQuery(DB,query)
Wolf_Sample<-dbFetch(rs)
Wolf_Sample <- Wolf_Sample %>%
  dplyr::rename(type_sample = type)

#Import genetic table
query <- paste("SELECT * FROM Genetics ;", sep= "")
rs = dbSendQuery(DB,query)
Genetic<-dbFetch(rs)

#Import kill table
query <- paste("SELECT * FROM `kora-next`.`Kill`;", sep= "")
rs = dbSendQuery(DB,query)
kill<-dbFetch(rs)

#Import observation table
query <- paste("SELECT * FROM Observation ;", sep= "")
rs = dbSendQuery(DB,query)
Observation <-dbFetch(rs)
Observation <- Observation%>%
  rename(Obs_date = date)

#Import event table
query <- paste("SELECT * FROM Event ;", sep= "")
rs = dbSendQuery(DB,query)
event<-dbFetch(rs)
event <- event%>%
  rename(event_status = status)



#### -- Merge the tables to have all the infos needed 

#Merge event and observation
M_obs_event <- merge(Observation,event, 
                     by.x = "eventId", by.y = "id",
                     all.x = TRUE)

#Merge kill and M_obs_event
M_kill_obsevent <- merge(kill,M_obs_event, 
                         by.x = "observationId", by.y = "id",
                         all.x = TRUE)
# Aggregate species.x by eventId and concatenate them with comma
M_kill_obsevent_aggregated <- M_kill_obsevent %>%
  group_by(eventId) %>%
  summarise(species.x = paste(unique(species.x), collapse = ", ")) %>%
  ungroup()

#add the killed species for each eventID
M_obs_event <- M_obs_event %>%
  left_join(dplyr::select(M_kill_obsevent_aggregated, eventId,species.x ),
            by = "eventId")%>%
  rename(KillSpecies = species.x)

#Merge genetic and M_obs_event
M_genetic_obsevent <-  merge(Genetic,M_obs_event, 
                             by.x = "observationId", by.y = "id",
                             all.x = TRUE)
#Merge sample and M_genetic_obsevent
M_samples_genObsEvent <- merge(Wolf_Sample,M_genetic_obsevent, 
                               by.x = "geneticsId", by.y = "id",
                               all.x = TRUE)
# Remove duplicate columns (it's the created at that we don't care about for the wolfpost)
M_samples_genObsEvent <- M_samples_genObsEvent %>%
  dplyr::select(-createdAt.x,-createdAt.y)
M_samples_genObsEvent <- M_samples_genObsEvent %>% 
  distinct(.keep_all = TRUE)

# Remove rows where the event status is rejected :
M_samples_genObsEvent <- M_samples_genObsEvent %>%
  filter(event_status != "REJECTED")


#change the id colname of the Wolf_individual table to be able to find it easily afterwards
Wolf_Individual <- Wolf_Individual %>%
  rename(id.wolf = id)

#Merge to obtain the wolf individual infos 
M_samples_genObsEvent <- merge(M_samples_genObsEvent,Wolf_Individual, 
                               by.x = "resultIndividual", by.y = "individualID",
                               all.x = TRUE)

# Convert UTC time to Swiss time (CET/CEST)
M_samples_genObsEvent <- M_samples_genObsEvent %>%
  mutate(
    received = with_tz(received, "CET"),
    sent = with_tz(sent, "CET"),
    dateSpecies = with_tz(dateSpecies, "CET"),
    dateIndividual = with_tz(dateIndividual, "CET"),
    Obs_date = with_tz(Obs_date, "CET")
  )

wolf_IndNachweise <- M_samples_genObsEvent %>%
  dplyr::select(resultIndividual,Obs_date,country, municipality,canton, type_sample) %>%
  dplyr::rename(sexID=resultIndividual,
                collectionDate=Obs_date,
                community=municipality,
                sampleType=type_sample) %>%
  dplyr::filter(sexID != "") 

# Regular expression to keep only individuals Id and not "nicht intepretierbar" and things like that
pattern <- "^(M|F|U)\\d{1,3}$"

# Create a logical index of rows where sexID matches the pattern
index <- grepl(pattern, wolf_IndNachweise$sexID)

# Subset the data frame to keep only rows where sexID matches the pattern
wolf_IndNachweise <- wolf_IndNachweise[index, ]



wolf_IndNachweise$sampleType <- as.factor(wolf_IndNachweise$sampleType)


wolf_IndNachweise$sampleType <- ifelse(wolf_IndNachweise$sampleType=="SCAT", "Kot",
                                           ifelse(wolf_IndNachweise$sampleType=="SALIVA", "Speichel",
                                                  ifelse(wolf_IndNachweise$sampleType=="TISSUE", "Gewebe",
                                                         ifelse(wolf_IndNachweise$sampleType=="URINE", "Urin",
                                                                ifelse(wolf_IndNachweise$sampleType=="BLOOD", "Blut",
                                                                       ifelse(wolf_IndNachweise$sampleType=="HAIR", "Haar",
                                                                              wolf_IndNachweise$sampleType))))))




# Ensure collectionDate is in the appropriate Date or Datetime format
wolf_IndNachweise$collectionDate <- as.POSIXct(wolf_IndNachweise$collectionDate)

# Group by sexID, arrange by collectionDate within each group, and get the last entry
latest_records <- wolf_IndNachweise %>%
  group_by(sexID) %>%
  arrange(collectionDate) %>%
  slice(n())

# View the result
print(latest_records)


# Sort the sexID in the right order F09, F10, F11 and not F10, F100, F101,...
sorted_wolf_IndNachweise <- wolf_IndNachweise %>%
  # Extract the letter and numeric parts from sexID
  mutate(letter = sub("\\d+", "", sexID),
         number = as.numeric(sub("^[A-Za-z]+", "", sexID))) %>%
  # Arrange by the letter part, then by the numeric part
  arrange(letter, number) %>%
  # Optionally, remove the extra sorting columns
  select(-letter, -number)



sorted_wolf_IndNachweise <- sorted_wolf_IndNachweise %>% 
  rename("Wolf ID" = sexID) %>% 
  rename("Datum" = collectionDate) %>% 
  rename("Gemeinde" = community) %>% 
  rename("Kanton" = canton) %>% 
  rename("Land"= country) %>%
  rename("Typ" = sampleType)

sorted_wolf_IndNachweise$Datum <- as.Date(sorted_wolf_IndNachweise$Datum)

#write.csv(wolf_genetics, "individualnachweise_genetik.csv", row.names = F)
#write.csv(wolf_genetics, paste0("individualnachweise_genetik_", format(Sys.Date(), "%d.%m.%Y"),".csv"), row.names = F)
write.xlsx(sorted_wolf_IndNachweise, paste0("individualnachweise_genetik_", format(Sys.Date(), "%d.%m.%Y"),".xlsx"), row.names = F, showNA = FALSE)
