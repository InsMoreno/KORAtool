################################################################################
####            This script was created to obtain the most recent           ####
####                 genetic evidence of each individual during a quartal   ####
####                        Created by Inès, the 04.07.2023                 ####
####                      Last edited by ... the ....                       ####
################################################################################

#Set working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
getwd()

#### ONLY THINGS TO ADAPT ####
#define start end end date for QB, QB number and current year
startDate <- as.Date("2023-01-01") #Startdatum (Format yyy-mm-dd) immer in ""
endDate <- as.Date("2023-12-31") #Enddatum betrifft immer das Sammeldatum der Probe und das Enddatum des jeweiligen Quartals (Format yyy-mm-dd) immer in ""
QB <- "4" #QB number
Year <- "2023" #current year


#load packages
library(dplyr)
library(RMariaDB)
library(stringr)
library(readxl)
library(xlsx)
library(readr)
library(tidyverse)
library(data.table)
library(tidyr)


#### Import raw data                                                        ####
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

#Import pack table
query <- paste("SELECT * FROM WolfPack ;", sep= "")
rs = dbSendQuery(DB,query)
packTable<-dbFetch(rs)

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

#Import User table
query <- paste("SELECT c.id, c.name FROM User c ;", sep= "")
rs = dbSendQuery(DB,query)
User<-dbFetch(rs)

#Import Dead table
query <- paste("SELECT * FROM Death ;", sep= "")
rs = dbSendQuery(DB,query)
Death<-dbFetch(rs)

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
  left_join(select(M_kill_obsevent_aggregated, eventId,species.x ),
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
  select(-createdAt.x,-createdAt.y)
M_samples_genObsEvent <- M_samples_genObsEvent %>% 
  distinct(.keep_all = TRUE)

# Remove rows where the event status is rejected :
M_samples_genObsEvent <- M_samples_genObsEvent %>%
  filter(event_status != "REJECTED")

#Merge to obtain the name of the reporter
M_samples_genObsEvent <- merge(M_samples_genObsEvent,User, 
                               by.x = "reporterId", by.y = "id",
                               all.x = TRUE)
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
# update the specified columns (received, sent, dateSpecies, dateIndividual, Obs_date) to Date format by applying the as.Date() function to them.
M_samples_genObsEvent <- M_samples_genObsEvent %>%
  mutate(across(c(received, sent, dateSpecies, dateIndividual, Obs_date), ~ as.Date(ymd_hms(.))))

# Left join to add the female and male names in the packTable that are in the Individual table 
packTable <- packTable %>%
  left_join(select(Wolf_Individual, id.wolf, individualID), by = c("male" = "id.wolf")) %>%
  rename(male_name = individualID)
packTable <- packTable %>%
  left_join(select(Wolf_Individual, id.wolf, individualID), by = c("female" = "id.wolf")) %>%
  rename(female_name = individualID)
# Left join to add the wolf names in the death that are in the Individual table 
Death <- Death %>%
  left_join(select(Wolf_Individual, id.wolf, individualID), by = c("individualId" = "id.wolf")) %>%
  rename(Wolf_name = individualID)
# Convert the integer column to character
Death$individualId <- as.character(Death$individualId)
# Filter out NA values from Death table
Death_filtered <- Death %>% filter(!is.na(Wolf_name))

# Perform the join operation
M_samples_genObsEventDeath <- M_samples_genObsEvent %>%
  left_join(select(Death_filtered, Wolf_name, circumstances), 
            by = c("resultIndividual" = "Wolf_name")) %>%
  rename(CauseOfDeath = circumstances)

# create a new column in M_samples_genObsEventDeath that will hold the corresponding pack names based on resultIndividual values
#first we want only the packs and not the pairs
pack_only <-  packTable %>%
  filter(type=="PACK")
pack_only$name <- ifelse(pack_only$name == "Glattwang ", "Glattwang", pack_only$name)

# Custom function to get the corresponding pack name based on bioYear and resultIndividual
# Function to perform the search and replace
search_replace <- function(individual) {
  if (is.na(individual)) return(NA)
  
  years_to_check <- c(2023:1998 ) # add more years as needed
  
  for (year in years_to_check) {
    found_row <- pack_only %>% 
      filter((male_name == individual | female_name == individual) & bioYear == year) %>%
      select(name) %>%
      pull()
    
    if (length(found_row) > 0) return(found_row[1])
  }
  
  return(NA) # return NA if no match found across years
}

# Apply the function to the 'resultIndividual' column
M_samples_genObsEventDeath$packName_current <- sapply(
  M_samples_genObsEventDeath$resultIndividual, 
  FUN = search_replace
)

# Replace NA values in the canton column with corresponding country value
# first name the country "FRANCE" FRA because otherwise it would be FR as the canton Fribourg
M_samples_genObsEventDeath$country <- ifelse(M_samples_genObsEventDeath$country == "FR", "FRA", M_samples_genObsEventDeath$country)
M_samples_genObsEventDeath$canton <- ifelse(
  is.na(M_samples_genObsEventDeath$canton),
  M_samples_genObsEventDeath$country,
  M_samples_genObsEventDeath$canton)

M_samples_genObsEventDeath <- M_samples_genObsEventDeath %>%
  mutate(canton = case_when(
    is.na(country) ~ canton, # Keeps original canton if country is NA
    country == "FRA" ~ "FRA", # Sets canton to "FRA" if country is "FRA"
    TRUE ~ canton # Default case to keep original canton
  ))

## Keep only the columns that interest us
Wolf_genetics <-M_samples_genObsEventDeath%>%
  select(keyOfficial,key,
         canton,municipality, fieldName,
         quality, type.x,
         Obs_date,received,sent, dateSpecies,resultSpecies,dateIndividual,resultIndividual,
         yob, packNameSource, packName_current,sourcePopulation,
         name, observer, observerType, copyright,
         origin, KillSpecies, priority,
         x,y, toBeSent, scalp,
         eventId, CauseOfDeath)

Wolf_genetics<- Wolf_genetics %>%
  rename("sexID" = resultIndividual,
         "sampleID"= keyOfficial,
         "documents"=key,
         "collectionDate"=Obs_date,
         "community"=municipality,
         "location"=fieldName,
         "sender"=observer,
         "senderType"=copyright,
         "sampleType"=type.x, 
         "receiptDate" = received,
         "submissionDate"=sent,
         "resultDate"=dateSpecies,
         "genotypeDate"=dateIndividual,
         "sampleOrigin"=KillSpecies,
         "sampleCondition"=quality)

WCH <- Wolf_genetics

# Convert collectionDate to date format
WCH$collectionDate <- as.Date(WCH$collectionDate)

# Filter the data based on the desired time period
WCH_filtered <- WCH %>%
  filter(collectionDate >= startDate & collectionDate <= endDate)

# Group by sexID, arrange by collectionDate in descending order, and select the first row within each group
sort(unique(WCH_filtered$sexID))
WCH_filtered$sexID <- toupper(WCH_filtered$sexID)

WCH_last_evidence <- WCH_filtered %>%
  filter(!is.na(sexID) & 
           sexID != "" & 
           !grepl("NOT|NICHT|PROFILE|CANIS", sexID)) %>%
  group_by(sexID) %>%
  arrange(desc(collectionDate)) %>%
  slice(1)

# Specify the column names we want to keep
#collectionDate
#Gemeinde
#Canton
#Probentyp
#ID wolf
#Status
#Rudel Name Herkunftsrudel, etc

# Add the info if a wolf is dead 
WCH_last_evidence$status <- ifelse(!is.na(WCH_last_evidence$CauseOfDeath), "Tot", NA)

#Translate 
WCH_last_evidence$sampleType <- ifelse(WCH_last_evidence$sampleType=="SCAT", "Kot",
                                           ifelse(WCH_last_evidence$sampleType=="SALIVA", "Speichel",
                                                  ifelse(WCH_last_evidence$sampleType=="TISSUE", "Gewebe",
                                                         ifelse(WCH_last_evidence$sampleType=="URINE", "Urin",
                                                                ifelse(WCH_last_evidence$sampleType=="BLOOD", "Blut",
                                                                       ifelse(WCH_last_evidence$sampleType=="HAIR", "Haar",
                                                                              WCH_last_evidence$sampleType))))))



# Replace some values by NA
WCH_last_evidence$packNameSource <- ifelse(WCH_last_evidence$packNameSource %in% c("noResult",""), NA, WCH_last_evidence$packNameSource)
WCH_last_evidence$packNameSource <- ifelse(WCH_last_evidence$packNameSource == "Val d_Hérens", "Val d'Hérens", WCH_last_evidence$packNameSource)
WCH_last_evidence$sourcePopulation <- ifelse(WCH_last_evidence$sourcePopulation == "ITALIAN_PENINSULA", NA, WCH_last_evidence$sourcePopulation)
WCH_last_evidence$sourcePopulation <- ifelse(WCH_last_evidence$sourcePopulation == "CENTRAL_EUROPEAN", "Zentraleuropa/Europe centrale",WCH_last_evidence$sourcePopulation)
WCH_last_evidence$sourcePopulation <- ifelse(WCH_last_evidence$sourcePopulation == "DINARIC_BALKAN", "Dinariden-Balkan/Dinarides-Balkans",WCH_last_evidence$sourcePopulation)

# Add the packs info in one column:
WCH_last_evidence <- WCH_last_evidence %>%
  rowwise() %>%
  mutate(
    Info = if (all(is.na(c(packNameSource, packName_current, sourcePopulation)))) {
      NA_character_
    } else {
      # Initialize an empty list to store the pieces of information
      info_list <- list()
      
      if (!is.na(packNameSource)) {
        info_list <- append(info_list, paste("Herkunftsrudel:", packNameSource))
      }
      if (!is.na(packName_current)) {
        if (startsWith(sexID, "F")) {
          info_list <- append(info_list, paste("Fähe des Rudels:", packName_current))
        } else if (startsWith(sexID, "M")) {
          info_list <- append(info_list, paste("Rüde des Rudels:", packName_current))
        } else {
          info_list <- append(info_list, paste("Elternteil des Rudels:", packName_current))
        }
      }
      if (!is.na(sourcePopulation)) {
        info_list <- append(info_list, paste("Herkunftspopulation:", sourcePopulation))
      }
      
      # Collapse the list into a string, separated by commas
      paste(info_list, collapse = ", ")
    }
  ) %>%
  ungroup()


# Keep only the desired columns in the dataframe
desired_columns <- c("collectionDate","community","canton","sampleType",
                     "sexID","status","Info")

WCH_last_evidence <- select(WCH_last_evidence, all_of(desired_columns))

#replace NA with empty cell
WCH_last_evidence[is.na(WCH_last_evidence)] <- ""
WCH_last_evidence <- as.data.frame(WCH_last_evidence)

# Save the data as an Excel file
write.xlsx(as.data.frame(WCH_last_evidence), paste0(Year,"_QB",QB,"/",Year,"_QB",QB,"_Most_recent_gen_evidence.xlsx"), row.names = FALSE)
write_csv(WCH_last_evidence,paste0(Year,"_QB",QB,"/",Year,"_QB",QB,"_Most_recent_gen_evidence.csv"))
