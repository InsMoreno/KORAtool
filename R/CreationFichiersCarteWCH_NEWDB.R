############################################################################
######                                                                 #####
######                         WOLF MONITORING                         #####   
######                               KORA                              #####
######                      Creation of QGIS files                     #####
######                  with data from the new 2023 DB                 #####
######                                                                 #####
######                               ***                               #####
######                                                                 #####     
######                     Scripting by Inès Moreno                    #####
######                      created in october 2023                    #####
######                                                                 #####
############################################################################

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
getwd()

## Define starting and ending dates for monitoring year
start_date_23_24 <- as.Date("2023-02-01")
end_date_23_24 <- as.Date("2024-01-31")
start_date_24_25 <- as.Date("2024-02-01")
end_date_24_25 <- as.Date("2025-01-31")


#### Preparation ####
library(readr)
library(tidyverse)
#library(readxl)
#library(xlsx)
library(data.table)
library(dplyr)
library(tidyr)
library(RMariaDB)
library(sf)
library(lubridate)

## Things you can change:
BioYear <- "2023"

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

# Read DB

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

#Import kinship table
query <- paste("SELECT * FROM WolfKinship ;", sep= "")
rs = dbSendQuery(DB,query)
Kinship<-dbFetch(rs)

# Import territories 
Territories_2023 <- st_read("../../Rudel_Paare Schweiz & Angrenzend/__AKTUELL/Rudel territorien/Territories_2023_FINAL.shp")


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


# Left join to add the female and male names in the packTable that are in the Individual table 
packTable <- packTable %>%
  left_join(dplyr::select(Wolf_Individual, id.wolf, individualID), by = c("male" = "id.wolf")) %>%
  rename(male_name = individualID)
packTable <- packTable %>%
  left_join(dplyr::select(Wolf_Individual, id.wolf, individualID), by = c("female" = "id.wolf")) %>%
  rename(female_name = individualID)
# Left join to add the wolf names in the death that are in the Individual table 
Death <- Death %>%
  left_join(dplyr::select(Wolf_Individual, id.wolf, individualID), by = c("individualId" = "id.wolf")) %>%
  rename(Wolf_name = individualID)
# Convert the integer column to character
Death$individualId <- as.character(Death$individualId)
# Filter out NA values from Death table
Death_filtered <- Death %>% 
  dplyr::filter(!is.na(Wolf_name)) %>%
  dplyr::select(-id,-createdAt)

# Remove rows where the event status is rejected :
M_obs_event <- M_obs_event %>%
  filter(event_status != "REJECTED")

## Create the wolf dead bio year table:
M_obs_event_death <- merge(Death,M_obs_event, 
                           by.x = "observationId", by.y = "id",
                           all.x = TRUE)
M_obs_event_death <- M_obs_event_death%>%
  filter(species=="WOLF")
M_obs_event_death$Obs_date <- as.Date(M_obs_event_death$Obs_date, format="%Y-%m-%d %H:%M:%S")

# Check for duplicates
table(M_obs_event_death$Wolf_name)

M_obs_event_death_nonNA <- M_obs_event_death%>%
  dplyr::filter(!is.na(Wolf_name))

# Perform the join operation to have the info for each genetic sample if an individual is dead or not
M_samples_genObsEventDeath <- M_samples_genObsEvent %>%
  left_join(dplyr::select(M_obs_event_death_nonNA, Wolf_name, circumstances), 
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
      dplyr::select(name) %>%
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

# ## Assign the origin based on the kinship table
# # first obtain for each pair of parent its pack name
# Packunique <- pack_only%>%
#   dplyr::select(male_name,female_name,name)%>%
#   dplyr::filter(!is.na(male_name) & !is.na(female_name))%>%
#   distinct()
# # In the kinship, add the names instead of the ids based on the individual table
# Kinship_names <- Kinship %>%
#   left_join(dplyr::select(Wolf_Individual, id.wolf, individualID), by = c("offspring" = "id.wolf")) %>%
#   rename(Offspring_name = individualID)
# 
# # Replace id2 with its corresponding name
# Kinship_names <- Kinship_names %>%
#   left_join(dplyr::select(Wolf_Individual, id.wolf, individualID), by = c("male" = "id.wolf")) %>%
#   rename(male_name = individualID)
# 
# # Replace id3 with its corresponding name
# Kinship_names <- Kinship_names %>%
#   left_join(dplyr::select(Wolf_Individual, id.wolf, individualID), by = c("female" = "id.wolf")) %>%
#   rename(female_name = individualID)
# 
# #replace pack id by pack name:
# Kinship_names <- Kinship_names %>%
#   left_join(dplyr::select(packTable, id, name, packID), by = c("wolfPackId" = "id")) %>%
#   rename(Kinship_Pack_name = name)
# 
# # keep only the rows and columns that interest us in the kinship 
# Kinship_success <- Kinship_names %>%
#   filter(status=="PARENTS")%>%
#   dplyr::select(Offspring_name,male_name,female_name,status, Kinship_Pack_name)%>%
#   distinct()
# 
# # Merge Kinship_success with Packunique based on male_name and female_name
# Parents_data <- Kinship_success %>%
#   left_join(Packunique, by = c("male_name" = "male_name", "female_name" = "female_name"))%>%
#   rename(kinship_male_name = male_name,
#          kinship_female_name = female_name,
#          kinship_packName_prov = name,
#          kinship_status = status)
# 
# Parents_data$Kinship_Pack_name <-  ifelse(is.na(Parents_data$Kinship_Pack_name),
#                                           Parents_data$kinship_packName_prov, 
#                                           Parents_data$Kinship_Pack_name)
# 
# Parents_data <- Parents_data %>%
#   dplyr::select(-kinship_packName_prov)%>%
#   distinct()
# 
# 
# # we have some packs that have Val d'Hérens and Mandelon-Hérens as pack name for ex, for now we should keep only one. 
# Parents_data_unique <- Parents_data[!duplicated(Parents_data[, names(Parents_data) != 'Kinship_Pack_name']), ]
# 
# # Join the tables
# M_samples_genObsEventDeath <- M_samples_genObsEventDeath %>%
#   left_join(Parents_data_unique, by = c("resultIndividual" = "Offspring_name"))
# 
# # Update packNameSource only if it is NA
# M_samples_genObsEventDeath <- M_samples_genObsEventDeath %>%
#   mutate(packNameSource = if_else(is.na(packNameSource), Kinship_Pack_name, packNameSource))

# Splitting the cubsID column and removing † characters
pack_only_expanded <- pack_only %>%
  dplyr::mutate(cubsID = gsub("†", "", cubsID)) %>% # Remove †
  tidyr::separate_rows(cubsID, sep = ",\\s*") %>% # Separate based on comma and optional spaces
  dplyr::mutate(cubsID = gsub(" ", "", cubsID)) %>% # Remove any remaining spaces (might be redundant)
  dplyr::filter(!is.na(cubsID) & cubsID != "") # Filter out NA and empty strings


pack_only_expanded$packID <- gsub("_.*", "", pack_only_expanded$packID)
# Creating a list of data frames, each containing rows for a unique cubsID
cubs_tables <- pack_only_expanded %>%
  group_by(cubsID) %>%
  summarize(bioYear = unique(bioYear),
            packID = unique(packID))

cubs_tables$cubsID <- as.character(cubs_tables$cubsID)
M_samples_genObsEventDeath$resultIndividual <- as.character(M_samples_genObsEventDeath$resultIndividual)
# Left join Table_luca2023 with cubs_tables
joined_table <- left_join(M_samples_genObsEventDeath, cubs_tables, by = c("resultIndividual" = "cubsID"))

# Update yob column where it's empty
M_samples_genObsEventDeath <- joined_table %>%
  mutate(yob = ifelse(is.na(yob), bioYear, yob)) %>%
  mutate(packNameSource = packID) %>%
  dplyr::select(-bioYear, -packID) # Removing the bioYear column after the update

# add the "non interpretable" results
M_samples_genObsEventDeath <- M_samples_genObsEventDeath %>%
  mutate(resultIndividual = if_else(((is.na(resultIndividual)) | resultIndividual %in% c("")) & 
                                      (grepl("^Canis lupus", resultSpecies) | grepl("^Canis spp", resultSpecies) | resultSpecies %in% c("Wolf", "Canis lupus")), 
                                       resultNGS, resultIndividual))

# have the comments in one comment
# Function to concatenate unique, non-empty comments
concat_unique <- function(...) {
  comments <- c(...)
  comments <- na.omit(comments) # Remove NAs
  comments <- comments[comments != ""] # Remove empty strings
  unique_comments <- unique(comments) # Remove duplicates
  if(length(unique_comments) == 0) return(NA)
  paste(unique_comments, collapse = "\n")
}

# Use pmap to apply the function across rows
M_samples_genObsEventDeath$all_comments <- pmap_chr(M_samples_genObsEventDeath[c("comment", "comments.x", "comments.y", "commentKORA")], concat_unique)


## Keep only the columns that interest us
Wolf_genetics <-M_samples_genObsEventDeath%>%
  dplyr::select(keyOfficial,key,
         canton,municipality, fieldName,
         quality, type.x,
         Obs_date,received,sent, dateSpecies,resultSpecies,dateIndividual,resultIndividual,
         yob,packNameSource, packName_current, sourcePopulation,
         name, observer, observerType, copyright,
         origin, KillSpecies, priority,
         x,y, toBeSent, scalp,
         mother, father,
         propagationFront, harmfulParent, suspectedHybrid, socialStatus, eventId,
         FIWIID, CauseOfDeath,
         all_comments, compartmentMain)



#------------ Split data into compartments ####

# --- Import shapefile
suppressWarnings(Rcompartment <- raster::shapefile("../../../GIS_Data/CH/Kompartimente_Referenzgebiete/Wolfkomp_18_07_2015.shp"))
# Transform the shapefile coordinates
Rcompartment<-sf::st_as_sf(Rcompartment)
Rcompartment <- st_transform(Rcompartment, crs = 2056)
# --- Genetic sample as spatial points
pts<- sf::st_as_sf(Wolf_genetics[,c("x","y")],coords = c("x", "y"),crs = st_crs(2056))
# Check CRS for both layers
print(sf::st_crs(Rcompartment))
print(sf::st_crs(pts))

# --- Import data Compartment for each point
pts<-sf::st_join(pts, Rcompartment)
pts<-as.data.table(pts)

# --- Insert data into w.genetic table
# Replace compartmen with Nummer from pts if compartmen is NA
Wolf_genetics$compartmentMain <- ifelse(is.na(Wolf_genetics$compartmentMain), pts$Nummer, Wolf_genetics$compartmentMain)

#### QGIS files ####

#Keep only the first 10 letters of the columns names (necessary for QGIS)
names(Wolf_genetics) <- substring(names(Wolf_genetics), 1,10)
Wolf_genetics$resultIndi[Wolf_genetics$resultIndi==""] <- NA
Wolf_genetics <- Wolf_genetics %>%
  mutate(across(c("Obs_date","received", "sent", "dateSpecie","dateIndivi"), ~as.Date(., format="%Y-%m-%d %H:%M:%S")))



#### Wolf individuum
## Since always
Loups_individu_depuistouslestemps <- Wolf_genetics %>%
  filter(!is.na(resultIndi) & 
           !resultIndi %in% c("nicht interpretierbar","CANIS_FAMILIARIS","canis familiaris", "not interpretable") &
           !grepl("^NOT_INTERPRETABLE", resultIndi) & 
           !grepl("^PROFILE_PARTIALLY", resultIndi))

# Bio year 2024
Loups_individu_2024 <- Wolf_genetics%>%
  subset(Obs_date >= as.Date("2024-02-01") & Obs_date <= as.Date("2025-01-31")) %>%
  filter(!is.na(resultIndi) & 
           !resultIndi %in% c("nicht interpretierbar","CANIS_FAMILIARIS","canis familiaris", "not interpretable") &
           !grepl("^NOT_INTERPRETABLE", resultIndi) & 
           !grepl("^PROFILE_PARTIALLY", resultIndi))
# Bio year 2023
Loups_individu_2023 <- Wolf_genetics%>%
  subset(Obs_date >= as.Date("2023-02-01") & Obs_date <= as.Date("2024-01-31")) %>%
  filter(!is.na(resultIndi) & 
           !resultIndi %in% c("nicht interpretierbar","CANIS_FAMILIARIS","canis familiaris", "not interpretable") &
           !grepl("^NOT_INTERPRETABLE", resultIndi) & 
           !grepl("^PROFILE_PARTIALLY", resultIndi))
# Bio year 2022
Loups_individu_2022 <- Wolf_genetics%>%
  subset(Obs_date >= as.Date("2022-02-01") & Obs_date <= as.Date("2023-01-31")) %>%
  filter(!is.na(resultIndi) & 
           !resultIndi %in% c("nicht interpretierbar","CANIS_FAMILIARIS","canis familiaris", "not interpretable") &
           !grepl("^NOT_INTERPRETABLE", resultIndi) & 
           !grepl("^PROFILE_PARTIALLY", resultIndi))

#### Samples at KORA (not sent)
# Bio year 2023
Echantillons_auKORA_2024 <- Wolf_genetics%>%
  filter(is.na(sent) 
         & Obs_date >= "2024-02-01"
         & Obs_date <= "2025-01-31")

# Bio year 2023
Echantillons_auKORA_2023 <- Wolf_genetics%>%
  filter(is.na(sent) 
         & Obs_date >= "2023-02-01"
         & Obs_date <= "2024-01-31")

# Bio year 2022
Echantillons_auKORA_2022 <- Wolf_genetics%>%
  filter(is.na(sent) 
         & Obs_date >= "2022-02-01"
         & Obs_date <= "2023-01-31")

#### Samples at the LBC, still no result
Echantillons_auLBC <- Wolf_genetics%>%
  filter(!is.na(sent) 
         & !toBeSent %in% c("TOBESENT")
         & is.na(dateSpecie)
         & (is.na(resultSpec) | resultSpec %in% c("")))

#### Sample to be sent
Submissiondate_today <- Wolf_genetics %>%
  filter(toBeSent %in% c("TOBESENT"))

#### Result species is not wolf
Result1_PASloup <- Wolf_genetics%>%
  subset(Obs_date >= as.Date("2023-02-01") & Obs_date <= Sys.Date()) %>%
  filter(!(grepl("^Canis lupus", resultSpec) | resultSpec %in% c("Wolf", "Canis lupus")) 
         & !is.na(resultSpec) 
         & !resultSpec %in% c("")
         & (is.na(resultIndi) | resultIndi %in% c("")))

#### Result wolf but not yet receive the individual result
Loups_PASindividu <-  Wolf_genetics%>%
  #subset(Obs_date >= as.Date("2024-02-01") & Obs_date <= as.Date("2025-01-31")) %>%
  filter((grepl("^Canis lupus", resultSpec) | grepl("^Canis spp", resultSpec) | resultSpec %in% c("Wolf", "Canis lupus"))
         & (is.na(resultIndi) | resultIndi %in% c(""))
         & is.na(dateIndivi))

#### Genotype was not interpretable
Genotype_nichtint_24_25 <- Wolf_genetics %>%
  filter(Obs_date>= as.Date("2024-02-01") & Obs_date <= as.Date("2025-01-31")
         & (resultIndi %in% c("nicht interpretierbar", "not interpretable") |
              grepl("^NOT_INTERPRETABLE", resultIndi) | 
              grepl("^PROFILE_PARTIALLY", resultIndi)))
Genotype_nichtint_23_24 <- Wolf_genetics %>%
  filter(Obs_date>= as.Date("2023-02-01") & Obs_date <= as.Date("2024-01-31")
         & (resultIndi %in% c("nicht interpretierbar", "not interpretable") |
           grepl("^NOT_INTERPRETABLE", resultIndi) | 
           grepl("^PROFILE_PARTIALLY", resultIndi)))

#### Dead table 
M_obs_event_death_bioYear23 <- M_obs_event_death %>%
  dplyr::filter(Obs_date >= start_date_23_24, Obs_date <= end_date_23_24,
         event_status != "REJECTED") %>%
  dplyr::select(-createdAt,-createdAt.x,-createdAt.y, -compartmentPart, -reporterId, -updatedAt,-KillSpecies)
M_obs_event_death_bioYear23$Indiv <- ifelse(is.na(M_obs_event_death_bioYear23$individualId), 'unbekannt', 'bekannt')
M_obs_event_death_bioYear24 <- M_obs_event_death %>%
  dplyr::filter(Obs_date >= start_date_24_25, Obs_date <= end_date_24_25,
                event_status != "REJECTED") %>%
  dplyr::select(-createdAt,-createdAt.x,-createdAt.y, -compartmentPart, -reporterId, -updatedAt,-KillSpecies)
M_obs_event_death_bioYear24$Indiv <- ifelse(is.na(M_obs_event_death_bioYear24$individualId), 'unbekannt', 'bekannt')

# # for VD canton:
# 
# Samples_VD_2023 <-  Wolf_genetics %>%
#   subset(Obs_date >= as.Date("2023-01-01") & Obs_date <= Sys.Date()) %>%
#   dplyr::filter(canton == "VD")
# 
# write.xlsx(Samples_VD_2023, "/Volumes/Public/01_KORA/IKK/IKK_2023_Power_Point_20231116/VD_15.11.23/Situation_VD_2023/22.11.2023/All_Samples_VD_2023_23112023.xlsx", row.names = FALSE)


#### pack pairs
packTable

# keep only the packs of the current bioyear and add the parent infos in the territories layer
pack_2023 <- pack_only %>%
  filter(bioYear=='2023')
# 
# Terr2023 <- st_read("/Volumes/Public/03_Data/Wolf DB/Rudel_Paare Schweiz & Angrenzend/__AKTUELL/Rudel territorien/Obs_territorien_oct2022/qgisfiles/Territories.shp")
# Terr2023_mod <- Terr2023
# Terr2023_mod$Annee <- "2023"
# Terr2023_mod$Type_meute <- NA
# Terr2023_mod <- Terr2023_mod %>%
#   select(Annee,NAME,female,male,Type_meute,Area_km2,geometry) %>%
#   rename(Nom_meute=NAME)
# 
# Terr2023_mod <- Terr2023_mod[!st_is_empty(Terr2023_mod$geometry), ]
# 
# # Function to convert XYZ to XY for a single geometry
# convert_xyz_to_xy <- function(geometry) {
#   coords <- st_coordinates(geometry)
#   new_coords <- coords[, 1:2] # Keep only X and Y coordinates
#   st_polygon(list(new_coords))
# }
# 
# # Apply the conversion to each geometry in the sf object
# Terr2023_mod_2D <- Terr2023_mod
# Terr2023_mod_2D$geometry <- lapply(Terr2023_mod$geometry, convert_xyz_to_xy) |> st_sfc(crs = st_crs(Terr2023_mod))
# 
# 
# VS2023 <- st_read("/Volumes/Public/03_Data/Wolf DB/Rudel_Paare Schweiz & Angrenzend/__AKTUELL/Rudel territorien/Obs_territorien_oct2022/qgisfiles/Rudel_VS_231025/Rudel_VS_231025.shp")
# VS2023_mod <- VS2023
# VS2023_mod$female <- NA
# VS2023_mod$male <- NA
# VS2023_mod <- VS2023_mod %>%
#   select(Annee,Nom_meute,female,male,Type_meute, Shape_Area, geometry)%>%
#   rename(Area_km2=Shape_Area)
# 
# Territories_2023 <- rbind(Terr2023_mod_2D,VS2023_mod)


result <- Territories_2023 %>%
  left_join(pack_2023, by = c("PACKID" = "packID"), keep = TRUE)
Territories_2023$male <- result$male_name
Territories_2023$female <- result$female_name
Territories_2023$Nom_meute <- ifelse(!is.na(result$packID), result$name, Territories_2023$Nom_meute)
# st_write(Territories_2023, "../../Rudel_Paare Schweiz & Angrenzend/__AKTUELL/Rudel territorien/Territories_2023_FINAL.shp",
#          delete_layer = TRUE)



#### Write tables
write_csv(Loups_individu_depuistouslestemps, "Files Ines dont touch/Loups_individu_depuistouslestemps.csv")
write_csv(Loups_individu_2022, "Files Ines dont touch/Loups_individu_22_23.csv")
write_csv(Loups_individu_2023, "Files Ines dont touch/Loups_individu_23_24.csv")
write_csv(Loups_individu_2024, "Files Ines dont touch/Loups_individu_24_25.csv")
write_csv(Echantillons_auKORA_2024, "Files Ines dont touch/Echantillons_auKORA2024.csv")
write_csv(Echantillons_auKORA_2023, "Files Ines dont touch/Echantillons_auKORA2023.csv")
write_csv(Echantillons_auKORA_2022, "Files Ines dont touch/Echantillons_auKORA2022.csv")
write_csv(Echantillons_auLBC, "Files Ines dont touch/Echantillons_auLBC.csv")
write_csv(Submissiondate_today, "Files Ines dont touch/Submissiondate_today.csv")
write_csv(Result1_PASloup, "Files Ines dont touch/Result1_PASloup.csv")
write_csv(Loups_PASindividu, "Files Ines dont touch/Loups_PASindividu.csv")
write_csv(Genotype_nichtint_24_25, "Files Ines dont touch/Genotype_nichtint_24_25.csv")
write_csv(Genotype_nichtint_23_24, "Files Ines dont touch/Genotype_nichtint_23_24.csv")

write_csv(M_obs_event_death_bioYear23, "Files Ines dont touch/Loups_morts_23_24.csv")
write_csv(M_obs_event_death_bioYear24, "Files Ines dont touch/Loups_morts_24_25.csv")

write_csv(packTable, "Files Ines dont touch/Pack_pair.csv")


#### Autres ####

# ### test
# # Load necessary libraries
# library(ggplot2)
# 
# # Assuming M_samples_genObsEventDeath is your dataframe and Obs_date is your date column
# # Convert the date strings to date objects
# M_samples_genObsEventDeath$Obs_date <- as.POSIXct(M_samples_genObsEventDeath$Obs_date, format="%Y-%m-%d %H:%M:%S", tz="CEST")
# 
# # Extract the month and year from the date
# M_samples_genObsEventDeath$Month <- format(M_samples_genObsEventDeath$Obs_date, "%Y-%m")
# 
# # Filter out only 2023 data
# data_2023 <- subset(M_samples_genObsEventDeath, format(Obs_date, "%Y") == "2023")
# 
# # Count the number of samples for each month
# monthly_counts <- table(data_2023$Month)
# 
# # Convert to a dataframe for plotting
# df <- as.data.frame(monthly_counts)
# 
# # Plot the histogram
# ggplot(df, aes(x=Var1, y=Freq)) +
#   geom_bar(stat="identity") +
#   xlab("Month") +
#   ylab("Number of Samples") +
#   ggtitle("Number of Samples per Month in 2023")

# #Pour Luca:
# 
# ## Keep only the columns that interest us
# Table_luca <-M_samples_genObsEventDeath%>%
#   dplyr::select(keyOfficial,key,
#                 municipality, fieldName,canton,x,y,
#                 observer, observerType,
#                 containerType,type.x,quality, 
#                 origin,KillSpecies,
#                 Obs_date,received,sent, dateSpecies,dateIndividual,
#                 resultSpecies,resultIndividual,resultLbcID1,resultLbcID2,
#                 yob,packNameSource, packName_current,
#                 name,  copyright,
#                 eventId,
#                 all_comments, compartmentMain)
# 
# # --- Import shapefile
# suppressWarnings(Rcompartment <- raster::shapefile("../../../GIS_Data/CH/Kompartimente_Referenzgebiete/Wolfkomp_18_07_2015.shp"))
# # Transform the shapefile coordinates
# Rcompartment<-sf::st_as_sf(Rcompartment)
# Rcompartment <- st_transform(Rcompartment, crs = 2056)
# # --- Genetic sample as spatial points
# pts<- sf::st_as_sf(Table_luca[,c("x","y")],coords = c("x", "y"),crs = st_crs(2056))
# # Check CRS for both layers
# print(sf::st_crs(Rcompartment))
# print(sf::st_crs(pts))
# 
# # --- Import data Compartment for each point
# pts<-sf::st_join(pts, Rcompartment)
# pts<-as.data.table(pts)
# 
# # --- Insert data into w.genetic table
# # Replace compartmen with Nummer from pts if compartmen is NA
# Table_luca$compartmentMain <- ifelse(is.na(Table_luca$compartmentMain), pts$Nummer, Table_luca$compartmentMain)
# 
# #### QGIS files ####
# 
# #Keep only the first 10 letters of the columns names (necessary for QGIS)
# Table_luca$resultIndividual[Table_luca$resultIndividual==""] <- NA
# Table_luca <- Table_luca %>%
#   mutate(across(c("Obs_date","received", "sent", "dateSpecies","dateIndividual"), ~as.Date(., format="%Y-%m-%d %H:%M:%S")))
# 
# Table_luca2023 <- Table_luca %>%
#   dplyr::filter(sent >= "2023-01-01" & sent <= "2023-12-31")
# 
# 
# # Splitting the cubsID column and removing † characters
# pack_only_expanded <- pack_only %>%
#   mutate(cubsID = gsub("†", "", cubsID)) %>%
#   separate_rows(cubsID, sep = ",\\s*") %>%
#   mutate(cubsID = gsub(" ", "", cubsID))%>%
#   filter(!is.na(cubsID) & cubsID != "")
# 
# 
# 
# # Creating a list of data frames, each containing rows for a unique cubsID
# cubs_tables <- pack_only_expanded %>%
#   group_by(cubsID) %>%
#   summarize(bioYear = unique(bioYear))
# 
# # Left join Table_luca2023 with cubs_tables
# joined_table <- left_join(Table_luca2023, cubs_tables, by = c("resultIndividual" = "cubsID"))
# 
# # Update yob column where it's empty
# joined_table <- joined_table %>%
#    mutate(yob = ifelse(is.na(yob), bioYear, yob)) %>%
#    select(-bioYear) # Removing the bioYear column after the update
# 
# write.xlsx(joined_table, "/Volumes/Public/03_Data/Wolf DB/Genetik/2024/Tables_LF/Samples_sent_2023.xlsx",
#            row.names = FALSE, showNA = FALSE)

# ## ---- demande VS, résumé 2023
# 
# VS <- M_samples_genObsEventDeath %>%
#   filter(canton == "VS" &
#            Obs_date >= "2023-01-01" & Obs_date <= "2023-12-31")
# VS$resultSpecies <- gsub("_"," ", VS$resultSpecies)
# VS$resultIndividual <- gsub("_"," ", VS$resultIndividual)
# VS$resultSpecies <- sapply(VS$resultSpecies, function(x) {
#   if (!is.na(x)) {
#     return(paste0(toupper(substr(x, 1, 1)), tolower(substr(x, 2, nchar(x)))))
#   } else {
#     return(NA)
#   }
# })
# VS$resultIndividual <- sapply(VS$resultIndividual, function(x) {
#   if (!is.na(x)) {
#     return(paste0(toupper(substr(x, 1, 1)), tolower(substr(x, 2, nchar(x)))))
#   } else {
#     return(NA)
#   }
# })
# VS$resultSpecies <- gsub("Canis lupus.*", "Canis lupus", VS$resultSpecies)
# 
# VS$sourcePopulation <- ifelse(is.na(VS$sourcePopulation) & VS$resultSpecies=="Canis lupus" &
#                                 !is.na(VS$resultIndividual) & VS$resultIndividual !="Not interpretable",
#                                     "ITALIAN_PENINSULA",VS$sourcePopulation)
# 
# VS <- VS %>%
#   select(eventId, keyOfficial, key, Obs_date,canton,municipality,fieldName,x,y,
#          observer, observerType, name, copyright, type.x, quality, containerType,
#          origin, KillSpecies, received, sent, resultSpecies, resultIndividual,
#          sourcePopulation, packNameSource,yob,packName_current)
# VS <- VS %>%
#   mutate(across(c("Obs_date","received", "sent"), ~as.Date(., format="%Y-%m-%d %H:%M:%S")))
# VS$packNameSource <-  ifelse(VS$packNameSource== "Aug2", "Augstbord 2 (2021)",
#                         ifelse(VS$packNameSource== "Aug3", "Augstbord 2 (2022)",
#                           ifelse(VS$packNameSource== "Aug4", "Augstbord 2 (2023)",
#                             ifelse(VS$packNameSource== "Cha", "Chablais",
#                               ifelse(VS$packNameSource== "Ise", "Isérables",
#                                 ifelse(VS$packNameSource== "MaH", "Mandelon-Hérens",
#                                   ifelse(VS$packNameSource== "Mit", "Mittelwallis",
#                                     ifelse(VS$packNameSource== "Nan", "Nanz",
#                                       ifelse(VS$packNameSource== "NeS", "Nendaz-Siviez",
#                                         ifelse(VS$packNameSource== "Ons", "Onsernone",
#                                           ifelse(VS$packNameSource== "Tou", "Les Toules",
#                                             ifelse(VS$packNameSource== "VHe", "Val d'Hérens",
#                                                     VS$packNameSource))))))))))))
# 
# VS <- VS %>%
#   mutate(across(c(Obs_date, received, sent), ~format(as.Date(.x), "%Y-%m-%d")))
# 
# VS <- VS %>% arrange(desc(Obs_date))
# 
# proportion_canis_lupus <- VS %>%
#   filter(!is.na(sent)) %>% # Exclut les lignes où sent est NA
#   summarise(
#     total_with_date = n(), # Nombre total d'entrées avec une date non NA dans sent
#     canis_lupus_count = sum(resultSpecies == "Canis lupus", na.rm = TRUE), # Compte "Canis lupus"
#     proportion = canis_lupus_count / total_with_date # Calcule la proportion
#   )
# 
# 
# M_samples_genObsEventDeath$resultIndividual <-  gsub("PROFILE_PARTIALLY", "",M_samples_genObsEventDeath$resultIndividual)
# 
# # Filtrer les lignes avec des ID valides et exclure les NA et les valeurs non interprétables
# filtered_df <- M_samples_genObsEventDeath %>%
#   filter(!is.na(resultIndividual)) %>%
#   filter(grepl("^[FMU]\\d{2,3}$", resultIndividual)) %>%
#   filter(!is.na(Obs_date))
# 
# # Assurez-vous que Obs_date est au format Date si ce n'est pas déjà le cas
# filtered_df$Obs_date <- as.Date(filtered_df$Obs_date)
# 
# # Créer un nouveau dataframe avec la première date d'identification pour chaque individu
# first_identification_df <- filtered_df %>%
#   group_by(resultIndividual) %>%
#   summarise(First_Obs_date = min(Obs_date)) %>%
#   ungroup()
# 
# # Joindre le dataframe résumé au dataframe VS pour ajouter la première date d'observation
# VS_with_first_obs_date <- VS %>%
#   left_join(first_identification_df, by = "resultIndividual")
# 
# # Ajouter si un loup est mort
# # Étape 1: Marquer comme "Yes" les individus décédés dans Death_filtered
# Death_filtered$Dead <- "Yes"
# # Réduire Death_filtered à une liste unique d'IDs avec "Yes" pour les individus décédés
# Death_filtered_unique <- Death_filtered %>%
#   select(Wolf_name) %>%
#   distinct() %>%
#   mutate(Dead = "Yes")
# # Étape 2: Joindre la colonne Dead basée sur l'ID de l'individu
# VS_with_first_obs_date <- VS_with_first_obs_date %>%
#   left_join(Death_filtered_unique[, c("Wolf_name", "Dead")], by = c("resultIndividual" = "Wolf_name"))
# 
# 
# write.xlsx(VS_with_first_obs_date, "/Volumes/Public/03_Data/Wolf DB/ADMIN/VS/Demande_YCrettenand_290124/KORAReport_VS_310124.xlsx",
#            row.names = FALSE,
#            showNA = FALSE)

