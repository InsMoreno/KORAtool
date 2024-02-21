#script to create the QB table for the statistic of genetic samples per canton
#written by Inès Moreno & edited by Ursi Sterrer 2022/2023

########################################################################################################

##### Preparation  ####
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
getwd()

library(RMariaDB)
library(data.table)
library(ggrepel)
library(tidyverse)
library(lubridate)
library(xlsx)


#define timeframe
QB_number <- "4"
prev_year_start <- "2022-01-01"
prev_year_end <- "2022-12-31"
Q1 <- "2023-01-01"
Q2 <- "2023-04-01"
Q3 <- "2023-07-01"
Q4 <- "2023-10-01"
Q_end <- "2023-12-31"
#define until which Quartal the total count should be made 
# (choose the NEW QUARTAL as we defined the BEGINNING dates!)
Q_end_tot <- Q_end

#define headers for table
year_current <- "2023"
year_before <- "2022"
Q1_header <- "1. Quartal 2023"
Q2_header <- "2. Quartal 2023"
Q3_header <- "3. Quartal 2023"
Q4_header <- "4. Quartal 2023"
sum_header <- "Summe 2023"

#### connect to DB and append Kora Report ####

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
query <- paste("SELECT * FROM `kora-next`.`Kill` ;", sep= "")
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

# Replace NA values in the canton column with corresponding country value
# first name the country "FRANCE" FRA because otherwise it would be FR as the canton Fribourg
M_samples_genObsEvent$country <- ifelse(M_samples_genObsEvent$country == "FR", "FRA", M_samples_genObsEvent$country)
M_samples_genObsEvent$canton <- ifelse(
  is.na(M_samples_genObsEvent$canton),
  M_samples_genObsEvent$country,
  M_samples_genObsEvent$canton)

M_samples_genObsEvent <- M_samples_genObsEvent %>%
  mutate(canton = case_when(
    is.na(country) ~ canton, # Keeps original canton if country is NA
    country == "FRA" ~ "FRA", # Sets canton to "FRA" if country is "FRA"
    TRUE ~ canton # Default case to keep original canton
  ))



## Keep only the columns that interest us
Wolf_genetics <-M_samples_genObsEvent%>%
  select(keyOfficial,key,
         canton,municipality, fieldName,
         quality, type.x,
         Obs_date,received,sent, dateSpecies,resultSpecies,dateIndividual,resultIndividual,
         yob, sourcePopulation,
         name, observer, observerType, copyright,
         origin, KillSpecies, priority,
         x,y, toBeSent, scalp,
         eventId)


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


####create table ####
sort(unique(WCH$canton))
essai <- data.frame(Kanton = c("AG", "AI", "AR", "BE", "BL", "FR","GE", "GL", "GR", "JU", "LU", "NE", "NW", 
                               "OW", "SG", "SH", "SO", "SZ", "TG", "TI", "UR", "VD", "VS", "ZG", "ZH", "AT","DE", "FRA", "IT", "LI"),
                    Ybefore_KORA = as.numeric(NA),
                    Ybefore_LBC = as.numeric(NA),
                    Q1_KORA = as.numeric(NA),
                    Q1_LBC = as.numeric(NA),
                    Q2_KORA = as.numeric(NA),
                    Q2_LBC = as.numeric(NA),
                    Q3_KORA = as.numeric(NA),
                    Q3_LBC = as.numeric(NA),
                    Q4_KORA = as.numeric(NA),
                    Q4_LBC = as.numeric(NA),
                    Qtot_KORA = as.numeric(NA),
                    Qtot_LBC = as.numeric(NA))

#create data for previous year
#received
Ybefore_KORA <- WCH %>%
  filter(receiptDate >= prev_year_start & receiptDate <= prev_year_end) %>%
  count(canton, sort = TRUE)
#sent to LBC - analysed
Ybefore_LBC <- WCH %>%
  filter(submissionDate >= prev_year_start & submissionDate <= prev_year_end) %>%
  count(canton, sort = TRUE)

#filter for QB
Q1_KORA <- WCH %>%
  filter(receiptDate>= Q1 & receiptDate < Q2) %>%
  count(canton, sort = TRUE)

Q1_LBC <- WCH %>%
  filter(submissionDate >= Q1 & submissionDate < Q2) %>%
  count(canton, sort = TRUE)

Q2_KORA <- WCH %>%
  filter(receiptDate >= Q2 & receiptDate < Q3) %>%
  count(canton, sort = TRUE)

Q2_LBC <- WCH %>%
  filter(submissionDate >= Q2 & submissionDate < Q3) %>%
  count(canton, sort = TRUE)

Q3_KORA <- WCH %>%
  filter(receiptDate >= Q3 & receiptDate < Q4) %>%
  count(canton, sort = TRUE)

Q3_LBC <- WCH %>%
  filter(submissionDate >= Q3 & submissionDate < Q4) %>%
  count(canton, sort = TRUE)

Q4_KORA <- WCH %>%
  filter(receiptDate >= Q4 & receiptDate <= Q_end) %>%
  count(canton, sort = TRUE)

Q4_LBC <- WCH %>%
  filter(submissionDate >= Q4 & submissionDate <= Q_end) %>%
  count(canton, sort = TRUE)

Qtot_KORA <- WCH %>%
  filter(receiptDate >=  Q1 & receiptDate < Q_end_tot) %>%
  count(canton, sort = TRUE)

Qtot_LBC <- WCH %>%
  filter(submissionDate >= Q1 & submissionDate < Q_end_tot) %>%
  count(canton, sort = TRUE)


essai$Ybefore_KORA <- Ybefore_KORA$n[match(essai$Kanton, Ybefore_KORA$canton)]
essai$Ybefore_LBC <- Ybefore_LBC$n[match(essai$Kanton, Ybefore_LBC$canton)]

essai$Q1_KORA <- Q1_KORA$n[match(essai$Kanton, Q1_KORA$canton)]
essai$Q1_LBC <- Q1_LBC$n[match(essai$Kanton, Q1_LBC$canton)]

essai$Q2_KORA <- Q2_KORA$n[match(essai$Kanton, Q2_KORA$canton)]
essai$Q2_LBC <- Q2_LBC$n[match(essai$Kanton, Q2_LBC$canton)]

essai$Q3_KORA <- Q3_KORA$n[match(essai$Kanton, Q3_KORA$canton)]
essai$Q3_LBC <- Q3_LBC$n[match(essai$Kanton, Q3_LBC$canton)]

essai$Q4_KORA <- Q4_KORA$n[match(essai$Kanton, Q4_KORA$canton)]
essai$Q4_LBC <- Q4_LBC$n[match(essai$Kanton, Q4_LBC$canton)]

essai$Qtot_KORA <- Qtot_KORA$n[match(essai$Kanton, Qtot_KORA$canton)]
essai$Qtot_LBC <- Qtot_LBC$n[match(essai$Kanton, Qtot_LBC$canton)]


essai <- rbind.data.frame(essai, c("Total",colSums(essai[,2:13], na.rm = TRUE)))

essai <- transform(essai, Ybefore_KORA = as.numeric(Ybefore_KORA),
                   Ybefore_LBC = as.numeric(Ybefore_LBC),
                   Q1_KORA = as.numeric(Q1_KORA),
                   Q1_LBC = as.numeric(Q1_LBC),
                   Q2_KORA = as.numeric(Q2_KORA),
                   Q2_LBC = as.numeric(Q2_LBC),
                   Q3_KORA = as.numeric(Q3_KORA),
                   Q3_LBC = as.numeric(Q3_LBC),
                   Q4_KORA = as.numeric(Q4_KORA),
                   Q4_LBC = as.numeric(Q4_LBC),
                   Qtot_KORA = as.numeric(Qtot_KORA),
                   Qtot_LBC = as.numeric(Qtot_LBC)
)

essai <- rbind(essai, c("Total CH",colSums(essai[1:25,2:13], na.rm = TRUE)),stringsAsFactors = FALSE)
essai <- essai[c(1:25,32,26:31),]
rownames(essai) <- 1:nrow(essai)
essai[is.na(essai)] <- "-"

#### save Output ####
write.csv(essai, paste0(year_current,"_QB",QB_number,"_genetic_samples_statistic.csv"), row.names = F)
write.xlsx(essai, paste0(year_current,"_QB",QB_number,"_genetic_samples_statistic.xlsx"), row.names = F)
 
#### create graph ####

tablegraph <- essai %>% 
  select(Kanton, Qtot_KORA, Qtot_LBC) %>% 
  slice(c(1:25),31)
tablegraph[tablegraph == "-"] <- NA

df.long <- pivot_longer(tablegraph,cols = 2:3, names_to = "KORALBC", values_to = "nbsamples")
df.long <- transform(df.long, nbsamples = as.numeric(nbsamples))
head(df.long)

samples_canton <- ggplot(data=df.long,
       aes(x=Kanton, y= nbsamples, fill= KORALBC))+
  geom_bar(stat = "identity", 
           position=position_dodge(),
           #width = 0.7
  ) +
  # geom_text(aes(label=nbsamples), vjust=1.6, color="white",
  #           position = position_dodge(0.9), size=2.5) +
  scale_fill_manual( labels = c("Erhalten - KORA", "Eingesandt - LBC"), values = c("#434AA5","#8F1923"))+
  labs(x= "Kanton",
       y = paste0("Erhaltene und eingesandte Proben ",format(Sys.Date(), "%Y")),
       title = "")+
  theme(legend.position = c(0.15, 0.8),
        legend.title = element_blank(),
        legend.background = element_rect(size=0.3, linetype="solid",
                                         colour ="darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "darkgrey"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "white"));samples_canton
ggsave(paste0(year_current,"_QB",QB_number,"_samples_per_canton.jpg"), samples_canton,
       width = 7, height = 7)


##### Table C2 Probentyp  ####
#create table
probentyp <- data.frame(year_current = c(as.character(paste0("Jahr: ",year_before)), 
                                         as.character(paste0("Jahr: ",year_current)), 
                                         "1Q","2Q","3Q","4Q","Total"), 
                        Speichel = NA,
                        Kot= NA,
                        Urin = NA,
                        An = NA,
                        Summe = NA,
                        Wolf = NA,
                        Luchs= NA,
                        Bär = NA,
                        Goldschakal = NA,
                        Canide = NA,
                        Hund = NA,
                        Fuchs= NA,
                        Carnivora = NA,
                        Mensch = NA,
                        Huftier = NA,
                        and_Arten = NA,
                        nicht_interp = NA,
                        nnb = NA)

probentyp <- probentyp %>% 
  remove_rownames %>% 
  column_to_rownames(var="year_current")

# Convert to lowercase
WCH$resultSpecies <- tolower(WCH$resultSpecies)
WCH$resultSpecies <- gsub("_", " ", WCH$resultSpecies)
uniquespecies <- sort(unique(WCH$resultSpecies))

# Define a custom function for each species filter
matches_wolf <- function(resultSpecies) {
  grepl("lupus", resultSpecies, ignore.case = TRUE) | resultSpecies %in% c("wolf", "canis lupus")
}

matches_dog <- function(resultSpecies) {
  grepl("familiaris|domesticus", resultSpecies, ignore.case = TRUE)
}

matches_lynx <- function(resultSpecies) {
  grepl("lynx", resultSpecies, ignore.case = TRUE)
}

matches_bear <- function(resultSpecies) {
  grepl("ursus", resultSpecies, ignore.case = TRUE)
}

# For Golden Jackal
matches_gold_jackal <- function(resultSpecies) {
  grepl("aureus", resultSpecies, ignore.case = TRUE)
}

# For Fox
matches_fox <- function(resultSpecies) {
  grepl("vulpes", resultSpecies, ignore.case = TRUE)
}

# For Canis spp
matches_canis_spp <- function(resultSpecies) {
  grepl("canis spp|canis_spp|canis spp.", resultSpecies, ignore.case = TRUE)
}

# For Huftier (Ungulates)
matches_huftier <- function(resultSpecies) {
  grepl("bos|capra|capreolus|cervus|dama|equus|lama|ovis|rupicapra|sus|ungulata", resultSpecies, ignore.case = TRUE)
}

# For Other Carnivores
matches_other_carnivores <- function(resultSpecies) {
  grepl("martes|meles|mustelidae|felidae|felis", resultSpecies, ignore.case = TRUE)
}

# For Other Species
matches_other_species <- function(resultSpecies) {
  grepl("lepus|other", resultSpecies, ignore.case = TRUE)
}

# For non interpretable results
matches_not_int <- function(resultSpecies) {
  grepl("no dna|not intepretable|not interpretable", resultSpecies, ignore.case = TRUE)
}

matches_nnb <-function(resultSpecies) {
  resultSpecies == "" | is.na(resultSpecies)
}

# nicht_interp <- c("nicht interpretierbar (kontaminiert)", "Nicht interpretierbar","nicht interpretierbar","keine DNA",
#                   "Keine DNA","Analyseproblem LBC",
#                   "other") #/!\ other is here for now because we can't put "nicht interpretierbar" in the KORA REPORT (michael version) !!! So we put other for now....


#calculate past year
probentyp[1,"Speichel"] <- nrow(filter(WCH, submissionDate >= prev_year_start & submissionDate <= prev_year_end 
                                          & sampleType == "SALIVA"))
probentyp[1,"Kot"] <- nrow(filter(WCH, submissionDate >= prev_year_start & submissionDate <= prev_year_end 
                                     & sampleType == "SCAT"))
probentyp[1,"Urin"] <- nrow(filter(WCH, submissionDate >= prev_year_start & submissionDate <= prev_year_end 
                                      & sampleType == "URINE"))
probentyp[1,"An"] <- nrow(filter(WCH, submissionDate >= prev_year_start & submissionDate <= prev_year_end 
                                    & sampleType != "URINE" & sampleType != "SCAT" & sampleType != "SALIVA"))
probentyp[1,"Summe"] <- rowSums(probentyp[1,1:4])

count_wolf <- WCH %>%
  filter(submissionDate >= prev_year_start & submissionDate <= prev_year_end) %>%
  filter(matches_wolf(resultSpecies)) %>%
  nrow()
probentyp[1,"Wolf"] <- count_wolf

count_lynx <- WCH %>%
  filter(submissionDate >= prev_year_start & submissionDate <= prev_year_end) %>%
  filter(matches_lynx(resultSpecies)) %>%
  nrow()
probentyp[1,"Luchs"] <- count_lynx

count_bear <- WCH %>%
  filter(submissionDate >= prev_year_start & submissionDate <= prev_year_end) %>%
  filter(matches_bear(resultSpecies)) %>%
  nrow()
probentyp[1,"Bär"] <- count_bear

count_goldjackal <- WCH %>%
  filter(submissionDate >= prev_year_start & submissionDate <= prev_year_end) %>%
  filter(matches_gold_jackal(resultSpecies)) %>%
  nrow()
probentyp[1,"Goldschakal"] <- count_goldjackal

count_canide <- WCH %>%
  filter(submissionDate >= prev_year_start & submissionDate <= prev_year_end) %>%
  filter(matches_canis_spp(resultSpecies)) %>%
  nrow()
probentyp[1,"Canide"] <- count_canide

count_dog <- WCH %>%
  filter(submissionDate >= prev_year_start & submissionDate <= prev_year_end) %>%
  filter(matches_dog(resultSpecies)) %>%
  nrow()
probentyp[1,"Hund"] <- count_dog

count_fox <- WCH %>%
  filter(submissionDate >= prev_year_start & submissionDate <= prev_year_end) %>%
  filter(matches_fox(resultSpecies)) %>%
  nrow()
probentyp[1,"Fuchs"] <- count_fox

count_carnivora <- WCH %>%
  filter(submissionDate >= prev_year_start & submissionDate <= prev_year_end) %>%
  filter(matches_other_carnivores(resultSpecies)) %>%
  nrow()
probentyp[1,"Carnivora"] <- count_carnivora

probentyp[1,"Mensch"] <- nrow(filter(WCH, submissionDate >= prev_year_start & submissionDate <= prev_year_end 
                                        & resultSpecies=="homo sapiens"))

count_huftier <- WCH %>%
  filter(submissionDate >= prev_year_start & submissionDate <= prev_year_end) %>%
  filter(matches_huftier(resultSpecies)) %>%
  nrow()
probentyp[1,"Huftier"] <- count_huftier

count_other <- WCH %>%
  filter(submissionDate >= prev_year_start & submissionDate <= prev_year_end) %>%
  filter(matches_other_species(resultSpecies)) %>%
  nrow()
probentyp[1,"and_Arten"] <- count_other

count_notint <- WCH %>%
  filter(submissionDate >= prev_year_start & submissionDate <= prev_year_end) %>%
  filter(matches_not_int(resultSpecies)) %>%
  nrow()
probentyp[1,"nicht_interp"] <- count_notint


count_nnb <- WCH %>%
  filter(submissionDate >= prev_year_start & submissionDate <= prev_year_end) %>%
  filter(matches_nnb(resultSpecies)) %>%
  nrow()
probentyp[1,"nnb"] <- count_nnb

probentyp[1,"Total"] <- rowSums(probentyp[1,6:18])

#assign values of the 1st quartal 
probentyp["1Q","Speichel"] <- nrow(filter(WCH, submissionDate >= Q1 & submissionDate < Q2 
                                          & sampleType == "SALIVA"))
probentyp["1Q","Kot"] <- nrow(filter(WCH, submissionDate >= Q1 & submissionDate < Q2 
                                     & sampleType == "SCAT"))
probentyp["1Q","Urin"] <- nrow(filter(WCH, submissionDate >= Q1 & submissionDate < Q2 
                                      & sampleType == "URINE"))
probentyp["1Q","An"] <- nrow(filter(WCH, submissionDate >= Q1 & submissionDate < Q2 
                                    & sampleType != "URINE" & sampleType != "SCAT" & sampleType != "SALIVA"))
probentyp["1Q","Summe"] <- rowSums(probentyp[3,1:4])


#assign values of the 2nd quartal 
probentyp["2Q","Speichel"] <- nrow(filter(WCH, submissionDate >= Q2 & submissionDate < Q3 
                                          & sampleType=="SALIVA"))
probentyp["2Q","Kot"] <- nrow(filter(WCH, submissionDate>= Q2 & submissionDate < Q3 
                                     & sampleType=="SCAT"))
probentyp["2Q","Urin"] <- nrow(filter(WCH, submissionDate>= Q2 & submissionDate < Q3 
                                      & sampleType=="URINE"))
probentyp["2Q","An"] <- nrow(filter(WCH, submissionDate>= Q2 & submissionDate < Q3 
                                    & sampleType != "URINE" & sampleType != "SCAT" & sampleType != "SALIVA"))
probentyp["2Q","Summe"] <- rowSums(probentyp[4,1:4])

#assign values of the 3rd quartal 
probentyp["3Q","Speichel"] <- nrow(filter(WCH, submissionDate>= Q3 & submissionDate < Q4 
                                          & sampleType=="SALIVA"))
probentyp["3Q","Kot"] <- nrow(filter(WCH, submissionDate>= Q3 & submissionDate < Q4 
                                     & sampleType=="SCAT"))
probentyp["3Q","Urin"] <- nrow(filter(WCH, submissionDate>= Q3 & submissionDate < Q4 
                                      & sampleType=="URINE"))
probentyp["3Q","An"] <- nrow(filter(WCH, submissionDate>= Q3 & submissionDate < Q4 
                                    & sampleType != "URINE" & sampleType != "SCAT" & sampleType != "SALIVA"))
probentyp["3Q","Summe"] <- rowSums(probentyp[5,1:4])

#assign values of the 4th quartal 
probentyp["4Q","Speichel"] <- nrow(filter(WCH, submissionDate>= Q4 & submissionDate < Q_end 
                                          & sampleType=="SALIVA"))
probentyp["4Q","Kot"] <- nrow(filter(WCH, submissionDate>= Q4 & submissionDate < Q_end 
                                     & sampleType=="SCAT"))
probentyp["4Q","Urin"] <- nrow(filter(WCH, submissionDate>= Q4 & submissionDate < Q_end 
                                      & sampleType=="URINE"))
probentyp["4Q","An"] <- nrow(filter(WCH, submissionDate>= Q4 & submissionDate < Q_end 
                                    & sampleType != "URINE" & sampleType != "SCAT" & sampleType != "SALIVA"))
probentyp["4Q","Summe"] <- rowSums(probentyp[6,1:4])

probentyp["Total",] <- colSums(probentyp[3:6,])


#### Table C2 Resultierende Art  ####

unique(WCH$resultSpecies)


#assign values of the 1st quartal 
count_wolf_Q1 <- WCH %>%
  filter(submissionDate >= Q1 & submissionDate <= Q2) %>%
  filter(matches_wolf(resultSpecies)) %>%
  nrow()
probentyp["1Q","Wolf"] <- count_wolf_Q1

count_lynx_Q1 <- WCH %>%
  filter(submissionDate >= Q1 & submissionDate <= Q2) %>%
  filter(matches_lynx(resultSpecies)) %>%
  nrow()
probentyp["1Q","Luchs"] <- count_lynx_Q1

count_bear_Q1 <- WCH %>%
  filter(submissionDate >= Q1 & submissionDate <= Q2) %>%
  filter(matches_bear(resultSpecies)) %>%
  nrow()
probentyp["1Q","Bär"] <- count_bear_Q1

count_goldjackal_Q1 <- WCH %>%
  filter(submissionDate >= Q1 & submissionDate <= Q2) %>%
  filter(matches_gold_jackal(resultSpecies)) %>%
  nrow()
probentyp["1Q","Goldschakal"] <- count_goldjackal_Q1

count_canide_Q1 <- WCH %>%
  filter(submissionDate >= Q1 & submissionDate <= Q2) %>%
  filter(matches_canis_spp(resultSpecies)) %>%
  nrow()
probentyp["1Q","Canide"] <- count_canide_Q1

count_dog_Q1 <- WCH %>%
  filter(submissionDate >= Q1 & submissionDate <= Q2) %>%
  filter(matches_dog(resultSpecies)) %>%
  nrow()
probentyp["1Q","Hund"] <- count_dog_Q1

count_fox_Q1 <- WCH %>%
  filter(submissionDate >= Q1 & submissionDate <= Q2) %>%
  filter(matches_fox(resultSpecies)) %>%
  nrow()
probentyp["1Q","Fuchs"] <- count_fox_Q1

count_carnivora_Q1 <- WCH %>%
  filter(submissionDate >= Q1 & submissionDate <= Q2) %>%
  filter(matches_other_carnivores(resultSpecies)) %>%
  nrow()
probentyp["1Q","Carnivora"] <- count_carnivora_Q1

probentyp["1Q","Mensch"] <- nrow(filter(WCH, submissionDate >= Q1 & submissionDate <= Q2 
                                     & resultSpecies=="homo sapiens"))

count_huftier_Q1 <- WCH %>%
  filter(submissionDate >= Q1 & submissionDate <= Q2) %>%
  filter(matches_huftier(resultSpecies)) %>%
  nrow()
probentyp["1Q","Huftier"] <- count_huftier_Q1

count_other_Q1 <- WCH %>%
  filter(submissionDate >= Q1 & submissionDate <= Q2) %>%
  filter(matches_other_species(resultSpecies)) %>%
  nrow()
probentyp["1Q","and_Arten"] <- count_other_Q1

count_notint_Q1 <- WCH %>%
  filter(submissionDate >= Q1 & submissionDate <= Q2) %>%
  filter(matches_not_int(resultSpecies)) %>%
  nrow()
probentyp["1Q","nicht_interp"] <- count_notint_Q1

count_nnb_Q1 <- WCH %>%
  filter(submissionDate >= Q1 & submissionDate <= Q2) %>%
  filter(matches_nnb(resultSpecies)) %>%
  nrow()
probentyp["1Q","nnb"] <- count_nnb_Q1

probentyp["1Q","Total"] <- rowSums(probentyp[3,6:18])

#assign values of the 2nd quartal 
count_wolf_Q2 <- WCH %>%
  filter(submissionDate >= Q2 & submissionDate <= Q3) %>%
  filter(matches_wolf(resultSpecies)) %>%
  nrow()
probentyp["2Q","Wolf"] <- count_wolf_Q2

count_lynx_Q2 <- WCH %>%
  filter(submissionDate >= Q2 & submissionDate <= Q3) %>%
  filter(matches_lynx(resultSpecies)) %>%
  nrow()
probentyp["2Q","Luchs"] <- count_lynx_Q2

count_bear_Q2 <- WCH %>%
  filter(submissionDate >= Q2 & submissionDate <= Q3) %>%
  filter(matches_bear(resultSpecies)) %>%
  nrow()
probentyp["2Q","Bär"] <- count_bear_Q2

count_goldjackal_Q2 <- WCH %>%
  filter(submissionDate >= Q2 & submissionDate <= Q3) %>%
  filter(matches_gold_jackal(resultSpecies)) %>%
  nrow()
probentyp["2Q","Goldschakal"] <- count_goldjackal_Q2

count_canide_Q2 <- WCH %>%
  filter(submissionDate >= Q2 & submissionDate <= Q3) %>%
  filter(matches_canis_spp(resultSpecies)) %>%
  nrow()
probentyp["2Q","Canide"] <- count_canide_Q2

count_dog_Q2 <- WCH %>%
  filter(submissionDate >= Q2 & submissionDate <= Q3) %>%
  filter(matches_dog(resultSpecies)) %>%
  nrow()
probentyp["2Q","Hund"] <- count_dog_Q2

count_fox_Q2 <- WCH %>%
  filter(submissionDate >= Q2 & submissionDate <= Q3) %>%
  filter(matches_fox(resultSpecies)) %>%
  nrow()
probentyp["2Q","Fuchs"] <- count_fox_Q2

count_carnivora_Q2 <- WCH %>%
  filter(submissionDate >= Q2 & submissionDate <= Q3) %>%
  filter(matches_other_carnivores(resultSpecies)) %>%
  nrow()
probentyp["2Q","Carnivora"] <- count_carnivora_Q2

probentyp["2Q","Mensch"] <- nrow(filter(WCH, submissionDate >= Q2 & submissionDate <= Q3 
                                        & resultSpecies=="homo sapiens"))

count_huftier_Q2 <- WCH %>%
  filter(submissionDate >= Q2 & submissionDate <= Q3) %>%
  filter(matches_huftier(resultSpecies)) %>%
  nrow()
probentyp["2Q","Huftier"] <- count_huftier_Q2

count_other_Q2 <- WCH %>%
  filter(submissionDate >= Q2 & submissionDate <= Q3) %>%
  filter(matches_other_species(resultSpecies)) %>%
  nrow()
probentyp["2Q","and_Arten"] <- count_other_Q2

count_notint_Q2 <- WCH %>%
  filter(submissionDate >= Q2 & submissionDate <= Q3) %>%
  filter(matches_not_int(resultSpecies)) %>%
  nrow()
probentyp["2Q","nicht_interp"] <- count_notint_Q2

count_nnb_Q2 <- WCH %>%
  filter(submissionDate >= Q2 & submissionDate <= Q3) %>%
  filter(matches_nnb(resultSpecies)) %>%
  nrow()

probentyp["2Q","nnb"] <- count_nnb_Q2

probentyp["2Q","Total"] <- rowSums(probentyp[4,6:18])


#assign values of the 3rd quartal 
count_wolf_Q3 <- WCH %>%
  filter(submissionDate >= Q3 & submissionDate <= Q4) %>%
  filter(matches_wolf(resultSpecies)) %>%
  nrow()
probentyp["3Q","Wolf"] <- count_wolf_Q3

count_lynx_Q3 <- WCH %>%
  filter(submissionDate >= Q3 & submissionDate <= Q4) %>%
  filter(matches_lynx(resultSpecies)) %>%
  nrow()
probentyp["3Q","Luchs"] <- count_lynx_Q3

count_bear_Q3 <- WCH %>%
  filter(submissionDate >= Q3 & submissionDate <= Q4) %>%
  filter(matches_bear(resultSpecies)) %>%
  nrow()
probentyp["3Q","Bär"] <- count_bear_Q3

count_goldjackal_Q3 <- WCH %>%
  filter(submissionDate >= Q3 & submissionDate <= Q4) %>%
  filter(matches_gold_jackal(resultSpecies)) %>%
  nrow()
probentyp["3Q","Goldschakal"] <- count_goldjackal_Q3

count_canide_Q3 <- WCH %>%
  filter(submissionDate >= Q3 & submissionDate <= Q4) %>%
  filter(matches_canis_spp(resultSpecies)) %>%
  nrow()
probentyp["3Q","Canide"] <- count_canide_Q3

count_dog_Q3 <- WCH %>%
  filter(submissionDate >= Q3 & submissionDate <= Q4) %>%
  filter(matches_dog(resultSpecies)) %>%
  nrow()
probentyp["3Q","Hund"] <- count_dog_Q3

count_fox_Q3 <- WCH %>%
  filter(submissionDate >= Q3 & submissionDate <= Q4) %>%
  filter(matches_fox(resultSpecies)) %>%
  nrow()
probentyp["3Q","Fuchs"] <- count_fox_Q3

count_carnivora_Q3 <- WCH %>%
  filter(submissionDate >= Q3 & submissionDate <= Q4) %>%
  filter(matches_other_carnivores(resultSpecies)) %>%
  nrow()
probentyp["3Q","Carnivora"] <- count_carnivora_Q3

probentyp["3Q","Mensch"] <- nrow(filter(WCH, submissionDate >= Q3 & submissionDate <= Q4 
                                        & resultSpecies=="homo sapiens"))

count_huftier_Q3 <- WCH %>%
  filter(submissionDate >= Q3 & submissionDate <= Q4) %>%
  filter(matches_huftier(resultSpecies)) %>%
  nrow()
probentyp["3Q","Huftier"] <- count_huftier_Q3

count_other_Q3 <- WCH %>%
  filter(submissionDate >= Q3 & submissionDate <= Q4) %>%
  filter(matches_other_species(resultSpecies)) %>%
  nrow()
probentyp["3Q","and_Arten"] <- count_other_Q3

count_notint_Q3 <- WCH %>%
  filter(submissionDate >= Q3 & submissionDate <= Q4) %>%
  filter(matches_not_int(resultSpecies)) %>%
  nrow()
probentyp["3Q","nicht_interp"] <- count_notint_Q3


count_nnb_Q3 <- WCH %>%
  filter(submissionDate >= Q3 & submissionDate <= Q4) %>%
  filter(matches_nnb(resultSpecies)) %>%
  nrow()
probentyp["3Q","nnb"] <- count_nnb_Q3

probentyp["3Q","Total"] <- rowSums(probentyp[5,6:18])

#assign values of the 4th quartal 
count_wolf_Q4 <- WCH %>%
  filter(submissionDate >= Q4 & submissionDate < Q_end) %>%
  filter(matches_wolf(resultSpecies)) %>%
  nrow()
probentyp["4Q","Wolf"] <- count_wolf_Q4

count_lynx_Q4 <- WCH %>%
  filter(submissionDate >= Q4 & submissionDate < Q_end) %>%
  filter(matches_lynx(resultSpecies)) %>%
  nrow()
probentyp["4Q","Luchs"] <- count_lynx_Q4

count_bear_Q4 <- WCH %>%
  filter(submissionDate >= Q4 & submissionDate < Q_end) %>%
  filter(matches_bear(resultSpecies)) %>%
  nrow()
probentyp["4Q","Bär"] <- count_bear_Q4

count_goldjackal_Q4 <- WCH %>%
  filter(submissionDate >= Q4 & submissionDate < Q_end) %>%
  filter(matches_gold_jackal(resultSpecies)) %>%
  nrow()
probentyp["4Q","Goldschakal"] <- count_goldjackal_Q4

count_canide_Q4 <- WCH %>%
  filter(submissionDate >= Q4 & submissionDate < Q_end) %>%
  filter(matches_canis_spp(resultSpecies)) %>%
  nrow()
probentyp["4Q","Canide"] <- count_canide_Q4

count_dog_Q4 <- WCH %>%
  filter(submissionDate >= Q4 & submissionDate < Q_end) %>%
  filter(matches_dog(resultSpecies)) %>%
  nrow()
probentyp["4Q","Hund"] <- count_dog_Q4

count_fox_Q4 <- WCH %>%
  filter(submissionDate >= Q4 & submissionDate < Q_end) %>%
  filter(matches_fox(resultSpecies)) %>%
  nrow()
probentyp["4Q","Fuchs"] <- count_fox_Q4

count_carnivora_Q4 <- WCH %>%
  filter(submissionDate >= Q4 & submissionDate < Q_end) %>%
  filter(matches_other_carnivores(resultSpecies)) %>%
  nrow()
probentyp["4Q","Carnivora"] <- count_carnivora_Q4

probentyp["4Q","Mensch"] <- nrow(filter(WCH, submissionDate >= Q4 & submissionDate < Q_end 
                                        & resultSpecies=="homo sapiens"))

count_huftier_Q4 <- WCH %>%
  filter(submissionDate >= Q4 & submissionDate < Q_end) %>%
  filter(matches_huftier(resultSpecies)) %>%
  nrow()
probentyp["4Q","Huftier"] <- count_huftier_Q4

count_other_Q4 <- WCH %>%
  filter(submissionDate >= Q4 & submissionDate < Q_end) %>%
  filter(matches_other_species(resultSpecies)) %>%
  nrow()
probentyp["4Q","and_Arten"] <- count_other_Q4

count_notint_Q4 <- WCH %>%
  filter(submissionDate >= Q4 & submissionDate < Q_end) %>%
  filter(matches_not_int(resultSpecies)) %>%
  nrow()
probentyp["4Q","nicht_interp"] <- count_notint_Q4


count_nnb_Q4 <- WCH %>%
  filter(submissionDate >= Q4 & submissionDate < Q_end) %>%
  filter(matches_nnb(resultSpecies)) %>%
  nrow()
probentyp["4Q","nnb"] <- count_nnb_Q4

probentyp["4Q","Total"] <- rowSums(probentyp[6,6:18])

#Total

if(QB_number == "1"){
  probentyp["Total",] <- colSums(probentyp[3:3,])
  probentyp["2Q",] <- 0
  probentyp[probentyp == 0] <- "-"
}

if(QB_number == "2"){
  probentyp["Total",] <- colSums(probentyp[3:4,])
  probentyp["3Q",] <- 0
  probentyp[probentyp == 0] <- "-"
}

if(QB_number == "3"){
  probentyp["Total",] <- colSums(probentyp[3:5,])
  probentyp["4Q",] <- 0
  probentyp[probentyp == 0] <- "-"
}

if(QB_number == "4"){
  probentyp["Total",] <- colSums(probentyp[3:6,])
  probentyp[probentyp == 0] <- "-"
}

probentyp[is.na(probentyp)] <- ""

write.csv(probentyp, paste0(year_current,"_QB",QB_number,"_sample_type.csv"))
write.xlsx(probentyp, paste0(year_current,"_QB",QB_number,"_sample_type.xlsx"))


#### Sample received and sent per week ####

sample_received <- WCH %>% 
  filter(receiptDate >= Q1 & receiptDate < Q_end_tot) %>% 
  mutate(wk_rec = isoweek(receiptDate)) %>% 
  group_by(wk_rec) %>% 
  summarise(total_received = n()) %>% 
  mutate(cum_sum_received = cumsum(total_received))
  
sample_submit <- WCH %>% 
  filter(submissionDate >= Q1 & submissionDate < Q_end_tot) %>% 
  mutate(wk_sub = isoweek(submissionDate)) %>% 
  group_by(wk_sub) %>% 
  summarise(total_submit = n())

quota <-  data.frame(week = c(1:52), cont = c(80, 0)) 
quota[51:52,2] <- 0
quota <- quota %>% 
  mutate(quot = cumsum(cont))

data <- left_join(quota, sample_received, by = c("week" = "wk_rec"))
data <- left_join(data, sample_submit, by = c("week"= "wk_sub"))

if(QB_number == "1"){
data[1:13,][is.na(data[1:13,])] <- 0 
}
if(QB_number == "2") {
  data[1:26,][is.na(data[1:26,])] <- 0
}
if(QB_number == "3") {
  data[1:39,][is.na(data[1:39,])] <- 0
}
if(QB_number == "4") {
  data[1:52,][is.na(data[1:52,])] <- 0
}

data <- data %>% 
  mutate(cum_sum_submit = cumsum(total_submit)) %>% 
  select(week,quot, cum_sum_received, cum_sum_submit)

data_graph <- data %>% 
  pivot_longer(cols = c("quot", "cum_sum_received", "cum_sum_submit"), names_to = "type", values_to = "numbers")
data_graph$type <- as.factor(data_graph$type)
if(QB_number == "1"){
  data_ends <- data_graph %>% filter(week == 13) %>% filter(type != "Kontingent")
}
if(QB_number == "2"){
  data_ends <- data_graph %>% filter(week == 26) %>% filter(type != "Kontingent")
}
if(QB_number == "3"){
  data_ends <- data_graph %>% filter(week == 39) %>% filter(type != "Kontingent")
}
if(QB_number == "4"){
  data_ends <- data_graph %>% filter(week == 52) %>% filter(type != "Kontingent")
}

levels(data_graph$type) <- c("Erhalten", "Kontingent", "Eingesandt")

week_samples <- data_graph %>%
  ggplot(aes(x = week, 
             y = numbers, 
             group = rev(type))) +
  geom_line(aes(linetype = rev(type), 
                color =rev(type), 
                size =rev(type))) +
  scale_x_continuous(breaks = seq(1,52, by = 2)) +
  scale_y_continuous(breaks = seq(0,2000, by = 400)) +
  scale_color_manual(values = c("#434AA5", "#8F1923","grey"), 
                     breaks = c("Erhalten", "Eingesandt", "Kontingent")) +
  scale_size_manual(values = c(1.7,1.7, 1), 
                    breaks = c("Erhalten", "Eingesandt", "Kontingent")) +
  scale_linetype_manual(values = c("solid", "solid", "solid"), 
                        breaks = c("Erhalten", "Eingesandt", "Kontingent")) +
  labs(x= "Kalenderwochen",
       y = "",
       title = "") +
  geom_text_repel(aes(label = numbers), 
                  data = data_ends,
                  box.padding = 1,
                  nudge_x = 1
                  ) +
  theme_bw()+
  theme(legend.position = c(0.8, 0.2),
        legend.title = element_blank(),
        legend.background = element_rect(size=0.3, linetype="solid",
                                         colour ="darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "darkgrey"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"));week_samples

ggsave(paste0(year_current,"_QB",QB_number,"_samples_per_week.jpg"), week_samples)
 
