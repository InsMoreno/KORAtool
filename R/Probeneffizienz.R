##################################################################
#### This script allows to have both "Probeneffizienz" plots  ####
####.                 Created by Inès Moreno                  ####
####     Last edited on the ... by Inès Moreno         ####
##################################################################


##### Preparation  ####
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
getwd()

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

## Things you can change:
BioYear <- "2023"

#Import KORA logo 
get_png <- function(filename) {
  grid::rasterGrob(png::readPNG(filename), interpolate = TRUE)
}
path_KORA_logo <- file.path("..","..","..","..","..","01_KORA/KORA Logo (Jacques Rime)/neu_2022_07/DIGITAL/LOGO_KORA_RGB_MC.png")

l <- get_png(path_KORA_logo)
t <- grid::roundrectGrob()


Koralogo <- ggplot(mapping = aes(x = 0:1, y = 1)) +
  theme_void() +
  annotation_custom(l, xmin = .8, xmax = 1)


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

#Import kinship table
query <- paste("SELECT * FROM WolfKinship ;", sep= "")
rs = dbSendQuery(DB,query)
Kinship<-dbFetch(rs)

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

# add the "non interpretable" results
M_samples_genObsEvent <- M_samples_genObsEvent %>%
  mutate(resultIndividual = if_else(((is.na(resultIndividual)) | resultIndividual %in% c("")) & 
                                      (grepl("^Canis lupus", resultSpecies) | grepl("^Canis spp", resultSpecies) | resultSpecies %in% c("Wolf", "Canis lupus")), 
                                    resultNGS, resultIndividual))

## Keep only the columns that interest us
Wolf_genetics <-M_samples_genObsEvent%>%
  dplyr::select(keyOfficial,key,
                canton,municipality, fieldName,
                quality, type.x,
                Obs_date,received,sent, dateSpecies,resultSpecies,dateIndividual,resultIndividual,
                yob,
                name, observer, observerType, copyright,
                origin, KillSpecies, priority,
                x,y, toBeSent, scalp,
                propagationFront, harmfulParent, suspectedHybrid, socialStatus, eventId,
                compartmentMain)

Wolf_genetics$resultIndividual <-  gsub("PROFILE_PARTIALLY", "", Wolf_genetics$resultIndividual)
Wolf_genetics$resultIndividual <- ifelse(Wolf_genetics$resultIndividual %in% c("not interpretable","NOT_INTERPRETABLE"),
                                         "nicht interpretierbar", Wolf_genetics$resultIndividual 
                                         )
Wolf_genetics$resultIndividual <-  gsub("NOT_INTERPRETABLE", "", Wolf_genetics$resultIndividual)

Wolf_genetics <- Wolf_genetics %>%
  mutate(across(c("Obs_date","received", "sent", "dateSpecies","dateIndividual"), ~as.Date(., format="%Y-%m-%d %H:%M:%S")))
# Convert to lowercase
Wolf_genetics$resultSpecies <- tolower(Wolf_genetics$resultSpecies)
Wolf_genetics$resultSpecies <- gsub("_", " ", Wolf_genetics$resultSpecies)
uniquespecies <- sort(unique(Wolf_genetics$resultSpecies))


#### Data to obtain percentage of success in the LBC analyses each year ####
#Keep only samples that were sent for analyses to the LBC
submitted <- Wolf_genetics %>%
  filter(!is.na(sent))
#Extract the year from the "submissionDate" column and create a new column:
submitted$sent <- as.Date(submitted$sent)
submitted$year <- format(submitted$sent, "%Y")

# create a column with 1 if the species result succeeded  and a 0 if not
submitted$Art_result <- ifelse(
  submitted$resultSpecies %in% c("no DNA", "not interpretable",  NA),
  0,
  1)
# create a column with 1 if the species result is Wolf and a 0 if not
submitted$Art_Wolf <- ifelse(grepl("lupus", submitted$resultSpecies, ignore.case = TRUE) | 
                               submitted$resultSpecies %in% c("Wolf", "Canis lupus") , 1, 0)


# create a column with 1 if an individual was identified and a 0 if not
submitted$Individual <- ifelse(submitted$resultIndividual %in% c("nicht interpretierbar", "",
                                                      "canis familiaris", "CANIS_FAMILIARIS",NA),
                               0,1)

# the number of samples that have a 1 in the column Art_result on the total number of samples (rows) per year.
percentage_Art_result <- aggregate(Art_result ~ year, submitted, function(x) sum(x == 1) / length(x) * 100)

#the number of samples that have a 1 in the column Art_Wolf on the total number of samples (rows) per year.
percentage_Art_Wolf <- aggregate(Art_Wolf ~ year, submitted, function(x) sum(x == 1) / length(x) * 100)

#the number of samples that have a 1 in the column Individual on the total number of samples (rows) per year.
percentage_Individual <- aggregate(Individual ~ year, submitted, function(x) sum(x == 1) / length(x) * 100)

#the number of samples that have a 1 in the column Individual on the  number of samples that have a 1 in the column Art_Wolf per year.
subset_data <- subset(submitted, Art_Wolf == 1)
percentage_Individual_Wolf <- aggregate(Individual ~ year, subset_data, function(x) sum(x == 1) / length(x) * 100)

# Calculate the total number of samples per year
total_samples <- aggregate(Art_result ~ year, submitted, function(x) length(x))

# Rename the column to "total_samples"
colnames(total_samples)[2] <- "total_samples"

#Combine the percentage data into a single dataframe:
combined_data <- merge(percentage_Art_result, percentage_Art_Wolf, by = "year", all = TRUE)
combined_data <- merge(combined_data, percentage_Individual, by = "year", all = TRUE)
combined_data <- merge(combined_data, percentage_Individual_Wolf, by = "year", all = TRUE)
combined_data <- merge(combined_data, total_samples, by = "year", all = TRUE)

# Reshape the data into long format
combined_data_long <- gather(combined_data, column, percentage, -year, -total_samples, na.rm = TRUE)

# Calculate the mean percentage for each category
mean_percentage_Art_result <- mean(percentage_Art_result$Art_result)
mean_percentage_Art_Wolf <- mean(percentage_Art_Wolf$Art_Wolf)
mean_percentage_Individual <- mean(percentage_Individual$Individual)
mean_percentage_Individual_Wolf <- mean(percentage_Individual_Wolf$Individual)
sum_nb_samples <- sum(total_samples$total_samples)

# Create the text caption
caption <- paste("\nMoyenne par catégorie (n=",sum_nb_samples,"):",
                 "\n - Résultats d'espèces:", sprintf("%.1f%%", mean_percentage_Art_result),
                 "\n - Résultats d'espèces = loup:", sprintf("%.1f%%", mean_percentage_Art_Wolf),
                 "\n - Résultats d'individus:", sprintf("%.1f%%", mean_percentage_Individual),
                 "\n - Résultats loups qui ont donné l'individu:", sprintf("%.1f%%", mean_percentage_Individual_Wolf))

# Plot the data
Probeneffizienz <- ggplot(combined_data_long, aes(x = year, 
                                                  y = percentage, 
                                                  group = column, 
                                                  color = column, 
                                                  shape = column)) +
  geom_line(size=1) +
  geom_point(size=2) +
  geom_text(aes(label = paste0(sprintf("%.0f", percentage), "%")), 
            vjust = -1, color = "black", size=3) +
  scale_x_discrete(labels = paste0(combined_data_long$year, 
                                   "\n (n=", combined_data_long$total_samples, ")")) +
  scale_y_continuous(labels = function(x) paste0(x, "%"))+
  scale_color_manual(values = c("#FFB6C1","#F9E79F","#85C1E9","#D2B4DE"),
                     labels=c("% d'échantillons qui ont donné un résultat d'espèce avec succès",
                              "% d'échantillons qui ont donné un résultat d'espèce = loup",
                              "% d'échantillons qui ont donné un résultat d'individu",
                              "% d'échantillons ayant comme résultat d'espèce 'loup' \nqui ont été identifié au niveau de l'individu")) +
  scale_shape_manual(values = c(16,15,17,18),
                     labels=c("% d'échantillons qui ont donné un résultat d'espèce avec succès",
                              "% d'échantillons qui ont donné un résultat d'espèce = loup",
                              "% d'échantillons qui ont donné un résultat d'individu",
                              "% d'échantillons ayant comme résultat d'espèce 'loup' \nqui ont été identifié au niveau de l'individu")) +
  xlab("Année où l'échantillon a été envoyé au LBC pour analyses")+
  ylab("")+
  # labs(color = "", shape = "")+
  ggtitle("Entwicklung der Ergebnisse analysierter Proben") +
  labs(subtitle = paste("Etat le", format(Sys.Date(),"%d.%m.%Y")),
       caption = caption
  )+
  theme_bw()+
  theme(
    legend.position = c(0.99,0.15),
    legend.box = "vertical",
    legend.direction = "vertical",  # Arrange legend items vertically
    legend.justification = "right",  # Position the legend at the bottom left
    legend.title=element_blank(),
    strip.text = element_text(face = 'bold'),
    axis.text.x = element_text(size=8,
                               margin = margin(t = 0, r = 0, b = 5, l = 0)
                               # angle = 15, 
                               #hjust = 1
    ),
    axis.text.y=element_text(size=9),
    plot.title=element_text(size=20, hjust=0.5),
    axis.title=element_text(size=10),
    plot.caption = element_text(size=10, hjust=0,margin = margin(t = 30, r = 0, b = 0, l = 0) 
                                #face = "italic"
    ),
    plot.subtitle = element_text(size=7, color = "darkgrey", face = "italic"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.background = element_rect(fill="#FBFCFC",
                                     linewidth=0.5, linetype="solid", 
                                     colour ="darkgrey")) ; Probeneffizienz

#add the KORA logo
ProbeneffizienzKORA <- gridExtra::grid.arrange(Probeneffizienz, Koralogo, heights = c(.93, .07))

#save the plot in jpg format
ggsave(paste0("Probeneffizienz_Jahr_", Sys.Date(), ".jpg"), ProbeneffizienzKORA, width = 13, height = 8, dpi = 500)

#DE
# Create the text caption
caption_DE <- paste("\nDurchschnitt pro Probetyp (n=",sum_nb_samples,"):",
                 "\n - Artresultate:", sprintf("%.1f%%", mean_percentage_Art_result),
                 "\n - Artresultat « Wolf »:", sprintf("%.1f%%", mean_percentage_Art_Wolf),
                 "\n - Individualresultat:", sprintf("%.1f%%", mean_percentage_Individual),
                 "\n - Artresultat « Wolf » und Individualresultat:", sprintf("%.1f%%", mean_percentage_Individual_Wolf))

color <- c("#F7DC6F","#7ED67D","#64B5F6","#C385CF")

# Plot the data
Probeneffizienz_DE <- ggplot(combined_data_long, aes(x = year, 
                                                  y = percentage, 
                                                  group = column, 
                                                  color = column, 
                                                  shape = column)) +
  geom_line(size=1) +
  geom_point(size=2) +
  geom_text(aes(label = paste0(sprintf("%.0f", percentage), "%")), 
            vjust = -1, color = "black", size=5) +
  scale_x_discrete(labels = paste0(combined_data_long$year, 
                                   "\n (n=", combined_data_long$total_samples, ")")) +
  scale_y_continuous(labels = function(x) paste0(x, "%"))+
  scale_color_manual(values = color,
                     labels=c("% der Proben die ein Artresultat ergaben",
                              "% der Proben mit einem Artresultat « Wolf »",
                              "% der Proben mit einem Individualresultat",
                              "% der Proben mit einem Artresultat « Wolf »\n und mit einem Individualresultat")) +
  scale_shape_manual(values = c(16,15,17,18),
                     labels=c("% der Proben die ein Artresultat ergaben",
                              "% der Proben mit einem Artresultat « Wolf »",
                              "% der Proben mit einem Individualresultat",
                              "% der Proben mit einem Artresultat « Wolf »\n und mit einem Individualresultat")) +
  xlab("Jahr, in dem die Probe zur Analyse an das LBC geschickt wurde")+
  ylab("")+
  # labs(color = "", shape = "")+
  ggtitle("Entwicklung der Ergebnisse analysierter Proben") +
  labs(subtitle = paste("Stand am", format(Sys.Date(),"%d.%m.%Y")),
       caption = caption_DE
  )+
  theme_bw()+
  theme(
    legend.position = c(0.99,0.15),
    legend.box = "vertical",
    legend.direction = "vertical",  # Arrange legend items vertically
    legend.justification = "right",  # Position the legend at the bottom left
    legend.title=element_blank(),
    legend.text = element_text(size=12),
    strip.text = element_text(face = 'bold'),
    axis.text.x = element_text(size=12,
                               margin = margin(t = 0, r = 0, b = 5, l = 0)
                               # angle = 15, 
                               #hjust = 1
    ),
    axis.text.y=element_text(size=12),
    plot.title=element_text(size=24, hjust=0.5),
    axis.title=element_text(size=12),
    plot.caption = element_text(size=12, hjust=0,margin = margin(t = 30, r = 0, b = 0, l = 0) 
                                #face = "italic"
    ),
    plot.subtitle = element_text(size=10, colour = "darkgrey", face = "italic"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.background = element_rect(fill="#FBFCFC",
                                     linewidth=0.5, linetype="solid", 
                                     colour ="darkgrey")) ; Probeneffizienz_DE

#add the KORA logo
ProbeneffizienzKORA_DE <- gridExtra::grid.arrange(Probeneffizienz_DE, Koralogo, heights = c(.93, .07))

#save the plot in jpg format
ggsave(paste0("DE_Probeneffizienz_Jahr_", Sys.Date(), ".jpg"), ProbeneffizienzKORA_DE, width = 13, height = 8, dpi = 500)


#####-- Calculate percentages for each sample type and column ####
submitted$type.x <- tolower(submitted$type.x)
submitted <- submitted %>%
  dplyr::rename(sampleType = type.x)
result <- submitted %>%
  filter(sampleType %in% c("tissue", "saliva", "urine", "scat","hair", "other", "blood")) %>%
  group_by(sampleType) %>%
  summarize(
    saliva_art_result_percentage = sum(Art_result[sampleType == "saliva"] == 1) / sum(sampleType == "saliva") * 100,
    saliva_art_wolf_percentage = sum(Art_Wolf[sampleType == "saliva"] == 1) / sum(sampleType == "saliva") * 100,
    saliva_individual_percentage = sum(Individual[sampleType == "saliva"] == 1) / sum(sampleType == "saliva") * 100,
    saliva_individual_art_wolf_ratio = sum(Individual[sampleType == "saliva"] == 1 & Art_Wolf[sampleType == "saliva"] == 1) /
      sum(Art_Wolf[sampleType == "saliva"] == 1) * 100,
    scat_art_result_percentage = sum(Art_result[sampleType == "scat"] == 1) / sum(sampleType == "scat") * 100,
    scat_art_wolf_percentage = sum(Art_Wolf[sampleType == "scat"] == 1) / sum(sampleType == "scat") * 100,
    scat_individual_percentage = sum(Individual[sampleType == "scat"] == 1) / sum(sampleType == "scat") * 100,
    scat_individual_art_wolf_ratio = sum(Individual[sampleType == "scat"] == 1 & Art_Wolf[sampleType == "scat"] == 1) /
      sum(Art_Wolf[sampleType == "scat"] == 1) * 100,
    urine_art_result_percentage = sum(Art_result[sampleType == "urine"] == 1) / sum(sampleType == "urine") * 100,
    urine_art_wolf_percentage = sum(Art_Wolf[sampleType == "urine"] == 1) / sum(sampleType == "urine") * 100,
    urine_individual_percentage = sum(Individual[sampleType == "urine"] == 1) / sum(sampleType == "urine") * 100,
    urine_individual_art_wolf_ratio = sum(Individual[sampleType == "urine"] == 1 & Art_Wolf[sampleType == "urine"] == 1) /
      sum(Art_Wolf[sampleType == "urine"] == 1) * 100,
    hair_art_result_percentage = sum(Art_result[sampleType == "hair"] == 1) / sum(sampleType == "hair") * 100,
    hair_art_wolf_percentage = sum(Art_Wolf[sampleType == "hair"] == 1) / sum(sampleType == "hair") * 100,
    hair_individual_percentage = sum(Individual[sampleType == "hair"] == 1) / sum(sampleType == "hair") * 100,
    hair_individual_art_wolf_ratio = sum(Individual[sampleType == "hair"] == 1 & Art_Wolf[sampleType == "hair"] == 1) /
      sum(Art_Wolf[sampleType == "hair"] == 1) * 100,
    blood_art_result_percentage = sum(Art_result[sampleType == "blood"] == 1) / sum(sampleType == "blood") * 100,
    blood_art_wolf_percentage = sum(Art_Wolf[sampleType == "blood"] == 1) / sum(sampleType == "blood") * 100,
    blood_individual_percentage = sum(Individual[sampleType == "blood"] == 1) / sum(sampleType == "blood") * 100,
    blood_individual_art_wolf_ratio = sum(Individual[sampleType == "blood"] == 1 & Art_Wolf[sampleType == "blood"] == 1) /
      sum(Art_Wolf[sampleType == "blood"] == 1) * 100,
    other_art_result_percentage = sum(Art_result[sampleType == "other"] == 1) / sum(sampleType == "other") * 100,
    other_art_wolf_percentage = sum(Art_Wolf[sampleType == "other"] == 1) / sum(sampleType == "other") * 100,
    other_individual_percentage = sum(Individual[sampleType == "other"] == 1) / sum(sampleType == "other") * 100,
    other_individual_art_wolf_ratio = sum(Individual[sampleType == "other"] == 1 & Art_Wolf[sampleType == "other"] == 1) /
      sum(Art_Wolf[sampleType == "other"] == 1) * 100,
    tissue_art_result_percentage = sum(Art_result[sampleType == "tissue"] == 1) /
      sum(sampleType %in% c("tissue")) * 100,
    tissue_art_wolf_percentage = sum(Art_Wolf[sampleType == "tissue"] == 1) /
      sum(sampleType %in% c("tissue")) * 100,
    tissue_individual_percentage = sum(Individual[sampleType == "tissue"] == 1) /
      sum(sampleType %in% c("tissue")) * 100,
    tissue_individual_art_wolf_ratio = sum(Individual[sampleType == "tissue"] == 1 &
                                                    Art_Wolf[sampleType == "tissue"] == 1) /
      sum(Art_Wolf[sampleType == "tissue"] == 1) * 100
  )

# Reshape the data into a long format and remove rows with NA values
long_data <- result %>%
  gather(Column, Percentage, -sampleType) %>%
  na.omit() %>%
  gather(Column, Percentage, starts_with("saliva_"), starts_with("scat_"), 
         starts_with("urine_"),starts_with("hair_"), starts_with("blood_"),
         starts_with("other_"), starts_with("tissue_")) %>%
  mutate(Column = sub("saliva_", "", Column),
         Column = sub("scat_", "", Column),
         Column = sub("urine_", "", Column),
         Column = sub("hair_", "", Column),
         Column = sub("blood_", "", Column),
         Column = sub("other_", "", Column),
         Column = sub("tissue_", "", Column))
#force the order for the plot 
long_data$Column <- factor(long_data$Column, levels = c("art_result_percentage",
                                                        "art_wolf_percentage",
                                                        "individual_percentage",
                                                        "individual_art_wolf_ratio"))
#Obtain the total number of each sample type
sum_saliva <- sum(submitted$sampleType == "saliva")
sum_scat <- sum(submitted$sampleType == "scat")
sum_urine <- sum(submitted$sampleType == "urine")
sum_hair <- sum(submitted$sampleType == "hair")
sum_blood <- sum(submitted$sampleType == "blood")
sum_other <- sum(submitted$sampleType == "other")
sum_tissue <- sum(submitted$sampleType == "tissue")

color <- c("#F7DC6F","#7ED67D","#64B5F6","#C385CF")

# Create the barplot
perType <- ggplot(long_data, aes(x = factor(sampleType), y = Percentage, fill = Column, colour= Column)) +
  geom_bar(stat = "identity", position = "dodge", alpha=0.5) +
  labs(x = "", y = "") +
  scale_fill_manual(values = color,
                    #c("#FFB6C1","#F9E79F","#85C1E9","#D2B4DE"),
                    labels=c("% d'échantillons qui ont donné un résultat d'espèce avec succès",
                             "% d'échantillons qui ont donné un résultat d'espèce = loup",
                             "% d'échantillons qui ont donné un résultat d'individu",
                             "% d'échantillons ayant comme résultat d'espèce 'loup' qui ont été identifié au niveau de l'individu")) +
  scale_colour_manual(values = color,
                      #c("#FFB6C1","#F9E79F","#85C1E9","#D2B4DE"),
                      labels=c("% d'échantillons qui ont donné un résultat d'espèce avec succès",
                               "% d'échantillons qui ont donné un résultat d'espèce = loup",
                               "% d'échantillons qui ont donné un résultat d'individu",
                               "% d'échantillons ayant comme résultat d'espèce 'loup' qui ont été identifié au niveau de l'individu")) +
  
  scale_y_continuous(labels = function(x) paste0(x, "%"))+
  scale_x_discrete(limits = c("tissue","saliva","scat","urine","hair", "blood","other" ),
                   labels= c(paste("Tissus \n( n=",sum_tissue,")"),
                             paste("Salives \n( n=",sum_saliva,")"),
                             paste("Crottes\n( n=",sum_scat,")"),
                             paste("Urines\n( n=",sum_urine,")"),
                             paste("Poils \n( n=",sum_hair,")"),
                             paste("Sang\n( n=",sum_blood,")"),
                             paste("Autres\n( n=",sum_other,")")
                             
  ))+
  geom_text(aes(label = sprintf("%.1f%%", Percentage)),
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size=3, col="black") +
  ggtitle("Efficience par type d'échantillon") +
  labs(subtitle = paste("Etat le", format(Sys.Date(),"%d.%m.%Y")),
       # caption = caption2
  )+
  theme_bw()+
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.direction = "vertical",  # Arrange legend items vertically
    legend.justification = "left",  # Position the legend at the bottom left
    legend.title=element_blank(),
    strip.text = element_text(face = 'bold'),
    axis.text.x = element_text(size=8,
                               # angle = 15, 
                               #hjust = 1
    ),
    axis.text.y=element_text(size=8),
    plot.title=element_text(size=20, hjust=0.5),
    axis.title=element_text(size=10),
    plot.caption = element_text(size=10, hjust=0, 
                                #face = "italic"
    ),
    plot.subtitle = element_text(size=7, color = "darkgrey", face = "italic"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
    # legend.background = element_rect(fill="#FBFCFC",
    #                                  linewidth=0.5, linetype="solid", 
    #                                  colour ="darkgrey")
  );perType

#add the KORA logo
perTypeKORA <- gridExtra::grid.arrange(perType, Koralogo, heights = c(.93, .07))

#Save the plot in jpg format
ggsave(paste0("Probeneffizienz_SampleType_", Sys.Date(), ".jpg"), perTypeKORA, width = 13, height = 8, dpi = 500)


#DE
# Create the barplot
perType_DE <- ggplot(long_data, aes(x = factor(sampleType), y = Percentage, fill = Column, colour= Column)) +
  geom_bar(stat = "identity", position = "dodge", alpha=0.5) +
  labs(x = "", y = "") +
  scale_fill_manual(values = color,
                    #c("#FFB6C1","#F9E79F","#85C1E9","#D2B4DE"),
                    labels=c("% der Proben die ein Artresultat ergaben",
                             "% der Proben mit einem Artresultat « Wolf »",
                             "% der Proben mit einem Individualresultat",
                             "% der Proben mit einem Artresultat « Wolf » und mit einem Individualresultat")) +
  scale_colour_manual(values = color,
                      #c("#FFB6C1","#F9E79F","#85C1E9","#D2B4DE"),
                      labels=c("% der Proben die ein Artresultat ergaben",
                               "% der Proben mit einem Artresultat « Wolf »",
                               "% der Proben mit einem Individualresultat",
                               "% der Proben mit einem Artresultat « Wolf » und mit einem Individualresultat")) +
  
  scale_y_continuous(labels = function(x) paste0(x, "%"))+
  scale_x_discrete(limits = c("tissue","saliva","scat","urine","hair", "blood","other" ),
                   labels= c(paste("Gewebe\n( n=",sum_tissue,")"),
                             paste("Speichel\n( n=",sum_saliva,")"),
                             paste("Kot\n( n=",sum_scat,")"),
                             paste("Urin\n( n=",sum_urine,")"),
                             paste("Haare\n( n=",sum_hair,")"),
                             paste("Blut\n( n=",sum_blood,")"),
                             paste("Andere\n( n=",sum_other,")")
                             
                   ))+
  geom_text(aes(label = sprintf("%.1f%%", Percentage)),
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size=3, col="black") +
  ggtitle("Effizienz nach Probentyp") +
  labs(subtitle = paste("Stand am", format(Sys.Date(),"%d.%m.%Y")),
       # caption = caption2
  )+
  theme_bw()+
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.direction = "vertical",  # Arrange legend items vertically
    legend.justification = "left",  # Position the legend at the bottom left
    legend.title=element_blank(),
    strip.text = element_text(face = 'bold'),
    axis.text.x = element_text(size=8,
                               # angle = 15, 
                               #hjust = 1
    ),
    axis.text.y=element_text(size=8),
    plot.title=element_text(size=20, hjust=0.5),
    axis.title=element_text(size=10),
    plot.caption = element_text(size=10, hjust=0, 
                                #face = "italic"
    ),
    plot.subtitle = element_text(size=7, color = "darkgrey", face = "italic"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
    # legend.background = element_rect(fill="#FBFCFC",
    #                                  linewidth=0.5, linetype="solid", 
    #                                  colour ="darkgrey")
  );perType_DE

#add the KORA logo
perTypeKORA <- gridExtra::grid.arrange(perType_DE, Koralogo, heights = c(.93, .07))

#Save the plot in jpg format
ggsave(paste0("DE_Probeneffizienz_SampleType_", Sys.Date(), ".jpg"), perTypeKORA_DE, width = 13, height = 8, dpi = 500)


# filter by canton of interest:
canton_tofilter <-  "VS"

submitted$type.x <- tolower(submitted$type.x)
submitted <- submitted %>%
  dplyr::rename(sampleType = type.x)

submitted_canton <- submitted %>%
  filter(canton == canton_tofilter)

result_canton <- submitted_canton %>%
  filter(sampleType %in% c("tissue", "saliva", "urine", "scat","hair", "other", "blood")) %>%
  group_by(sampleType) %>%
  summarize(
    saliva_art_result_percentage = sum(Art_result[sampleType == "saliva"] == 1) / sum(sampleType == "saliva") * 100,
    saliva_art_wolf_percentage = sum(Art_Wolf[sampleType == "saliva"] == 1) / sum(sampleType == "saliva") * 100,
    saliva_individual_percentage = sum(Individual[sampleType == "saliva"] == 1) / sum(sampleType == "saliva") * 100,
    saliva_individual_art_wolf_ratio = sum(Individual[sampleType == "saliva"] == 1 & Art_Wolf[sampleType == "saliva"] == 1) /
      sum(Art_Wolf[sampleType == "saliva"] == 1) * 100,
    scat_art_result_percentage = sum(Art_result[sampleType == "scat"] == 1) / sum(sampleType == "scat") * 100,
    scat_art_wolf_percentage = sum(Art_Wolf[sampleType == "scat"] == 1) / sum(sampleType == "scat") * 100,
    scat_individual_percentage = sum(Individual[sampleType == "scat"] == 1) / sum(sampleType == "scat") * 100,
    scat_individual_art_wolf_ratio = sum(Individual[sampleType == "scat"] == 1 & Art_Wolf[sampleType == "scat"] == 1) /
      sum(Art_Wolf[sampleType == "scat"] == 1) * 100,
    urine_art_result_percentage = sum(Art_result[sampleType == "urine"] == 1) / sum(sampleType == "urine") * 100,
    urine_art_wolf_percentage = sum(Art_Wolf[sampleType == "urine"] == 1) / sum(sampleType == "urine") * 100,
    urine_individual_percentage = sum(Individual[sampleType == "urine"] == 1) / sum(sampleType == "urine") * 100,
    urine_individual_art_wolf_ratio = sum(Individual[sampleType == "urine"] == 1 & Art_Wolf[sampleType == "urine"] == 1) /
      sum(Art_Wolf[sampleType == "urine"] == 1) * 100,
    hair_art_result_percentage = sum(Art_result[sampleType == "hair"] == 1) / sum(sampleType == "hair") * 100,
    hair_art_wolf_percentage = sum(Art_Wolf[sampleType == "hair"] == 1) / sum(sampleType == "hair") * 100,
    hair_individual_percentage = sum(Individual[sampleType == "hair"] == 1) / sum(sampleType == "hair") * 100,
    hair_individual_art_wolf_ratio = sum(Individual[sampleType == "hair"] == 1 & Art_Wolf[sampleType == "hair"] == 1) /
      sum(Art_Wolf[sampleType == "hair"] == 1) * 100,
    blood_art_result_percentage = sum(Art_result[sampleType == "blood"] == 1) / sum(sampleType == "blood") * 100,
    blood_art_wolf_percentage = sum(Art_Wolf[sampleType == "blood"] == 1) / sum(sampleType == "blood") * 100,
    blood_individual_percentage = sum(Individual[sampleType == "blood"] == 1) / sum(sampleType == "blood") * 100,
    blood_individual_art_wolf_ratio = sum(Individual[sampleType == "blood"] == 1 & Art_Wolf[sampleType == "blood"] == 1) /
      sum(Art_Wolf[sampleType == "blood"] == 1) * 100,
    other_art_result_percentage = sum(Art_result[sampleType == "other"] == 1) / sum(sampleType == "other") * 100,
    other_art_wolf_percentage = sum(Art_Wolf[sampleType == "other"] == 1) / sum(sampleType == "other") * 100,
    other_individual_percentage = sum(Individual[sampleType == "other"] == 1) / sum(sampleType == "other") * 100,
    other_individual_art_wolf_ratio = sum(Individual[sampleType == "other"] == 1 & Art_Wolf[sampleType == "other"] == 1) /
      sum(Art_Wolf[sampleType == "other"] == 1) * 100,
    tissue_art_result_percentage = sum(Art_result[sampleType == "tissue"] == 1) /
      sum(sampleType %in% c("tissue")) * 100,
    tissue_art_wolf_percentage = sum(Art_Wolf[sampleType == "tissue"] == 1) /
      sum(sampleType %in% c("tissue")) * 100,
    tissue_individual_percentage = sum(Individual[sampleType == "tissue"] == 1) /
      sum(sampleType %in% c("tissue")) * 100,
    tissue_individual_art_wolf_ratio = sum(Individual[sampleType == "tissue"] == 1 &
                                             Art_Wolf[sampleType == "tissue"] == 1) /
      sum(Art_Wolf[sampleType == "tissue"] == 1) * 100
  )

# Reshape the data into a long format and remove rows with NA values
long_data_canton <- result_canton %>%
  gather(Column, Percentage, -sampleType) %>%
  na.omit() %>%
  gather(Column, Percentage, starts_with("saliva_"), starts_with("scat_"), 
         starts_with("urine_"),starts_with("hair_"), starts_with("blood_"),
         starts_with("other_"), starts_with("tissue_")) %>%
  mutate(Column = sub("saliva_", "", Column),
         Column = sub("scat_", "", Column),
         Column = sub("urine_", "", Column),
         Column = sub("hair_", "", Column),
         Column = sub("blood_", "", Column),
         Column = sub("other_", "", Column),
         Column = sub("tissue_", "", Column))
#force the order for the plot 
long_data_canton$Column <- factor(long_data_canton$Column, levels = c("art_result_percentage",
                                                        "art_wolf_percentage",
                                                        "individual_percentage",
                                                        "individual_art_wolf_ratio"))
#Obtain the total number of each sample type
sum_saliva_canton <- sum(submitted_canton$sampleType == "saliva")
sum_scat_canton <- sum(submitted_canton$sampleType == "scat")
sum_urine_canton <- sum(submitted_canton$sampleType == "urine")
sum_hair_canton <- sum(submitted_canton$sampleType == "hair")
sum_blood_canton <- sum(submitted_canton$sampleType == "blood")
sum_other_canton <- sum(submitted_canton$sampleType == "other")
sum_tissue_canton <- sum(submitted_canton$sampleType == "tissue")

color <- c("#F7DC6F","#7ED67D","#64B5F6","#C385CF")

# Create the barplot
perType_canton_FR <- ggplot(long_data_canton, aes(x = factor(sampleType), y = Percentage, fill = Column, colour= Column)) +
  geom_bar(stat = "identity", position = "dodge", alpha=0.5) +
  labs(x = "", y = "") +
  scale_fill_manual(values = color,
                    #c("#FFB6C1","#F9E79F","#85C1E9","#D2B4DE"),
                    labels=c("% d'échantillons qui ont donné un résultat d'espèce avec succès",
                             "% d'échantillons qui ont donné un résultat d'espèce = loup",
                             "% d'échantillons qui ont donné un résultat d'individu",
                             "% d'échantillons ayant comme résultat d'espèce 'loup' qui ont été identifié au niveau de l'individu")) +
  scale_colour_manual(values = color,
                      #c("#FFB6C1","#F9E79F","#85C1E9","#D2B4DE"),
                      labels=c("% d'échantillons qui ont donné un résultat d'espèce avec succès",
                               "% d'échantillons qui ont donné un résultat d'espèce = loup",
                               "% d'échantillons qui ont donné un résultat d'individu",
                               "% d'échantillons ayant comme résultat d'espèce 'loup' qui ont été identifié au niveau de l'individu")) +
  
  scale_y_continuous(labels = function(x) paste0(x, "%"))+
  scale_x_discrete(limits = c("tissue","saliva","scat","urine","hair", "blood","other" ),
                   labels= c(paste("Tissus \n( n=",sum_tissue_canton,")"),
                             paste("Salives \n( n=",sum_saliva_canton,")"),
                             paste("Crottes\n( n=",sum_scat_canton,")"),
                             paste("Urines\n( n=",sum_urine_canton,")"),
                             paste("Poils \n( n=",sum_hair_canton,")"),
                             paste("Sang\n( n=",sum_blood_canton,")"),
                             paste("Autres\n( n=",sum_other_canton,")")
                             
                   ))+
  geom_text(aes(label = sprintf("%.1f%%", Percentage)),
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size=3, col="black") +
  ggtitle("Efficience par type d'échantillon (VS)") +
  labs(subtitle = paste("Etat le", format(Sys.Date(),"%d.%m.%Y")),
       # caption = caption2
  )+
  theme_bw()+
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.direction = "vertical",  # Arrange legend items vertically
    legend.justification = "left",  # Position the legend at the bottom left
    legend.title=element_blank(),
    strip.text = element_text(face = 'bold'),
    axis.text.x = element_text(size=8,
                               # angle = 15, 
                               #hjust = 1
    ),
    axis.text.y=element_text(size=8),
    plot.title=element_text(size=20, hjust=0.5),
    axis.title=element_text(size=10),
    plot.caption = element_text(size=10, hjust=0, 
                                #face = "italic"
    ),
    plot.subtitle = element_text(size=7, color = "darkgrey", face = "italic"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
    # legend.background = element_rect(fill="#FBFCFC",
    #                                  linewidth=0.5, linetype="solid", 
    #                                  colour ="darkgrey")
  );perType_canton_FR

#pas fini les modifications depuis ici !!!!!!!
#add the KORA logo
perTypeKORA <- gridExtra::grid.arrange(perType, Koralogo, heights = c(.93, .07))

#Save the plot in jpg format
ggsave(paste0("Probeneffizienz_SampleType_", Sys.Date(), ".jpg"), perTypeKORA, width = 13, height = 8, dpi = 500)


#DE
# Create the barplot
perType_DE <- ggplot(long_data, aes(x = factor(sampleType), y = Percentage, fill = Column, colour= Column)) +
  geom_bar(stat = "identity", position = "dodge", alpha=0.5) +
  labs(x = "", y = "") +
  scale_fill_manual(values = color,
                    #c("#FFB6C1","#F9E79F","#85C1E9","#D2B4DE"),
                    labels=c("% der Proben die ein Artresultat ergaben",
                             "% der Proben mit einem Artresultat « Wolf »",
                             "% der Proben mit einem Individualresultat",
                             "% der Proben mit einem Artresultat « Wolf » und mit einem Individualresultat")) +
  scale_colour_manual(values = color,
                      #c("#FFB6C1","#F9E79F","#85C1E9","#D2B4DE"),
                      labels=c("% der Proben die ein Artresultat ergaben",
                               "% der Proben mit einem Artresultat « Wolf »",
                               "% der Proben mit einem Individualresultat",
                               "% der Proben mit einem Artresultat « Wolf » und mit einem Individualresultat")) +
  
  scale_y_continuous(labels = function(x) paste0(x, "%"))+
  scale_x_discrete(limits = c("tissue","saliva","scat","urine","hair", "blood","other" ),
                   labels= c(paste("Gewebe\n( n=",sum_tissue,")"),
                             paste("Speichel\n( n=",sum_saliva,")"),
                             paste("Kot\n( n=",sum_scat,")"),
                             paste("Urin\n( n=",sum_urine,")"),
                             paste("Haare\n( n=",sum_hair,")"),
                             paste("Blut\n( n=",sum_blood,")"),
                             paste("Andere\n( n=",sum_other,")")
                             
                   ))+
  geom_text(aes(label = sprintf("%.1f%%", Percentage)),
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size=3, col="black") +
  ggtitle("Effizienz nach Probentyp") +
  labs(subtitle = paste("Stand am", format(Sys.Date(),"%d.%m.%Y")),
       # caption = caption2
  )+
  theme_bw()+
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.direction = "vertical",  # Arrange legend items vertically
    legend.justification = "left",  # Position the legend at the bottom left
    legend.title=element_blank(),
    strip.text = element_text(face = 'bold'),
    axis.text.x = element_text(size=8,
                               # angle = 15, 
                               #hjust = 1
    ),
    axis.text.y=element_text(size=8),
    plot.title=element_text(size=20, hjust=0.5),
    axis.title=element_text(size=10),
    plot.caption = element_text(size=10, hjust=0, 
                                #face = "italic"
    ),
    plot.subtitle = element_text(size=7, color = "darkgrey", face = "italic"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
    # legend.background = element_rect(fill="#FBFCFC",
    #                                  linewidth=0.5, linetype="solid", 
    #                                  colour ="darkgrey")
  );perType_DE

#add the KORA logo
perTypeKORA <- gridExtra::grid.arrange(perType_DE, Koralogo, heights = c(.93, .07))

#Save the plot in jpg format
ggsave(paste0("DE_Probeneffizienz_SampleType_", Sys.Date(), ".jpg"), perTypeKORA_DE, width = 13, height = 8, dpi = 500)



#Cas VS janvier 2024

#### Data to obtain percentage of success in the LBC analyses each year ####
#Keep only samples that were sent for analyses to the LBC
submitted <- Wolf_genetics %>%
  filter(!is.na(sent))

submitted <- submitted %>%
  dplyr::filter(canton == "VS")
#Extract the year from the "submissionDate" column and create a new column:
submitted$sent <- as.Date(submitted$sent)
submitted$year <- format(submitted$sent, "%Y")
submitted <- submitted %>%
  dplyr::filter(sent < "2024-01-01")
submitted <- submitted %>%
  dplyr::filter(sent >= "2013-01-01")

# create a column with 1 if the species result succeeded  and a 0 if not
submitted$Art_result <- ifelse(
  submitted$resultSpecies %in% c("no DNA", "not interpretable",  NA),
  0,
  1)
# create a column with 1 if the species result is Wolf and a 0 if not
submitted$Art_Wolf <- ifelse(grepl("lupus", submitted$resultSpecies, ignore.case = TRUE) | 
                               submitted$resultSpecies %in% c("Wolf", "Canis lupus") , 1, 0)


# create a column with 1 if an individual was identified and a 0 if not
submitted$Individual <- ifelse(submitted$resultIndividual %in% c("nicht interpretierbar", "",
                                                                 "canis familiaris", "CANIS_FAMILIARIS",NA),
                               0,1)

# the number of samples that have a 1 in the column Art_result on the total number of samples (rows) per year.
percentage_Art_result <- aggregate(Art_result ~ year, submitted, function(x) sum(x == 1) / length(x) * 100)

#the number of samples that have a 1 in the column Art_Wolf on the total number of samples (rows) per year.
percentage_Art_Wolf <- aggregate(Art_Wolf ~ year, submitted, function(x) sum(x == 1) / length(x) * 100)

#the number of samples that have a 1 in the column Individual on the total number of samples (rows) per year.
percentage_Individual <- aggregate(Individual ~ year, submitted, function(x) sum(x == 1) / length(x) * 100)

#the number of samples that have a 1 in the column Individual on the  number of samples that have a 1 in the column Art_Wolf per year.
subset_data <- subset(submitted, Art_Wolf == 1)
percentage_Individual_Wolf <- aggregate(Individual ~ year, subset_data, function(x) sum(x == 1) / length(x) * 100)

# Calculate the total number of samples per year
total_samples <- aggregate(Art_result ~ year, submitted, function(x) length(x))

# Rename the column to "total_samples"
colnames(total_samples)[2] <- "total_samples"

#Combine the percentage data into a single dataframe:
combined_data <- merge(percentage_Art_result, percentage_Art_Wolf, by = "year", all = TRUE)
combined_data <- merge(combined_data, percentage_Individual, by = "year", all = TRUE)
combined_data <- merge(combined_data, percentage_Individual_Wolf, by = "year", all = TRUE)
combined_data <- merge(combined_data, total_samples, by = "year", all = TRUE)

# Reshape the data into long format
combined_data_long <- gather(combined_data, column, percentage, -year, -total_samples, na.rm = TRUE)

# Calculate the mean percentage for each category
mean_percentage_Art_result <- mean(percentage_Art_result$Art_result)
mean_percentage_Art_Wolf <- mean(percentage_Art_Wolf$Art_Wolf)
mean_percentage_Individual <- mean(percentage_Individual$Individual)
mean_percentage_Individual_Wolf <- mean(percentage_Individual_Wolf$Individual)
sum_nb_samples <- sum(total_samples$total_samples)

# Create the text caption
caption <- paste("\nMoyenne par catégorie (n=",sum_nb_samples,"):",
                 "\n - Résultats d'espèces:", sprintf("%.1f%%", mean_percentage_Art_result),
                 "\n - Résultats d'espèces = loup:", sprintf("%.1f%%", mean_percentage_Art_Wolf),
                # "\n - Résultats d'individus:", sprintf("%.1f%%", mean_percentage_Individual),
                 "\n - Résultats loups qui ont donné l'individu:", sprintf("%.1f%%", mean_percentage_Individual_Wolf))

# Plot the data
Probeneffizienz <- ggplot(combined_data_long, aes(x = year, 
                                                  y = percentage, 
                                                  group = column, 
                                                  color = column, 
                                                  shape = column)) +
  geom_line(size=1) +
  geom_point(size=2) +
  geom_text(aes(label = paste0(sprintf("%.0f", percentage), "%")), 
            vjust = -1, color = "black", size=5) +
  scale_x_discrete(labels = paste0(combined_data_long$year, 
                                   "\n (n=", combined_data_long$total_samples, ")")) +
  scale_y_continuous(labels = function(x) paste0(x, "%"))+
  scale_color_manual(values = color,
                     labels=c("% d'échantillons qui ont donné un résultat d'espèce avec succès",
                              "% d'échantillons qui ont donné un résultat d'espèce = loup",
                              #"% d'échantillons qui ont donné un résultat d'individu",
                              "% d'échantillons ayant comme résultat d'espèce 'loup' \nqui ont été identifié au niveau de l'individu")) +
  scale_shape_manual(values = c(16,15,17),#,18
                     labels=c("% d'échantillons qui ont donné un résultat d'espèce avec succès",
                              "% d'échantillons qui ont donné un résultat d'espèce = loup",
                              #"% d'échantillons qui ont donné un résultat d'individu",
                              "% d'échantillons ayant comme résultat d'espèce 'loup' \nqui ont été identifié au niveau de l'individu")) +
  xlab("Année où l'échantillon a été envoyé au LBC pour analyses")+
  ylab("")+
  # labs(color = "", shape = "")+
  ggtitle("VS: Efficience des échantillons") +
  labs(subtitle = paste("Etat le", format(Sys.Date(),"%d.%m.%Y")),
       caption = caption
  )+
  theme_bw()+
  theme(
    legend.position = c(0.75,-0.24),
    legend.box = "vertical",
    legend.direction = "vertical",  # Arrange legend items vertically
    legend.justification = "right",  # Position the legend at the bottom left
    legend.title=element_blank(),
    legend.text = element_text(size=12),
    strip.text = element_text(face = 'bold'),
    axis.text.x = element_text(size=12,
                               margin = margin(t = 0, r = 0, b = 5, l = 0)
                               # angle = 15, 
                               #hjust = 1
    ),
    axis.text.y=element_text(size=12),
    plot.title=element_text(size=24, hjust=0.5),
    axis.title=element_text(size=12),
    plot.caption = element_text(size=12, hjust=0,margin = margin(t = 30, r = 0, b = 0, l = 0) 
                                #face = "italic"
    ),
    plot.subtitle = element_text(size=10, colour = "darkgrey", face = "italic"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.background = element_rect(fill="#FBFCFC",
                                     linewidth=0.5, linetype="solid", 
                                     colour ="darkgrey")) ; Probeneffizienz

#add the KORA logo
ProbeneffizienzKORA <- gridExtra::grid.arrange(Probeneffizienz, Koralogo, heights = c(.93, .07))

#save the plot in jpg format
ggsave(paste0("VS_FR_Probeneffizienz_Jahr_", Sys.Date(), ".jpg"), ProbeneffizienzKORA, width = 13, height = 10, dpi = 500)

#DE
# Create the text caption
caption_DE <- paste("\nDurchschnitt pro Probetyp (n=",sum_nb_samples,"):",
                    "\n - Artresultate:", sprintf("%.1f%%", mean_percentage_Art_result),
                    "\n - Artresultat « Wolf »:", sprintf("%.1f%%", mean_percentage_Art_Wolf),
                    #"\n - Individualresultat:", sprintf("%.1f%%", mean_percentage_Individual),
                    "\n - Artresultat « Wolf » und Individualresultat:", sprintf("%.1f%%", mean_percentage_Individual_Wolf))

color <- c("#F7DC6F","#7ED67D","#64B5F6")#,"#C385CF"

combined_data_long <- combined_data_long %>%
  filter(column != "Individual.x")
# Plot the data
Probeneffizienz_DE <- ggplot(combined_data_long, aes(x = year, 
                                                     y = percentage, 
                                                     group = column, 
                                                     color = column, 
                                                     shape = column)) +
  geom_line(size=1) +
  geom_point(size=2) +
  geom_text(aes(label = paste0(sprintf("%.0f", percentage), "%")), 
            vjust = -1, color = "black", size=5) +
  scale_x_discrete(labels = paste0(combined_data_long$year, 
                                   "\n (n=", combined_data_long$total_samples, ")")) +
  scale_y_continuous(labels = function(x) paste0(x, "%"))+
  scale_color_manual(values = color,
                     labels=c("% der Proben die ein Artresultat ergaben",
                              "% der Proben mit einem Artresultat « Wolf »",
                              #"% der Proben mit einem Individualresultat",
                              "% der Proben mit einem Artresultat « Wolf »\n und mit einem Individualresultat")) +
  scale_shape_manual(values = c(16,15,17),#,18
                     labels=c("% der Proben die ein Artresultat ergaben",
                              "% der Proben mit einem Artresultat « Wolf »",
                              #"% der Proben mit einem Individualresultat",
                              "% der Proben mit einem Artresultat « Wolf »\n und mit einem Individualresultat")) +
  xlab("Jahr, in dem die Probe zur Analyse an das LBC geschickt wurde")+
  ylab("")+
  # labs(color = "", shape = "")+
  ggtitle("VS: Probeneffizienz") +
  labs(subtitle = paste("Stand am", format(Sys.Date(),"%d.%m.%Y")),
       caption = caption_DE
  )+
  theme_bw()+
  theme(
    legend.position = c(0.7,-0.24),
    legend.box = "vertical",
    legend.direction = "vertical",  # Arrange legend items vertically
    legend.justification = "right",  # Position the legend at the bottom left
    legend.title=element_blank(),
    legend.text = element_text(size=12),
    strip.text = element_text(face = 'bold'),
    axis.text.x = element_text(size=12,
                               margin = margin(t = 0, r = 0, b = 5, l = 0)
                               # angle = 15, 
                               #hjust = 1
    ),
    axis.text.y=element_text(size=12),
    plot.title=element_text(size=24, hjust=0.5),
    axis.title=element_text(size=12),
    plot.caption = element_text(size=12, hjust=0,margin = margin(t = 30, r = 0, b = 0, l = 0) 
                                #face = "italic"
    ),
    plot.subtitle = element_text(size=10, colour = "darkgrey", face = "italic"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.background = element_rect(fill="#FBFCFC",
                                     linewidth=0.5, linetype="solid", 
                                     colour ="darkgrey")) ; Probeneffizienz_DE

#add the KORA logo
ProbeneffizienzKORA_DE <- gridExtra::grid.arrange(Probeneffizienz_DE, Koralogo, heights = c(.93, .07))

#save the plot in jpg format
ggsave(paste0("VS_DE_Probeneffizienz_Jahr_", Sys.Date(), ".jpg"), ProbeneffizienzKORA_DE, width = 13, height = 10, dpi = 500)



