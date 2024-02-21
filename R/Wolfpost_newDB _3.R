#   ---   Wolfpost script for the new DB  ---     

#### Choose Result Date, Email type and communication date: #####
sent.date<-c("2024-01-16") #Date d'envoi au LBC
result.date<-c("2024-02-02") #Date des résultats dans la base de donnée (Result_date / genotype date)
Email.Type<-"genotype" #"species" "genotype"
communication.date<-"2024-02-05"

bio.year <-  "2023"

#------------ Library: ####
library(Microsoft365R)
library(data.table)
library(xlsx)
library(readxl)
library(stringr)
library(RMariaDB)
library(dplyr)
library(rgdal)
library(lubridate)


#------------ Set working directory to source file location
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
getwd()

#------------ Merge KORA OS with KORA Report ####

# --- Import raw data ####
# --------- Link to KORA DB server ####
#Access to the DB from different computers
if (Sys.info()["sysname"] == "Darwin") {  # macOS
  ssl_ca_value <- "/etc/ssl/cert.pem"
} else if (Sys.info()["sysname"] == "Windows") {
  ssl_ca_value <- "C:/cacert.pem"
} else {
  # Handle other operating systems if needed
}

#Read DB (add the logins)

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

#Import kinship table
query <- paste("SELECT * FROM WolfKinship ;", sep= "")
rs = dbSendQuery(DB,query)
Kinship<-dbFetch(rs)



#### -- Merge the tables to have all the infos needed ####

#Merge sample and genetic
M_obs_event <- merge(Observation,event, 
                           by.x = "eventId", by.y = "id",
                           all.x = TRUE)

#Merge kill and M_obs_event
M_kill_obsevent <- merge(kill,M_obs_event, 
                     by.x = "observationId", by.y = "id",
                     all.x = TRUE)

M_genetic_obsevent <-  merge(Genetic,M_obs_event, 
                             by.x = "observationId", by.y = "id",
                             all.x = TRUE)
#Merge sample and M_genetic_obsevent
M_samples_genObsEvent <- merge(Wolf_Sample,M_genetic_obsevent, 
                           by.x = "geneticsId", by.y = "id",
                           all.x = TRUE)
M_samples_genObsEvent <- M_samples_genObsEvent %>%
  select(-createdAt.x,-createdAt.y)
# Remove duplicate columns (it's the created at that we don't care about for the wolfpost)
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



# Left join to add the female and male names in the packTable that are in the Individual table 
packTable <- packTable %>%
  left_join(select(Wolf_Individual, id.wolf, individualID), by = c("male" = "id.wolf")) %>%
  rename(male_name = individualID)
packTable <- packTable %>%
  left_join(select(Wolf_Individual, id.wolf, individualID), by = c("female" = "id.wolf")) %>%
  rename(female_name = individualID)


## Assign the origin based on the kinship table
# first obtain for each pair of parent its pack name
#first we want only the packs and not the pairs
pack_only <-  packTable %>%
  filter(type=="PACK")
pack_only$name <- ifelse(pack_only$name == "Glattwang ", "Glattwang", pack_only$name)
Packunique <- pack_only%>%
  select(male_name,female_name,name)%>%
  filter(!is.na(male_name) & !is.na(female_name))%>%
  distinct()
# In the kinship, add the names instead of the ids based on the individual table
Kinship_names <- Kinship %>%
  left_join(select(Wolf_Individual, id.wolf, individualID), by = c("offspring" = "id.wolf")) %>%
  rename(Offspring_name = individualID)

# Replace id2 with its corresponding name
Kinship_names <- Kinship_names %>%
  left_join(select(Wolf_Individual, id.wolf, individualID), by = c("male" = "id.wolf")) %>%
  rename(male_name = individualID)

# Replace id3 with its corresponding name
Kinship_names <- Kinship_names %>%
  left_join(select(Wolf_Individual, id.wolf, individualID), by = c("female" = "id.wolf")) %>%
  rename(female_name = individualID)

# keep only the rows and columns that interest us in the kinship 
Kinship_success <- Kinship_names %>%
  filter(status=="PARENTS")%>%
  select(Offspring_name,male_name,female_name,status)%>%
  distinct()

Packunique$name <- ifelse(Packunique$name == "Val d'Hérens", "Mandelon-Hérens", 
                          ifelse(Packunique$name == "Val Gronda", "Valgronda", 
                                 ifelse(Packunique$name == "Isérables", "Nendaz-Siviez", Packunique$name)))
Packunique <- unique(Packunique)
# Merge Kinship_success with Packunique based on male_name and female_name
Parents_data <- Kinship_success %>%
  left_join(Packunique, by = c("male_name" = "male_name", "female_name" = "female_name"))%>%
  rename(kinship_male_name = male_name,
         kinship_female_name = female_name,
         kinship_packName = name,
         kinship_status = status)


# Join the tables
M_samples_genObsEvent <- M_samples_genObsEvent %>%
  left_join(Parents_data, by = c("resultIndividual" = "Offspring_name"))

# Update packNameSource only if it is NA
M_samples_genObsEvent <- M_samples_genObsEvent %>%
  mutate(packNameSource = if_else(is.na(packNameSource), kinship_packName, packNameSource))


# Splitting the cubsID column and removing † characters
pack_only_expanded <- pack_only %>%
  dplyr::mutate(cubsID = gsub("†", "", cubsID)) %>%
  tidyr::separate_longer_delim(cubsID, delim = ",\\s*") %>%
  dplyr::mutate(cubsID = gsub(" ", "", cubsID)) %>%
  dplyr::filter(!is.na(cubsID) & cubsID != "")


# Creating a list of data frames, each containing rows for a unique cubsID
cubs_tables <- pack_only_expanded %>%
  group_by(cubsID) %>%
  summarize(bioYear = unique(bioYear))

# Left join Table_luca2023 with cubs_tables
joined_table <- left_join(M_samples_genObsEvent, cubs_tables, by = c("resultIndividual" = "cubsID"))

# Update yob column where it's empty
M_samples_genObsEvent <- joined_table %>%
  mutate(yob = ifelse(is.na(yob), bioYear, yob)) %>%
  select(-bioYear) # Removing the bioYear column after the update

M_samples_genObsEvent$packNameSource <- ifelse(
  !is.na(M_samples_genObsEvent$packNameSource) & !is.na(M_samples_genObsEvent$yob), 
  paste(M_samples_genObsEvent$packNameSource, M_samples_genObsEvent$yob), 
  M_samples_genObsEvent$packNameSource
)



#### -- Manage the data ####

# Convert UTC time to Swiss time (CET/CEST)
M_samples_genObsEvent <- M_samples_genObsEvent %>%
  mutate(
    received = with_tz(received, "CET"),
    sent = with_tz(sent, "CET"),
    dateSpecies = with_tz(dateSpecies, "CET"),
    dateIndividual = with_tz(dateIndividual, "CET"),
    date = with_tz(date, "CET")
  )

# Select the samples with the correct result date
if(Email.Type == "genotype"){
  Result_by_genotype<- M_samples_genObsEvent %>%
    filter((format(as.POSIXct(sent), format="%Y-%m-%d") %in% sent.date) 
           # | keyOfficial %in% c("KORA-0000004978",
           #                      "KORA-0000005028",
           #                      "KORA-0000000506",
           #                      "KORA-0000007164",
           #                      "KORA-0000000495")
           )
  Table<-Result_by_genotype
  Table$date<-as.Date(Table$date, format="%Y-%m-%d")
}


if(Email.Type == "species"){
  Result_by_date<-M_samples_genObsEvent %>%
    filter(format(as.POSIXct(dateSpecies), format="%Y-%m-%d") %in% result.date)
  Table<-Result_by_date
  Table$date<-as.Date(Table$date, format="%Y-%m-%d")
}


#Keep only the columns that interest us:
columns <- c("keyOfficial",
             "date",
             "compartmentMain",
             "canton",
             "municipality",
             "fieldName",
             "x","y",
             "observer",
             "observerType",
             "name", #reporter 
             "copyright",#"senderType",
             "key",
             "origin",
             #species kill
             "type.x",
             "resultSpecies",
             "resultIndividual",
             "sourcePopulation",
             "packNameSource",
             "resultNGS",
             "sensitive")
Table <- Table[,columns]
#new columns names
new_colnames <- c("ID_Genetics" ,               
                  "Funddatum_Date_Decouverte",
                  "Kompartiment_Compartiment",
                  "Kanton_Canton",              
                  "Gemeinde_Commune",           
                  "Flurname_Localité",          
                  "x",                          
                  "y",                          
                  "Name_Finder_Nom_Trouveur",   
                  "Typ_Finder_Type_Trouveur",
                  "Name_Melder_Nom_Annonceur", 
                  "©",
                  "Dokumente_Documents",       
                  "Fundort_Origine",            
                  "Typ_Probe_Type_Echantillon", 
                  "Art_Espece",           
                  "Wolf_ID_Loup_ID",
                  "Herkunft_Provenance",
                  "Herkunftsrudel_Meute_d'Origine",
                  "resultNGS",
                  "sensitive")
#replace the column names by the new names
names(Table) <- new_colnames

## Add not interpretable when the individual result was not interpretable. 
if(Email.Type == "genotype"){
  Table$Wolf_ID_Loup_ID[Table$Wolf_ID_Loup_ID == ""] <-  NA
  Table$Wolf_ID_Loup_ID <- ifelse(is.na(Table$Wolf_ID_Loup_ID) & Table$resultNGS== "not interpretable",
                                "nicht interpretierbar/non interprétable",
                                Table$Wolf_ID_Loup_ID)
}


#### -- Traductions ####
#??

Table$Fundort_Origine <- ifelse((Table$Fundort_Origine == "FARM_ANIMAL" & Table$Typ_Probe_Type_Echantillon != "TISSUE"), "Nutztierriss/animal de rente prédaté",
                                ifelse(Table$Fundort_Origine == "TRACK", "Spur/Trace",
                                       ifelse((Table$Fundort_Origine == "WILD_ANIMAL"& Table$Typ_Probe_Type_Echantillon != "TISSUE"), "Wildtierriss/gibier prédaté",
                                              ifelse(Table$Typ_Probe_Type_Echantillon == "TISSUE", "Andere/Autre",
                                              ifelse(Table$Fundort_Origine == "OTHER", "Andere/Autre",
                                                     Table$Fundort_Origine)))))

Table$Typ_Probe_Type_Echantillon <- ifelse(Table$Typ_Probe_Type_Echantillon=="SCAT", "Kot/Crotte",
                                           ifelse(Table$Typ_Probe_Type_Echantillon=="SALIVA", "Speichel/Salive",
                                                  ifelse(Table$Typ_Probe_Type_Echantillon=="TISSUE", "Gewebe/Tissu",
                                                         ifelse(Table$Typ_Probe_Type_Echantillon=="URINE", "Urin/Urine",
                                                                ifelse(Table$Typ_Probe_Type_Echantillon=="BLOOD", "Blut/Sang",
                                                                       ifelse(Table$Typ_Probe_Type_Echantillon=="HAIR", "Haar/Poil",
                                                                              Table$Typ_Probe_Type_Echantillon))))))


Table$Art_Espece <- gsub("not interpretable", "nicht interpretierbar/non interprétable",Table$Art_Espece)
Table$Art_Espece <- gsub("no DNA", "kein DNA/pas d'ADN",Table$Art_Espece)
Table$Art_Espece <- gsub("no sample", "keine Probe/pas d'échantillon",Table$Art_Espece)
Table$Art_Espece <- gsub("Canis lupus.*", "Wolf/Loup",Table$Art_Espece)
Table$Art_Espece <- gsub("Canis spp.*", "Canis spp.",Table$Art_Espece)
Table$Art_Espece <- gsub("Canis familiaris.*", "Hund/Chien",Table$Art_Espece)
Table$Art_Espece <- gsub("mouton.*|cerf.*", "Huftier/Ongulé",Table$Art_Espece)
Table$Art_Espece <- gsub("Vulpes vulpes.*", "Fuchs/Renard",Table$Art_Espece)
Table$Art_Espece <- gsub("Ungulata.*", "Huftier/Ongulé",Table$Art_Espece)
Table$Art_Espece <- gsub("Canis aureus.*", "Goldschakal/Chacal doré",Table$Art_Espece)
Table$Art_Espece <- gsub("Felidae.*", "Felidae",Table$Art_Espece)
Table$Art_Espece <- gsub("Felis spp.*", "Felis spp.",Table$Art_Espece)
#Homo sapiens
Table$Art_Espece <- gsub("Lynx lynx.*", "Luchs/Lynx",Table$Art_Espece)
Table$Art_Espece <- gsub("Martes martes.*", "Marder/Martre",Table$Art_Espece)
Table$Art_Espece <- gsub("Meles meles.*", "Dachs/Blaireau",Table$Art_Espece)
Table$Art_Espece <- gsub("Ursus arctos.*", "Braunbär/Ours brun",Table$Art_Espece)
Table$Art_Espece <- gsub("Sus scrofa.*", "Wildschwein/Sanglier",Table$Art_Espece)

Table <- Table %>%
  left_join(Wolf_Individual %>% select(individualID, sourcePopulation), 
            by = c("Wolf_ID_Loup_ID" = "individualID")) %>%
  mutate(Herkunft_Provenance = coalesce(sourcePopulation, Herkunft_Provenance)) %>%
  select(-sourcePopulation)

Table$Herkunft_Provenance <- ifelse(is.na(Table$Herkunft_Provenance) & Table$Art_Espece=="Wolf/Loup" & !is.na(Table$Wolf_ID_Loup_ID) & Table$Wolf_ID_Loup_ID!="nicht interpretierbar/non interprétable",
                                    "Italienische Halbinsel/Péninsule italienne",Table$Herkunft_Provenance)
Table$Herkunft_Provenance <- ifelse(Table$Herkunft_Provenance == "ITALIAN_PENINSULA", "Italienische Halbinsel/Péninsule italienne",Table$Herkunft_Provenance)
Table$Herkunft_Provenance <- ifelse(Table$Herkunft_Provenance == "CENTRAL_EUROPEAN", "Zentraleuropa/Europe centrale",Table$Herkunft_Provenance)
Table$Herkunft_Provenance <- ifelse(Table$Herkunft_Provenance == "DINARIC_BALKAN", "Dinariden-Balkan/Dinarides-Balkans",Table$Herkunft_Provenance)

Table$Typ_Finder_Type_Trouveur <- ifelse(Table$Typ_Finder_Type_Trouveur=="GAMEWARDEN", "Wildhut/Garde faune",
                                         ifelse(Table$Typ_Finder_Type_Trouveur=="OTHER", "Andere/Autre",
                                                ifelse(Table$Typ_Finder_Type_Trouveur=="HUNTER", "Jäger/Chasseur", 
                                                Table$Typ_Finder_Type_Trouveur)))

Table$`Herkunftsrudel_Meute_d'Origine` <- ifelse(Table$`Herkunftsrudel_Meute_d'Origine`=="noResult", NA, Table$`Herkunftsrudel_Meute_d'Origine`)
#---- Add info #### 

#If a individual is a parent in a pack
packTable <- packTable %>%
  filter(bioYear == bio.year)
packTable$pack_type_DE <- ifelse(packTable$type == "PACK", "Rudel",
                                 ifelse(packTable$type == "PAIR", "Paar",
                                        packTable$type))
packTable$pack_type_FR <- ifelse(packTable$type == "PACK", "meute",
                                 ifelse(packTable$type == "PAIR", "paire",
                                        packTable$type))

# Loop through each row in the 'Table' dataframe
for (i in 1:nrow(Table)) {
  wolf_id <- Table$Wolf_ID_Loup_ID[i]
  
  # Find matching rows in 'packTable' where 'Wolf_ID_Loup_ID' matches 'male'
  male_match <- packTable$male_name == wolf_id
  if (any(male_match, na.rm = TRUE)) {
    pack_name <- packTable$name[which(male_match)]
    pack_type_DE <- packTable$pack_type_DE[which(male_match)]
    pack_type_FR <- packTable$pack_type_FR[which(male_match)]
    female <- packTable$female_name[which(male_match)]
    
    Table$Info[i] <- paste0("Territorialer Rüde ", pack_name," ", pack_type_DE, " mit der Fähe ", female, "/", "Mâle territorial de la ", pack_type_FR," '",pack_name,"' ", "formée avec la femelle ", female)
  } else {
    # Find matching rows in 'packTable' where 'Wolf_ID_Loup_ID' matches 'female'
    female_match <- packTable$female_name == wolf_id
    if (any(female_match, na.rm = TRUE)) {
      pack_name <- packTable$name[which(female_match)]
      pack_type_DE <- packTable$pack_type_DE[which(female_match)]
      pack_type_FR <- packTable$pack_type_FR[which(female_match)]
      male <- packTable$male_name[which(female_match)]
      
      Table$Info[i] <- paste0("Territoriale Fähe ", pack_name," ", pack_type_DE, " mit dem Rüden ", male, "/", "Femelle territoriale de la ",  pack_type_FR, " '",pack_name,"' ", "formée avec le mâle ", male)
    } else {
      Table$Info[i] <- NA
    }
  }
}



#------------ Create Dir. Output ####
if(Email.Type == "genotype"){
  folder_name <- paste("./",result.date,"_G",sep="")
  dir.create(folder_name, recursive = TRUE)
}

if(Email.Type == "species"){
  folder_name <- paste("./",communication.date,"_S",sep="")
  dir.create(folder_name, recursive = TRUE)
}


#------------ Projection  ####
#Projection to be used for the map (CH1903 / LV03):
#Warnings OK
suppressWarnings(CRS<- sp::CRS("+init=epsg:2056"))

#------------ Split data into compartments ####

# --- Import shapefile
suppressWarnings(Rcompartment <- raster::shapefile("MAP_Data/Wolfkomp_18_07_2015.shp"))
# Transform the shapefile coordinates
Rcompartment <- spTransform(Rcompartment,CRS)
Rcompartment<-sf::st_as_sf(Rcompartment)
# --- Genetic sample as spatial points
pts<- sf::st_as_sf(Table[,c("x","y")],coords = c("x", "y"))
sf::st_crs(pts)<-CRS

# --- Import data Compartment for each point
pts<-sf::st_join(pts, Rcompartment)
pts<-as.data.table(pts)

# --- Insert data into w.genetic table
Table[,"Kompartiment_Compartiment"]<-pts$Nummer

#If there is a new individual
Table$Info <- ifelse(Table$resultNGS=="new individual", 
                     "Individuum erstmals in der Schweiz genetisch nachgewiesen/Individu génétiquement identifié pour la première fois en Suisse",
                     Table$Info)
Table$resultNGS <- NULL

#Remove results that have no values in Wolf_ID_Loup_ID
if(Email.Type == "genotype"){
Table<-Table[!is.na(Table$Wolf_ID_Loup_ID),]
}

# Write Table for visualization in QGIS
write.csv(Table, paste0("../../","carte/Files Ines dont touch/Results_wolfpost.csv"))


# Remove sensitive samples from the general table and put it in a sensitive table 
# Create a new dataframe with rows where sensitive column is 1
sensitiveSamples <- Table[Table$sensitive == 1, ]

# Remove those rows from the original dataframe
Table <- Table[Table$sensitive == 0, ]

#drop the sensitive column from the non-sensitive dataframe:
Table$sensitive <- NULL
sensitiveSamples$sensitive <- NULL


# Check if 'sensitiveSamples' is not empty by checking if it has any rows
if (nrow(sensitiveSamples) > 0) {
  for(i in 1:length(unique(sensitiveSamples$Kanton_Canton))){
    
    file<-paste(folder_name,"/w_SENSITIVE", Email.Type,"_genetic_",unique(sensitiveSamples$Kanton_Canton)[i],"_",communication.date,".xlsx",sep="")
    
    if(is.na(unique(sensitiveSamples$Kanton_Canton)[i])){
      # ---- Data to be presented in table (outside CH)
      Data.xlsx=sensitiveSamples[is.na(sensitiveSamples$Kanton_Canton),]}
    else{
      # ---- Data to be presented in table (within CH)
      Data.xlsx=sensitiveSamples[sensitiveSamples$Kanton_Canton==unique(sensitiveSamples$Kanton_Canton)[i],]}
    
    # ---- create a new workbook for outputs
    wb<-createWorkbook(type="xlsx")
    
    # ---- Create a new sheet in the workbook
    sheet <- createSheet(wb, sheetName = "Genetic_KORA_LBC")
    
    # ---- Define COLNAMES_STYLE
    TABLE_COLNAMES_STYLE <- CellStyle(wb) + Font(wb, isBold=TRUE) +
      Alignment(wrapText=TRUE, horizontal="ALIGN_CENTER",vertical = "VERTICAL_CENTER") +
      Border(color="black", position=c("TOP", "BOTTOM"), 
             pen=c("BORDER_THIN", "BORDER_THICK")) 
    
    # ---- Date as text
    
    Data.xlsx$Funddatum_Date_Decouverte<-as.character(Data.xlsx$Funddatum_Date_Decouverte)
    
    # ---- Replace / by line break
    Data.xlsx<- data.frame(lapply(Data.xlsx, function(x) {
      gsub("/", "\n", x)}))
    
    # ---- Change columns names
    if(Email.Type == "genotype"){
      colnames(Data.xlsx) <-c("ID_Genetics",
                              "Funddatum\nDate de découverte",
                              "Kompartiment\nCompartiment",
                              "Kanton\nCanton",
                              "Gemeinde\nCommune",
                              "Flurname\nLocalité",
                              "x" ,
                              "y" ,
                              "Name_Finder\nNom Trouveur",   
                              "Typ_Finder\nType Trouveur",   
                              "Name_Melder_Nom_Annonceur",
                              "©",
                              "Dokumente\nDocuments" ,       
                              "Herkunft\nOrigine" ,           
                              "Typ_Probe\nType d'échantillon",
                              "Art\nEspece",         
                              "Wolf_ID\nLoup_ID" ,           
                              "Herkunftpopulation\nPopulation d'origine" , 
                              "Herkunftsrudel\nMeute d'origine",
                              "Zusatzinfo zum Sozialstatus\nInfo supplémentaire sur le statut social")}
    if(Email.Type == "species"){
      colnames(Data.xlsx) <-c("ID_Genetics",
                              "Funddatum\nDate de découverte",
                              "Kompartiment\nCompartiment",
                              "Kanton\nCanton",
                              "Gemeinde\nCommune",
                              "Flurname\nLocalité",
                              "x" ,
                              "y" ,
                              "Name_Finder\nNom Trouveur",   
                              "Typ_Finder\nType Trouveur",
                              "Name_Melder_Nom_Annonceur",
                              "©",
                              "Dokumente\nDocuments" ,       
                              "Herkunft\nOrigine" ,           
                              "Typ_Probe\nType d'échantillon",
                              "Art\nEspece",         
                              "Wolf_ID\nLoup_ID" ,           
                              "Herkunft\nProvenance" , 
                              "Info")}
    
    # ---- Insert data into workbook
    addDataFrame(Data.xlsx,
                 sheet, startRow=1, startColumn=1,
                 colnamesStyle = TABLE_COLNAMES_STYLE,
                 row.names = FALSE)
    
    # ---- Change column width to auto
    autoSizeColumn(sheet, colIndex=c(1:ncol(sensitiveSamples)))
    
    # ---- Center justify table
    cs <- CellStyle(wb) + Alignment(wrapText=TRUE,horizontal = "ALIGN_CENTER",vertical = "VERTICAL_CENTER")
    all.rows <- getRows(sheet, rowIndex = 2:nrow(sensitiveSamples))
    all.cells <- getCells(all.rows)
    invisible(lapply(all.cells, setCellStyle, cs))
    
    # # ---- Red color if new individual:
    # New_individual_style <- CellStyle(wb) + Font(wb,color="red", isBold=TRUE)+
    #   Alignment(wrapText=TRUE, horizontal = "ALIGN_CENTER", vertical = "VERTICAL_CENTER")
    # apply <- getRows(sheet, rowIndex = which(!is.na(Data.xlsx$Info))+1)
    # apply <- getCells(apply,colIndex=c(15,19))
    # invisible(lapply(apply, setCellStyle, New_individual_style))
    # ---- Red color if new individual:
    New_individual_value <- "Individuum erstmals in der Schweiz genetisch nachgewiesen\nIndividu génétiquement identifié pour la première fois en Suisse"
    New_individual_style <- CellStyle(wb) + Font(wb, color = "red", isBold = TRUE) +
      Alignment(wrapText = TRUE, horizontal = "ALIGN_CENTER", vertical = "VERTICAL_CENTER")
    apply <- getRows(sheet, rowIndex = which(Data.xlsx$`Zusatzinfo zum Sozialstatus
                                             Info supplémentaire sur le statut social` == New_individual_value)+1)
    apply <- getCells(apply,colIndex=c(17,20))
    invisible(lapply(apply, setCellStyle, New_individual_style))
    
    
    # ---- Save the workbook into xlsx
    saveWorkbook(wb, file)
  }
} else {
  cat("No rows in sensitiveSamples to write to file.")
}



# Sorting 'df' by 'char_column' in ascending order
Table <- Table[order(Table$Kanton_Canton), ]


#------------ Export as excel Table ####

for(i in 1:length(unique(pts$Nummer))){
  
  file<-paste(folder_name,"/w_", Email.Type,"_genetic_compartment_",unique(pts$Nummer)[i],"_",communication.date,".xlsx",sep="")
  
  if(is.na(unique(pts$Nummer)[i])){
    # ---- Data to be presented in table (outside CH)
    Data.xlsx=Table[is.na(Table$Kompartiment_Compartiment),]}
  else{
    # ---- Data to be presented in table (within CH)
    Data.xlsx=Table[Table$Kompartiment_Compartiment==unique(pts$Nummer)[i],]}
  
  # ---- create a new workbook for outputs
  wb<-createWorkbook(type="xlsx")
  
  # ---- Create a new sheet in the workbook
  sheet <- createSheet(wb, sheetName = "Genetic_KORA_LBC")
  
  # ---- Define COLNAMES_STYLE
  TABLE_COLNAMES_STYLE <- CellStyle(wb) + Font(wb, isBold=TRUE) +
    Alignment(wrapText=TRUE, horizontal="ALIGN_CENTER",vertical = "VERTICAL_CENTER") +
    Border(color="black", position=c("TOP", "BOTTOM"), 
           pen=c("BORDER_THIN", "BORDER_THICK")) 
  
  # ---- Date as text
  
  Data.xlsx$Funddatum_Date_Decouverte<-as.character(Data.xlsx$Funddatum_Date_Decouverte)
  
  # ---- Replace / by line break
  Data.xlsx<- data.frame(lapply(Data.xlsx, function(x) {
    gsub("/", "\n", x)}))
  
  # ---- Change columns names
  if(Email.Type == "genotype"){
    colnames(Data.xlsx) <-c("ID_Genetics",
                            "Funddatum\nDate de découverte",
                            "Kompartiment\nCompartiment",
                            "Kanton\nCanton",
                            "Gemeinde\nCommune",
                            "Flurname\nLocalité",
                            "x" ,
                            "y" ,
                            "Name_Finder\nNom Trouveur",   
                            "Typ_Finder\nType Trouveur",   
                            "Name_Melder_Nom_Annonceur",
                            "©",
                            "Dokumente\nDocuments" ,       
                            "Herkunft\nOrigine" ,           
                            "Typ_Probe\nType d'échantillon",
                            "Art\nEspece",         
                            "Wolf_ID\nLoup_ID" ,           
                            "Herkunftpopulation\nPopulation d'origine" , 
                            "Herkunftsrudel\nMeute d'origine",
                            "Zusatzinfo zum Sozialstatus\nInfo supplémentaire sur le statut social")}
  if(Email.Type == "species"){
    colnames(Data.xlsx) <-c("ID_Genetics",
                            "Funddatum\nDate de découverte",
                            "Kompartiment\nCompartiment",
                            "Kanton\nCanton",
                            "Gemeinde\nCommune",
                            "Flurname\nLocalité",
                            "x" ,
                            "y" ,
                            "Name_Finder\nNom Trouveur",   
                            "Typ_Finder\nType Trouveur",
                            "Name_Melder_Nom_Annonceur",
                            "©",
                            "Dokumente\nDocuments" ,       
                            "Herkunft\nOrigine" ,           
                            "Typ_Probe\nType d'échantillon",
                            "Art\nEspece",         
                            "Wolf_ID\nLoup_ID" ,           
                            "Herkunft\nProvenance" , 
                            "Info")}
  
  # ---- Insert data into workbook
  addDataFrame(Data.xlsx,
               sheet, startRow=1, startColumn=1,
               colnamesStyle = TABLE_COLNAMES_STYLE,
               row.names = FALSE)
  
  # ---- Change column width to auto
  autoSizeColumn(sheet, colIndex=c(1:ncol(Table)))
  
  # ---- Center justify table
  cs <- CellStyle(wb) + Alignment(wrapText=TRUE,horizontal = "ALIGN_CENTER",vertical = "VERTICAL_CENTER")
  all.rows <- getRows(sheet, rowIndex = 2:nrow(Table))
  all.cells <- getCells(all.rows)
  invisible(lapply(all.cells, setCellStyle, cs))
  
  # # ---- Red color if new individual:
  # New_individual_style <- CellStyle(wb) + Font(wb,color="red", isBold=TRUE)+
  #   Alignment(wrapText=TRUE, horizontal = "ALIGN_CENTER", vertical = "VERTICAL_CENTER")
  # apply <- getRows(sheet, rowIndex = which(!is.na(Data.xlsx$Info))+1)
  # apply <- getCells(apply,colIndex=c(15,19))
  # invisible(lapply(apply, setCellStyle, New_individual_style))
  # ---- Red color if new individual:
  New_individual_value <- "Individuum erstmals in der Schweiz genetisch nachgewiesen\nIndividu génétiquement identifié pour la première fois en Suisse"
  New_individual_style <- CellStyle(wb) + Font(wb, color = "red", isBold = TRUE) +
    Alignment(wrapText = TRUE, horizontal = "ALIGN_CENTER", vertical = "VERTICAL_CENTER")
  apply <- getRows(sheet, rowIndex = which(Data.xlsx$`Zusatzinfo zum Sozialstatus
Info supplémentaire sur le statut social` == New_individual_value)+1)
  apply <- getCells(apply,colIndex=c(17,20))
  invisible(lapply(apply, setCellStyle, New_individual_style))
  
  
  # ---- Save the workbook into xlsx
  saveWorkbook(wb, file)
}






