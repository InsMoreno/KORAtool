#   ---   Wolfpost script  ---     

#---------------- Ralph Manz Info ----- Choose Result Date and Email type: ####
result.date<-"2023-03-24" #Date of results in the database (Result_date / genotype date)
Email.Type<-"genotype" #"species" "genotype"
communication.date<-"2023-03-28"


#------------ Library: ####
library(Microsoft365R)
library(data.table)
library(xlsx)
library(readxl)
library(stringr)
library(RMariaDB)
library(dplyr)

#------------ Set working directory to source file location
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
getwd()

#------------ Merge KORA OS with KORA Report ####

# --- Import raw data ####

# - Info LBC ####
LBC<- as.data.table(read_excel("Input/LBC.xlsx"))

# - Data KORA OS ####
# --------- Link to KORA DB server ####
storiesDb <- dbConnect(RMariaDB::MariaDB(), user='kb_app_r', password="46Pb2f9DP08mj83T", dbname='kora_base', host='dedi5097.your-server.de')

query <- paste("SELECT * FROM v_samples_wolf ;", sep= "")
rs = dbSendQuery(storiesDb,query)
Wolf_genetics<-dbFetch(rs)


if(Email.Type == "genotype"){
  Result_by_genotype<- Wolf_genetics %>%
    filter(genotypeDate==result.date)
  Table<-Result_by_genotype
  Table$collectionDate<-as.Date(Table$collectionDate, format="%Y-%m-%d")
}


if(Email.Type == "species"){
  Result_by_date<-Wolf_genetics %>%
    filter(resultDate==result.date)
  Table<-Result_by_date
  Table$collectionDate<-as.Date(Table$collectionDate, format="%Y-%m-%d")
}


#Keep only the columns that interest us:
columns <- c("sampleID",
             "collectionDate",
             "canton",
             "community",
             "location",
             "x","y",
             "sender",
             "senderType",
             "documents",
             "sampleOrigin",
             "sampleType",
             "result1",
             "result2",
             "wolf",
             "provenance")
Table <- Table[,columns]
#new columns names
new_colnames <- c("ID_Genetics" ,               
                  "Funddatum_Date_Decouverte",  
                  "Kanton_Canton",              
                  "Gemeinde_Commune",           
                  "Flurname_Location",          
                  "x",                          
                  "y",                          
                  "Name_Finder_Nom_Trouveur",   
                  "Typ_Finder_Type_Trouveur",   
                  "Dokumente_Documents",       
                  "Fundort_Origine",            
                  "Typ_Probe_Type_Echantillon", 
                  "Art_1_Espece_1",           
                  "Art_2_Espece_2" , 
                  "Wolf_ID_Loup_ID",
                  "Herkunft_Provenance")
#replace the column names by the new names
names(Table) <- new_colnames

# - Data KORA Report ####

# --------- Link to KORA DB server ####
storiesDb <- dbConnect(RMariaDB::MariaDB(), user='kb_app_r', password="46Pb2f9DP08mj83T", dbname='kora_base', host='dedi5097.your-server.de')

query<-paste("SELECT e.id,e.uid,e.evt_species,e.evt_reporter,e.evt_observer,e.evt_x, 
             e.evt_y,e.evt_canton, e.evt_field_name, e.evt_observer_type, 
             o.obs_type,o.kill_species,  o.obs_date, o.uid
             FROM kr_event e JOIN kr_observation o ON o.fk_evt_id = e.id WHERE e.evt_species = 'wolf' 
             AND e._status = 1 AND o._status = 1 AND (o.obs_type = 'genetics' OR o.obs_type = 'kill');" ,sep="")
rs = dbSendQuery(storiesDb,query)
event_obs<-dbFetch(rs)
colnames(event_obs)[14]  <- "fk_obs_uid"

query<-paste("SELECT s.sample_key, s.sample_type, s.sample_key_official, 
             s.sample_result_species, s.sample_result_individual, s.fk_obs_uid,
             s.sample_date_species, s.sample_date_individual
             FROM kr_samples s;",sep="")
rs = dbSendQuery(storiesDb,query)
sample<-dbFetch(rs)



# --- Merge KORA Report data needed ####
event_obs_test <- event_obs
event_obs <- event_obs_test %>% 
  group_by(uid) %>%
  mutate(kill_species = paste(ifelse(is.na(kill_species), "", kill_species), collapse = "\n"))

M.table <- merge(event_obs_test, sample,
                 by = "fk_obs_uid")

M.table$evt_reporter <- gsub(',','',M.table$evt_reporter)
M.table$evt_observer <- gsub(',','',M.table$evt_observer)
M.table$Name_Finder_Nom_Trouveur <- ifelse(M.table$evt_reporter != M.table$evt_observer, 
                                           str_c(M.table$evt_reporter, "_", M.table$evt_observer), 
                                           M.table$evt_reporter)

#Only keep rows with the right result date
if(Email.Type == "genotype"){
  M.table <- subset(M.table, sample_date_individual == result.date)
}

if(Email.Type == "species"){
  M.table <- subset(M.table, sample_date_species == result.date)
}

#remove rows that don't interest us
M.table <- M.table %>% select(-fk_obs_uid, -id, -uid, -evt_observer,
                              -evt_reporter, -evt_species,
                              -obs_type, -sample_date_species, -sample_date_individual)

# Have x and y coordinates in the same format as Wolf genetics
M.table$evt_x <- substr(M.table$evt_x, nchar(M.table$evt_x) - 5, nchar(M.table$evt_x))
M.table$evt_y <- substr(M.table$evt_y, nchar(M.table$evt_y) - 5, nchar(M.table$evt_y))


# the municipality, result 2 and origin are missing !!!!  So we create empty columns in the meantime
M.table$Gemeinde_Commune <- NA
M.table$Art_2_Espece_2 <- NA
M.table$Herkunft_Provenance <- NA
M.table$obs_date <- as.Date(M.table$obs_date, format = "%Y-%m-%d")



# --- Set column order ####
#first, assign actual column names and change the order of the columns
old_colnames <- c("sample_key_official", 
                  "obs_date",
                  "evt_canton",
                  "Gemeinde_Commune",
                  "evt_field_name",
                  "evt_x",
                  "evt_y",
                  "Name_Finder_Nom_Trouveur",
                  "evt_observer_type",
                  "sample_key",
                  "kill_species",
                  "sample_type",
                  "sample_result_species",
                  "Art_2_Espece_2",
                  "sample_result_individual",
                  "Herkunft_Provenance")
#change the order of the columns so that the "old" names would be replaced by the right new names
M.table <- M.table[, old_colnames]
#new names
new_colnames <- c("ID_Genetics" ,               
                  "Funddatum_Date_Decouverte",  
                  "Kanton_Canton",              
                  "Gemeinde_Commune",           
                  "Flurname_Location",          
                  "x",                          
                  "y",                          
                  "Name_Finder_Nom_Trouveur",   
                  "Typ_Finder_Type_Trouveur",   
                  "Dokumente_Documents",       
                  "Fundort_Origine",            
                  "Typ_Probe_Type_Echantillon", 
                  "Art_1_Espece_1",           
                  "Art_2_Espece_2" ,     
                  "Wolf_ID_Loup_ID",
                  "Herkunft_Provenance")
#replace the column names by the new names
names(M.table)[names(M.table) %in% old_colnames] <- new_colnames

#When there is "not interpretable" in the Wolf_ID_Loup_ID, put it in Art_2_Espece_2 column 
# and delete it from Wolf_ID_Loup_ID column
M.table$Art_2_Espece_2[M.table$Wolf_ID_Loup_ID == "nicht interpretierbar"] <- "nicht interpretierbar"
M.table$Wolf_ID_Loup_ID[M.table$Wolf_ID_Loup_ID == "nicht interpretierbar"] <- NA

#replace english names by german
M.table$Typ_Finder_Type_Trouveur <- ifelse(M.table$Typ_Finder_Type_Trouveur=="gamewarden", "Wildhut Kanton",M.table$Typ_Finder_Type_Trouveur)
M.table$Art_1_Espece_1 <- ifelse(M.table$Art_1_Espece_1=="Canis lupus", "Wolf",
                                 ifelse(M.table$Art_1_Espece_1=="Vulpes vulpes","Fuchs",
                                        ifelse(M.table$Art_1_Espece_1=="Cervus elaphus","Hirsch",
                                        M.table$Art_1_Espece_1)))
M.table$Typ_Probe_Type_Echantillon <- ifelse(M.table$Typ_Probe_Type_Echantillon=="scat", "Kot",
                                      ifelse(M.table$Typ_Probe_Type_Echantillon=="spleen", "Milz",
                                      ifelse(M.table$Typ_Probe_Type_Echantillon=="saliva", "Speichel",
                                      ifelse(M.table$Typ_Probe_Type_Echantillon=="tissue", "Gewebe",
                                      ifelse(M.table$Typ_Probe_Type_Echantillon=="urine", "Urin",
                                      ifelse(M.table$Typ_Probe_Type_Echantillon=="blood", "Blut",
                                      ifelse(M.table$Typ_Probe_Type_Echantillon=="hair", "Haar",
                                      M.table$Typ_Probe_Type_Echantillon)))))))
#add the provenance ! Nealry always Italy that's why I automatise that like that for the moment
# BUT keep in mind that it could be another provenance and we should always verify!!
M.table$Herkunft_Provenance <- ifelse(M.table$Art_1_Espece_1=="Wolf", "Italy", NA)

# --- Bind all data ####

Table <- rbind(Table,M.table)


#------------ Projection  ####
#Projection to be used for the map (CH1903 / LV03):
#Warnings OK
suppressWarnings(CRS<- sp::CRS("+init=epsg:21781"))

#------------ Create Dir. Output ####
if(Email.Type == "genotype"){
  folder_name <- paste("./",result.date,"_G",sep="")
  dir.create(folder_name, recursive = TRUE)
}

if(Email.Type == "species"){
  folder_name <- paste("./",result.date,"_S",sep="")
  dir.create(folder_name, recursive = TRUE)
}




#------------ Split data into compartments ####

# --- Import shapefile
suppressWarnings(Rcompartment <- raster::shapefile("MAP_Data/Wolfkomp_18_07_2015.shp"))
Rcompartment<-sf::st_as_sf(Rcompartment)
sf::st_crs(Rcompartment)<-CRS

# --- Genetic sample as spatial points
pts<- sf::st_as_sf(Table[,c("x","y")],coords = c("x", "y"))
sf::st_crs(pts)<-CRS

# --- Import data Compartment for each point
pts<-sf::st_join(pts, Rcompartment)
pts<-as.data.table(pts)


# --- Insert data into w.genetic table
Table[,"Kompartiment_Compartiment"]<-pts$Nummer

# --- Remove x/y and location if wolf dead
Table$Flurname_Location <- ifelse(Table$Typ_Finder_Type_Trouveur == "FIWI"
                                  & Table$Fundort_Origine == "Wolf", "", Table$Flurname_Location)
Table$x <- ifelse(Table$Typ_Finder_Type_Trouveur == "FIWI"
                                  & Table$Fundort_Origine == "Wolf", "", Table$x)
Table$y <- ifelse(Table$Typ_Finder_Type_Trouveur == "FIWI"
                                  & Table$Fundort_Origine == "Wolf", "", Table$y)

# --- Translation within columns:

Table.T<-Table[,c("Fundort_Origine","Typ_Probe_Type_Echantillon","Art_1_Espece_1","Art_2_Espece_2")]

Table.T<- data.frame(lapply(Table.T, function(x) {
  gsub("Schaf", "Schaf/Mouton", x)}))
Table.T <- data.frame(lapply(Table.T, function(x) {
  gsub("Schafe", "Schaf/Mouton", x)}))
Table.T <- data.frame(lapply(Table.T, function(x) {
  gsub("gerissenes Schaf", "Schaf/Mouton", x)}))
Table.T<- data.frame(lapply(Table.T, function(x) {
  gsub("Pecora", "Schaf/Mouton", x)}))

Table.T<- data.frame(lapply(Table.T, function(x) {
  gsub("Hund", "Hund/Chien", x)}))

Table.T<- data.frame(lapply(Table.T, function(x) {
  gsub("Luchs", "Luchs/Lynx", x)}))

Table.T <- data.frame(lapply(Table.T, function(x) {
  gsub("Lamm", "Lamm/Âgneau", x)}))


Table.T <- data.frame(lapply(Table.T, function(x) {
  gsub("Bouquetin", "Steinbock/Bouquetin", x)}))

Table.T <- data.frame(lapply(Table.T, function(x) {
  gsub("Esel", "Esel/Ane", x)}))


Table.T<- data.frame(lapply(Table.T, function(x) {
  gsub("Pferd", "Pferd/Cheval", x)}))

Table.T <- data.frame(lapply(Table.T, function(x) {
  gsub("Wolf", "Wolf/Loup", x)}))

Table.T <- data.frame(lapply(Table.T, function(x) {
  gsub("Ziege", "Ziege/Chèvre", x)}))
Table.T <- data.frame(lapply(Table.T, function(x) {
  gsub("Pfauenziege", "Pfauenziege/Chèvre Paon", x)}))

Table.T <- data.frame(lapply(Table.T, function(x) {
  gsub("Hase", "Hase/Lièvre", x)}))

Table.T <- data.frame(lapply(Table.T, function(x) {
  gsub("Wildschwein", "Wildschwein/Sanglier", x)}))

Table.T <- data.frame(lapply(Table.T, function(x) {
  gsub("Rind", "Rind/Bovin", x)}))
Table.T<- data.frame(lapply(Table.T, function(x) {
  gsub("Kuh", "Rind/Bovin", x)}))
Table.T <- data.frame(lapply(Table.T, function(x) {
  gsub("Mucca", "Rind/Bovin", x)}))
Table.T <- data.frame(lapply(Table.T, function(x) {
  gsub("Kalb", "Kalb/Veau", x)}))


Table.T <- data.frame(lapply(Table.T, function(x) {
  gsub("Hirsch", "Hirsch/Cerf", x)}))
Table.T <- data.frame(lapply(Table.T, function(x) {
  gsub("Hirsch/Cerfkalb", "Hirschkalb/Faon", x)}))
Table.T <- data.frame(lapply(Table.T, function(x) {
  gsub("Hirsch/Cerfkuh", "Hirsch/Biche", x)}))
Table.T <- data.frame(lapply(Table.T, function(x) {
  gsub("Rotwild", "Rotwild/Cerf", x)}))
Table.T <- data.frame(lapply(Table.T, function(x) {
  gsub("Rothirsch", "Rothirsch/Cerf", x)}))
Table.T <- data.frame(lapply(Table.T, function(x) {
  gsub("Wildkalb", "Wildkalb/Faon", x)}))


Table.T <- data.frame(lapply(Table.T, function(x) {
  gsub("Huftier", "Huftier/Ongulé", x)}))

Table.T<- data.frame(lapply(Table.T, function(x) {
  gsub("Gämse", "Gämse/Chamois", x)}))
Table.T <- data.frame(lapply(Table.T, function(x) {
  gsub("Gams", "Gämse/Chamois", x)}))

Table.T<- data.frame(lapply(Table.T, function(x) {
  gsub("Fuchs", "Fuchs/Renard", x)}))

Table.T<- data.frame(lapply(Table.T, function(x) {
  gsub("Reh", "Reh/Chevreuil", x)}))
Table.T<- data.frame(lapply(Table.T, function(x) {
  gsub("Reh/Chevreuilgeiss", "Rehgeiss/Chevreuil", x)}))

Table.T<- data.frame(lapply(Table.T, function(x) {
  gsub("nicht interpretierbar", "nicht interpretierbar/non interprétable", x)}))



# --- Translation Fundort_Origine:
Table.T<- data.frame(lapply(Table.T, function(x) {
  gsub("Gewebe", "Gewebe/Tissu", x)}))
Table.T <- data.frame(lapply(Table.T, function(x) {
  gsub("Milz", "Milz/Rate", x)}))
Table.T <- data.frame(lapply(Table.T, function(x) {
  gsub("Haar", "Haar/Poils", x)}))
Table.T <- data.frame(lapply(Table.T, function(x) {
  gsub("Urin", "Urin/Urine", x)}))
Table.T<- data.frame(lapply(Table.T, function(x) {
  gsub("Kot", "Kot/Crotte", x)}))
Table.T<- data.frame(lapply(Table.T, function(x) {
  gsub("Speichel", "Speichel/Salive", x)}))
Table.T<- data.frame(lapply(Table.T, function(x) {
  gsub("Blut", "Blut/Sang", x)}))
Table.T<- data.frame(lapply(Table.T, function(x) {
  gsub("tissue", "Gewebe/Tissu", x)}))
Table.T <- data.frame(lapply(Table.T, function(x) {
  gsub("spleen", "Milz/Rate", x)}))
Table.T <- data.frame(lapply(Table.T, function(x) {
  gsub("hair", "Haar/Poils", x)}))
Table.T <- data.frame(lapply(Table.T, function(x) {
  gsub("urine", "Urin/Urine", x)}))
Table.T<- data.frame(lapply(Table.T, function(x) {
  gsub("scat", "Kot/Crotte", x)}))
Table.T<- data.frame(lapply(Table.T, function(x) {
  gsub("saliva", "Speichel/Salive", x)}))
Table.T<- data.frame(lapply(Table.T, function(x) {
  gsub("blood", "Blut/Sang", x)}))

# --- Replace translation in original table:

Table$Fundort_Origine <-Table.T$Fundort_Origine
Table$Typ_Probe_Type_Echantillon<-Table.T$Typ_Probe_Type_Echantillon
Table$Art_1_Espece_1<-Table.T$Art_1_Espece_1
Table$Art_2_Espece_2<-Table.T$Art_2_Espece_2

#Add info New individual
Table[Table$Wolf_ID_Loup_ID %in% unique(LBC[LBC$Result=="new individual",ID_KORA]),"Info"]<-"Individuum erstmals in der Schweiz genetisch nachgewiesene./Individu génétiquement identifié pour la première fois en Suisse."

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
  colnames(Data.xlsx) <-c("ID_Genetics",
                          "Funddatum\nDate_Decouverte",
                          "Kanton\nCanton",
                          "Gemeinde\nCommune",
                          "Flurname\nLocation",
                          "x" ,
                          "y" ,
                          "Name_Finder\nNom_Trouveur",   
                          "Typ_Finder\nType_Trouveur",   
                          "Dokumente\nDocuments" ,       
                          "Fundort\nOrigine" ,           
                          "Typ_Probe\nType_Echantillon",
                          "Art_1\nEspece_1",         
                          "Art_2\nEspece_2" ,            
                          "Wolf_ID\nLoup_ID" ,           
                          "Herkunft\nProvenance" ,       
                          "Kompartiment\nCompartiment",  
                          "Info")
  
  
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
  
  # ---- Red color if new individual:
  New_individual_style <- CellStyle(wb) + Font(wb,color="red", isBold=TRUE)+
    Alignment(wrapText=TRUE, horizontal = "ALIGN_CENTER", vertical = "VERTICAL_CENTER")
  apply <- getRows(sheet, rowIndex = which(!is.na(Data.xlsx$Info))+1)
  apply <- getCells(apply,colIndex=c(15,18))
  invisible(lapply(apply, setCellStyle, New_individual_style))
  
  # ---- Save the workbook into xlsx
  saveWorkbook(wb, file)
  
}

#------------ Creation of the map: ####

# -- Altitude layer #
Alt <- raster::raster("MAP_Data/CHHS.TIF")
# indicate CRS
raster::crs(Alt) <- CRS 
# convert to a df for plotting in two steps:
# First, to a SpatialPointsDataFrame
Alt_pts <- raster::rasterToPoints(Alt, spatial = TRUE)

# -- Use info to compute Study Area 
study_area<-sp::bbox(Alt_pts)
study_area<-rgeos::readWKT(paste("POLYGON((",
                                 study_area[1,1]," ",study_area[2,1],",",
                                 study_area[1,1]," ",study_area[2,2],",",
                                 study_area[1,2]," ",study_area[2,2],",",
                                 study_area[1,2]," ",study_area[2,1],",",
                                 study_area[1,1]," ",study_area[2,1],"))",sep=""),p4s= CRS)

# Then convert SpatialPointsDataFrame to a 'conventional' dataframe
Alt_df  <- data.frame(Alt_pts)
rm(Alt_pts, Alt)

# -- Import shapefile lakes
suppressWarnings(Lakes <- raster::shapefile("MAP_Data/grandlacs.shp"))
Lakes<-raster::crop(Lakes,study_area)
Lakes<-sf::st_as_sf(Lakes)

#Import shapefile white background
Mask <- suppressWarnings(raster::shapefile("MAP_Data/Mask_CH_Liechtenstein.shp"))
Mask<-raster::crop(Mask,study_area)
Mask<-sf::st_as_sf(Mask)

# --- Import shapefile Study area Polygon
suppressWarnings(Rcompartment <- raster::shapefile("MAP_Data/Wolfkomp_18_07_2015.shp"))
Rcompartment<-sf::st_as_sf(Rcompartment)

map<-ggplot2::ggplot() +
  #Alt Raster
  ggplot2::geom_raster(data = Alt_df , ggplot2::aes(x = x, y = y, alpha = CHHS))+ 
  ggplot2::scale_alpha(name = "", range = c(0.6, 0))+
  #Background white
  ggplot2::geom_sf(data=Mask,fill="white")+
  #Lakes
  ggplot2::geom_sf(data=Lakes,fill="#56B4E9")+
  #Reference Area all
  ggplot2::geom_sf(data=Rcompartment,col="darkblue",fill="deepskyblue1",lwd=1,alpha=0.1)

#--- add genetic sample
map<-map+ggplot2::geom_point(data=Table,
                             ggplot2::aes(x=x,y=y),
                             col="red", pch=19, cex=1.5)

#Theme
map<-map+ggplot2::theme(plot.title =ggplot2::element_blank(),
                        strip.background = ggplot2::element_blank(),
                        panel.background = ggplot2::element_blank(),
                        panel.grid = ggplot2::element_blank(),
                        axis.title =ggplot2::element_blank(),
                        axis.text = ggplot2::element_blank(),
                        axis.ticks= ggplot2::element_blank(),
                        legend.position="none",
                        panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=0.1))+
  ggplot2::scale_x_continuous(expand = c(0, 0)) +
  ggplot2::scale_y_continuous(expand = c(0, 0))



# --- Add KORA GIS Logo:
img <- png::readPNG("MAP_Data/KORAlogo.png")#KoraGis_transp
g <- grid::rasterGrob(img, interpolate=TRUE)

map<-map+
  ggplot2::annotation_custom(g, xmin=study_area[1]@bbox[1,2]-40000, xmax=study_area[1]@bbox[1,2],
                             ymin=study_area[1]@bbox[2,1], ymax=study_area[1]@bbox[2,1]+40000)


ggplot2::ggsave(paste(result.date,"/Plot_Genetic_Sample_",communication.date,".jpeg", sep=""),plot=map,
                units = "cm",
                width = 16.5,
                height = 10.3)





#------------ Create Actual email : ###########

# --- link to outlook ####
my_outlook <- get_personal_outlook()

# --- Email Type Species####

if(Email.Type=="species"){
  # --- Build up text fot the email
  
  subject.FR<-"Résultats des analyses génétiques d'espèces - KORA"
  subject.DE<-"Art-Resultate genetische Analysen - KORA"
  
  #body.DE####
  body.DE<-"
Sehr geehrte Damen und Herren

Die Art-Resultate der genetischen Analysen liegen vor: Sie finden die Resultate als Excel- Anhang. Sie können alle Nachweise auch auf dem MonitoringCenter https://www.koracenter.ch einsehen. 

Bitte zitieren Sie das Laboratoire de Biologie de la Conservation de l'Université de Lausanne in allfälligen Pressecommuniqués.

Mit freundlichen Grüssen

Ralph Manz"
  
  #body.FR####
  body.FR<-"
Mesdames et Messieurs, 

Les résultats des analyses génétiques d'espèces sont maintenant disponibles: Vous trouverez les résultats sous forme de pièce jointe Excel. Vous pouvez également consulter toutes les observations sur notre MonitoringCenter https://www.koracenter.ch . 

Merci de bien vouloir citer le Laboratoire de Biologie de la Conservation de l'Université de Lausanne dans les éventuels communiqués de presse relatant les résultats de la présente analyse. 

Avec mes meilleures salutations,

Ralph Manz"
  
  #body.FRDE####
  body.FRDE<-"
Mesdames et Messieurs, 

Les résultats des analyses génétiques d'espèces sont maintenant disponibles: Vous trouverez les résultats sous forme de pièce jointe Excel. Vous pouvez également consulter toutes les observations sur notre MonitoringCenter https://www.koracenter.ch . 

Merci de bien vouloir citer le Laboratoire de Biologie de la Conservation de l'Université de Lausanne dans les éventuels communiqués de presse relatant les résultats de la présente analyse. 

Avec mes meilleures salutations,

Ralph Manz

---------------------------------------------------

Sehr geehrte Damen und Herren

Die Art-Resultate der genetischen Analysen liegen vor: Sie finden die Resultate als Excel- Anhang. Sie können alle Nachweise auch auf dem MonitoringCenter https://www.koracenter.ch einsehen. 

Bitte zitieren Sie das Laboratoire de Biologie de la Conservation de l'Université de Lausanne in allfälligen Pressecommuniqués.

Mit freundlichen Grüssen

Ralph Manz
"
  
  # --- creat email for each compartment ####
  
  #i=1
  
  for(i in 1:length(unique(pts$Nummer))){
    
    if(is.na(unique(pts$Nummer)[i]))next
    
    #Email FR+DE
    if(unique(pts$Nummer)[i] %in% c("I","IV")){
      my_email <- my_outlook$create_email(body.FRDE,
                                          subject = paste(subject.FR," ",subject.DE," Komp-",unique(pts$Nummer)[i]," (",
                                                          paste(unique(Table[Table$Kompartiment_Compartiment==unique(pts$Nummer)[i],Kanton_Canton]),collapse = "/"),")", sep=""),
                                          to = c("l.legrand@kora.ch","i.moreno@kora.ch","r.manz@kora.ch"),send_now=FALSE)
      
      
      
      # attach table to the email
      file<-paste(result.date,"/w_genetic_compartment_",unique(pts$Nummer)[i],"_",communication.date,".xlsx",sep="")
      my_email$add_attachment(file)  
      
    }
    
    #Email DE
    if(unique(pts$Nummer)[i] %in% c("II","III","V")){
      my_email <- my_outlook$create_email(body.DE,
                                          subject = paste(subject.DE," Komp-",unique(pts$Nummer)[i]," (",
                                                          paste(unique(Table[Table$Kompartiment_Compartiment==unique(pts$Nummer)[i],Kanton_Canton]),collapse = "/"),")", sep=""),
                                          to = c("l.legrand@kora.ch","i.moreno@kora.ch","r.manz@kora.ch"),send_now=FALSE)
      
      # attach table to the email
      file<-paste(result.date,"/w_genetic_compartment_",unique(pts$Nummer)[i],"_",communication.date,".xlsx",sep="")
      my_email$add_attachment(file)  
      
    }
    
    
    # attach map to the email
    #my_email$add_attachment(paste(result.date,"/Plot_Genetic_Sample_",communication.date,".jpeg", sep=""))
    
    #Send the email
    my_email$send()
    
  }
}

# --- Email Type Genotype####
if(Email.Type=="genotype"){
  # --- Build up text fot the email
  
  subject.FR<-"Résultats des analyses génétiques des individus"
  subject.DE<-"Resultate genetische Individual Analysen"
  
  
  #body.DE####
  body.DE<-"
Sehr geehrte Damen und Herren

Die Resultate der genetischen Individual Analysen liegen vor: Sie finden die Resultate als Excel- Anhang.
Sie können alle Nachweise auch auf dem MonitoringCenter https://www.koracenter.ch/#/ einsehen. 

Die Elternschaftsanalysen können wir kommunizieren sobald diese vorliegen.

Bitte zitieren Sie das Laboratoire de Biologie de la Conservation de l'Université de Lausanne in allfälligen Pressecommuniqués.

Mit freundlichen Grüssen

Ralph Manz"
  
  #body.FR####
  body.FR<-"
Mesdames et Messieurs, 

Les résultats des analyses génétiques pour déterminer les individus sont disponibles: Vous trouverez les résultats sous forme de pièce jointe Excel.
Vous pouvez également consulter toutes les observations sur le MonitoringCenter https://www.koracenter.ch/#/ . 

Nous pourrons communiquer les analyses de parentées dès qu'elles seront disponibles.

Merci de bien vouloir citer le Laboratoire de Biologie de la Conservation de l'Université de Lausanne dans les éventuels communiqués de presse relatant les résultats de la présente analyse. 

Avec mes meilleures salutations,

Ralph Manz"
  
  #body.FRDE####
  body.FRDE<-"
Mesdames et Messieurs, 

Les résultats des analyses génétiques individuel sont disponibles: Vous trouverez les résultats sous forme de pièce jointe Excel.
Vous pouvez également consulter toutes les observation sur le MonitoringCenter https://www.koracenter.ch/#/ . 

Nous pourrons communiquer les analyses des parentées dès qu'elles seront disponibles.

Merci de bien vouloir citer le Laboratoire de Biologie de la Conservation de l'Université de Lausanne dans les éventuels communiqués de presse relatant les résultats de la présente analyse. 

Avec mes meilleures salutations,

Ralph Manz

---------------------------------------------------

Sehr geehrte Damen und Herren

Die Resultate der genetischen Individual Analysen liegen vor: Sie finden die Resultate als Excel- Anhang.
Sie können alle Nachweise auch auf dem MonitoringCenter https://www.koracenter.ch/#/ einsehen. 

Die Elternschaftsanalysen können wir kommunizieren sobald diese vorliegen.

Bitte zitieren Sie das Laboratoire de Biologie de la Conservation de l'Université de Lausanne in allfälligen Pressecommuniqués.

Mit freundlichen Grüssen

Ralph Manz
"
  
  # --- creat email for each compartment ####
  
  
  
  for(i in 1:length(unique(pts$Nummer))){
    
    if(is.na(unique(pts$Nummer)[i]))next
    
    #Email FR+DE
    if(unique(pts$Nummer)[i] %in% c("I","IV")){
      my_email <- my_outlook$create_email(body.FRDE,
                                          subject = paste(subject.FR," ",subject.DE," Komp-",unique(pts$Nummer)[i]," (",
                                                          paste(unique(Table[Table$Kompartiment_Compartiment==unique(pts$Nummer)[i],"Kanton_Canton"]),collapse = "/"),")", sep=""),
                                          to = c("l.legrand@kora.ch","i.moreno@kora.ch","r.manz@kora.ch"),send_now=FALSE)
      
      # attach table to the email
      file<-paste(result.date,"/w_genetic_compartment_",unique(pts$Nummer)[i],"_",communication.date,".xlsx",sep="")
      my_email$add_attachment(file)  
      
    }
    
    #Email DE
    if(unique(pts$Nummer)[i] %in% c("II","III","V")){
      my_email <- my_outlook$create_email(body.DE,
                                          subject = paste(subject.DE," Komp-",unique(pts$Nummer)[i]," (",
                                                          paste(unique(Table[Table$Kompartiment_Compartiment==unique(pts$Nummer)[i],"Kanton_Canton"]),collapse = "/"),")", sep=""),
                                          to = c("l.legrand@kora.ch","i.moreno@kora.ch","r.manz@kora.ch"),send_now=FALSE)
      
      # attach table to the email
      file<-paste(result.date,"/w_genetic_compartment_",unique(pts$Nummer)[i],"_",result.date,".xlsx",sep="")
      my_email$add_attachment(file)  
      
    }
    
    
    # attach map to the email
    #my_email$add_attachment(paste(result.date,"/Plot_Genetic_Sample_",communication.date,".jpeg", sep=""))
    
    #Send the email
    my_email$send()
    
  }
}






