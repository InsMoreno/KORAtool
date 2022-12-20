#   ---   Wolfpost script  ---     

#--------------------------------------- Ralph Manz Info ----- Choose Result Date and Email type: ####
result.date<-"2022-12-08" #Date quand Ines met dans la base de donnée (Result_date)
Email.Type<-"genotype" #"species" "genotype"
communication.date<-"2022-12-08"


#------------ Library: ####
library(Microsoft365R)
library(data.table)
library(xlsx)
library(readxl)
library(stringr)

#------------ Merge KORA OS with KORA Report ####

# --- Import raw data ####

# - Info LBC ####
LBC<- as.data.table(read_excel("Input/LBC.xlsx"))
# - Info wold dead  ####
w.dead<-fread("Input/wolf_dead.csv")
# - Data KORA Report ####

event<-fread("Input/event_data.csv", encoding = 'UTF-8')
obs<-fread("Input/observation_data.csv", encoding = 'UTF-8')
obs$obs_creation_date <- as.Date.POSIXct(obs$obs_creation_date)
sample<-fread("Input/sample_data.csv", encoding = 'UTF-8')
# - Data KORA os supervisor ####
if(Email.Type == "genotype"){
  Result_by_genotype<-fread("Input/Result_by_genotype.csv",encoding = 'Latin-1')
  Table<-Result_by_genotype
  Table$Zustand_Probe_Etat_Echantillon<-NULL
  Table$V18<-NULL
  Table$Funddatum_Date_Decouverte<-as.Date(Table$Funddatum_Date_Decouverte, format="%Y-%m-%d")
}

if(Email.Type == "species"){
  Result_by_date<-fread("Input/Result_by_date.csv",  encoding = 'Latin-1')
  Table<-Result_by_date
  Table$Zustand_Probe_Etat_Echantillon<-NULL
  Table$V18<-NULL
  Table$Funddatum_Date_Decouverte<-as.Date(Table$Funddatum_Date_Decouverte, format="%Y-%m-%d")
}


# --- Merge KORA Report data needed ####
M.table<-merge(sample[,c("sample_key_official",
                         "fk_evt_uid", "sample_key",
                         "sample_type", "sample_result_species","sample_result_individual")],
               event[,c("evt_uid","evt_x","evt_y", "evt_canton","evt_field_name","evt_observer","evt_reporter", "evt_observer_type")], 
               by.x="fk_evt_uid", 
               by.y="evt_uid")

M.table<-merge(M.table,obs[,c("obs_kill_species","fk_evt_uid","obs_creation_date")],
               by.x="fk_evt_uid", by.y="fk_evt_uid",all.x=TRUE)


M.table$evt_reporter <- gsub(',','',M.table$evt_reporter)
M.table$evt_observer <- gsub(',','',M.table$evt_observer)
M.table$Name_Finder_Nom_Trouveur <- str_c(M.table$evt_reporter, "_", M.table$evt_observer)
M.table <- M.table[,-c(1,11,12)]

# il manque la commune, le result 2 et la provenance !!!!  donc on créé des colonnes vide en attendant
M.table$Gemeinde_Commune <- NA
M.table$Art_2_Espece_2 <- NA
M.table$Herkunft_Provenance <- NA

# --- Set column order ####
M.table <- M.table[, c(1,12 ,8 ,14 ,9,6,7,13,10, 2,11,3,4,15,16,5 )]

colnames(M.table) <- c("ID_Genetics" ,               
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
                       "Herkunft_Provenance",       
                       "Wolf_ID_Loup_ID")

# set date format

M.table$Funddatum_Date_Decouverte<-as.Date(M.table$Funddatum_Date_Decouverte, format="%Y-%m-%d")

# --- Bind all data ####

if(Email.Type == "genotype"){
  M.table <- rbind(Table,M.table)
}

if(Email.Type == "species"){
  M.table <- rbind(Table,M.table)
}

#------------ Projection  ####
#Projection to be used for the map (CH1903 / LV03):
#Warnings OK
suppressWarnings(CRS<- sp::CRS("+init=epsg:21781"))





#------------ Create Dir. Output ####
dir.create(paste("./",result.date,sep=""), recursive = TRUE)

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
suppressWarnings(Table[Table$Wolf_ID_Loup_ID %in% w.dead[,wolf] & Table$Wolf_ID_Loup_ID!="", c("Flurname_Location","x","y")]<-"")

suppressWarnings(Table[Table$ID_Genetics %in% c("WCH-09668", "WCH-09669"), c("Flurname_Location","x","y")]<-"")

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

# --- Replace translation in original table:

Table$Fundort_Origine <-Table.T$Fundort_Origine
Table$Typ_Probe_Type_Echantillon<-Table.T$Typ_Probe_Type_Echantillon
Table$Art_1_Espece_1<-Table.T$Art_1_Espece_1
Table$Art_2_Espece_2<-Table.T$Art_2_Espece_2

#Add info New individual
Table[Table$Wolf_ID_Loup_ID %in% unique(LBC[LBC$Result=="new individual",ID_KORA]),"Info"]<-"Individuum erstmals in der Schweiz genetischnachgewiesene./Individu génétiquement identifié pour la première fois."

#------------ Export as excel Table ####


for(i in 1:length(unique(pts$Nummer))){
  
  file<-paste(result.date,"/w_genetic_compartment_",unique(pts$Nummer)[i],"_",communication.date,".xlsx",sep="")
  
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
    Alignment(wrapText=TRUE, horizontal="ALIGN_CENTER") +
    Border(color="black", position=c("TOP", "BOTTOM"), 
           pen=c("BORDER_THIN", "BORDER_THICK")) 
  
  # ---- Date as text
  
  Data.xlsx$Funddatum_Date_Decouverte<-as.character(Data.xlsx$Funddatum_Date_Decouverte)
  # ---- Insert data into workbook
  addDataFrame(Data.xlsx,
               sheet, startRow=1, startColumn=1,
               colnamesStyle = TABLE_COLNAMES_STYLE)
  
  # ---- Change column width to auto
  autoSizeColumn(sheet, colIndex=c(1:ncol(Table)))
  
  # ---- Center justify table
  cs <- CellStyle(wb) + Alignment(horizontal = "ALIGN_CENTER")
  all.rows <- getRows(sheet, rowIndex = 2:nrow(Table))
  all.cells <- getCells(all.rows)
  invisible(lapply(all.cells, setCellStyle, cs))
  
  # ---- Red color if new individual:
  New_individual_style <- CellStyle(wb) + Font(wb,color="red", isBold=TRUE)+
    Alignment(horizontal = "ALIGN_CENTER")
  apply <- getRows(sheet, rowIndex = which(!is.na(Data.xlsx$Info))+1)
  apply <- getCells(apply,colIndex=c(16,19))
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






