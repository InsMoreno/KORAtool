#' Input Table for multimarkClosedSCR models function
#'
#' @param KORA.Photo.Output 
#' @param Period.day 
#' @param simultaneous.minute 
#'
#' @return
#' @export
#'
#' @examples
multimarkClosedSCR.sometimes.Input.Table<-function(
  KORA.Photo.Output,
  Period.day,
  simultaneous.minute,
  UnwantedID,
  Species,
  Random,
  Name.Matrix
){
  # ---- Import Data ####
  
  # Import KORA Photo data
  Data<-data.table::fread(KORA.Photo.Output, select=c(
    "exposure_date",
    "exposure_time",
    "session_study_start_date",
    "session_study_start_time",
    "session_study_end_date",
    "session_study_end_time",
    "id_individual",
    "id_flank","x","y","site_name","film_id","animal_species"),encoding = "Latin-1")
  

  # ---- Create Encounter Matrix ####
  
  #A matrix containing the observed encounter histories with rows corresponding
  #to individuals and (ntraps*noccas) columns corresponding to traps and sampling
  #occasions. The first noccas columns correspond to trap 1, the second
  #noccas columns corresopond to trap 2, etc. Ignored unless mms=NULL.
  
  # Start and Stop Session
  
  Start<-Start.Session
  
  Stop<-Stop.Session
  
  site_name<-sort(unique(Data$site_name))
  
  #keep only species of interest
  Data<-Data[Data$animal_species==Species]
  
  # Remove unused columns in Data
  Data$session_study_start_date<-NULL
  Data$session_study_start_time<-NULL
  Data$session_study_end_date<-NULL
  Data$session_study_end_time<-NULL
  Data$animal_species<-NULL
  
  
  # Format Date-Time in Data
  Data$Time<-as.POSIXct(paste(Data$exposure_date,Data$exposure_time,sep=" "),
                        format= '%Y-%m-%d %H:%M:%S')
  
  # Remove unused columns in Data
  Data$exposure_date<-NULL
  Data$exposure_time<-NULL
  
  # ---- Clean  Table 
  
  # Remove unwanted individuals
  if(exists("UnwantedID")){
    Data<-Data[Data$id_individual %in%  UnwantedID == FALSE,]
  }
  

  
  # ---- Create Empty Occurrence Matrix  
  Period = Period.day #Period in days
  simultaneous = simultaneous.minute #minutes
  
  
  occasions<-seq(from = Start,to = Stop, by = paste(Period,"days", sep=" "))
  
  Occ.table<-data.table::data.table(Period = rep(occasions,each=length(site_name)),
                                    Site = rep(site_name,length(occasions)))
  
  Occ.table[,c(unique(Data$id_individual)):= 0]
  
  # ---- Fill in Matrix
  #type 0 (Non-detection)
  #type 1 corresponds to patch patterns on the left side (L or B individual)
  #type 2 corresponds to patterns on the right side (R or B individual)
  #type 3 indicates a non simultaneous type 1 and type 2 encounter during one sampling occasion (for a B individual)
  #type 4 indicates a simultaneous type 1 and type 2 encounter during one sampling occasion (for a B individual)

  for(j in 3:length(Occ.table)){ #ID
    
    for(i in 1:length(Occ.table$Period)){ #Period
      
      Data_loop<- Data[Data$Time > Occ.table[i,Period] &
                         Data$Time < Occ.table[i,Period]+ Period*86400 & #select days occasion
                         Data$id_individual == names(Occ.table)[j] & # select individuals
                         Data$site_name == Occ.table[i,Site],  # select site
                          c("id_flank","Time","film_id")]
      
      ###### Random selection:
      if(exists("Random")){
        if(Random==TRUE){
          chooseside<-Data_loop$film_id
          if(length(Data_loop$film_id) != 0){
            Data_loop<-Data_loop[Data_loop$film_id==sample(chooseside, 1, replace=TRUE),]
          }
        }
      }
    
      ###### Fin selection Random
      
      sides_seen<-as.numeric(Data_loop$id_flank)
      
      if(length(sides_seen)> 0){
        if(1 %in% sides_seen & 2 %in% sides_seen){
          
          #Both seen:
          
          #At the same time? (within simultaneous)
          
          x <- paste(Data_loop$id_flank,Data_loop$Time, sep= ";")
          y <- paste(Data_loop$id_flank,Data_loop$Time, sep= ";")
          
          dt <- data.table::as.data.table(expand.grid(x = x, y = y))
          
          dt[, c("Flank", "Time") := data.table::tstrsplit(x, ";", fixed=TRUE)]
          dt[, c("Flanky", "Timey") := data.table::tstrsplit(y, ";", fixed=TRUE)]
          dt$x<-NULL
          dt$y<-NULL
          dt$Compare<-as.numeric(dt$Flank)!=as.numeric(dt$Flanky)
          dt<-dt[dt$Compare==TRUE,]
          
          
          if(min(abs(difftime(dt$Time,dt$Timey,units = "mins"))) < simultaneous){
            Occ.table[i,j]<-4
          }else{Occ.table[i,j]<-3}
        }else{
          if(1 %in% sides_seen){
            Occ.table[i,j]<-1
          }else{
            Occ.table[i,j]<-2
          }
        }
      }else{Occ.table[i,j]<-0}
      
    }}

  
  # ---- Create trapCoords Matrix
  
  #A matrix of dimension ntraps x (2 + noccas) indicating the Cartesian coordinates
  #and operating occasions for the traps, where rows correspond to trap,
  #the first column the x-coordinate (“x”), and the second column the y-coordinate
  #(“y”). The last noccas columns indicate whether or not the trap was operating
  #on each of the occasions, where ‘1’ indciates the trap was operating and ‘0’
  #indicates the trap was not operating. Ignored unless mms=NULL.
  
  trapCoords<-data.table::data.table(Site = site_name)
  trapCoords[,c(as.character(unique(Occ.table$Period))):= 1]
  utils::write.csv(trapCoords,"trapCoords.csv")
  
  
  # ---- Reverse TEncounter Matrix
  Occ.table<-cbind(paste(Occ.table$Period,Occ.table$Site),Occ.table)
  Occ.table$Period<-NULL
  Occ.table$Site<-NULL
  names(Occ.table)[1]<-"P.Site"
  
  
  Occ.table<-data.table::data.table(data.table::dcast(data.table::melt(Occ.table, id.vars = "P.Site"), variable ~ P.Site))
  
  # ---- Add Category Info 
  names(Occ.table)[1]<-"ID"

  # ---- Write CSV 
  utils::write.csv(Occ.table, Name.Matrix)
  
  #studyArea is a 3-column matrix containing the coordinates for the centroids of a contiguous
  #grid of cells that define the study area and available habitat. Each row corresponds
  #to a grid cell. The first 2 columns (“x” and “y”) indicate the Cartesian
  #x- and y-coordinate for the centroid of each grid cell, and the third column
  #(“avail”) indicates whether the cell is available habitat (=1) or not (=0). All cells
  #must be square and have the same resolution. If studyArea=NULL (the default)
  #and mms=NULL, then a square study area grid composed of ncells cells of available
  #habitat is drawn around the bounding box of trapCoords based on buffer.
  #Ignored unless mms=NULL. Note that rows should be ordered in raster cell order
  #(raster cell numbers start at 1 in the upper left corner, and increase from left to
  # right, and then from top to bottom).
  
  
  
}
#' @examples
#' multimarkClosedSCR.Input.Table(KORA.Photo.Output = "Output1.csv",
#' Period.day = 3, simultaneous.minute = 30,UnwantedID= c("VD2020U"))



  
