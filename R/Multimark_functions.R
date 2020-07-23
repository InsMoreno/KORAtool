

#' Multimark Model Closed Sometimes (Table Maker)
#'
#' @return csv table
#' @export
MltiM.Closed.Sometimes.TM<-function(
  KORA.Photo.Output,
  Lynx.Master.Output,
  Period.day,
  simultaneous.minute
){
  # ---- Import Data ####

  # Import KORA Photo data
  Lynx<-data.table::fread(KORA.Photo.Output, select=c(
    "exposure_date",
    "exposure_time",
    "session_study_start_date",
    "session_study_start_time",
    "session_study_end_date",
    "session_study_end_time",
    "id_individual",
    "id_flank"))

  # Import KORA Mother Info
  Mother<-data.table::fread(Lynx.Master.Output, select=c("mother","fofaID","yearOfBirth"))

  # ---- Info to Create Empty Matrix ####

  # Start and Stop Session
  Start<-as.POSIXct(paste(Lynx[1,"session_study_start_date"],
                          Lynx[1,"session_study_start_time"],sep=" "),
                    format= '%Y-%m-%d %H:%M:%S')

  Stop<-as.POSIXct(paste(Lynx[1,"session_study_end_date"],
                         Lynx[1,"session_study_end_time"],sep=" "),
                   format= '%Y-%m-%d %H:%M:%S')

  # Remove unused columns in Lynx
  Lynx$session_study_start_date<-NULL
  Lynx$session_study_start_time<-NULL
  Lynx$session_study_end_date<-NULL
  Lynx$session_study_end_time<-NULL

  # Format Date-Time in Lynx
  Lynx$Time<-as.POSIXct(paste(Lynx$exposure_date,Lynx$exposure_time,sep=" "),
                        format= '%Y-%m-%d %H:%M:%S')

  # Remove unused columns in Lynx
  Lynx$exposure_date<-NULL
  Lynx$exposure_time<-NULL

  # ---- Merge Mother Info to Lynx Table ####

  Lynx<-merge(Lynx, Mother, by.x= "id_individual", by.y="fofaID", all.x=TRUE)

  # ---- Clean Lynx Table ####

  # Remove unidentified individual "U"

  Lynx<-Lynx[Lynx$id_individual != "U",]

  # Remove juvenile without a known mother

  Lynx[Lynx$yearOfBirth >= year(Start) &
         Lynx$mother== "","mother"]<-"UnknownMother"
  Lynx<-Lynx[Lynx$mother!="UnknownMother",]

  # ---- Create Empty Occurrence Matrix  ####
  Period = Period.day #Period in days
  simultaneous = simultaneous.minute #minutes

  Occ.table<-data.table(Period = seq(from = Start,
                                     to = Stop, by = paste(Period,"days", sep=" ")))

  Occ.table[,c(unique(Lynx$id_individual)):= 0]

  # ---- Fill in Matrix
  #type 0 (Non-detection)
  #type 1 left (L individual)
  #type 2 right (R individual)
  #type 3 right or left only (for a B individual)
  #type 4 right and left simultaneously within sampling occasion (for a B individual)
  for(j in 2:length(Occ.table)){

    for(i in 1:length(Occ.table$Period)){

      Lynx_loop<- Lynx[Lynx$Time > Occ.table[i,Period] &
                         Lynx$Time < Occ.table[i,Period]+ Period*86400 & #Indicate days
                         Lynx$id_individual == names(Occ.table)[j],c("id_flank","Time")]

      sides_seen<-as.numeric(Lynx_loop$id_flank)

      if(length(sides_seen)> 0){
        if(1 %in% sides_seen & 2 %in% sides_seen){

          #Both seen:

          #At the same time? (within simultaneous)

          x <- paste(Lynx_loop$id_flank,Lynx_loop$Time, sep= ";")
          y <- paste(Lynx_loop$id_flank,Lynx_loop$Time, sep= ";")

          dt <- as.data.table(expand.grid(x = x, y = y))

          dt[, c("Flank", "Time") := tstrsplit(x, ";", fixed=TRUE)]
          dt[, c("Flanky", "Timey") := tstrsplit(y, ";", fixed=TRUE)]
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
  # ---- Mother - Juvenile Relation ####
  # If mother not seen but Juvenile (of the year) yes: Juvenile counts as mother

  Occ.table<-as.data.frame(Occ.table)
  Lynx[Lynx$mother=="","mother"]<-NA
  U.Lynx<-unique(Lynx[,c("id_individual","yearOfBirth","mother")])
  mothers<-unique(U.Lynx[U.Lynx$yearOfBirth==year(Start),mother])
  Juv<-unique(U.Lynx[U.Lynx$mother %in% mothers,id_individual])


  if(length(mothers)>0){

    for(m in 1:length(mothers)) {

      #juvenile of the mother
      juv=unique(Lynx[Lynx$mother==mothers[m],id_individual])

      #add mother actual value if available (Or all 0 if not)
      if(mothers[m] %in% names(Occ.table)){

        Occ.table[,paste(mothers[m],"Juv",sep="_")]<-Occ.table[,mothers[m]]

      }else{Occ.table[,paste(mothers[m],"Juv",sep="_")]<-0}

      #if 0 but juvenile seen -> replace by juvenile value for the line
      for(m1 in 1:length(Occ.table$Period)){
        if(Occ.table[m1,paste(mothers[m],"Juv",sep="_")]==0){

          Occ.table[m1,paste(mothers[m],"Juv",sep="_")]<-max(Occ.table[as.numeric(m1),juv])
        }
      }


    }
  }
  # ---- Reverse Table ####

  Occ.table<-data.table(dcast(melt(Occ.table, id.vars = "Period"), variable ~ Period))

  # ---- Add Category Info ####
  names(Occ.table)[1]<-"ID"


  Occ.table[ID %in% mothers,"Category"]<-"mother"
  Occ.table[ID %in% paste(mothers,"_Juv", sep=""),"Category"]<-"mother with Juvenile Info"
  Occ.table[ID %in% Juv,"Category"]<-"juvenile of the year with seen mother"
  U.Lynx[U.Lynx$yearOfBirth=="","yearOfBirth"]<-NA
  Occ.table[is.na(Occ.table$Category),"Category"]<-paste("YOB",U.Lynx[U.Lynx$id_individual %in% Occ.table[is.na(Occ.table$Category),ID],yearOfBirth])

  # ---- Write CSV ####
  write.csv(Occ.table, "Input_Table_Multimark_Closed_sometimes.csv")
}




























