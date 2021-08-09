#' Event Counting
#'
#'
#'This function creates a table with the number of event for a given species.
#'Input needed:
#'    - KORA.Photo.Output = "Nameofcsv.csv" KORA ouput with lynx or any other species observed (All have been IDed)
#'    Important Note:
#'    - The csv must be in the folder of the used RStudio Project
#'    - Date in the csv must be in '%Y-%m-%d %H:%M:%S' (Standard KORA data and R format)
#'
#' @param # Author: Luc Le Grand
#' @return
#' @export
EvCount<-function(
  KORA.Photo.Output,
  Event_length,
  species
){

  # ---- Import Data ####

  # Import KORA Photo data
  M.Table<-data.table::fread(KORA.Photo.Output, select=c(
    "animal_species",
    "exposure_date",
    "exposure_time",
    "id_individual",
    "id_flank",
    "x","y"))

  # ---- Arange Master Table ####

  #Time/Date info
  M.Table$TIME <- as.POSIXct(paste(M.Table$exposure_date,M.Table$exposure_time ,sep=" "), format= '%Y-%m-%d %H:%M:%S')
  M.Table$XY<-paste(M.Table$x,M.Table$y, sep=";")

  # ---- Create output table ####
  Table<-data.table::data.table(ID=unique(M.Table[M.Table$animal_species==species,id_individual]), Event=0)

  # Count number of events/N pictures

  for(i in 1:length(Table$ID)){
    df<-data.table::data.table(timestamp = M.Table[M.Table$animal_species==species &
                                                     M.Table$id_individual == Table[i, ID],TIME],
                               XY =M.Table[M.Table$animal_species==species &
                                             M.Table$id_individual == Table[i, ID],XY])
    df<-df[, .(
      startTime = timestamp[1L],
      endTime = timestamp[.N],
      sensorCount = .N,
      duration = difftime(timestamp[.N], timestamp[1L], units = "mins")
    ),
    by = .(XY,
           cumsum(difftime(timestamp, data.table::shift(timestamp, fill = timestamp[1L]), "mins") > Event_length))]

    Table[i, "Event"]<-length(df$cumsum)
    Table[i,"N pictures"]<-sum(df$sensorCount)
    Table[i,"N site"]<-length(unique(df$XY))
  }

  # creates the "Total" row
  Table<-rbind(Table,data.table::data.table(ID = "Total", t(colSums(Table[, -1]))))

  # Total site should be the total of unique site
  Table[Table$ID=="Total","N site"]<-length(unique(M.Table[M.Table$animal_species==species,XY]))

  # ---- Print ouput ####
  print(Table)
  warning("Total N site is equal to the total number of UNIQUE site where the species has been seen")
}
