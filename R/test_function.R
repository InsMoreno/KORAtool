#' Test Function
#'
#'#'This function creates a table with the number of event for a given species.
#'Input needed:
#'    - KORA.Photo.Output = "Nameofcsv.csv" KORA ouput with lynx or any other species observed (All have been IDed)
#'    Important Note:
#'    - The csv must be in the folder of the used RStudio Project
#'    - Date in the csv must be in '%Y-%m-%d %H:%M:%S' (Standard KORA data and R format)
#'
#' @param # Author: Luc Le Grand
#'
#' @return
#' @export
test.function<-function(
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

  data.table::is.data.table(M.Table)

}
