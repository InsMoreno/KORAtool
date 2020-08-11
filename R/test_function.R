#' Test Function
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
