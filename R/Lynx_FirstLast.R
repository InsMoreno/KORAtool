#' Lynx_FirstLast
#'
#' @param LynxMaster 
#' @param LynxObs 
#' @param PhotoPredator 
#'
#' @return
#' @export
#'
#' @examples
Lynx_FirstLast<-function(LynxMaster=LynxMaster,
                         LynxObs=LynxObs,
                         PhotoPredator=PhotoPredator){
 #Import Raw Data (CSV from KORA Data): 
  LynxMaster<-data.table::fread(LynxMaster)
  LynxObs<-data.table::fread(LynxObs,select=c("lynx_ID","image_uid","date"))
  PhotoPredator<-data.table::fread(PhotoPredator,select=c("id_individual","exposure_date","session_name"))
#Select interesting Columns:  
  LynxMaster<-LynxMaster[,c("name","coatPattern","yearOfBirth","deathDate","mother")]
#Fill in info First/Last seen: 
 for(i in 1:length(LynxMaster$name)){
   LynxMaster[i,"FirstSeen"]<-suppressWarnings(min(min(as.POSIXct(LynxObs[LynxObs$lynx_ID==as.character(LynxMaster[i,"name"]),"date"],format='%Y-%m-%d',tz="")),
                                   min(as.POSIXct(PhotoPredator[PhotoPredator$id_individual==as.character(LynxMaster[i,"name"]),"exposure_date"],format='%Y-%m-%d'))))
   LynxMaster[i,"LastSeen"]<- suppressWarnings(max(max(as.POSIXct(LynxObs[LynxObs$lynx_ID==as.character(LynxMaster[i,"name"]),"date"],format='%Y-%m-%d',tz="")),
                                   max(as.POSIXct(PhotoPredator[PhotoPredator$id_individual==as.character(LynxMaster[i,"name"]),"exposure_date"],format='%Y-%m-%d'))))
}
#Call Table
LynxMaster
  
}
