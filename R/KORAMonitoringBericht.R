#' KORAMonitoringBericht
#'
#' @param effective.TN 
#' @param potentially.TN 
#' @param Title 
#' @param Language 
#' @param Authors 
#' @param KORAphoto 
#' @param Lynxmaster 
#' @param PhotoPredator
#' @param LynxObs
#' @param Compartment 
#' @param Refarea
#' @param favorable 
#' @param Period.selected
#'
#' @return
#' @export
#'
#' @examples
#' 
KORAMonitoringBericht<-function(
  effective.TN,
  potentially.TN,
  Title,
  Language,
  Authors,
  KORAphoto,
  Lynxmaster,
  PhotoPredator,
  LynxObs,
  Compartment,
  Refarea,
  favorable,
  Period.selected){
  
    rmarkdown::render(input = "KORA_Report_R.Rmd", 
                      output_file = "KORA_Report_R_1.docx",
                      params = list(#Trap night calendar information:
                                    effective.TN= effective.TN,
                                    potentially.TN= potentially.TN,
                                    #Title:
                                    Title= Title,
                                    #Language: (FR,DE)
                                    Language= Language,
                                    #Authors: ex."Luc Le Grand, Florin Kunz, Fridolin Zimmermann"
                                    Authors= Authors,
                                    #KORAphoto data csv name:
                                    KORAphoto= KORAphoto,
                                    #Lynx master data csv name:
                                    Lynxmaster= Lynxmaster,
                                    #Photo Predator csv name:
                                    PhotoPredator= PhotoPredator,
                                    #Lynx Observation CSV name:
                                    LynxObs= LynxObs,
                                    #Compartment: 
                                    #1:"Nordostschweiz"
                                    #2:"Misox (Mesolcina)"
                                    #3:"Tessin"
                                    #4:"Surselva"
                                    #5:"Zentralschweiz Ost"
                                    #6:"Rhone-Nord"
                                    #7:"Unterwallisüd"
                                    #8:"Simme-Saane"
                                    #9:"Engadin"
                                    #10:"Jura Nord"
                                    #11:"Jura Süd"
                                    #12:"Mittelbünden"
                                    #13:"Zentralschweiz West"
                                    #14:"Zentralschweiz Mitte"
                                    #15:"Berner Oberland Ost"
                                    #16:"Oberwallis")
                                    Compartment= Compartment,
                                    #Sutdy Area (Reference Area):
                                    #1 Surselva    V c 
                                    #2 Sud du Bas Valais   IV d 
                                    #3 Haut-Valais   IV e 
                                    #4 Simme-Saane   IV a 
                                    #5 Nord-est de la Suisse     II  
                                    #6 Val Mesolcina-Sud du Tessin    V b  
                                    #7 Est de la Suisse centrale  III c 
                                    #8 Centre de la Suisse centrale  III b 
                                    #9 Engadine    V e 
                                    #10 Tessin    V a 
                                    #11 Sud du Jura    I a  
                                    #12 Nor due Rhône   IV c
                                    #13  Ouest de la Suisse centrale  III a  
                                    #14  Nord du Jura    I b  
                                    #15  Centre des Grisons    V d 
                                    #16  Est de l'Oberland Bernois   IV b   
                                    Refarea = Refarea,
                                    #Suitable habitat (km2):
                                    favorable= favorable),
                                    #Period length selected (3 other 5 days)
                                    Period.selected= Period.selected)
 
 }
  
