#' KORAMonitoringBericht
#'
#' @param effective.TN 
#' @param potentially.TN 
#' @param Title 
#' @param Language 
#' @param Authors 
#' @param KORAphoto 
#' @param Lynxmaster 
#' @param Einlesung_Luchs 
#' @param Compartment 
#' @param favorable 
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
  Einlesung_Luchs,
  Compartment,
  favorable){
  
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
                                    #Lynx Einlesung Luchs csv name:
                                    Einlesung_Luchs= Einlesung_Luchs,
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
                                    #Suitable habitat (km2):
                                    favorable= favorable))
  
 }
  
