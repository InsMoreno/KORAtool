#' KORAmapopp
#'
#' @param LynxObs 
#' @param Start 
#' @param Stop 
#' @param Compartment 
#' @param Refarea 
#' @param IDremove
#' @param Buffer.polygon
#'
#' @return
#' @export
#'
#' @examples
KORAmapopp<-function(LynxObs,Start,Stop,Compartment,Refarea,IDremove,Buffer.polygon){
  
  # ------ Projection : ####
  #Projection to be used for the map (CH1903 / LV03):
  #Warnings OK
  suppressWarnings(CRS<- sp::CRS("+init=epsg:21781"))
  
  # Import Lynx Observation Data
  Data.opp<-data.table::fread(LynxObs,select=(c("lynx_ID","date","type","x","y","canton","SCALP")))
  Data.opp$date<-as.POSIXct(Data.opp$date,  format= '%Y-%m-%d' )
  
  #Subset data Session window
  Data.opp<-Data.opp[Data.opp$date>Start & Data.opp$date<Stop,]
  
  #Observations as spatial points
  pts <- sf::st_as_sf(Data.opp, coords = c("x","y"))
  pts<-sf::st_set_crs(pts, 21781) 
  
  #Import shapefile compartment
  Komp <- sf::read_sf(dsn = "MAP_Data/Luchs_Komp_21_07_2015_new.shp")
  
  # --- Import shapefile Study area Polygon
  suppressWarnings(Rcompartment <- raster::shapefile("MAP_Data/Reference_areas_alln.shp"))
  Rcompartment<-Rcompartment[Refarea,]
  Rcompartment<-sf::st_as_sf(Rcompartment)
  
  #Subset observation within the compartment
  #Warnings ok
  suppressWarnings(pts<-sf::st_intersection(pts,Rcompartment))
  
  # -- Study Area ####
  study_area<-sp::bbox(as.matrix(Komp$geometry[Compartment][[1]]))
  
  study_area<-rgeos::readWKT(paste("POLYGON((",
                                   study_area[1,1]," ",study_area[2,1],",",
                                   study_area[1,1]," ",study_area[2,2],",",
                                   study_area[1,2]," ",study_area[2,2],",",
                                   study_area[1,2]," ",study_area[2,1],",",
                                   study_area[1,1]," ",study_area[2,1],"))",sep=""),p4s= CRS)
    
  # -- Altitude layer #
  Alt <- raster::raster("MAP_Data/CHHS.TIF")
  # indicate CRS
  raster::crs(Alt) <- CRS 
  
  # crop 
  Alt<-raster::crop(Alt,study_area)
  
  # convert to a df for plotting in two steps:
  # First, to a SpatialPointsDataFrame
  Alt_pts <- raster::rasterToPoints(Alt, spatial = TRUE)
  # Then to a 'conventional' dataframe
  Alt_df  <- data.frame(Alt_pts)
  rm(Alt_pts, Alt)
  
  # -- Import shapefile lakes
  suppressWarnings(Lakes <- raster::shapefile("MAP_Data/grandlacs.shp"))
  Lakes<-raster::crop(Lakes,study_area)
  Lakes<-sf::st_as_sf(Lakes)
 
  # --- Start maping
  map<-ggplot2::ggplot() +
    #Alt Raster
    ggplot2::geom_raster(data = Alt_df , ggplot2::aes(x = x, y = y, alpha = CHHS))+
    ggplot2::scale_alpha(name = "", range = c(0.6, 0))+
    #Polygon Compartment
    ggplot2::geom_sf(data=Komp$geometry[Compartment],fill="white", alpha = 0.4)+
    #Lakes
    ggplot2::geom_sf(data=Lakes,fill="#56B4E9")+
    #Reference Area
    ggplot2::geom_sf(data=Rcompartment,col="#009E73",fill=NA,lwd=1.1)
  
  # -- Add Points where U were seen

  #Subset "U"
  pts<- pts %>% dplyr::filter(lynx_ID=="U")
  #Add Points
  map<-map+ggplot2::geom_sf(data = pts,col="#D55E00",size=3)
 
  # --- Add the polygons and labels: ####
  
  # -- Create Table
  ID.names<-data.table::data.table(ID=pts$lynx_ID)
  ID.names<-unique(ID.names)
  
  # -- remove unwanted ID
  if(exists("IDremove")){ID.names<-ID.names[!grepl(paste(IDremove,collapse = "|"),ID.names$ID),]}
  
  # -- Indicate colors
  #(used https://www.datanovia.com/en/blog/how-to-stimulate-colorblindness-vision-in-r-figures/)
  
  # If not enough colors, will repeat the 8 colors
  ID.names$col<-rep_len(c("#000000", "#E69F00",
                          "#56B4E9", "#009E73",
                          "#F0E442", "#0072B2",
                          "#D55E00", "#CC79A7"),length.out=length(ID.names$ID))
  # -- Compute the polygons
  sp_poly.all <- vector(mode = "list")
  
  if(length(ID.names$ID)>0){
  
    
  suppressWarnings(
    for(i in 1:length(ID.names$ID)){
      dat <- Data.opp[Data.opp$lynx_ID==ID.names[i,ID],c("x","y")] #if in github ID should be in "ID"
      
      ch <- grDevices::chull(dat)
      coords<-dat[c(ch, ch[1]), ]  # closed polygon
      
      sp_poly <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(coords)), ID=ID.names[i,"ID"])))
      raster::crs(sp_poly)<-CRS
      
      sp_poly<-rgeos::gBuffer(sp_poly,width=Buffer.polygon)
      raster::crs(sp_poly)<-CRS
      sp_poly.all[i]<-sp_poly
      
    })
    
  # -- Data labels
  data_labels<-data.frame()
  for(i in 1:length(ID.names$ID)){
    
    #Label origin to be closest from the side
    diff.left<-min(sp_poly.all[[i]]@polygons[[1]]@Polygons[[1]]@coords[,1])-sp::bbox(study_area)[1,1]
    diff.right<-sp::bbox(study_area)[1,2]-max(sp_poly.all[[i]]@polygons[[1]]@Polygons[[1]]@coords[,1])
    
    if(diff.left>diff.right){ #if label should be on the right
      
      df<-data.frame(sp_poly.all[[i]]@polygons[[1]]@Polygons[[1]]@coords)
      data_labels[i, "lon"]<-df[which.max(df$x),1]
      data_labels[i, "lat"]<-df[which.max(df$x),2]
      data_labels[i, "side"]<-"right"
      
    }else{ #label should be on the left
      
      df<-data.frame(sp_poly.all[[i]]@polygons[[1]]@Polygons[[1]]@coords)
      data_labels[i, "lon"]<-df[which.min(df$x),1]
      data_labels[i, "lat"]<-df[which.min(df$x),2]
      data_labels[i, "side"]<-"left"
    }
    

    data_labels[i, "ID"]<- ID.names[i,ID]
    data_labels[i, "col.ID"]<- ID.names[i,col]
  }
  
  # -- Add to the map
  
  for(i in 1:length(ID.names$ID)){
    
    map<-map+
      ggplot2::geom_polygon(data = sp_poly.all[[i]], ggplot2::aes(x=long, y=lat, group = group),
                            colour=ID.names[i,col], fill=NA, alpha=1, lwd=1.5)
  }
  
  # -- Add the labels:
  map<-map+
    ggrepel::geom_text_repel(data = data_labels, ggplot2::aes(lon, lat, label = ID),
                             colour = data_labels[,"col.ID"],cex=4,
                             segment.size=0.5,
                             force = 10,
                             bg.color = "white",
                             bg.r = 0.25)

  #Theme
  map<-map+ggplot2::theme(plot.title =ggplot2::element_blank(),
                          strip.background = ggplot2::element_blank(),
                          panel.background= ggplot2::element_rect(fill = "white",color="black"),
                          panel.grid = ggplot2::element_line(colour = "white"),
                          axis.title =ggplot2::element_blank(),
                          axis.text = ggplot2::element_blank(),
                          axis.ticks= ggplot2::element_blank(),
                          legend.position="none",
                          panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=1.5))
  
  map
  
  }else{
  #If no opp observation print empty map with message on top of it:
    
    map<-map+ggplot2::theme(plot.title =ggplot2::element_blank(),
                          strip.background = ggplot2::element_blank(),
                          panel.background= ggplot2::element_rect(fill = "white",color="black"),
                          panel.grid = ggplot2::element_line(colour = "white"),
                          axis.title =ggplot2::element_blank(),
                          axis.text = ggplot2::element_blank(),
                          axis.ticks= ggplot2::element_blank(),
                          legend.position="none",
                          panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=1.5))+
                          # Add text element to plot
                          ggplot2::annotate("text",
                                            x = mean(c(study_area@bbox[1,1],study_area@bbox[1,2])),
                                            y = mean(c(study_area@bbox[2,1],study_area@bbox[2,2])),
                                            label = "! NO IDENTIFIED LYNX SEEN (Opp. Mon.) !")
    
    map
  }
}
