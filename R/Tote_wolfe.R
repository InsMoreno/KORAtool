############################################################################
######                                                                 #####
######                         WOLF MONITORING                         #####   
######                               KORA                              #####
######                          Map dead wolves                        #####
######                                                                 #####
######                               ***                               #####
######                                                                 #####     
######       Scripting by Inès Moreno from Ursi's old script           #####
######                     created in november 2023                    #####
######                                                                 #####
############################################################################

#define start end end date
startDate <- "2023-01-01" #Startdatum (Format yyy-mm-dd) immer in ""
endDate <- Sys.Date() #Enddatum (Format yyy-mm-dd) immer in ""
bioyear <- "2023" #Biojahr -- es werden alle Einträge für die Analyse gefiltert, die >= dem eingegebenen Biojahr sind
formatted_endDate <- format(as.Date(endDate), "%d.%m.%Y")

title_map5 <- "Tote Wölfe in der Schweiz" #map with dead wolves
caption <- paste0("Stand am ", formatted_endDate)

title_map5_FR <- "Loups morts en Suisse" #map with dead wolves
caption_FR <- paste0("Etat le ", formatted_endDate)

#load packages
library(shinyjs)
library(tidyverse)
library(tmap)
library(sf)
library(raster)
library(lubridate)
library(data.table)
library(RMariaDB)
library(gridExtra)
library(ggplot2)
library(cowplot)
library(magick)
library(RCurl)
library(png)
library(jpeg)
library(grid)
library(dplyr)

#set working directory
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
getwd()

#Main
if (Sys.info()["sysname"] == "Darwin") {  # macOS
  ssl_ca_value <- "/etc/ssl/cert.pem"
} else if (Sys.info()["sysname"] == "Windows") {
  ssl_ca_value <- "C:/cacert.pem"
} else {
  # Handle other operating systems if needed
}

# Read DB


#Import observation table
query <- paste("SELECT * FROM Observation ;", sep= "")
rs = dbSendQuery(DB,query)
Observation <-dbFetch(rs)
Observation <- Observation%>%
  rename(Obs_date = date)

#Import event table
query <- paste("SELECT * FROM Event WHERE species = 'WOLF';", sep= "")
rs = dbSendQuery(DB,query)
event<-dbFetch(rs)


#Import Dead table
query <- paste("SELECT * FROM Death ;", sep= "")
rs = dbSendQuery(DB,query)
Death<-dbFetch(rs)


#Merge event and observation
M_obs_event <- merge(Observation,event, 
                     by.x = "eventId", by.y = "id",
                     all.x = TRUE)

M_obs_event_death <- merge(Death,M_obs_event, 
                           by.x = "observationId", by.y = "id",
                           all.x = TRUE)
M_obs_event_death <- M_obs_event_death%>%
  filter(species=="WOLF")
M_obs_event_death$Obs_date <- as.Date(M_obs_event_death$Obs_date, format="%Y-%m-%d %H:%M:%S")
M_obs_event_death_bioYear <- M_obs_event_death %>%
  filter(Obs_date >= startDate, Obs_date <= endDate,
         status.y != "REJECTED")

Dead_to_plot <- M_obs_event_death_bioYear %>%
  dplyr::select(individualId,x,y,sex, Obs_date)



# Convert the data frame to an sf object with the initial CRS of EPSG:2056
coordinates_sf <- st_as_sf(Dead_to_plot, coords = c("x", "y"), crs = 2056)

# Transform the coordinates to EPSG:21781
coordinates_sf_transformed <- st_transform(coordinates_sf, crs = 21781)


# Add the coordinates as regular columns to the non-spatial data frame
Dead_to_plot$X_21781 <- st_coordinates(coordinates_sf_transformed)[, 'X']
Dead_to_plot$Y_21781 <- st_coordinates(coordinates_sf_transformed)[, 'Y']


#import the required Baselayer shapefiles ----
g1l <- st_read(dsn ="../../../../03_Data/GIS_Data/CH/Polit/g1l98.shp")
g1k <- st_read(dsn ="../../../../03_Data/GIS_Data/CH/Polit/g1k98.shp")
background1 <- st_read(dsn ="../../../../03_Data/GIS_Data/CH/Geostat/Mask_CH_Liechtenstein.shp")
see <- st_read(dsn ="../../../../03_Data/GIS_Data/CH/Gew/grandlacs.shp")
riv <- st_read(dsn ="../../../../03_Data/GIS_Data/CH/Gew/Gew_Klasse_4-5.shp")
chhs <- raster("../../../../03_Data/GIS_Data/CH/Geostat/grids/chhs/w001001.adf")
comp <- st_read(dsn ="../../../../03_Data/GIS_Data/CH/KOmpartimente_Referenzgebiete/Wolfkomp_18_07_2015.shp")
bbox <- st_bbox(c(xmin = 433000, xmax = 830000, ymax = 320000, ymin = 64000), crs = st_crs(21781))
eu <- st_read(dsn ="../../../../03_Data/GIS_Data/Europe/European_country_borders/country_borders_europe.shp")
territories <- st_read(dsn= "../../Rudel_Paare Schweiz & Angrenzend/__AKTUELL/Rudel territorien/Territories_2023_FINAL.shp") 
territories23 <- st_transform(territories, crs = 21781)

#Import KORA logo 
get_png <- function(filename) {
  grid::rasterGrob(png::readPNG(filename), interpolate = TRUE)
}

# Get the full path to the image file
path_KORA_logo <- file.path("..","..","..","..","01_KORA/KORA Logo (Jacques Rime)/neu_2022_07/DIGITAL/LOGO_KORA_RGB_MC.png")
path_LBC_logo <- file.path("logos/LBC/research-groups-fumagalli.jpeg")
path_UNIL_logo <- file.path("logos/LBC/UNIL.png")

# Load the image
l <- get_png(path_KORA_logo)
t <- grid::roundrectGrob()

Koralogo <- ggplot(mapping = aes(x = 0:1, y = 1)) +
  theme_void() +
  annotation_custom(l, xmin = .8, xmax = 1)

KORA_logo <- rasterGrob(readPNG(path_KORA_logo), interpolate = TRUE)
LBC_logo <- rasterGrob(readJPEG(path_LBC_logo), interpolate = TRUE)
UNIL_logo <- rasterGrob(readPNG(path_UNIL_logo), interpolate = TRUE)

# create list of logos
logos <- list(KORA_logo, LBC_logo, UNIL_logo)


# lets plot the Wolf Maps ----

# Background maps 

tmap_mode("plot") # set to "view" to have an interactive map

crs(chhs) <- "+proj=somerc +lat_0=46.95240555555556 +lon_0=7.439583333333333 +k_0=1 +x_0=600000 +y_0=200000 +ellps=bessel +towgs84=674.4,15.1,405.3,0,0,0,0 +units=m +no_defs"

# Define your custom palette
colors <- c("#000000", "#2E2E2E", "#646464", "#9C9C9C", "#F3E4D7", "#FF7E00", "#FFCC00", "#FFFA00")
colors_terrain <- c("#1B4F7C", "#2A6E96", "#398CB0", "#49A9CA", "#59C6E4", "#68E4FF", "#96FFFF", "#C4FFFF")
colors_elevation <- c("#2E2E2E", "#646464", "#9C9C9C", "#D4D4D4", "#FF7E00", "#FFCC00", "#FFFA00", "#FFFFFF")
colors_gray <- c("#000000", "#222222", "#444444", "#666666", "#888888", "#AAAAAA", "#CCCCCC", "#FFFFFF")
colors_hyspometric <- c( "#2E2E2E", "#646464","#A6D96A", "#FFFFBF","#EDDCC4","#E8DBC7" ,"#C1A994","#FDFBF7")
colors2 <- c("#000000", "#2E2E2E", "#646464", "#A6D96A", "#F3E4D7", "#78643F","#C1A994", "#FFCC00")

CHHS <- tm_shape(chhs)+
  tm_raster(alpha = 0.3, 
            palette = colors2,
            #n = 9, 
            contrast = c(0.05, 0.8),legend.show = F)

background <- tm_shape(background1, bbox = bbox) + 
  tm_fill("white") +
  tm_credits(caption,
             position = c("0.9", "0"),
             size=0.5,
             fontface = "italic")
background_FR <- tm_shape(background1, bbox = bbox) + 
  tm_fill("white") +
  tm_credits(caption_FR,
             position = c("0.9", "0"),
             size=0.5,
             fontface = "italic")


#background

seen <- tm_shape(see) + 
  tm_fill("#DBF1F3") + tm_borders("lightblue", lwd = 0.9)

rivers <- tm_shape(riv) + 
  tm_lines("#ADD8E6", lwd = 1.3)

#CHHS + rivers + seen

# comps <- tm_shape(comp) +
#   tm_borders("grey7", lwd = .8) +
#   tm_fill("#99cf99", alpha = 0.2)

comps_empty <- tm_shape(comp) +
  tm_borders("#444444", lwd = .8)+
  tm_text("Nummer",
          size=.8, col= "#444444", fontface = "bold", auto.placement = F, xmod = -1.1)

cantons <- tm_shape(g1k) + 
  tm_borders("#444444", lwd = .5) + 
  tm_fill(alpha = 0)

country <- tm_shape(g1l)+
  tm_borders("black", lwd = 1)

eumap <- tm_shape(eu) + 
  tm_borders("#444444", lwd = .9)
#tm_text("countryNam", size = .7, auto.placement = T)

territ_2023 <- tm_shape(territories23)+
  tm_fill(col = "Nom_meute",alpha = 0.5)

##Plot the logos
##Plot the logos
#take the canton logos from file:
dir_logos <- "logos/armoiries/"
plots <- list()
file_list <- list.files(dir_logos, pattern = "\\.png$", full.names = TRUE)
# 
for (i in 1:length(file_list)) {
  # read image
  img <- readPNG(file_list[i])
  
  # add to plot list
  plots[[i]] <- rasterGrob(img, interpolate = TRUE)
}

# combine plots into a grid
Plot_canton_flags <- grid.arrange(grobs = plots, ncol = 26, nrow = 1)
#Plot_canton_flags

####
# combine additional logos into a row
logo_row <- arrangeGrob(LBC_logo, UNIL_logo,KORA_logo, ncol = 3, 
                        widths = c(0.7, 0.7, 0.7), heights = c(1))

# set up viewport layout
vp = viewport(layout = grid.layout(2, 1, heights = unit(c(1, 0.2), c("null", "null"))))

# plot the two grobs together
combined_plot <- grid.arrange(Plot_canton_flags, logo_row, ncol = 2, widths = c(10, 3), vp = vp)



Dead_to_plot$shape <- ifelse(Dead_to_plot$sex == 'FEMALE', 17, # triangle for female
                             ifelse(Dead_to_plot$sex == 'MALE', 19, 22)) # circle for male
Dead_to_plot$Indiv <- ifelse(is.na(Dead_to_plot$individualId), 'unbekannt', 'bekannt')
bekannt <- sum(!is.na(Dead_to_plot$individualId))
unbekannt <- sum(is.na(Dead_to_plot$individualId))
females <- sum(Dead_to_plot$sex == 'FEMALE')
males <- sum(Dead_to_plot$sex == 'MALE')
unknown <- sum(!Dead_to_plot$sex %in%c('FEMALE',"MALE"))

# Now convert the data frame to an sf object
Dead_to_plot_sf <- st_as_sf(Dead_to_plot, coords = c("X_21781", "Y_21781"), crs = 21781)

# Make sure to remove any additional attributes that are not part of the spatial data frame
Dead_to_plot_sf <- Dead_to_plot_sf[, c("sex", "individualId", "shape", "Indiv")]

dead_map <- tm_shape(Dead_to_plot_sf) + 
  tm_dots(col = "Indiv", 
          title = "Individuum",
          labels = c(paste0("Individuum bekannt (", bekannt, ")"), 
                     paste0("Noch nicht genotypisiert (", unbekannt, ")")
          ),
          palette = c("red", "honeydew4"),
          size = 1, 
          border.col = "grey39",
          alpha = 0.75,
          border.lwd = 1,
          sizes.legend = 5,
          shape ="sex", 
          title.shape ="Geschlecht", 
          shapes = c(24,21,22),
          shapes.labels = c(paste0("Weibchen (", females, ")"),
                            paste0("Männchen (", males, ")"),
                            paste0("Unbekannt (", unknown, ")")
          ), 
          shapes.legend = 22,
          shapes.legend.fill = "white")+ # include symbol for wolf of other population as soon as field exists in KoraOS
  tm_layout(title = title_map5,
            legend.position = c("left","top"),
            legend.width = 15,
            title.fontface = "bold",
            legend.text.size = 1.1, 
            legend.title.size = 1.5, legend.height = .5	,
            scale = 1.05)

dead_map

dead_map_de <- 
  background + 
  CHHS + 
  rivers + 
  seen +
  cantons +
  country + 
  eumap+
  #comps_empty + 
  dead_map 

#wolf_pack_only_map_de

dead_map_comp_de <- 
  background + 
  CHHS + 
  rivers + 
  country + 
  eumap+
  seen +
  comps_empty + 
  dead_map 

#wolf_pack_only_map_comp_de

dead_map_de_grob <- tmap_grob(dead_map_de)
dead_map_comp_de_grob <- tmap_grob(dead_map_comp_de)


#ggdraw(map_pack_grob)+
Plot_dead_cantons <- ggdraw() +
  draw_plot(dead_map_de_grob, 0, 0.07, 1, 0.9) +
  draw_plot(combined_plot, 0.1, 0, 0.8, 0.1) 

Plot_dead_cantons

Plot_dead_comp <- ggdraw() +
  draw_plot(dead_map_comp_de_grob, 0, 0.07, 1, 0.9) +
  draw_plot(combined_plot, 0.1, 0, 0.8, 0.1) 

Plot_dead_comp

# Save the ggdraw as a file
ggsave(paste0("DE_DeadWolves_cantons_", Sys.Date(), ".jpg"), Plot_dead_cantons, 
       width = 12, height = 8, dpi = 500)
ggsave(paste0("DE_DeadWolves_comp_", Sys.Date(), ".jpg"), Plot_dead_comp, 
       width = 12, height = 8, dpi = 500)


## In french:
dead_map_FR <- tm_shape(Dead_to_plot_sf) + 
  tm_dots(col = "Indiv", 
          title = "Individus:",
          labels = c(paste0("connus (", bekannt, ")"), 
                     paste0("pas encore génotypés (", unbekannt, ")")
          ),
          palette = c("red", "honeydew4"),
          size = 1, 
          border.col = "grey39",
          alpha = 0.75,
          border.lwd = 1,
          sizes.legend = 5,
          shape ="sex", 
          title.shape ="Sexe", 
          shapes = c(24,21,22),
          shapes.labels = c(paste0("Femelles (", females, ")"),
                            paste0("Mâles (", males, ")"),
                            paste0("Inconnus (", unknown, ")")
          ), 
          shapes.legend = 22,
          shapes.legend.fill = "white")+ # include symbol for wolf of other population as soon as field exists in KoraOS
  tm_layout(title = title_map5_FR,
            legend.position = c("left","top"),
            legend.width = 15,
            title.fontface = "bold",
            legend.text.size = 1.1, 
            legend.title.size = 1.5, legend.height = .5	,
            scale = 1.05)

dead_map_FR

dead_map_cantons_FR <- 
  background_FR + 
  CHHS + 
  rivers + 
  seen +
  cantons +
  country + 
  eumap+
  #comps_empty + 
  dead_map_FR 

dead_map_cantons_FR

dead_map_comp_FR <- 
  background_FR + 
  CHHS + 
  rivers + 
  country + 
  eumap+
  seen +
  comps_empty + 
  dead_map_FR 

dead_map_comp_FR

dead_map_cantons_FR_grob <- tmap_grob(dead_map_cantons_FR)
dead_map_comp_FR_grob <- tmap_grob(dead_map_comp_FR)


#ggdraw(map_pack_grob)+
Plot_dead_cantons_FR <- ggdraw() +
  draw_plot(dead_map_cantons_FR_grob, 0, 0.07, 1, 0.9) +
  draw_plot(combined_plot, 0.1, 0, 0.8, 0.1) 

Plot_dead_cantons_FR

Plot_dead_comp_FR <- ggdraw() +
  draw_plot(dead_map_comp_FR_grob, 0, 0.07, 1, 0.9) +
  draw_plot(combined_plot, 0.1, 0, 0.8, 0.1) 

Plot_dead_comp_FR

# Save the ggdraw as a file
ggsave(paste0("FR_DeadWolves_cantons_", Sys.Date(), ".jpg"), Plot_dead_cantons_FR, 
       width = 12, height = 8, dpi = 500)
ggsave(paste0("FR_DeadWolves_comp_", Sys.Date(), ".jpg"), Plot_dead_comp_FR, 
       width = 12, height = 8, dpi = 500)


#Visualize in which pack are the dead wolves

dead_map_comp_territories <- 
  background_FR + 
  CHHS + 
  rivers + 
  country + 
  eumap+
  seen +
  comps_empty +
  territ_2023+
  dead_map_FR 
dead_map_comp_territories

dead_territories <- sf::st_join(Dead_to_plot_sf, territories23)

dead_territories_df<-as.data.table(dead_territories)

M_obs_event_death_bioYear$meute <- dead_territories_df$Nom_meute
