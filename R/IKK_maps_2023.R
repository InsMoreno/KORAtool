#### MAPS PRO COMPARTIMENT for the IKKs


#define start end end date for QB (24 months)
startDate <- "2023-07-01" #Startdatum (Format yyy-mm-dd) immer in ""
endDate <- Sys.Date() #Enddatum (Format yyy-mm-dd) immer in ""
bioyear <- "2023" #Biojahr -- es werden alle Einträge für die Analyse gefiltert, die >= dem eingegebenen Biojahr sind
formatted_endDate <- format(as.Date(endDate), "%d.%m.%Y")

#ändere den Titel, der auf den Karten erscheinen wird (welcher Zeitraum)
title_map2 <- "Meutes et paires en Suisse" #map with packs & pairs
title_map3 <- "Meutes en Suisse" #map with packs
title_map4 <- "Paires en Suisse" #map with pairs
caption <- paste0("Etat le ", formatted_endDate)
#### AB HIER MUSS ER NICHTS MEHR ÄNDERN ####

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

#set working directory
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
getwd()

# Access to DB from different computers:
if (Sys.info()["sysname"] == "Darwin") {  # macOS
  ssl_ca_value <- "/etc/ssl/cert.pem"
} else if (Sys.info()["sysname"] == "Windows") {
  ssl_ca_value <- "C:/cacert.pem"
} else {
  # Handle other operating systems if needed
}

#DB connection to KORA NEXT

#Load Individual Table from KORA Next
query <- paste("SELECT * from Individual WHERE species = 'WOLF'")
rs = dbSendQuery(DB, query)
master <- dbFetch(rs)

#Import Pack table from KORA NEXT
query <- paste("SELECT * FROM WolfPack ;", sep= "")
rs = dbSendQuery(DB,query)
packs<-dbFetch(rs)

#filter pack table to current bioYear
packs <- packs %>% 
  filter(bioYear >= bioyear) %>% 
  filter(hidden != 1) %>%
  droplevels()

#add numbering and abbreviation to Pack table
numbering <- fread("pack_numbering.csv")
packs <- left_join(packs, numbering, by = c("packID" = "packID"))

# Create a vector of letters from a to z
letters_vec <- letters

# Replace the values of column "no" with letters if the corresponding value in column "type" is "pair"
used_letters <- c()
for (i in which(packs$type == "PAIR")) {
  pack_name <- packs$name[i]
  next_letter <- letters_vec[which(!letters_vec %in% used_letters)][order(pack_name %in% packs$name)][1]
  packs$no[i] <- next_letter
  used_letters <- c(used_letters, next_letter)
}

#import the required Baselayer shapefiles ----
g1l <- st_read(dsn ="../../../../03_Data/GIS_Data/CH/Polit/g1l98.shp")
g1k <- st_read(dsn ="../../../../03_Data/GIS_Data/CH/Polit/g1k98.shp")
background1 <- st_read(dsn ="../../../../03_Data/GIS_Data/CH/Geostat/Mask_CH_Liechtenstein.shp")
see <- st_read(dsn ="../../../../03_Data/GIS_Data/CH/Gew/grandlacs.shp")
riv <- st_read(dsn ="../../../../03_Data/GIS_Data/CH/Gew/Gew_Klasse_4-5.shp")
chhs <- raster("../../../../03_Data/GIS_Data/CH/Geostat/grids/chhs/w001001.adf")
comp <- st_read(dsn ="../../../../03_Data/GIS_Data/CH/KOmpartimente_Referenzgebiete/Wolfkomp_18_07_2015.shp")
eu <- st_read(dsn ="../../../../03_Data/GIS_Data/Europe/European_country_borders/country_borders_europe.shp")


bbox <- st_bbox(c(xmin = 433000, xmax = 830000, ymax = 320000, ymin = 64000), crs = st_crs(21781))

# Assuming 'comp' is your sf object
# Extract the 'Nummer' column to use as names for your bboxes
nummers <- comp$Nummer

# Create a list of bbox objects for each feature
bbox_list <- lapply(st_geometry(comp), st_bbox)

# Name each bbox in the list with the corresponding 'Nummer'
names(bbox_list) <- nummers

# Now bbox_list is a named list with bboxes named after the 'Nummer' field

# Optionally, if you want to create individual bbox objects for each feature in the global environment
# with names like bbox_V, bbox_II, etc., you can use list2env as follows:
list2env(setNames(lapply(seq_along(bbox_list), function(i) {
  st_bbox(bbox_list[[i]], crs = st_crs(comp))
}), paste0("bbox_", nummers)), envir = .GlobalEnv)

bbox_IV <- st_bbox(c(xmin = 528583, xmax = 679760, ymax = 235327, ymin = 78575), crs = st_crs(21781))


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


#generate shapefiles out of xy data ----
#CRS<- sp::CRS("+init=epsg:21781")
packs_shp <- st_as_sf(packs, coords = c("x", "y"), crs = 2056) # crs 21781 stats for CH1903 LV03
packs_shp <- st_transform(packs_shp,crs = 21781 ) #transform to LV03


# calculate distance of packs to border (Grenzüberschreitende Rudel)
borderdist <- st_geometry(obj = g1l) %>% 
  st_cast(to = 'LINESTRING') %>% 
  st_distance(y = packs_shp)
borderdist <- as.list(borderdist)
packs_shp$borderdist <- borderdist
packs_shp$name <- ifelse(packs_shp$name == "Marchairuz 2", "Marchairuz",
                         ifelse(packs_shp$name == "Risoud 2", "Risoud", packs_shp$name))
packs_shp <- packs_shp %>% 
  mutate(type2 = ifelse((borderdist < 9000 & type != "PAIR" & name != "Moesola" 
                         & name != "Glattwang" & name != "Mont Tendre" & name != "Fuorn" 
                         & name != "Chablais" & name != "Rügiul" & name != "Marchairuz" ),
                        "pack transboundary", type)) # Transboundary if closer to Border than 9km



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
background_KI <- tm_shape(background1, bbox = bbox_I) + 
  tm_fill("white") +
  tm_credits(caption,
             position = c("0.9", "0"),
             size=0.5,
             fontface = "italic")
background_KII <- tm_shape(background1, bbox = bbox_II) + 
  tm_fill("white") +
  tm_credits(caption,
             position = c("0.9", "0"),
             size=0.5,
             fontface = "italic")
background_KIII <- tm_shape(background1, bbox = bbox_III) + 
  tm_fill("white") +
  tm_credits(caption,
             position = c("0.87", "0"),
             size=0.5,
             fontface = "italic")
background_KIV <- tm_shape(background1, bbox = bbox_IV) + 
  tm_fill("white") +
  tm_credits(caption,
             position = c("0.85", "0"),
             size=0.5,
             fontface = "italic")
background_KV <- tm_shape(background1, bbox = bbox_V) + 
  tm_fill("white") +
  tm_credits(caption,
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
  tm_borders(col = "#7B7D7D", lwd = .5, lty = "dashed") +  # Use "dashed" or another appropriate lty value
  tm_fill(alpha = 0)


country <- tm_shape(g1l)+
  tm_borders("black", lwd = 1)

eumap <- tm_shape(eu) + 
  tm_borders("#444444", lwd = .9)
#tm_text("countryNam", size = .7, auto.placement = T)


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
Plot_canton_flags

####
# combine additional logos into a row
logo_row <- arrangeGrob(LBC_logo, UNIL_logo,KORA_logo, ncol = 3, 
                        widths = c(0.7, 0.7, 0.7), heights = c(1))

# set up viewport layout
vp = viewport(layout = grid.layout(2, 1, heights = unit(c(1, 0.2), c("null", "null"))))

# plot the two grobs together
combined_plot <- grid.arrange(Plot_canton_flags, logo_row, ncol = 2, widths = c(10, 3), vp = vp)


# # --- Import shapefile
suppressWarnings(Rcompartment <- raster::shapefile("../../../GIS_Data/CH/Kompartimente_Referenzgebiete/Wolfkomp_18_07_2015.shp"))
# Transform the shapefile coordinates
Rcompartment<-sf::st_as_sf(Rcompartment)
Rcompartment <- st_transform(Rcompartment, crs = 21781)
# --- Genetic sample as spatial points
# Check CRS for both layers
print(sf::st_crs(Rcompartment))
print(sf::st_crs(packs_shp))

# --- Import data Compartment for each point
pts<-sf::st_join(packs_shp, Rcompartment)
pts<-as.data.table(pts)

# --- Insert data into w.genetic table
# Replace compartmen with Nummer from pts if compartmen is NA
packs_shp[,"Komp"]<-pts$Nummer


#count the number of entries per category

pack_sui <- packs_shp %>% 
  filter(type2 == "PACK") %>% 
  distinct(name, .keep_all = TRUE)

pack_sui_KI <- packs_shp %>% 
  filter(type2 == "PACK" &
           Komp == "I") %>% 
  distinct(name, .keep_all = TRUE)
pack_sui_KII <- packs_shp %>% 
  filter(type2 == "PACK" &
           Komp == "II") %>% 
  distinct(name, .keep_all = TRUE)
pack_sui_KIII <- packs_shp %>% 
  filter(type2 == "PACK" &
           Komp == "III") %>% 
  distinct(name, .keep_all = TRUE)
pack_sui_KIV <- packs_shp %>% 
  filter(type2 == "PACK" &
           Komp == "IV") %>% 
  distinct(name, .keep_all = TRUE)
pack_sui_KV <- packs_shp %>% 
  filter(type2 == "PACK" &
           Komp == "V") %>% 
  distinct(name, .keep_all = TRUE)

pack_transb <- packs_shp %>% 
  filter(type2 == "pack transboundary") %>% 
  distinct(name, .keep_all = TRUE)
pack_transb_KI <- packs_shp %>% 
  filter(type2 == "pack transboundary"&
           Komp == "I") %>% 
  distinct(name, .keep_all = TRUE)
pack_transb_KII <- packs_shp %>% 
  filter(type2 == "pack transboundary"&
           Komp == "II") %>% 
  distinct(name, .keep_all = TRUE)
pack_transb_KIII <- packs_shp %>% 
  filter(type2 == "pack transboundary"&
           Komp == "III") %>% 
  distinct(name, .keep_all = TRUE)
pack_transb_KIV <- packs_shp %>% 
  filter(type2 == "pack transboundary"&
           Komp == "IV") %>% 
  distinct(name, .keep_all = TRUE)
pack_transb_KV <- packs_shp %>% 
  filter(type2 == "pack transboundary"&
           Komp == "V") %>% 
  distinct(name, .keep_all = TRUE)


packs_map_only_df <- packs_shp %>% 
  filter(type == "PACK")
packs_map_only_df$no <- as.numeric(packs_map_only_df$no)
packs_map_only_df <- packs_map_only_df[order(packs_map_only_df$no), ]


####

packs_map_only_df_KIV <- packs_map_only_df %>%
  filter(Komp=="IV")


packs_map_only_KIV <- tm_shape(packs_map_only_df_KIV) + 
  tm_dots("type2" , 
          title = "", 
          size = 5,
          palette = c("royalblue4", "#298bd6"), 
          shapes = 21,
          border.lwd = NA,
          alpha = 0.5,
          legend.show = FALSE) +
  tm_text("no", 
          size=0.8, 
          fontface = "bold") +
  tm_add_legend(type = "symbol",
                col = c("royalblue4", "#298bd6", rep("white", nrow(packs_map_only_df_KIV))),
                labels = c(paste0("Meutes (", nrow(pack_sui_KIV), ")"),
                           paste0("Meutes transfrontalières (", nrow(pack_transb_KIV), ")"),
                           paste0(packs_map_only_df_KIV$no," = ", packs_map_only_df_KIV$name)),
                title = "",
                border.col = "white",
                alpha = 0.5)+
  tm_legend(legend.text.size = 0.9,
            legend.title.size = 0.2
  ) +
  tm_layout(##title = title_map3_KIV,
            #title.size = 2,
            #title.fontface = "bold",
            legend.position = c('left','top'),
            legend.bg.color = "white",
            legend.bg.alpha = 0.8
            #title.position = c('left','top')
            )
# packs_map_only +
#   CHHS


wolf_pack_only_map_comp_de_KIV <- 
  background_KIV + 
  CHHS + 
  rivers + 
  cantons +
  comps_empty + 
  country + 
  eumap+
  seen +
  packs_map_only_KIV 
wolf_pack_only_map_comp_de_KIV

wolf_pack_only_map_comp_de_grob <- tmap_grob(wolf_pack_only_map_comp_de_KIV)


Plot_pack_comp <- ggdraw() +
  draw_plot(wolf_pack_only_map_comp_de_grob, 0, 0.07, 1, 0.9) +
  draw_plot(combined_plot, 0.225, 0, 0.55, 0.1) 

Plot_pack_comp

# Save the ggdraw as a file

ggsave(paste0("IKKs_2023/FR_Rudel_compKIV", Sys.Date(), ".jpg"), Plot_pack_comp, 
       width = 12, height = 8, dpi = 500)



## -- K1


packs_map_only_df_KI <- packs_map_only_df %>%
  filter(Komp=="I")


packs_map_only_KI <- tm_shape(packs_map_only_df_KI) + 
  tm_dots("type2" , 
          title = "", 
          size = 5,
          palette = c("royalblue4", "#298bd6"), 
          shapes = 21,
          border.lwd = NA,
          alpha = 0.5,
          legend.show = FALSE) +
  tm_text("no", 
          size=1, 
          fontface = "bold") +
  tm_add_legend(type = "symbol",
                col = c("royalblue4", "#298bd6", rep("white", nrow(packs_map_only_df_KI))),
                labels = c(paste0("Meutes (", nrow(pack_sui_KI), ")"),
                           paste0("Meutes transfrontalières (", nrow(pack_transb_KI), ")"),
                           paste0(packs_map_only_df_KI$no," = ", packs_map_only_df_KI$name)),
                title = "",
                border.col = "white",
                alpha = 0.5)+
  tm_legend(legend.text.size = 1.2,
            legend.title.size = 0.2
  ) +
  tm_layout(##title = title_map3_KI,
    #title.size = 2,
    #title.fontface = "bold",
    legend.position = c('left','top'),
    legend.bg.color = "white",
    legend.bg.alpha = 0.8
    #title.position = c('left','top')
  )
# packs_map_only +
#   CHHS


wolf_pack_only_map_comp_de_KI <- 
  background_KI + 
  CHHS + 
  rivers + 
  cantons +
  comps_empty + 
  country + 
  eumap+
  seen +
  packs_map_only_KI 
wolf_pack_only_map_comp_de_KI

wolf_pack_only_map_comp_de_grob <- tmap_grob(wolf_pack_only_map_comp_de_KI)


Plot_pack_comp <- ggdraw() +
  draw_plot(wolf_pack_only_map_comp_de_grob, 0, 0.07, 1, 0.9) +
  draw_plot(combined_plot, 0.2, 0, 0.6, 0.1) 

Plot_pack_comp

# Save the ggdraw as a file

ggsave(paste0("IKKs_2023/FR_Rudel_compKI", Sys.Date(), ".jpg"), Plot_pack_comp, 
       width = 12, height = 8, dpi = 500)


