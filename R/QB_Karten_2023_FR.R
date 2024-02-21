#SKRIPT ZUR ERSTELLUNG DER KARTEN FÜR QB WOLF
#last modification 2023-05.09
#created by Ursi Sterrer, modified by Inès Moreno

#-------------  MANUAL -------------#
#Mache einen Export der Tabellen "wolf_master", "wolf_genetics", wolf_packs" und "wolf_dead" aus dem KORAdata
#speichere sie im gleichen Ordner, wie dieses Skript. Die Dateinamen müssen exact so wie oben geschrieben werden!!
#Nach dem modifizieren der gewünschten Werte (siehe weiter unten)
#Um das Skript laufen zu lassen, klicke mit der Maus vor den ersten Befehl rm(list = ls())
#drücke auf der Tastatur gleichzeitig "Ctrl" (= Strg) und "shift" (Grossschreibtaste) und dann "Enter"
#ACHTUNG: Falls die Sonderzeichen nicht richtig angezeigt werden, oben links auf File --> Reopen with Encoding --> UTF-8
#-----------------------------------#
### run
rm(list = ls())

#### HIER MUSS RALPH DIE EINTRÄGE MODIFIZIEREN ####

#define start end end date for QB 
startDate <- "2023-04-01" #Startdatum (Format yyy-mm-dd) immer in ""
endDate <- Sys.Date() #Enddatum (Format yyy-mm-dd) immer in ""
bioyear <- "2023" #Biojahr -- es werden alle Einträge für die Analyse gefiltert, die >= dem eingegebenen Biojahr sind
formatted_endDate <- format(as.Date(endDate), "%d.%m.%Y")

#ändere den Titel, der auf den Karten erscheinen wird (welcher Zeitraum)
title_map2 <- "Packs and pairs in Switzerland" #map with packs & pairs
title_map3 <- "Packs in Switzerland" #map with packs
title_map4 <- "Pairs in Switzerland" #map with pairs
caption <- paste0("State the ", formatted_endDate)
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

#Main
if (Sys.info()["sysname"] == "Darwin") {  # macOS
  ssl_ca_value <- "/etc/ssl/cert.pem"
} else if (Sys.info()["sysname"] == "Windows") {
  ssl_ca_value <- "C:/cacert.pem"
} else {
  # Handle other operating systems if needed
}

#load data
koraNextDB <- dbConnect(RMariaDB::MariaDB(), 
                              user='8sjecxkl1vfwp1mj0fl7',
                              password="pscale_pw_x3D6OlYREaNmInlFr3MP7etCfcioLHClnC2MC9dBYub",
                              dbname='kora-next', 
                              host='eu-west.connect.psdb.cloud',
                              ssl.ca=ssl_ca_value)


query <- paste("SELECT * from WolfPack")
rs = dbSendQuery(koraNextDB, query)
packs <- dbFetch(rs)

query <- paste("SELECT * from Individual WHERE species = 'WOLF'")
rs = dbSendQuery(koraNextDB, query)
master <- dbFetch(rs)

packs <- packs %>% 
  filter(bioYear >= bioyear) %>% 
  filter(hidden != 1) %>%
  droplevels()

numbering <- read.csv("pack_numbering.csv", sep = ";")
packs <- left_join(packs, numbering, by = c("packID" = "packID"))

# Create a vector of letters from a to z
letters_vec <- letters

# Replace the values of column "no" with letters if the corresponding value in column "type" is "pair"
used_letters <- c()
for (i in which(packs$type == "PAIR")) {
  pack_name <- packs$name[i]
  next_letter <- letters_vec[which(!letters_vec %in% used_letters)][order(pack_name %in% packs$pack_name)][1]
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
bbox <- st_bbox(c(xmin = 433000, xmax = 830000, ymax = 320000, ymin = 64000), crs = st_crs(21781))


# make some bbox magic
bbox_new <- st_bbox(background1) # current bounding box

xrange <- bbox_new$xmax - bbox_new$xmin # range of x values
yrange <- bbox_new$ymax - bbox_new$ymin # range of y values

# bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
bbox_new[3] <- bbox_new[3] + (0.25 * xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.2 * yrange) # ymax - top

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon
eu <- st_read(dsn ="../../../../03_Data/GIS_Data/Europe/European_country_borders/country_borders_europe.shp")

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
packs_shp <- st_as_sf(packs, coords = c("x", "y"), crs = 2056) # crs 2056 stats for CH1903+ LV95
packs_shp <- st_transform(packs_shp,crs = 21781 )

# calculate distance of packs to border (Grenzüberschreitende Rudel)
borderdist <- st_geometry(obj = g1l) %>% 
  st_cast(to = 'LINESTRING') %>% 
  st_distance(y = packs_shp)
borderdist <- as.list(borderdist)
packs_shp$borderdist <- borderdist
packs_shp <- packs_shp %>% 
  mutate(type2 = ifelse((borderdist < 9000 & type != "PAIR" & name != "Moesola" 
                         & name != "Glattwang" & name != "Mont Tendre" & name != "Fuorn" 
                         & name != "Chablais" & name != "Rügiul" & name != "Marchairuz"),
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

# background


seen <- tm_shape(see) + 
  tm_fill("#DBF1F3") + tm_borders("lightblue", lwd = 0.9)

rivers <- tm_shape(riv) + 
  tm_lines("#ADD8E6", lwd = 1.3)

# CHHS + rivers + seen

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

cantons2 <- tm_shape(g1k) + 
  tm_borders(col = "#7B7D7D", lwd = .5, lty = "dashed") +  # Use "dashed" or another appropriate lty value
  tm_fill(alpha = 0)

country <- tm_shape(g1l)+
  tm_borders("black", lwd = 1)

eumap <- tm_shape(eu) + 
  tm_borders("#444444", lwd = .9)
  #tm_text("countryNam", size = .7, auto.placement = T)


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
Plot_canton_flags

####
# combine additional logos into a row
logo_row <- arrangeGrob(LBC_logo, UNIL_logo,KORA_logo, ncol = 3, 
                        widths = c(0.7, 0.7, 0.7), heights = c(1))

# set up viewport layout
vp = viewport(layout = grid.layout(2, 1, heights = unit(c(1, 0.2), c("null", "null"))))

# plot the two grobs together
combined_plot <- grid.arrange(Plot_canton_flags, logo_row, ncol = 2, widths = c(10, 3), vp = vp)

pack_sui <- packs_shp %>% 
  filter(type2 == "PACK") %>% 
  distinct(name, .keep_all = TRUE) 

pack_transb <- packs_shp %>% 
  filter(type2 == "pack transboundary") %>% 
  distinct(name, .keep_all = TRUE)

pair_count <- packs_shp %>% 
  filter(type == "PAIR")

#--------------------------------------------PACK Map---------------------------------

packs_map_only_df <- packs_shp %>% 
  filter(type == "PACK")
packs_map_only_df$no <- as.numeric(packs_map_only_df$no)
packs_map_only_df <- packs_map_only_df[order(packs_map_only_df$no), ]

packs_map_only <- tm_shape(packs_map_only_df) + 
  tm_dots("type2" , 
          title = "", 
          size = 5,
          palette = c("royalblue4", "#298bd6"), 
          shapes = 21,
          border.lwd = NA,
          alpha = 0.5,
          legend.show = FALSE) +
  tm_text("no", 
          size=0.75, 
          fontface = "bold") +
  tm_add_legend(type = "symbol",
                col = c("royalblue4", "#298bd6", rep("white", nrow(packs_map_only_df))),
                labels = c(paste0("Packs (", nrow(pack_sui), ")"),
                           paste0("Transboundary packs (", nrow(pack_transb), ")"),
                           paste0(packs_map_only_df$no," = ", packs_map_only_df$name)),
                title = "",
                border.col = "white",
                alpha = 0.5)+
  tm_legend(legend.text.size = 0.75,
            legend.title.size = 0.2
  ) +
  tm_layout(title = title_map3,
            title.size = 2,
            title.fontface = "bold",
            legend.position = c('left','top'),
            title.position = c('left','top'))
# packs_map_only +
#   CHHS

wolf_pack_only_map_de <- 
  background + 
  CHHS + 
  rivers + 
  seen +
  cantons +
  country + 
  eumap+
  #comps_empty + 
  packs_map_only 

#wolf_pack_only_map_de

wolf_pack_only_map_comp_de <- 
  background + 
  CHHS + 
  rivers + 
  country + 
  eumap+
  seen +
  comps_empty + 
  packs_map_only 

#wolf_pack_only_map_comp_de
wolf_pack_only_map_cant_comp_de <- 
  background + 
  CHHS + 
  rivers + 
  country + 
  eumap+
  cantons2+
  seen +
  comps_empty + 
  packs_map_only 
wolf_pack_only_map_cant_comp_de

wolf_pack_only_map_de_grob <- tmap_grob(wolf_pack_only_map_de)
wolf_pack_only_map_comp_de_grob <- tmap_grob(wolf_pack_only_map_comp_de)
wolf_pack_only_map_cant_comp_de_grob <- tmap_grob(wolf_pack_only_map_cant_comp_de)


#ggdraw(map_pack_grob)+
Plot_pack_cantons <- ggdraw() +
  draw_plot(wolf_pack_only_map_de_grob, 0, 0.07, 1, 0.9) +
  draw_plot(combined_plot, 0.1, 0, 0.8, 0.1) 
Plot_pack_cantons
Plot_pack_comp <- ggdraw() +
  draw_plot(wolf_pack_only_map_comp_de_grob, 0, 0.07, 1, 0.9) +
  draw_plot(combined_plot, 0.1, 0, 0.8, 0.1) 

#Plot_pack_comp
Plot_pack_cant_comp <- ggdraw() +
  draw_plot(wolf_pack_only_map_cant_comp_de_grob, 0, 0.07, 1, 0.9) +
  draw_plot(combined_plot, 0.1, 0, 0.8, 0.1) 

Plot_pack_cant_comp

# Save the ggdraw as a file
ggsave(paste0("EN_Rudel_cantons_", Sys.Date(), ".jpg"), Plot_pack_cantons, 
       width = 12, height = 8, dpi = 500)
ggsave(paste0("EN_Rudel_comp_", Sys.Date(), ".jpg"), Plot_pack_comp, 
       width = 12, height = 8, dpi = 500)
ggsave(paste0("EN_Rudel_cantons_comp_", Sys.Date(), ".jpg"), Plot_pack_cant_comp, width = 12, height = 8, dpi = 500)

#packmap only without legend
packs_map_NOleg_only <- tm_shape(packs_map_only_df) + 
  tm_dots("type2" , 
          title = "", 
          size = 5,
          palette = c("royalblue4", "#298bd6"), 
          shapes = 21,
          border.lwd = NA,
          alpha = 0.5,
          legend.show = FALSE) +
  # tm_text("no", 
  #         size=0.75, 
  #         fontface = "bold") +
  tm_add_legend(type = "symbol",
                col = c("royalblue4", "#298bd6",rep("white", nrow(packs_map_only_df))),
                labels = c(paste0("Packs (", nrow(pack_sui), ")"),
                           paste0("Transboundary packs (", nrow(pack_transb), ")"),
                           rep("",nrow(packs_map_only_df))),
                title = "",
                border.col = "white",
                alpha = 0.5)+
  tm_legend(legend.text.size = 0.75,
            legend.title.size = 0.2
  ) +
  tm_layout(title = title_map3,
            title.size = 2,
            title.fontface = "bold",
            legend.position = c('left','top'),
            title.position = c('left','top'))
# packs_map_only +
#   CHHS

wolf_pack_only_map_NOleg_de <- 
  background + 
  CHHS + 
  rivers + 
  cantons +
  country + 
  eumap+
  seen +
  #comps_empty + 
  packs_map_NOleg_only 
#wolf_pack_only_map_NOleg_de

wolf_pack_only_map_NOleg_comp_de <- 
  background + 
  CHHS + 
  rivers + 
  country + 
  eumap+
  seen +
  comps_empty + 
  packs_map_NOleg_only 
#wolf_pack_only_map_NOleg_comp_de
wolf_pack_only_map_NOleg_cant_comp_de <- 
  background + 
  CHHS + 
  rivers + 
  country + 
  eumap+
  cantons2 +
  seen +
  comps_empty + 
  packs_map_NOleg_only 

wolf_pack_only_map_NOleg_de_grob <- tmap_grob(wolf_pack_only_map_NOleg_de)
wolf_pack_only_map_NOleg_comp_de_grob <- tmap_grob(wolf_pack_only_map_NOleg_comp_de)
wolf_pack_only_map_NOleg_cant_comp_de_grob <- tmap_grob(wolf_pack_only_map_NOleg_cant_comp_de)


#ggdraw(map_pack_grob)+
Plot_pack_cantons_NOleg <- ggdraw() +
  draw_plot(wolf_pack_only_map_NOleg_de_grob, 0, 0.07, 1, 0.9) +
  draw_plot(combined_plot, 0.1, 0, 0.8, 0.1) 

#Plot_pack_cantons_NOleg

Plot_pack_comp_NOleg <- ggdraw() +
  draw_plot(wolf_pack_only_map_NOleg_comp_de_grob, 0, 0.07, 1, 0.9) +
  draw_plot(combined_plot, 0.1, 0, 0.8, 0.1) 

#Plot_pack_comp_NOleg

Plot_pack_cant_comp_NOleg <- ggdraw() +
  draw_plot(wolf_pack_only_map_NOleg_cant_comp_de_grob, 0, 0.07, 1, 0.9) +
  draw_plot(combined_plot, 0.1, 0, 0.8, 0.1) 
Plot_pack_cant_comp_NOleg

# Save the ggdraw as a file
ggsave(paste0("EN_Rudel_cantons_NOleg_", Sys.Date(), ".jpg"), Plot_pack_cantons_NOleg, 
       width = 12, height = 8, dpi = 500)
ggsave(paste0("EN_Rudel_comp_NOleg_", Sys.Date(), ".jpg"), Plot_pack_comp_NOleg, 
       width = 12, height = 8, dpi = 500)
ggsave(paste0("EN_Rudel_cantons_comp_NOleg_", Sys.Date(), ".jpg"), Plot_pack_cant_comp_NOleg, width = 12, height = 8, dpi = 500)


#-------------------------------------------PAIR Map------------------------------------
# pair_map_only_df <- packs_shp %>% 
#   filter(type == "PAIR")
# pair_map_only_df <- pair_map_only_df[order(pair_map_only_df$no), ]
# 
# pair_map_only <- tm_shape(pair_map_only_df) + 
#   tm_dots("type2" ,
#           size = 5,
#           palette = "darkorange2",
#           shape = "type",
#           title.shape = "Status",
#           shapes.labels = paste0("Pair (", nrow(pair_count), ")"),
#           labels = paste0("Pair (", nrow(pair_count), ")"),
#           legend.show = FALSE,
#           shapes = 22,
#           border.lwd = NA,
#           alpha = 0.5) +
#   tm_add_legend(type = "symbol",
#                 col = c(rep("white", nrow(pair_map_only_df))),
#                 labels = c(paste0(pair_map_only_df$no," = ", pair_map_only_df$name)),
#                 title = "",
#                 border.col = "white",
#                 alpha = 0.5)+
#   tm_text("no", 
#           size=0.8, 
#           fontface = "bold") +
#   tm_legend(legend.text.size = 0.75,
#             legend.title.size = 1) +
#   tm_layout(title = title_map4,
#             title.size = 2,
#             title.fontface = "bold",
#             #legend.position = c('left','bottom'),
#             title.position = c('left','top'),
#             #legend.show = FALSE,
#             #bg.color = "transparent"
#   )
# 
# wolf_pair_only_map_de <- 
#   background + 
#   CHHS + 
#   rivers + 
#   cantons +
#   country +
#   eumap+
#   seen + 
#   #comps_empty + 
#   pair_map_only
# #wolf_pair_only_map_de
# 
# wolf_pair_only_map_de_comp <- 
#   background + 
#   CHHS + 
#   rivers + 
#   #cantons +
#   eumap+
#   country +
#   seen + 
#   comps_empty + 
#   pair_map_only
# #wolf_pair_only_map_de_comp
# 
# # backgroundFZ <- tm_shape(background1, bbox = bbox) + 
# #   tm_fill(alpha = 0) 
# # 
# # FZ <-  backgroundFZ +
# #   CHHS + 
# #   rivers + 
# #   #cantons +
# #   seen + 
# #   eumap+
# #   comps_empty + 
# #   country +
# #   pair_map_only
# # FZ
# # tmap_save(FZ, filename = paste0("FZPaare_", Sys.Date(), ".jpg"))
# 
# 
# wolf_pair_only_map_de_grob <- tmap_grob(wolf_pair_only_map_de)
# wolf_pair_only_map_de_comp_grob <- tmap_grob(wolf_pair_only_map_de_comp)
# 
# #plot the map
# Plot_pair <- ggdraw() +
#   draw_plot(wolf_pair_only_map_de_grob, 0, 0.07, 1, 0.9) +
#   draw_plot(combined_plot, 0.1, 0, 0.8, 0.1) 
# #Plot_pair
# 
# Plot_pair_comp <- ggdraw() +
#   draw_plot(wolf_pair_only_map_de_comp_grob, 0, 0.07, 1, 0.9) +
#   draw_plot(combined_plot, 0.1, 0, 0.8, 0.1) 
# #Plot_pair_comp
# 
# # Save the ggdraw as a file
# ggsave(paste0("EN_Paare_canton", Sys.Date(), ".jpg"), Plot_pair, 
#        width = 12, height = 8, dpi = 500)
# ggsave(paste0("EN_Paare_comp", Sys.Date(), ".jpg"), Plot_pair_comp, 
#        width = 12, height = 8, dpi = 500)
# 
# 
# #-------------------------------------PACK & PAIR map----------------------------------  
# 
# packs_map2 <- tm_shape(packs_shp) + 
#   tm_dots("type2" , 
#           title = "Colors", 
#           labels = c(paste0("Packs (", nrow(pack_sui), ")"), 
#                      paste0("Transboundary packs (", nrow(pack_transb), ")"), 
#                      paste0("Pairs (", nrow(pair_count), ")")),
#           size = 5, 
#           palette = c("royalblue4", "#298bd6",  "darkorange2"), 
#           shape = "type",
#           title.shape = "Symbols",
#           shapes.labels = c("Pack", "Pair"),
#           shapes = c(21, 22),
#           border.lwd = NA,
#           alpha = 0.5) +
#   # tm_text("no", 
#   #         size=0.8, 
#   #         fontface = "bold") +
#   tm_layout(title = title_map2,
#             title.size = 2,
#             title.fontface = "bold",
#             title.position = c('left','top'))
# #packs_map2
# 
# wolf_pack_map_de <- background + 
#   CHHS + 
#   rivers + 
#   cantons + 
#   country + 
#   eumap + 
#   seen + 
#   packs_map2
# 
# #wolf_pack_map_de
# 
# wolf_pack_map_comp_de <- background + 
#   CHHS + 
#   rivers + 
#   country + 
#   eumap + 
#   seen + 
#   comps_empty +
#   packs_map2
# 
# #wolf_pack_map_comp_de
# 
#   
# wolf_pack_map_de_grob <- tmap_grob(wolf_pack_map_de)
# wolf_pack_map_comp_de_grob <- tmap_grob(wolf_pack_map_comp_de)
# 
# 
# #ggdraw(map_pack_grob)+
# Plot_packPair_cantons <- ggdraw() +
#   draw_plot(wolf_pack_map_de_grob, 0, 0.07, 1, 0.9) +
#   draw_plot(combined_plot, 0.1, 0, 0.8, 0.1)
# 
# #Plot_packPair_cantons
# 
# Plot_packPair_comp <- ggdraw() +
#   draw_plot(wolf_pack_map_comp_de_grob, 0, 0.07, 1, 0.9) +
#   draw_plot(combined_plot, 0.1, 0, 0.8, 0.1)
# 
# #Plot_packPair_comp
# 
# # Save the ggdraw as a file
# ggsave(paste0("EN_RudelundPaare_cantons_", Sys.Date(), ".jpg"), Plot_packPair_cantons, 
#        width = 12, height = 8, dpi = 500)
# ggsave(paste0("EN_RudelundPaare_comp_", Sys.Date(), ".jpg"), Plot_packPair_comp, 
#        width = 12, height = 8, dpi = 500)
# 
# ## With legend:
# packs_shp_test <- packs_shp[order(packs_shp$no), ]
# 
# packs_map2_legend <- tm_shape(packs_shp_test) + 
#   tm_dots("type2" , 
#           title = "Colors", 
#           labels = c(paste0("Packs (", nrow(pack_sui), ")"), 
#                      paste0("Transboundary packs (", nrow(pack_transb), ")"), 
#                      paste0("Pairs (", nrow(pair_count), ")")),
#           size = 5, 
#           palette = c("royalblue4", "#298bd6",  "darkorange2"), 
#           shape = "type",
#           title.shape = "Symbols",
#           shapes.labels = c("Pack", "Pair"),
#           shapes = c(21, 22),
#           border.lwd = NA,
#           alpha = 0.5) +
#   tm_add_legend(type = "symbol",
#                 col = c(rep("white", nrow(packs_map_only_df))),
#                 labels = c(paste0(packs_map_only_df$no," = ", packs_map_only_df$name),
#                            paste0(pair_map_only_df$no," = ", pair_map_only_df$name)),
#                 title = "",
#                 border.col = "white",
#                 alpha = 0.5,
#                 legend.format= list(text.separator = "<br>", text.orient = "h", columns = 2))+
#   tm_text("no",
#           size=0.8,
#           fontface = "bold") +
#   tm_layout(title = title_map2,
#             title.size = 1.8,
#             title.fontface = "bold",
#             title.position = c('left','top'));packs_map2_legend
# 
# wolf_pack_map_leg_de <- background + 
#   CHHS + 
#   rivers + 
#   country + 
#   cantons + 
#   country + 
#   eumap + 
#   seen + 
#   packs_map2_legend
# #wolf_pack_map_leg_de
# 
# wolf_pack_map_leg_comp_de <- background + 
#   CHHS + 
#   rivers + 
#   seen + 
#   comps_empty +
#   country + 
#   eumap + 
#   seen + 
#   comps_empty +
#   packs_map2_legend
# #wolf_pack_map_leg_comp_de
# 
# 
# wolf_pack_map_leg_de_grob <- tmap_grob(wolf_pack_map_leg_de)
# wolf_pack_map_leg_comp_de_grob <- tmap_grob(wolf_pack_map_leg_comp_de)
# 
# 
# #ggdraw(map_pack_grob)+
# Plot_packPair_leg_cantons <- ggdraw() +
#   draw_plot(wolf_pack_map_leg_de_grob, 0, 0.07, 1, 0.9) +
#   draw_plot(combined_plot, 0.1, 0, 0.8, 0.1)
# 
# #Plot_packPair_leg_cantons
# 
# Plot_packPair_leg_comp <- ggdraw() +
#   draw_plot(wolf_pack_map_leg_comp_de_grob, 0, 0.07, 1, 0.9) +
#   draw_plot(combined_plot, 0.1, 0, 0.8, 0.1)
# 
# #Plot_packPair_leg_comp
# # Save the ggdraw as a file
# ggsave(paste0("EN_RudelundPaare_leg_cantons_", Sys.Date(), ".jpg"), Plot_packPair_leg_cantons, 
#        width = 12, height = 8, dpi = 500)
# ggsave(paste0("EN_RudelundPaare_leg_comp_", Sys.Date(), ".jpg"), Plot_packPair_leg_comp, 
#        width = 12, height = 8, dpi = 500)
# 


#### -- map proactive regulations
caption_reg <- "*regardless of its implementation"
background_reg <- tm_shape(background1, bbox = bbox) + 
  tm_fill("white") +
  tm_credits(caption_reg,
             position = c("0.01", "0"),
             size=0.65,
             fontface = "italic");background_reg

packs_shp$Reg_proa <- ifelse(packs_shp$Reg_proa =="", NA,packs_shp$Reg_proa)
packs_shp$Reg_rea <- ifelse(packs_shp$Reg_rea =="", NA,packs_shp$Reg_rea)
packs_shp$Recours <- ifelse(packs_shp$Recours =="", NA,packs_shp$Recours)

packs_map_only_df$Reg_proa <- factor(packs_map_only_df$Reg_proa, levels = c("PA_all", "PA_some"))
packs_map_only_df$Reg_rea <- factor(packs_map_only_df$Reg_rea, levels = "RA")


packs_map_only_df_PA_all <- packs_map_only_df %>%
  filter(Reg_proa=="PA_all")
packs_map_only_df_PA_some <- packs_map_only_df %>%
  filter(Reg_proa=="PA_some")
packs_map_only_df_RA <- packs_map_only_df %>%
  filter(Reg_rea=="RA")
packs_map_only_df_notPA <- packs_map_only_df %>%
  filter(is.na(Reg_proa) & is.na(Reg_rea))
packs_map_only_df_Recours <- packs_map_only_df %>%
  filter(Recours=="yes")

# packs_map_only_NOTreg <- tm_shape(packs_map_only_df_notPA) + 
#   tm_dots("type2" , 
#           title = "", 
#           size = 5,
#           palette = c("royalblue4", "#298bd6"), 
#           shapes = 21,
#           border.lwd = NA,
#           alpha = 0.5,
#           legend.show = FALSE) +
#   tm_text("no", 
#           size=0.75, 
#           fontface = "bold") +
#   tm_layout(title = title_map3,
#             title.size = 2,
#             title.fontface = "bold",
#             #legend.position = c('left','top'),
#             title.position = c('left','top'));packs_map_only_NOTreg

packs_map_only_All <- tm_shape(packs_map_only_df) + 
  tm_dots("type2" , 
          title = "", 
          size = 5,
          palette = c("royalblue4", "#298bd6"), 
          shapes = 21,
          border.lwd = NA,
          alpha = 0.5,
          legend.show = FALSE) +
  tm_text("no", 
          size=0.75, 
          fontface = "bold") +
  tm_layout(title = title_map3,
            title.size = 1.5,
            title.fontface = "bold",
            #legend.position = c('left','top'),
            title.position = c('center','top'));packs_map_only_All

packs_map_only_PA_all <- tm_shape(packs_map_only_df_PA_all) + 
  tm_symbols("type2" , 
             #title = "", 
             size = 7,
             palette = c("royalblue4", "#298bd6"), 
             shapes = 21,
             border.col = "red",
             border.lwd = 2,
             alpha = 0,
             legend.size.show = FALSE,
             legend.col.show = FALSE,
             legend.shape.show = FALSE,
  ) +
  # tm_text("no", 
  #         size=0.75, 
  #         fontface = "bold") +
  tm_layout(title = title_map3,
            title.size = 2,
            title.fontface = "bold",
            #legend.position = c('left','top'),
            title.position = c('center','top'));packs_map_only_PA_all


packs_map_only_PA_some <- tm_shape(packs_map_only_df_PA_some) + 
  tm_symbols("type2" , 
             size = 7,
             palette = c("royalblue4", "#298bd6"), 
             shapes = 21,
             border.col = "orange",
             border.lwd = 2,
             alpha = 0,
             legend.size.show = FALSE,
             legend.col.show = FALSE,
             legend.shape.show = FALSE,
  ) +
  # tm_text("no", 
  #         size=0.75, 
  #         fontface = "bold") +
  tm_layout(title = title_map3,
            title.size = 2,
            title.fontface = "bold",
            #legend.position = c('left','top'),
            title.position = c('center','top'));packs_map_only_PA_some

packs_map_only_RA <- tm_shape(packs_map_only_df_RA) + 
  tm_symbols("type2" , 
             #title = "", 
             size = 5.5,
             palette = c("royalblue4", "#298bd6"), 
             shapes = 21,
             border.col = "#00E804",
             border.lwd = 2,
             alpha = 0,
             legend.size.show = FALSE,
             legend.col.show = FALSE,
             legend.shape.show = FALSE,
  ) +
  # tm_text("no", 
  #         size=0.75, 
  #         fontface = "bold") +
  tm_layout(title = title_map3,
            title.size = 2,
            title.fontface = "bold",
            #legend.position = c('left','top'),
            title.position = c('center','top'));packs_map_only_RA


packs_map_only_Recours <- tm_shape(packs_map_only_df_Recours) + 
  tm_symbols("type2" , 
             #title = "", 
             size = 9,
             palette = c("white", "white"), 
             shapes = 21,
             border.col = "#0017FF",
             border.lwd = 2.5,
             alpha = 0,
             legend.size.show = FALSE,
             legend.col.show = FALSE,
             legend.shape.show = FALSE,
  ) +
  # tm_text("no", 
  #         size=0.75, 
  #         fontface = "bold") +
  tm_layout(title = title_map3,
            title.size = 2,
            title.fontface = "bold",
            #legend.position = c('left','top'),
            title.position = c('center','top'));packs_map_only_Recours


custom_leg <- tm_shape(packs_map_only_df_PA_all)+
  tm_dots(col = "white",
          alpha=1)+
  tm_add_legend(type = "symbol",
                col = c("royalblue4", "#298bd6", rep("white", nrow(packs_map_only_df))),
                labels = c(paste0("Packs CH (", nrow(pack_sui), ")"),
                           paste0("Transboundary packs (", nrow(pack_transb), ")"),
                           paste0(packs_map_only_df$no," = ", packs_map_only_df$name)),
                title = "",
                border.col = "white",
                alpha = 0.5) +
  tm_legend(legend.text.size = 0.75,
            legend.title.size = 0.9) +
  tm_layout(title = title_map3,
            title.size = 2,
            title.fontface = "bold",
            legend.position = c('left','center'),
            title.position = c('center','top'))

custom_leg_reg <- tm_shape(packs_map_only_df_notPA)+
  tm_dots(col = "white",
          alpha=0)+
  tm_add_legend(type = "symbol",
                col = c("white", "white","white", "white"), #,rep("white", nrow(packs_map_only_df))
                border.col = c("orange","red","white","#0017FF"),#,rep("white", nrow(packs_map_only_df))
                labels = c(paste0("partial regulation: 2/3 of the cubs (", nrow(packs_map_only_df_PA_some), ")"), # \n (2/3 des louveteaux ou géniteur particulièrement nuisible)
                           paste0("complete elimination (", nrow(packs_map_only_df_PA_all), ")"),
                           "(JSV 2023/24 - authorisations valid from 01.12.23 to 31.01.24)",
                           paste0("regulation suspended following an appeal to the Federal Administrative Court (", nrow(packs_map_only_df_Recours), ")")
                           
                ),
                title = "Proactive regulations authorised, planned by the cantons*:",
                alpha = 0.5)+
  tm_legend(legend.text.size = 0.75,
            legend.title.size = 0.9
  ) +
  tm_layout(title = title_map3,
            title.size = 2,
            title.fontface = "bold",
            legend.position = c('right','center'),
            title.position = c('center','top'));custom_leg_reg

custom_leg_reg_RA <- tm_shape(packs_map_only_df_notPA)+
  tm_dots(col = "white",
          alpha=0)+
  tm_add_legend(type = "symbol",
                col = c("white","white"), #,rep("white", nrow(packs_map_only_df))
                border.col = c("#00E804","white"),#,rep("white", nrow(packs_map_only_df))
                labels = c(paste0("partial regulation: 1/2 to 2/3 of the cubs (", nrow(packs_map_only_df_RA), ")"), 
                           "(JSV 2023 - authorisations valid until 31.03.24)" ),
                title = "Reactive regulations authorised, planned by the cantons*:",
                alpha = 0.5)+
  tm_legend(legend.text.size = 0.75,
            legend.title.size = 0.9
  ) +
  tm_layout(title = title_map3,
            title.size = 2,
            title.fontface = "bold",
            legend.position = c('right','center'),
            title.position = c('center','top'),
            title.bg.color = TRUE);custom_leg_reg_RA


wolf_pack_only_reg_canton <- 
  background +
  background_reg +
  custom_leg_reg_RA+
  custom_leg_reg +
  custom_leg+ 
  CHHS + 
  rivers + 
  country + 
  eumap+
  seen +
  cantons + 
  packs_map_only_Recours+
  packs_map_only_RA+
  packs_map_only_PA_all+
  #packs_map_only_NOTreg+
  packs_map_only_All+
  packs_map_only_PA_some
wolf_pack_only_reg_canton

wolf_pack_only_reg_comp <- 
  background +
  background_reg +
  custom_leg_reg_RA+
  custom_leg_reg +
  custom_leg+ 
  CHHS + 
  rivers + 
  country + 
  eumap+
  seen +
  comps_empty + 
  packs_map_only_Recours+
  packs_map_only_RA +
  packs_map_only_PA_all+
  #packs_map_only_NOTreg+
  packs_map_only_All+
  packs_map_only_PA_some
wolf_pack_only_reg_comp

wolf_pack_only_reg_cant_comp <- 
  background +
  background_reg +
  custom_leg_reg_RA+
  custom_leg_reg +
  custom_leg+ 
  CHHS + 
  rivers + 
  country + 
  eumap+
  cantons2+
  seen +
  comps_empty + 
  packs_map_only_Recours+
  packs_map_only_RA +
  packs_map_only_PA_all+
  #packs_map_only_NOTreg+
  packs_map_only_All+
  packs_map_only_PA_some
wolf_pack_only_reg_cant_comp

wolf_pack_only_reg_canton_grob <- tmap_grob(wolf_pack_only_reg_canton)
wolf_pack_only_reg_comp_grob <- tmap_grob(wolf_pack_only_reg_comp)
wolf_pack_only_reg_cant_comp_grob <- tmap_grob(wolf_pack_only_reg_cant_comp)


#ggdraw(map_pack_grob)+
Plot_pack_reg_cantons <- ggdraw() +
  draw_plot(wolf_pack_only_reg_canton_grob, 0, 0.07, 1, 0.9) +
  draw_plot(combined_plot, 0.1, 0, 0.8, 0.1) 

Plot_pack_reg_cantons

Plot_pack_reg_comp <- ggdraw() +
  draw_plot(wolf_pack_only_reg_comp_grob, 0, 0.07, 1, 0.9) +
  draw_plot(combined_plot, 0.1, 0, 0.8, 0.1) 

Plot_pack_reg_comp

Plot_pack_reg_cant_comp <- ggdraw() +
  draw_plot(wolf_pack_only_reg_cant_comp_grob, 0, 0.07, 1, 0.9) +
  draw_plot(combined_plot, 0.1, 0, 0.8, 0.1) 

Plot_pack_reg_cant_comp


# Save the ggdraw as a file
ggsave(paste0("EN_Rudel_Regulation_cantons_", Sys.Date(), ".jpg"), Plot_pack_reg_cantons, width = 12, height = 8, dpi = 500)
ggsave(paste0("EN_Rudel_Regulation_comp_", Sys.Date(), ".jpg"), Plot_pack_reg_comp, width = 12, height = 8, dpi = 500)
ggsave(paste0("EN_Rudel_Regulation_cantons_comp_", Sys.Date(), ".jpg"), Plot_pack_reg_cant_comp, width = 12, height = 8, dpi = 500)

