packages <- c("ggplot2", "sf", "osmdata","gridExtra","cowplot")
install_if_missing(packages)

library(ggplot2)
library(sf)
library(osmdata)
library(gridExtra)
library(cowplot)

#########################What cities should we plot?############################

#which year were the most appartments for sale treated?
sort((table(wk$sell_year[wk$freisetzungen==1])))

#which cities do have the most treated apartments for sale in 2020?
sort(prop.table(table(wk$city_name[wk$freisetzungen==1 & wk$sell_year==2020])))
sort(table(wk$city_name[wk$freisetzungen==1 & wk$sell_year==2020]))
# Hannover      Düsseldorf    Munich      Berlin 

#which cities do have the most apartments for sale in 2020?
sort(prop.table(table(wk$city_name[wk$sell_year==2020])))
#Cologne    Hamburg     Munich     Berlin 


par(mfrow=c(2,2),mai=c(0,0,0,0.1),oma=c(0,0.1,1.5,0.2))
################################Plot Berlin##################################### 
df_2020<-subset(df,(min_year<=2020 & max_year>= 2020))



#here we have all facilities in Berlin in 2020
lat_Berlin<-df_2020$lat[df_2020$ort=="Berlin" & !is.na(df_2020$lat) ]
lon_Berlin<-df_2020$lon[df_2020$ort=="Berlin" & !is.na(df_2020$lon) ]

#here we have facilities, that are affecting properties for sale

Berlin_ergg<-wk[ wk$freisetzungen==1 & wk$city_name=="Berlin" &
                             wk$sell_year == 2020, ]$ergg_1km

lat_Berlin_aff<-subset(df_2020, ergg_1km %in% Berlin_ergg)$lat
lon_Berlin_aff<-subset(df_2020, ergg_1km %in% Berlin_ergg)$lon




# Fetching the geodata for Berlin from OpenStreetMap
berlin_bbox <- getbb("Berlin, Germany")
berlin_osm <- opq(bbox = berlin_bbox) %>%
  add_osm_feature(key = "boundary", value = "administrative") %>%
  osmdata_sf()

# Extracting the polygon data for Berlin
berlin_polygon <- berlin_osm$osm_multipolygons %>%
  filter(name == "Berlin")

# Generate dataset for all companies
Berlin <- data.frame(
  name = "Berlin",
  lat = lat_Berlin, # Latitude
  lon = lon_Berlin  # Longitude
)

# Generate dataset only for "affecting" companies 

Berlin_aff <- data.frame(
  name = "Berlin_aff",
  lat = lat_Berlin_aff, # Latitude
  lon = lon_Berlin_aff  # Longitude
)


# Converting the sample data into an sf object
sf_Berlin <- st_as_sf(Berlin, coords = c("lon", "lat"), crs = 4326)


# Converting the sample data into an sf object
sf_Berlin_aff <- st_as_sf(Berlin_aff, coords = c("lon", "lat"), crs = 4326)


# Plotting the map
ggplot1<-ggplot() +                                             # Berlin-Kontur
  geom_sf(data = berlin_polygon, fill = "grey96", color = "black") +  
  geom_sf(data = sf_Berlin, color = "#C2A5CF", size = 2) +   
  geom_sf(data = sf_Berlin_aff, color = "#40004B", size = 2) +             
  theme_minimal()+
  labs(title = "Berlin")+
  theme(                                              #Titel fett und links oben
    plot.title = element_text(face = "bold", hjust = 0, vjust = 1) 
  )

################################Plot München#################################### 

#here we have all facilities in München in 2020
lat_München<-df_2020$lat[df_2020$ort=="München" & !is.na(df_2020$lat) ]
lon_München<-df_2020$lon[df_2020$ort=="München" & !is.na(df_2020$lon) ]

#here we have facilities, that are affecting properties for sale

München_ergg<-wk[ wk$freisetzungen==1 & wk$city_name=="Munich" &
                   wk$sell_year == 2020, ]$ergg_1km

lat_München_aff<-subset(df_2020, ergg_1km %in% München_ergg)$lat
lon_München_aff<-subset(df_2020, ergg_1km %in% München_ergg)$lon




# Fetching the geodata for Berlin from OpenStreetMap
München_bbox <- getbb("Munich, Germany")
München_osm <- opq(bbox = München_bbox) %>%
  add_osm_feature(key = "boundary", value = "administrative") %>%
  osmdata_sf()

# Extracting the polygon data for Munich
München_polygon <- München_osm$osm_multipolygons %>%
  filter(name == "München")

# Generate dataset for all companies 
München <- data.frame(
  name = "München",
  lat = lat_München, # Latitude
  lon = lon_München  # Longitude
)

# Generate dataset only for "affecting" companies 

München_aff <- data.frame(
  name = "München_aff",
  lat = lat_München_aff, # Latitude
  lon = lon_München_aff  # Longitude
)


# Converting the sample data into an sf object
sf_München <- st_as_sf(München, coords = c("lon", "lat"), crs = 4326)


# Converting the sample data into an sf object
sf_München_aff <- st_as_sf(München_aff, coords = c("lon", "lat"), crs = 4326)


# Plotting the map
ggplot2<-ggplot() +                                           # München-Kontur
  geom_sf(data = München_polygon, fill = "grey96", color = "black") + 
  geom_sf(data = sf_München, color = "#C2A5CF", size = 2) +   
  geom_sf(data = sf_München_aff, color = "#40004B", size = 2) +  
  scale_x_continuous(breaks = seq(11.35, 11.75, by = 0.1)) + 
  scale_y_continuous(breaks = seq(48.25, 48.05, by = -0.05)) + 
  theme_minimal() +
  labs(title = "Munich")+
  theme(                                                  # Fett und links oben
    plot.title = element_text(face = "bold", hjust = 0, vjust = 1)  
  )
    


################################Plot Düsseldorf#################################

#here we have all facilities in Düsseldorf in 2020
lat_Düsseldorf<-df_2020$lat[df_2020$ort=="Düsseldorf" & !is.na(df_2020$lat) ]
lon_Düsseldorf<-df_2020$lon[df_2020$ort=="Düsseldorf" & !is.na(df_2020$lon) ]

#here we have facilities, that are affecting properties for sale

Düsseldorf_ergg<-wk[ wk$freisetzungen==1 & wk$city_name=="Düsseldorf" &
                   wk$sell_year == 2020, ]$ergg_1km

lat_Düsseldorf_aff<-subset(df_2020, ergg_1km %in% Düsseldorf_ergg)$lat
lon_Düsseldorf_aff<-subset(df_2020, ergg_1km %in% Düsseldorf_ergg)$lon




# Fetching the geodata for Berlin from OpenStreetMap
Düsseldorf_bbox <- getbb("Düsseldorf, Germany")
Düsseldorf_osm <- opq(bbox = Düsseldorf_bbox) %>%
  add_osm_feature(key = "boundary", value = "administrative") %>%
  osmdata_sf()

# Extracting the polygon data for Düsseldorf
Düsseldorf_polygon <- Düsseldorf_osm$osm_multipolygons %>%
  filter(name == "Düsseldorf")

# Generate dataset for all companies 
Düsseldorf <- data.frame(
  name = "Düsseldorf",
  lat = lat_Düsseldorf, # Breitengrad
  lon = lon_Düsseldorf  # Längengrad
)

# Generate dataset only for "affecting" companies 

Düsseldorf_aff <- data.frame(
  name = "Düsseldorf_aff",
  lat = lat_Düsseldorf_aff, # Breitengrad
  lon = lon_Düsseldorf_aff  # Längengrad
)


# Converting the sample data into an sf object
sf_Düsseldorf <- st_as_sf(Düsseldorf, coords = c("lon", "lat"), crs = 4326)


# Converting the sample data into an sf object
sf_Düsseldorf_aff <- st_as_sf(Düsseldorf_aff, coords = c("lon", "lat"), 
                              crs = 4326)


# Plotting the map
  ggplot3 <- ggplot() +     # Düsseldorf-Kontur
  geom_sf(data = Düsseldorf_polygon, fill = "grey96", color = "black") +  
  geom_sf(data = sf_Düsseldorf, color = "#C2A5CF", size = 2) +   
  geom_sf(data = sf_Düsseldorf_aff, color = "#40004B", size = 2) +             
  scale_x_continuous(breaks = seq(-6.7, 7, by = 0.1)) +  
  theme_minimal() +         # Beispielwerte: Abstände der x-Achse auf 0,1 setzen
  labs(title = "Düsseldorf") +
  theme(                                        #Titel fett und links oben
    plot.title = element_text(face = "bold", hjust = 0, vjust = 1) 
  )

################################Plot Hannover###################################

#here we have all facilities in Hannover in 2020
lat_Hannover<-df_2020$lat[df_2020$ort=="Hannover" & !is.na(df_2020$lat) ]
lon_Hannover<-df_2020$lon[df_2020$ort=="Hannover" & !is.na(df_2020$lon) ]

#here we have facilities, that are affecting properties for sale

Hannover_ergg<-wk[ wk$freisetzungen==1 & wk$city_name=="Hannover" &
                       wk$sell_year == 2020, ]$ergg_1km

lat_Hannover_aff<-subset(df_2020, ergg_1km %in% Hannover_ergg)$lat
lon_Hannover_aff<-subset(df_2020, ergg_1km %in% Hannover_ergg)$lon




# Fetching the geodata for Berlin from OpenStreetMap
Hannover_bbox <- getbb("Hannover, Germany")
Hannover_osm <- opq(bbox = Hannover_bbox) %>%
  add_osm_feature(key = "boundary", value = "administrative") %>%
  osmdata_sf()

# Extrahieren der Polygon-Daten für Hannover
Hannover_polygon <- Hannover_osm$osm_multipolygons %>%
  filter(name == "Hannover")

# Generate dataset for all companies 
Hannover <- data.frame(
  name = "Hannover",
  lat = lat_Hannover, # Breitengrad
  lon = lon_Hannover  # Längengrad
)

# Generate dataset only for "affecting" companies 

Hannover_aff <- data.frame(
  name = "Hannover_aff",
  lat = lat_Hannover_aff, # Breitengrad
  lon = lon_Hannover_aff  # Längengrad
)


# Converting the sample data into an sf object
sf_Hannover <- st_as_sf(Hannover, coords = c("lon", "lat"), crs = 4326)


# Converting the sample data into an sf object
sf_Hannover_aff <- st_as_sf(Hannover_aff, coords = c("lon", "lat"), crs = 4326)


# Plotting the map
ggplot4<-ggplot() +                                           # Hannover-Kontur
  geom_sf(data = Hannover_polygon, fill = "grey96", color = "black") +  
  geom_sf(data = sf_Hannover, color = "#C2A5CF", size = 2) +   
  geom_sf(data = sf_Hannover_aff, color = "#40004B", size = 2) +             
  theme_minimal()+
  scale_x_continuous(breaks = seq(9.6, 9.9, by = 0.1)) + 
  scale_y_continuous(breaks = seq(52.40, 52.30, by = -0.05)) + 
  labs(title = "Hannover")+
  theme(                                            # Titel fett und links oben
    plot.title = element_text(face = "bold", hjust = 0, vjust = 1)  
   
  )

##############################Plot legend#######################################


# Create example graphic with legend
ggplot_with_legend <- ggplot(mpg, aes(x = displ, y = hwy, color = factor(cyl)))+ 
  geom_point() + 
  scale_color_manual(name = NULL,
                   values = c("4" = "#40004B", "6" = "#C2A5CF"),
                   labels = c("Affecting facility", "Non-affecting facility")) +
  theme_void() + 
  theme(
    legend.position = "right",
    legend.text = element_text(size = 11)  # Doppelte Schriftgröße
  ) +
  guides(color = guide_legend(override.aes = list(shape = 22, size = 3, 
         fill = c("#40004B", "#C2A5CF")), direction = "vertical"))

# Extract legend
ggplot5 <- get_legend(ggplot_with_legend)

##############################Plot all four cities###########################

hlay <- rbind(c(1,1,1,2,2,2),
              c(5,3,3,4,4,4))

grid.arrange(ggplot1, ggplot2,ggplot3, ggplot4, ggplot5,ncol = 2,
             layout_matrix=hlay)

#Delete newly initialized variable

rm(list=ls()[!(ls() %in% c("logit","freisetzungen","install_if_missing",
"s.model","model","s.model_ad","model_ad","s.model_ad_r","model_ad_r",
"matched_r_wk","matched_wk","wk", "sum.st","sum.st_ad","sum.st_ad_r","df")
)])
