packages <- c("sgo", "dplyr")
install_if_missing(packages)

library(sgo)
library(dplyr)

'########################data cleaning: Emissions##############################'

#Keep only facilities that release emissions to the air. 
freisetzungen<-subset(freisetzungen, (umweltkompartiment=="Luft"))


'adding "errg_1km" variable using lon/lat of emission units'


# The lonlat_to_ergg1km() sub-function takes a dataframe with given
# longitudinal (lon) and latitudinal (lat) column data as its only argument.
# It falls back on the {sgo} package to convert theses lon/lat variables
# to x/y coordinate variables (x_sw/ y_sw) in the Lambert Azimuthal Equal Area 
# (LAEA) projection  system, marking the south-west-corner of a spanned 1-skm
# grid cell, as defined by the 'INfrastructure for SPatial InfoRmation in
# Europe' (INSPIRE). The Equal Area Grids (ETRS89-LAEA (EPSG:3035)) are used.
# Fianlly, we convert the two coordinate variables to one unique 1-skm grid cell
# variable, called 'ergg_1km'.
# For illustration, one basic example:
'
# We convert lon = 13.607872 and lat = 50.958631 to LAEA (EPSG:3035) coordinates
  by:
coord <- sgo_etrs_laea(sgo_points(list(13.607872, 50.958631), epsg=4258))
coord
# Converting it back yields the exact lon/lat data which we started with:
sgo_laea_etrs(sgo_points(list(coord$x, coord$y), epsg=3035))
# In this case, the respective errg_1km variable would be
paste(substr(coord$x, 1, 4), substr(coord$y, 1, 4), sep = "_") # 4574_3100
'
df<-freisetzungen %>% mutate(lat=geo_lat_wgs84,lon=geo_long_wgs84) %>% 
  select(!c(geo_lat_wgs84,geo_long_wgs84))


# The function returns the input dataframe with the computed ergg_1km variable.
lonlat_to_ergg1km <- function(df) {
  df$ergg_1km <- NA
  df$x_sw <- NA
  df$y_sw <- NA
  for (i in 1:nrow(df)) {
    laea_coordinates <- sgo::sgo_etrs_laea(sgo_points(list(df$lon[i],
                                                      df$lat[i]), epsg = 4258))
    x_sw <- substr(laea_coordinates$x, 1, 4)
    y_sw <- substr(laea_coordinates$y, 1, 4)
    df$x_sw[i] <- as.integer(x_sw)*1000
    df$y_sw[i] <- as.integer(y_sw)*1000
    df$ergg_1km[i] <- paste(x_sw, y_sw, sep = "_")
  }
  df$x_sw <- as.integer(df$x_sw)
  df$y_sw <- as.integer(df$y_sw)
  return(df)
}

freisetzungen<-lonlat_to_ergg1km(df)
rm(df,lonlat_to_ergg1km)

#If a company, i.e. the lat,lon,betriebsname (ergg_1km is unique for each 
#combination of lat lon) emitted in a year e.g. 2010 and then in 2015 it is 
#assumed that it has been there the whole time in between 

'unite observations with the same location but slyghtly different enterprise
name'

#first we check manualy  that the companies name for each lat,lon combination
#is approximitly tha same, e.g. "Kraftwerk Grenzach-Wyhlen" and 
#"Kraftwerk Grenzach-Wyhlen  KGW GmbH". See "aaa":

ccc <- freisetzungen %>%
       group_by(lat,lon,betriebsname,ergg_1km) %>% count()

ccc<-ccc[-ncol(ccc)]

ddd<-subset((ccc %>% group_by(lat,lon,ergg_1km) %>% 
     dplyr::summarize(n=n())), n>1)

# only lat-lon combinations with more than one compony name:

aaa <- ccc %>%
  semi_join(ddd, by = c("lat", "lon", "ergg_1km")) %>%
  arrange(lat, lon)

#for each lat,lon combination there is only one company, 
#whereby the names slightly differ. Now we can use (lat,lon) combination as
#unique identifier of a production facility.

rm(aaa,ccc,ddd)

#Note that 'ergg_1km' is unique for the combination of longitude and latitude 

df<-freisetzungen %>% group_by(lat,lon,ergg_1km) %>% 
  dplyr::summarize( min_year = min(jahr), max_year = max(jahr), 
  betriebsname=unique(betriebsname)[1],sector=unique(sector)[1], 
  ort=unique(ort)[1])
 

'create for each year the data set with in that year "active" 1km^2 sells'

df_2012<-subset(df,(min_year<=2012 & max_year>= 2012))
df_2013<-subset(df,(min_year<=2013 & max_year>= 2013))
df_2014<-subset(df,(min_year<=2014 & max_year>= 2014))
df_2015<-subset(df,(min_year<=2015 & max_year>= 2015))
df_2016<-subset(df,(min_year<=2016 & max_year>= 2016))
df_2017<-subset(df,(min_year<=2017 & max_year>= 2017))
df_2018<-subset(df,(min_year<=2018 & max_year>= 2018))
df_2019<-subset(df,(min_year<=2019 & max_year>= 2019))
df_2020<-subset(df,(min_year<=2020 & max_year>= 2020))
df_2021<-subset(df,(min_year<=2021 & max_year>= 2021))

#df %>% group_by(lat,lon) %>% count()

'#########################END OF THE DATA CLEANING#############################'
'##############################################################################'