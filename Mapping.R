##11/13/2019
library(tidyverse)
install.packages("ggmap")
install.packages("osmdata") ##alllows you to query a database with a lot of spatial information.
library(ggmap)
library(osmdata)


##downloading maps

##STAMEN MAPS:terain, toner,watercolor

read_csv(file="LDWF2008seine.csv")

#querying info on LA
LA = getbb("Louisiana")
##gives lat/long most north n south, east n west of LA
LA

##change "stamen"to google to query googlemaps
##Louisiana "Terrain" map
map = get_stamenmap(bbox= LA, zoom = 8, 
                    maptype= "terrain")
map
##use ggmap function to plot map
ggmap(map)

##LA "Toner" map
map.toner= get_stamenmap(bbox=LA, zoom =8, 
                   maptype= "toner-background") 

ggmap(map.toner)

##LA "Watercolor" map
map.wc= get_stamenmap(bbox=LA, zoom =8, 
                         maptype= "watercolor") 

ggmap(map.wc)


##Ohio watercolor

map.oh= get_stamenmap(bbox=getbb('ohio'), zoom =8, 
                      maptype= "watercolor") 
ggmap(map.oh)


##Italy
IT=getbb("italy")

map.it= get_stamenmap(bbox=IT, zoom =8, 
                         maptype= "toner-background") 
ggmap(map.it)


VI=getbb("Venice Italy")
LAF=getbb("lafayette Louisiana")

map.ven= get_stamenmap(bbox=VI, zoom =12, 
                      maptype= "toner-background") 
ggmap(map.ven)

map.laf=get_stamenmap(bbox=LAF, zoom =15, 
                      maptype= "toner-background") 
ggmap(map.laf)

##using manual values
bbox = c(left = 20, bottom = 0, right = 30, top = 20)

map1=get_stamenmap( bbox=bbox, zoom =6, 
                    map= "terrain")
ggmap(map1)


## points on the map

df=read_csv ("LDWF2008seine.csv")

##plot maps using this dataset, to plot only were datapoints occur
##lon=xaxis
##set bbox to have the leftmost, rightmost, topmost and bottommost values 
##from the dataset using "min and max" functions
bb=c(left = min(df$lon)-0.2, bottom = min(df$lat)-0.2, ##the + and minus 0.2 is so that our points are not on the edge of the map.
     right = max(df$lon)+0.2, top = max(df$lat)+0.2)
bb

la.map = get_stamenmap (bbox =bb, zoom =8, 
                        map ="terrain-background")
ggmap(la.map) +
  geom_point(data = df, aes (x =lon, y =lat))


##add color to datapoints
## put outside"aes"
ggmap(la.map) +
  geom_point(data = df, aes (x =lon, y =lat), color ="red")

##color code samples by the basin they were taken from
##color by basin(put inside "aes")
ggmap(la.map) +
  geom_point(data = df, aes (x =lon, y =lat, color=basin))


##manually assign specific colors to datapoints from each basin
ggmap(la.map) +
  geom_point(data = df, aes (x =lon, y =lat, color=basin)) +
scale_color_manual(values = c("purple", "red", "blue", "orange", "yellow", "green"))

unique((df$species))
## plotting info from largemouth bass with size, by abundance

ggmap(la.map)+
  geom_point(data = df[df$species =="Largemouth Bass",],
             aes(x = lon, y = lat))


##map where you caught largemouth bass, relative to where you caught all the other spp
ggmap(la.map)+
  geom_point(data = df, aes (x =lon, y =lat)) +
  geom_point(data = df[df$species =="Largemouth Bass",],
             aes(x = lon, y = lat),color="red")


##group by the spp type and summarize by the number of individuals caught
n=df %>% group_by(species) %>% summarize(n=n())
d=n[order(-n$n),]##sorting data from least common to most common (n= the number of id caught)
view(d)


##plot menhaden by abundance(size of dot= number of individuals caught/location)
ggmap(la.map) +
  geom_point(data=df[df$species == "Gulf Menhaden",],
             aes(x = lon, y = lat, size = num)) ##"size"= it will make the dot bigger, if more individuals were caught at a specific location

##changing the color scale of the heatmap
ggmap(la.map) +
  geom_point(data=df[df$species == "Gulf Menhaden",],
             aes(x = lon, y = lat, color=num, size=num)) +
  scale_color_gradientn(colors = terrain.colors(10))






