#####11/04/2019 Class Notes

library("plyr")
library("stringr")
library("ggplot2")
library("reshape2")
library("grid")
library("gridextra")
library("scales")
library("dplyr")


##create directory to store plots
suppressWarnings(dir.create("plots"))

#load data
load("ost2014_phy_t.Robj")


#Call functions:
#-----------------
# Compute the straight line distance (km) from the starting point of a lat,lon trajectory
dist.from.start <- function(lat, lon) {
  library("oce")
  geodDist(lat1=lat, lon1=lon, lat2=na.omit(lat)[1], lon2=na.omit(lon)[1])/1.852 # use if you want to convert from km to nautical miles
}

# Spectral colour map from ColorBrewer
spectral <- function(n=6) {
  library("RColorBrewer")
  rev(brewer.pal(name="Spectral", n=n))
}

scale_fill_spectral <- function(...) {
  scale_fill_gradientn(colours=spectral(...))
}
scale_colour_spectral <- function(...) {
  scale_colour_gradientn(colours=spectral(...))
}
  
  ##make plots----
#identify variables of interest to plot
vars=c("temp", "salinity", "sw.density","chl.ug.l") ##assigning "vars" outside ddply function
  
ddply(.data=phy_t, .variables="transect.id", function(x){
  
  x=na.omit(x)
  
  x$depth_round=round(x$depth, digits=1)
  
  dm=melt(x, id.vars=c("dateTime","depth_round"),measure.vars=vars)
  ##melt take wide and makes into long format
  t=ggplot(dm[dm$variable=="temp",], aes(x=(dateTime),y=-depth_round))+
    geom_line(aes(colour=value, size=value), na.rm=T, show.legend=TRUE)+
    scale_color_gradient(high=spectral(), na.value=NA) +
    scale_x_datetime(name="Time", labels=date_format("%H:%M"),
                     breaks =date_breaks("15 min"), minor_breaks = "5 min")+
    scale_y_continuous("depth", expand=c(0.01, 0.01)) +
    facet_grid(variable~.)+
    theme(panel.background=element_rect(fill="white"),
          panel.grid.major = element_line(colour="black"),
   strip.text.y=element_text(face="bold",size=12)) +
    theme(axis.title.x = element_text(face="bold",size=12), 
          axis.title.y = element_text(face="bold", size=12))+
    ggtitle(label=unique(x$transect.id))
  
  d=ggplot(dm[dm$variable=="sw.density",], aes(x=(dateTime),y=-depth_round))+
    geom_line(aes(colour=value, size=value), na.rm=T, show.legend=TRUE)+
    scale_color_gradient(high=spectral(), na.value=NA) +
    scale_x_datetime(name="Time", labels=date_format("%H:%M"),
                     breaks =date_breaks("15 min"), minor_breaks = "5 min")+
    scale_y_continuous("depth", expand=c(0.01, 0.01)) +
    facet_grid(variable~.)+
    theme(panel.background=element_rect(fill="white"),
          panel.grid.major = element_line(colour="black"),
          strip.text.y=element_text(face="bold",size=12)) +
    theme(axis.title.x = element_text(face="bold",size=12), 
          axis.title.y = element_text(face="bold", size=12))
  
  
s=ggplot(dm[dm$variable=="salinity",], aes(x=(dateTime),y=-depth_round))+
  geom_line(aes(colour=value, size=value), na.rm=T, show.legend=TRUE)+
  scale_color_gradient(high=spectral(), na.value=NA) +
  scale_x_datetime(name="Time", labels=date_format("%H:%M"),
                   breaks =date_breaks("15 min"), minor_breaks = "5 min")+
  scale_y_continuous("depth", expand=c(0.01, 0.01)) +
  facet_grid(variable~.)+
  theme(panel.background=element_rect(fill="white"),
        panel.grid.major = element_line(colour="black"),
        strip.text.y=element_text(face="bold",size=12)) +
  theme(axis.title.x = element_text(face="bold",size=12), ##can do "element_blank" if you dont wanna have any axis labels and nont want any NAs
        axis.title.y = element_text(face="bold", size=12))

c=ggplot(dm[dm$variable=="chl.ug.l",], aes(x=(dateTime),y=-depth_round))+
  geom_line(aes(colour=value, size=value), na.rm=T, show.legend=TRUE)+
  scale_color_gradient(high=spectral(), na.value=NA) +
  scale_x_datetime(name="Time", labels=date_format("%H:%M"),
                   breaks =date_breaks("15 min"), minor_breaks = "5 min")+
  scale_y_continuous("depth", expand=c(0.01, 0.01)) +
  facet_grid(variable~.)+
  theme(panel.background=element_rect(fill="white"),
        panel.grid.major = element_line(colour="black"),
        strip.text.y=element_text(face="bold",size=12)) +
  theme(axis.title.x = element_text(face="bold",size=12), 
        axis.title.y = element_text(face="bold", size=12))

g=grid.arrange(t,d,s,c,ncol=2, nrow=2) ## this function allows all the graphs to be plotted on one page, in the order that you define them.
  



#print image files to a directory
png(file=paste0("plots/", unique(x$transect.id), ".png"), width=8.5,
    height=14, units="in", res=300)
plot(g)
##"dev.off()"= cleans up files temporarily stored in RAM
dev.off()

}, .progress="text"





library("nutshell")
library("ggplot2")
library("tidyverse")
library("dplyr")

####generating a  basic heatmap
data("batting.2008")
bat=batting.2008; rm(batting.2008)

##CREATE DATAFRAME
#summarize metrics by team:find mean using players

bat.metrics=bat%>% group_by(teamID) %>% summarize(home.run=mean(HR, na.rm =T),
                                                  runs=mean(R,na.rm=T),
                                                  runs.batted.in=mean(RBI, na.rm=T),
                                                  hits=mean(H,na.rm=T))

bat.metrics


##get names of metrics
metric.names= names(bat.metrics[,c(2:5)])

#melt data
library("reshape2")
bat.metrics.melt=melt(data=bat.metrics, id.vars=c("teamID"),
                      measure.vars=metric.names,
                      variable.name="metrics")

##pick teams from"bat.metrics.melt" data frame to plot
teams=c('HOU','SEA','WAS','COL','OAK','ATL')

##PLOT:heatmap
#here, we use geom_tile()

ggplot(data=bat.metrics.melt[bat.metrics.melt$teamID %in% teams,],
       aes(x=metrics, y=teamID)) +
















