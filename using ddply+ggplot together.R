10/30/2019

####ddply for plotting

library(plyr)
library(tidyverse)

load(file="fish_data.Rdata")
###whatever you wanna do inside a function, do it outside FIRST

ggplot(fish, aes(parcel.length.m, parcel.density.m3, color = depth_fac))+
geom_point()+
  xlab("Parcel Length (m)")+
  ylab(expression(paste("Parcel Density (",m^3,")")))
##now copy and paste the function inside ddply

##using ddply to plot multiple objects.----
##break up dataset(subsettig) by "variable" depth_fac, returns a DF
##have to SAVE the ggplot object, for the function to run, so put the "saveggplot" inside the function.
ddply(.data=fish, .variables="depth_fac", function(x){  ##since we name the function"x", replace "fish" with "x" everywhere in the function.
 pl= ggplot(x, aes(parcel.length.m, parcel.density.m3))+
    geom_point()
  name=unique(x$depth_fac)##makes an object, as a character string of the factor were separating by(depth_fac)
  
  ggsave(filename= paste0(name,".tiff"),###3 files will be made "shallow.tiff",mid.tiff and deep.tiff
         plot=pl, width=4, height=3, units="in",
         dpi= 600, compression="lzw")##"filenae"=name it saves the object as, compression=makes file size 1/10 original size
}, .progress="text")


###wanna know on the plot, which plot we are using, 
##modifying the way the plot looks:
##default:uses column name as axes name
##"labs"=labels, x, and y columns
ddply(.data=fish, .variables="depth_fac", function(x){
  name=unique(x$depth_fac) ##makes an object, as a character string of the factor were separating by(depth_fac)##since we name the function"x", replace "fish" with "x" everywhere in the function.
  pl= ggplot(x, aes(parcel.length.m, parcel.density.m3)) +
  ##makes an object, as a character string of the factor were separating by(depth_fac)
    geom_point() +
    xlab("Parcel Length (m)")+
    ylab(expression(paste("Parcel Density (",m^3,")")))+
    ggtitle(name)
  
  
  ggsave(filename= paste0(name,".tiff"),###3 files will be made "shallow.tiff",mid.tiff and deep.tiff
         plot=pl, width=4, height=3, units="in",
         dpi= 600, compression="lzw")##"filenae"=name it saves the object as, compression=makes file size 1/10 original size
}, .progress="text")


####separate by transect ID
ddply(.data=fish, .variables="transect.id", function(x){
  name=unique(x$transect.id) ##makes an object, as a character string of the factor were separating by(depth_fac)##since we name the function"x", replace "fish" with "x" everywhere in the function.
  pl= ggplot(x, aes(parcel.length.m, parcel.density.m3)) +
    ##makes an object, as a character string of the factor were separating by(depth_fac)
    geom_point() +
    xlab("Parcel Length (m)")+
    ylab(expression(paste("Parcel Density (",m^3,")")))+
    ggtitle(name) ##it will name it whatever the transect id is, because we sp;it the data by "transect.id"
  
  
  ggsave(filename= paste0(name,".tiff"),###3 files will be made "shallow.tiff",mid.tiff and deep.tiff
         plot=pl, width=4, height=3, units="in",
         dpi= 600, compression="lzw")##"filenae"=name it saves the object as, compression=makes file size 1/10 original size
}, .progress="text")

##facet wrap will put all 3 graphs, shallow, mid and deep on one plot



ddply(.data=fish, .variables="depth_fac", function(x){  ##since we name the function"x", replace "fish" with "x" everywhere in the function.
  pl= ggplot(x, aes(parcel.length.m, parcel.density.m3))+
    geom_point()
  name=unique(x$depth_fac)##makes an object, as a character string of the factor were separating by(depth_fac)
  facet_wrap(~depth_fac)
  ggsave(filename= paste0(name,".tiff"),###3 files will be made "shallow.tiff",mid.tiff and deep.tiff
         plot=pl, width=4, height=3, units="in",
         dpi= 600, compression="lzw")##"filenae"=name it saves the object as, compression=makes file size 1/10 original size
}, .progress="text")



##MPlotting 3 variables:make heatmaps on plots, using 3rd variable (size,color etc)
data("mtcars")
##3rd variable, by color
ggplot(mtcars, aes(vs, cyl, fill=mpg)) +
geom_tile()

##3rd variable, size and color
ggplot(mtcars, aes(wt, mpg, ))+
  geom_point(aes(size= cyl, color=cyl))

ggplot(mtcars, aes(wt, mpg, ))+
  geom_point(aes(color=hp))+
  scale_colour_continuous(type="viridis")+
  theme_classic()





