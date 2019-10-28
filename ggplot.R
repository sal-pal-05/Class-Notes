## Plotting----
## Using GGplot 2
library(ggplot2)
load(file="fish_data.Rdata")


##These are NON "GGPLOT", plotting functions( just the basic R plotting functions)
##subset fish data for only "Deep" (to make the datafile smaller)
fish.deep =fish[fish$depth_fac=="Deep",]

plot(x=fish.deep$parcel.start.lon,
     y= fish.deep$parcel.start.lat)

plot(x=fish$parcel.start.lon,
     y= fish$parcel.start.lat)

##histograms----
hist(fish$parcel.density.m3)

##log transform data to make the distribution look normal
hist(log10(fish$parcel.density.m3))



##ggplot2 functions----
##the plus sign adds layers, not doing math to function b/c outside parentheses)
ggplot(data=mpg, aes(x= displ, y= hwy)) + 
geom_point()

##adding color to the graph y adding onto "aes"
ggplot(data=mpg, aes(x= displ, y= hwy)) + 
  geom_point(colour="blue")

head(mpg)
unique(mpg$class)

####want to use colors to have data represent different car types(auto random colors)

ggplot(data=mpg, aes(x= displ, y= hwy, colour=class)) + 
  geom_point()

####want to use colors to have data represent different car types(manually entered colorscolors)
length(unique(mpg$class))

ggplot(data=mpg, aes(x= displ, y= hwy, colour=class)) + 
  geom_point() +
  scale_colour_manual(values=c("firebrick2","blue", "darkgreen","goldenrod","cornsilk2", "chocolate2", "deeppink1"))


####changed"geom_point" to "geom_line"
ggplot(data=mpg, aes(x= displ, y= hwy, colour=class)) + 
  geom_line() 


####changed"geom_point" to "geom_boxplot"
ggplot(data=mpg, aes(x= displ, y= hwy, colour=class)) + 
  geom_boxplot() 

##facets w/geom_line
ggplot(data=mpg, aes(x= displ, y= hwy, colour=class)) + 
  geom_line() +
  facet_wrap(~class)

##facets w/ geom_point

ggplot(data=mpg, aes(x= displ, y= hwy, colour=class)) + 
  geom_point() +
  facet_wrap(~class)


##facets w/ geom_boxplot

ggplot(data=mpg, aes(x= displ, y= hwy, colour=class)) + 
  geom_boxplot() +
  facet_wrap(~class)

##changing "ncol and nrow" to make the plots show up differently
ggplot(data=mpg, aes(x= displ, y= hwy, colour=class)) + 
  geom_point() +
  facet_wrap(~class, ncol=1)

####add a smoother---- adds line of best fit (can remove"color=class" to default data points to Black n White)

ggplot(data=mpg, aes(x= displ, y= hwy, color=class)) + 
  geom_point() +
  geom_smooth()

####adding parameters to "geom_smooth()" ##regression line
ggplot(data=mpg, aes(x= displ, y= hwy)) + 
  geom_point() +
  geom_smooth(method = "lm")

####histogram----:geom_histogram()"

ggplot(data=mpg, aes(displ, fill=drv)) +
  geom_histogram()

##adjusting"binwidth"
ggplot(data=mpg, aes(displ, fill=drv)) +
  geom_histogram(binwidth =0.5)

####histogram----:geom_freqpoly()"
ggplot(data=mpg, aes(displ, color=drv)) +
  geom_freqpoly(binwidth =0.5)
















