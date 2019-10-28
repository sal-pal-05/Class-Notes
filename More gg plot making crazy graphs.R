###10/28/2019

library(ggplot2)


##one geom:

data("economics")
e=economics
head(e)

##graphing unemployment vs date
unemploy=ggplot(data= e, aes(x= date, y= unemploy)) + geom_line()
unemploy

##multiple geoms----
##adding pres party by date to unemployment graph

data("presidential")
pres=presidential
head(pres)

## addigning the data for the rectangle to come from the"pres" dataset and not the "econ" dataset
###created rectangles, from pres dataset, starting at begining dat and ending at last sate from pres dtaset. fill=party(dem, rep etc), -Inf and Inf is to fill the whole screen w colors, alpha is transparency of colored rectangles
ggplot(e) +
  geom_rect(data= pres, aes(xmin= start, xmax= end, fill =party),
            ymin=-Inf, ymax=Inf, alpha =0.2) 


##add vertical lines(dividing boxes w light gray lines)
ggplot(e) +
  geom_rect(data= pres, aes(xmin= start, xmax= end, fill =party),
            ymin=-Inf, ymax=Inf, alpha =0.2) +
  geom_vline(data= pres, 
             aes(xintercept=as.numeric(start)),
             colour="grey50", alpha=0.5)


##adding text, from pres dataset, labelling the name pf the pres during each divided time rectangle.

ggplot(e) +
  geom_rect(data= pres, aes(xmin= start, xmax= end, fill =party),
            ymin=-Inf, ymax=Inf, alpha =0.2) +
  ##scale_fill will fill in the rectangles with the designated colors
  scale_fill_manual(values = c("dodgerblue", "firebrick3")) +
  geom_vline(data= pres, 
             aes(xintercept=as.numeric(start)),
             colour="grey50", alpha=0.5) +
  geom_text(data=pres, aes(x=start, y=2500, 
                           label=name), size=3, 
            vjust=0, hjust=0, nudge_x = 50)


####added line (unemployment rate vs time)
ggplot(e) +
  geom_line(aes( x=date, y=unemploy)) +
  geom_rect(data= pres, aes(xmin= start, xmax= end, fill =party),
            ymin=-Inf, ymax=Inf, alpha =0.2) +
  ##scale_fill will fill in the rectangles with the designated colors
  scale_fill_manual(values = c("dodgerblue", "firebrick3")) +
  geom_vline(data= pres, 
             aes(xintercept=as.numeric(start)),
             colour="grey50", alpha=0.5) +
  geom_text(data=pres, aes(x=start, y=2500, 
                           label=name), size=3, 
            vjust=0, hjust=0, nudge_x = 50)





caption=paste(strwrap("Unemployment Rates in the U.S.
                      have varied a lot over the years", 40),
              collapse ="\n")

## making new objects for the annotation
yrng=range(e$unemploy) ##highest and lowest number of unemployees
xrng=range(e$date) ##date where the highest and lowest unemployment rates were.


####add a caption "annotate" function

ggplot(e) +
  geom_line(aes( x=date, y=unemploy)) +
  geom_rect(data= pres, aes(xmin= start, xmax= end, fill =party),
            ymin=-Inf, ymax=Inf, alpha =0.2) +
  ##scale_fill will fill in the rectangles with the designated colors
  scale_fill_manual(values = c("dodgerblue", "firebrick3")) +
  geom_vline(data= pres, 
             aes(xintercept=as.numeric(start)),
             colour="grey50", alpha=0.5) +
  #geom_text(data=pres, aes(x=start, y=2500, 
   #                        label=name), size=3, 
    #        vjust=0, hjust=0, nudge_x = 50)
annotate("text", x=xrng[1], y=yrng[2], label=caption, 
         hjust=0, vjust=1, size=4)
  
  
  
 ##adjusting x and y ranges to move the caption around

date=as.Date("1960-01-01")





ggplot(e) +
  geom_line(aes( x=date, y=unemploy)) +
  geom_rect(data= pres, aes(xmin= start, xmax= end, fill =party),
            ymin=-Inf, ymax=Inf, alpha =0.2) +
  ##scale_fill will fill in the rectangles with the designated colors
  scale_fill_manual(values = c("dodgerblue", "firebrick3")) +
  geom_vline(data= pres, 
             aes(xintercept=as.numeric(start)),
             colour="grey50", alpha=0.5) +
  #geom_text(data=pres, aes(x=start, y=2500, 
  #                        label=name), size=3, 
  #        vjust=0, hjust=0, nudge_x = 50)
  annotate("text", x=date, y=yrng[2], label=caption, ##making "x=date" makes the annotation begin at the year 1960
           hjust=0, vjust=1, size=4)





##stack and group (stack by depth and group by area)
load(file="fish_data.Rdata")

##summarizing data first to make a smaller df w the data we want, yr, depth, and area, and mean parcel
library(tidyverse)

fs=fish %>% group_by(area_fac, depth_fac, yr_fac) %>% summarize (parcel.count=length(parcel.id))
head(fs)



ggplot(data=fs) +
  geom_bar(aes(x= area_fac, y= parcel.count), stat="identity")

##stacking
ggplot(data=fs) +
  geom_bar(aes(x= area_fac, y= parcel.count, fill=depth_fac) , ##fill by depth, thus 3 dif colors per area will be done
           position="stack",
           stat="identity")


##layering
ggplot(data=fs) +
  geom_bar(aes(x= area_fac, y= parcel.count, fill=depth_fac) , ##fill by depth, thus 3 dif colors per area will be done
           position="stack",
           stat="identity") +
  facet_grid(.~yr_fac)
  
##layering 2 (changing the position of "~." changes the layering)
ggplot(data=fs) +
  geom_bar(aes(x= area_fac, y= parcel.count, fill=depth_fac) , ##fill by depth, thus 3 dif colors per area will be done
           position="stack",
           stat="identity") +
  facet_grid(yr_fac~.)

###instead of stacked, now they are put next to each oter based on depth and year
ggplot(data=fs) +
  geom_bar(aes(x= area_fac, y= parcel.count, fill=depth_fac) , ##fill by depth, thus 3 dif colors per area will be done
           position="dodge",
           stat="identity") +
  facet_grid(yr_fac~.)




ggplot(data=fs) +
  geom_bar(aes(x= yr_fac, y= parcel.count, fill=depth_fac) , ##fill by depth, thus 3 dif colors per area will be done
           position="stack",
           stat="identity") +
  facet_wrap(.~area_fac)





