library(tidyverse)
load("fish_data.Rdata")

library(plyr)

##ddply
##adding column denoting depths denoting "shallow" and "deep"
##test suset to make sure all functions inside the"ddply" umbrella function work
##Sandwich function =lines 11-29
d=fish[fish$depth_fac=="Deep",] ##subsetting to get only ones in depth fac that are "deep"
##using, fish, make a subset "x" that is the different levels in "depth_fac"
nd=ddply(.data=fish, .variables="depth_fac",function(x){
  
  
 z=unique(x$depth_fac) 

 depth_condition=function(y){
   if(y=="Deep")
     q=50
   else if (y=="Mid")
   q=25
   else 
    q=15
 }
 
 x$depth_z=depth_condition(y = z)
 
  return(x)
 
}, .inform = T, .progress="text")



x$depth=depth_condition (y="Mid")
test
head(d) ##subsetter "d" section of data

head(nd)
unique(nd$depth_z)  ## should get 3 values based on the if else conditions it was given in the "sandwich function"

##adply----

#list all the physical data files in a given directory
batch_data=list.files("batch_data/", full=TRUE, pattern="ISIIS")
batch_data

##Umbrella Function with "adply" = lines 49-82
##phy will have all the months data collapsed into a SINGLE dataframe
phy=adply(batch_data, 1, function(file){
  
  
  #read the data
  d=read.table(batch_data[1], sep="\t", skip=10, header=TRUE, fileEncoding = "ISO-8859-1", stringsAsFactors = FALSE,
               quote="\"",check.names = FALSE, encoding = "UTF-8", 
               na.strings ="9999.99")
  
  #clean names
  head=names(d)
  head=str_replace(head, "\\(.*\\)", "")
  head=str_trim(head)
  head=make.names(head)
  head=tolower(head)
  head=str_replace(head,fixed(".."), ".")
  
  ##assign names
  names(d)=head
  
    #create a proper date+time format
  date=scan(batch_data[1], what="character", skip=1, nlines=1, quiet=TRUE)
  
  d$date=date[2]
  
  d$dateTime=str_c(d$date, d$time, sep=" ")
  
  d$dateTime= as.POSIXct(strptime(d$dateTime,
                                  format="%m/%d/%y %H:%M:&OS", 
                                  tz="America/New_York"))
  
  return(d)


}, .inform=T, .progress="text")



##for loops NOTE= I used "t" as object for each different function and rempoed and remade the object, so clear environment before re-running each function
#1) starting w temp conversions

t=data.frame(f_deg=seq(0,100,1))

##when making a new column, Fill it in with NAs, first, then start adding your values after.
t$c_deg=NA
t$k_deg=NA


##can do "in 1:10" if you want it to apply the function to only the 10 rows, can change the range also, if you do(t), it will go through EVERY single row in the dataset.
for(i in 1:nrow(t)){
  ##dont have to use"i" can use any letter you want, it just has to be the same letter inside the function
  t[i,]$c_deg=(t[i,]$f_deg - 32) * (9/5) ##pull out "if" row, pull out value in f_deg column, and do math on values in f_deg
  t[i,]$k_deg=(t[i,]$c_deg + 273.15)
  
}


##testing to see if mathematical operations inside for loop are working
i=1
t[1,]
t


##2) combine with if else, set a floor map

t=data.frame(f_deg=seq(0,100,1))
t$c_deg=NA
t$k_deg=NA
t$rel_temp=NA



###NOTE: Can use to rturn R^2 and p-values for all rown in a data set
for(i in 1:nrow(t)){
  t[i,]$c_deg=(t[i,]$f_deg - 32) * (9/5)
  t[i,]$k_deg=(t[i,]$c_deg + 273.15)

t[i,]$rel_temp=ifelse(test=t[i,]$c_deg < 0,
                      yes="cold",
                      no="not cold") ## if the values of "c_deg" is <0, it will display as "cold" in the newly created column"rel_temp" (and #'s>0 will be "not cold")
}
rm(t)


#####
t=data.frame(f_deg=seq(0,100,1))
t$c_deg=NA
t$k_deg=NA
t$rel_temp=NA
goldilocks=function(x){
  
  if (x<=0)
    t[i,]$rel_temp="frozen"
  
  else if (x>0 &x<=50)
    t[i,]$rel_temp ="cold"
  
  else if(x>50 & x<=70)
    t[i,]$rel_temp="warm"
  else
    t[i,]$rel_temp="hot"
    
  }

for(i in 1:nrow(t)){
  t[i,]$c_deg=(t[i,]$f_deg - 32) * (9/5)
  t[i,]$k_deg=(t[i,]$c_deg + 273.15)
  t[i,]$rel_temp=goldilocks(x=t[i,]$c_deg)
}

view(t)


##Example 1 making your own for loop to convert from g-> mg ->ug
##making a data frame to convert from grams to micrograms

##Also remember, that for the function to run, the object in the function"i" must have a value prior to running the function.
i=1

v=data.frame(mass_g=seq(0,100,1))


v$mil_grams=NA
v$mic_grams=NA



for(i in 1:nrow(v)){
  
  v[i,]$mil_grams=(v[i,]$mass_g / 1000) 
  v[i,]$mic_grams=(v[i,]$mass_g /1000000 )
  
}

view(v)


##3)
y=seq(1,10,0.5)
x=seq(1,20,1)

d=data.frame(interaction =seq(1,10,0.5))
##"y" could be diff spp

for(i in 1:length(y)) {
  
  for(k in 1:19) {
    
    d$output=y[i] + x[k]
  }
}


##10/23/2019----

patch.list=list()
max.brks.index=nrow(brks)
max=max(brks$no)-1

for(k in brks$no[1]:max {
  p.mid=in.patch[in.patch$r.index >=brks$r.index[k] &
                 in.patch$r.index <brks$r.index[k+1], ]
  
  if(nrow(p.mid)>0){
    p.mid$patch.id =k+1
    patch.list[[k]] =p.mid
  }
}  

patch.df =do.call("rbind", patch.list)    
 


    

  
  
  
  
  
  
  
  
  
  
  
  
  
  
})




