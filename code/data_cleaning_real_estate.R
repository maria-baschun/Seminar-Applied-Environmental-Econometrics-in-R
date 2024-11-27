packages <- c("tidyverse", "dplyr", "tidyr", "readxl", "readr")
install_if_missing(packages)

library(tidyverse)
library(dplyr)
library(tidyr)
library(readxl)
library(readr)


'#########################data cleaning: Real-Estate###########################'

#rename row data set

WK<- CampusFile_WK_cities

'modify `adat` and `edat` in HK and WK data sets (e.g. 2007m1 --> 200701 
2007m10-->200710)'

transform_datum<-function(y){if (nchar(y)==6 ){y<-gsub("m", "0",y)} 
  else{ y<-gsub("m", "",y )}}

#WK
adatWK<-WK$adat
head(adatWK)
adatWK<-unlist( lapply(X=adatWK, FUN =transform_datum )) 
head(adatWK)

edatWK<-WK$edat
head(edatWK)
edatWK<-unlist( lapply(X=edatWK, FUN = transform_datum)) 
head(edatWK)

WK$adat<-as.numeric(adatWK)
WK$edat<-as.numeric(edatWK)

rm(adatWK,edatWK)


#we need to eliminate the year 2022 'WK', since we don't have the
#data of emissions for this year 

#consider 10 year only

WK<-WK[(WK$edat<202201),]
WK<-WK[(WK$edat>201112),]
#row data has 927319
#here we lose 37,49% of original data set
#new data set has 579654 observations == 100% from now on 

#some `obid` can be observed in `WK` several times 
(WK %>% group_by(obid) %>% count())


#let's have a look on those objects, which ID are present at least twice 

double_id_WK<-WK %>% group_by(obid) %>% count()
(double_id_WK<-double_id_WK[double_id_WK$n>=2,])

#here we sort them and show all characteristics from objects with same ID next 
#to each other 
( double_id_WK<-arrange( WK[ WK$obid %in% as.numeric(unlist(double_id_WK[1])),]
  ,obid) )

# now we will delete objects that have same `obid` but different `ergg_1km`, as 
# the `ergg_1km` is identifying for the choice of group (treatment or control). 

## Which units fall into this category?

### how many times a certain observation of `obid` and `ergg_1km`  was observed  
(a_w<-double_id_WK %>% group_by(obid,ergg_1km)%>%count())

### which "obid" are observed with more than one `ergg_1km`? We remove those:

b_w<-a_w %>% group_by(obid) %>% count()
unique(b_w$n)
to_remove_w <-b_w[b_w$n>=2,1]

'In `to_remove_w` we extracted the id of objects that are 
listed with more then one post code. Now we can eliminate those from `WK` data 
set.'

WK<-subset(WK,subset=!(WK$obid %in% to_remove_w$obid) )
#here we have 576170 i.e. we lost with that step 3484  observations
#which correspond to 0,60% of original data set (2012-2021) 


#remove variables that are not necessary anymore:
rm(to_remove_w,a_w,b_w)

'We are going to continue cleaning the data set.
The goal is the data set where each `obid` is unique.'

#select only relevant variables 

(WK<-subset(WK,select=c("obid" ,"plz","kaufpreis","baujahr","wohnflaeche",
    "zimmeranzahl","blid", "adat","edat", "gid2019", "price_sqm","balkon",
    "ergg_1km","objektzustand","etage")))

################################################################################
#To not to lose 27,70 % of the row data set we will rename "Other missing"
# in "unknown" for the variable `etage`. We will still keep this observations 
# even though they are missing, since we will not use this variable for the 
# estimation of pscore, but just as a control variable. 

WK$etage[WK$etage=="Other missing"]<-"unknown"


#replace "Other missing" with NA
f<-function(x){na_if(x,"Other missing")}

WK<-na.omit(as.data.frame(apply(WK,2,FUN=f)))
#we first had 579654 observation 
#then we had 576170 i.e. we lost 3484  observations ~0.60%
#now we have 488959 i.e. we lost with that step 87211 observations
#which correspond to 15,05%% of original data set (2012-2021) 


#are there still objects, which ID are present at least twice? 
#Yes! There are less now, but still many those observations

double_id_WK<-WK %>% group_by(obid) %>% count()
(double_id_WK<-double_id_WK[double_id_WK$n>=2,])

#here we sort them and show all variables from objects with same ID next to 
#each other 
( double_id_WK<-arrange( WK[ WK$obid %in% as.numeric(unlist(double_id_WK[1])) 
                             , ],obid) )

'If two or more observations have same `obid`,`plz`,`baujahr`,`wohnflaeche`,
#`zimmeranzahl`,`blid`,`gid2019`, `balkon`,`ergg_1km`,`objektzustand`,`etage`
#but different (or same)  `kaufpreis`,`adat`,`edat`,`price_sqm` we just keep one 
#observation with the latest `edat`. 

If two or more observations with the same `obid` have different `plz`,`baujahr`,
`wohnflaeche`,`zimmeranzahl`,`blid` `balkon`,`ergg_1km`,`ergg_1km`,`gid2019`,
`objektzustand` or `etage` we delete all the observations with that `obid`.'
################################################################################

a_w<- WK %>% group_by(obid ,plz,baujahr,wohnflaeche,zimmeranzahl,blid, gid2019,balkon,
           ergg_1km,objektzustand,etage) %>% 
  dplyr::summarize(adat = adat[which.max(edat)], kaufpreis=kaufpreis[which.max(edat)], 
            price_sqm=price_sqm[which.max(edat)],edat = max(edat)) 

b_w<-a_w %>% group_by(obid)%>% count()
(to_remove_w<-(b_w[b_w$n >=2,1]))
#13714 unique observations

WK<-subset(a_w,subset=!(a_w$obid %in% to_remove_w$obid) )

#we first had 579654 observation 
#then we had 576170 i.e. we lost 3484  observations ~0.60%
#then we had 488959 i.e. we lost 87211 observations ~15,05%
#now we have 389605 i.e. we lost with that step 99354 observations
#which correspond to 17,14% of original data set (2012-2021).


#check results and remove remove variables that are not necessary anymore:

#number of observations (each `obid` is unique)
length(unique(WK$obid))
nrow(WK)

rm(to_remove_w,a_w,b_w,double_id_WK)

#modify variables   

WK$balkon[WK$balkon=="No"]<-0
WK$balkon[WK$balkon=="Yes"]<-1

character_in_numeric<-function(x){
  x<-as.numeric(x)
}

WK[c("plz","obid","baujahr","wohnflaeche","zimmeranzahl","balkon", "adat",
  "kaufpreis", "price_sqm","edat","gid2019")]<-
  apply(WK[c("plz","obid","baujahr","wohnflaeche","zimmeranzahl","balkon",
  "adat","kaufpreis" ,"price_sqm","edat","gid2019")],2,FUN=character_in_numeric)

#change mode of 'gid2019'
number_observations_panel$gid2019<-as.numeric(number_observations_panel$gid2019)


WK<-left_join(WK,number_observations_panel[c("city_name","gid2019")],
              by="gid2019")

rm(number_observations_panel)


WK$objektzustand[WK$objektzustand=="Completely renovated"]<-"Good"
WK$objektzustand[WK$objektzustand=="First occupancy after reconstruction"]<-
                                   "Good"
WK$objektzustand[WK$objektzustand=="Modernised"]<-"Good"
WK$objektzustand[WK$objektzustand=="Reconstructed"]<-"Good"
WK$objektzustand[WK$objektzustand=="First occupancy"]<-"Good"
WK$objektzustand[WK$objektzustand=="Like new"]<-"Good"
WK$objektzustand[WK$objektzustand=="Well kempt"]<-"Good"

WK$objektzustand[WK$objektzustand=="By arrangement"]<-"Bad"
WK$objektzustand[WK$objektzustand=="Needs renovation"]<-"Bad"
WK$objektzustand[WK$objektzustand=="Dilapidated"]<-"Bad"

WK$objektzustand <- relevel(as.factor(WK$objektzustand), 
                                 ref = "Not specified")

#aggregate floor

WK$etage[WK$etage=="Implausible value"]<-"1000"
WK$etage[WK$etage=="unknown"]<-"2000"

WK$etage[WK$etage=="-1"]<-"-1"
WK$etage[WK$etage=="0"]<-"0"
WK$etage[WK$etage=="1"]<-"1"
WK$etage[WK$etage=="2"]<-"2"
WK$etage[WK$etage=="3"]<-"3"
WK$etage[(as.numeric(WK$etage)>=4) & (as.numeric(WK$etage)<=6) ]<-"4_6"
WK$etage[(as.numeric(WK$etage)>6) & (as.numeric(WK$etage)<=500)
         & WK$etage!="4_6"]<-"7+"


WK$etage[WK$etage=="1000"]<-"Implausible value"
WK$etage[WK$etage=="2000"]<-"Unknown"
#ignore the warnings on that matter, they are harmless 

#check:
#pie(table(WK$etage,useNA = "ifany"))

WK$etage<- relevel(as.factor(WK$etage), 
                   ref = "Unknown")

rm(CampusFile_WK_cities,f,transform_datum,
   character_in_numeric)


#create the month and the year when the advertisement was not shown anymore 
WK$sell_year<-trunc((WK$edat)/100)
WK$sell_month<-(WK$edat)-(trunc((WK$edat)/100)*100)


# everything is same but different 'plz','obid','sell_month','adat' or 'edat'? 
#leave just one of them with the latest 'edat' 
WK <- WK %>%
  group_by(baujahr,wohnflaeche, zimmeranzahl, blid, gid2019 ,balkon 
           ,ergg_1km , objektzustand,kaufpreis ,price_sqm,city_name,etage,
           sell_year) %>%
  dplyr::summarize(n(),obid=unique(obid)[1],adat = adat[which.max(edat)],
                   edat=max(edat),plz=unique(plz)[1],sell_month=max(sell_month))
#deleate the "n()" column 
WK<-WK[,-14] 

#we first had 579654 observation 
#then we had  576170 i.e. we lost 3484  observations ~0,60%
#then we had  488959 i.e. we lost 87211 observations ~15,05%
#then we had  389605 i.e. we lost 99354 observations ~17,14% 
#now we have 302438 i.e. we lost with that step 87067 observations
#which correspond to 15,02% of original data set (2012-2021)



#TOTAL LOSS 47.81%
'##########################END OF THE DATA CLEANING############################'
'##############################################################################'

