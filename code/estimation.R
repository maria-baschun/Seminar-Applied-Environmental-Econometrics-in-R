packages <- c("tidyverse", "MatchIt", "cobalt","fixest","ggplot2","ggplot2","webshot","stargazer")
install_if_missing(packages)

library(RColorBrewer)
library(webshot)
library(fixest)
library(dplyr)
library(ggplot2)
library(stargazer)
library(tidyverse)
library(MatchIt)
library(cobalt)

'##############################estimation of ATT###############################'

#make a copy of the real estate dataset
wk<-WK

#create a treatment status
wk$freisetzungen<-0
wk$freisetzungen[(wk$sell_year==2012) & (wk$ergg_1km %in% df_2012$ergg_1km)]<-1
wk$freisetzungen[(wk$sell_year==2013) & (wk$ergg_1km %in% df_2013$ergg_1km)]<-1
wk$freisetzungen[(wk$sell_year==2014) & (wk$ergg_1km %in% df_2014$ergg_1km)]<-1
wk$freisetzungen[(wk$sell_year==2015) & (wk$ergg_1km %in% df_2015$ergg_1km)]<-1
wk$freisetzungen[(wk$sell_year==2016) & (wk$ergg_1km %in% df_2016$ergg_1km)]<-1
wk$freisetzungen[(wk$sell_year==2017) & (wk$ergg_1km %in% df_2017$ergg_1km)]<-1
wk$freisetzungen[(wk$sell_year==2018) & (wk$ergg_1km %in% df_2018$ergg_1km)]<-1
wk$freisetzungen[(wk$sell_year==2019) & (wk$ergg_1km %in% df_2019$ergg_1km)]<-1
wk$freisetzungen[(wk$sell_year==2020) & (wk$ergg_1km %in% df_2020$ergg_1km)]<-1
wk$freisetzungen[(wk$sell_year==2021) & (wk$ergg_1km %in% df_2021$ergg_1km)]<-1
rm(df_2012,df_2013,df_2014,df_2015,df_2016,df_2017,df_2018,df_2019,df_2020,
   df_2021)
str(wk)
head(wk)

#set color
col=brewer.pal(n = 10, name = "PRGn")

#Number of individuals in treatment and control group
addmargins(table(wk$freisetzungen, useNA = "ifany"))

##########################ATT via unadjusted sample#############################

#mean comparison between treated and control group

mdif<-(mean(log(wk$price_sqm[wk$freisetzungen == 1]))-
              mean(log(wk$price_sqm[wk$freisetzungen == 0])))

#Regression estimation of the ATE
##simple univariate linear regression 

s.model<-lm(log(price_sqm)~ freisetzungen,
                 data = wk)
summary(s.model)

##multivariate regression with fixed effects and clustering by "plz"
model<-feols(log(price_sqm) ~ freisetzungen +baujahr + wohnflaeche +
            zimmeranzahl + balkon + objektzustand+
            etage + as.factor(sell_year)
            |as.factor(plz), 
             cluster="plz",
             data = wk)


summary(model)

###############################Estimating the PS ###############################

#How many % of an unmatched dataset are treated?
share.treated<-sum(wk$freisetzungen)/length(wk$freisetzungen)

share.treated_per.a<-rep(0,10)

for (i in 1:10){
  share.treated_per.a[i]<-
  sum(wk$freisetzungen[wk$sell_year==2011+i])/length(wk$freisetzungen[wk$sell_year==2011+i])}

#share.treated_per.a indicates the share of treated per year 


#use logit model to estimate PS

#To fit the model, uncomment the lines below
'ATTENTION! This can take up to 8 hours. You can load the PS from already 
fitted model instead (see section data_loading)'

#logit <- glm(freisetzungen ~ baujahr + wohnflaeche +
#                  balkon + objektzustand+ sell_year +as.factor(plz),
#                  data = wk,
#                  family = binomial(link = "logit"))

#saveRDS(logit, "logit_model.rds")
                                                                                                                                                                #summary(logit)
#predict PS
#PS <- predict(logit, type = "response")

#save PS
#saveRDS(PS, "PS.rds")

wk$pscore <-PS


#assess share of correct predictions, using the share of observations
#receiving treatment as cut-off

wk$predicted[wk$pscore <= share.treated] <- 0
wk$predicted[wk$pscore > share.treated] <- 1

(correct.pred <- addmargins(table(wk$freisetzungen, wk$predicted, 
                                  useNA = "ifany")))

(correct.pred[1,1]+ correct.pred[2,2])/correct.pred[3,3]

#0.8991926
# without postcode only: 0.5590766

# The share of correct predictions is higher than without 'ptz'.
# So the location have strong explanatory power for the treatment
# status of appartments. Hence the model that account for location explains a 
#larger share of variation in treatment assignment.
#(overfitting the PS model can make matching really difficult)


#####################sample balance before matching#############################

nrow(subset(wk,wk$freisetzungen==1))
nrow(subset(wk,wk$freisetzungen==0))
nrow(subset(wk,wk$freisetzungen==1))+nrow(subset(wk,wk$freisetzungen==0))
nrow(wk)


variables<-c("balkon","baujahr","kaufpreis","price_sqm","wohnflaeche",
             "zimmeranzahl","pscore","sell_year")

mean_1<-c( mean(wk$balkon[wk$freisetzungen==1],na.rm = T),
           mean(wk$baujahr[wk$freisetzungen==1],na.rm = T),
           mean(wk$kaufpreis[wk$freisetzungen==1],na.rm = T),
           mean(wk$price_sqm[wk$freisetzungen==1],na.rm = T),
           mean(wk$wohnflaeche[wk$freisetzungen==1],na.rm = T),
           mean(wk$zimmeranzahl[wk$freisetzungen==1],na.rm = T),
           mean(wk$pscore[wk$freisetzungen==1],na.rm = T),
           mean(wk$sell_year[wk$freisetzungen==1],na.rm = T))
            
mean_0<-c( mean(wk$balkon[wk$freisetzungen==0],na.rm = T),
           mean(wk$baujahr[wk$freisetzungen==0],na.rm = T),
           mean(wk$kaufpreis[wk$freisetzungen==0],na.rm = T),
           mean(wk$price_sqm[wk$freisetzungen==0],na.rm = T),
           mean(wk$wohnflaeche[wk$freisetzungen==0],na.rm = T),
           mean(wk$zimmeranzahl[wk$freisetzungen==0],na.rm = T),
           mean(wk$pscore[wk$freisetzungen==0],na.rm = T),
           mean(wk$sell_year[wk$freisetzungen==0],na.rm = T))

sd_1<-c( sd(wk$balkon[wk$freisetzungen==1],na.rm = T),
         sd(wk$baujahr[wk$freisetzungen==1],na.rm = T),
         sd(wk$kaufpreis[wk$freisetzungen==1],na.rm = T),
         sd(wk$price_sqm[wk$freisetzungen==1],na.rm = T),
         sd(wk$wohnflaeche[wk$freisetzungen==1],na.rm = T),
         sd(wk$zimmeranzahl[wk$freisetzungen==1],na.rm = T),
         sd(wk$pscore[wk$freisetzungen==1],na.rm = T),
         sd(wk$sell_year[wk$freisetzungen==1],na.rm = T))

sd_0<-c( sd(wk$balkon[wk$freisetzungen==0],na.rm = T),
         sd(wk$baujahr[wk$freisetzungen==0],na.rm = T),
         sd(wk$price_sqm[wk$freisetzungen==0],na.rm = T),
         sd(wk$wohnflaeche[wk$freisetzungen==0],na.rm = T),
         sd(wk$wohnflaeche[wk$freisetzungen==0],na.rm = T),
         sd(wk$zimmeranzahl[wk$freisetzungen==0],na.rm = T),
         sd(wk$pscore[wk$freisetzungen==0],na.rm = T),
         sd(wk$sell_year[wk$freisetzungen==0],na.rm = T))

diff<- mean_1 - mean_0


#  T-test on equivalence 

t_for_all_varibles<-function(){ t<-rep(NA,length(variables))

for ( i in 1:length(variables) ){
  test<- t.test(wk[[variables[i]]][wk$freisetzungen == 1], 
                alternative = "two.sided", wk[[variables[i]]][wk$freisetzungen == 0])
  
  t[i]<-test$statistic }

return(t)}

p_value_for_all_varibles<-function(){ t<-rep(NA,length(variables))

for ( i in 1:length(variables) ){
  test<- t.test(wk[[variables[i]]][wk$freisetzungen == 1], 
                alternative = "two.sided", wk[[variables[i]]][wk$freisetzungen == 0])
  
  t[i]<-test$p.value }

return(t)}

t<-t_for_all_varibles()
p_value<-p_value_for_all_varibles()


(sum.st<-data.frame(variables,mean_1,mean_0,sd_1,sd_0,
                    diff, t,p_value))

# ignore t and p-values for discrete variables, i.e. consider t and p values 
# only for static or binary variables 

rm(mean_1,mean_0,sd_1,sd_0,diff,t,p_value)

###########################Common support & overlap#############################

# --- Min/Max pscore in treatment and control group, remove observations
#according to min/max criterion
min(wk$pscore[wk$freisetzungen==0])
min(wk$pscore[wk$freisetzungen==1])
max(wk$pscore[wk$freisetzungen==0])
max(wk$pscore[wk$freisetzungen==1])

min.max <- wk %>% group_by(freisetzungen) %>% 
  summarize(min = min(pscore), 
            max = max(pscore))


wk<- wk %>% 
  mutate(minmax = ifelse(pscore >= max(min.max$min) & pscore <= 
                           min(min.max$max) , "in", "out"))

cs<-addmargins(table(wk$minmax,wk$freisetzungen))

wk_cs<-subset(wk, minmax == "in")

#'wk_cs' only containes obsevations withing the common support 

################### 3-to-1 NN matching without replacement######################

#matching can take up to 5 min:
nearest.match <- matchit(freisetzungen ~ baujahr + wohnflaeche +
                         balkon + objektzustand+as.factor(plz)+sell_year,
                         data = wk, 
                         method = "nearest",
                         ratio = 3,
                         replace = F, 
                         distance = wk$pscore,
                         discard = "both")


#MatchIt's summary statistics

#summary(nearest.match)

           #Control Treated
#All        297076    5362
#Matched     16011    5337
#Unmatched   50610       0
#Discarded  230455      25

###############sample balance after matching without replacment#################

# match.data() creates a data frame after matching

matched_wk<-match.data(nearest.match)

nrow(subset(matched_wk,matched_wk$freisetzungen==1))
nrow(subset(matched_wk,matched_wk$freisetzungen==0))
nrow(subset(matched_wk,matched_wk$freisetzungen==1))+nrow(
subset(matched_wk,matched_wk$freisetzungen==0))
nrow(matched_wk)



variables<-c("balkon","baujahr","kaufpreis","price_sqm","wohnflaeche",
             "zimmeranzahl","pscore","sell_year")

mean_1<-c( 
  mean(matched_wk$balkon[matched_wk$freisetzungen==1],na.rm = T),
  mean(matched_wk$baujahr[matched_wk$freisetzungen==1],na.rm = T),
  mean(matched_wk$kaufpreis[matched_wk$freisetzungen==1],na.rm = T),
  mean(matched_wk$price_sqm[matched_wk$freisetzungen==1],na.rm = T),
  mean(matched_wk$wohnflaeche[matched_wk$freisetzungen==1],na.rm = T),
  mean(matched_wk$zimmeranzahl[matched_wk$freisetzungen==1],na.rm = T),
  mean(matched_wk$pscore[matched_wk$freisetzungen==1],na.rm = T),
  mean(matched_wk$sell_year[matched_wk$freisetzungen==1],na.rm = T))

mean_0<-c( 
  mean(matched_wk$balkon[matched_wk$freisetzungen==0],na.rm = T),
  mean(matched_wk$baujahr[matched_wk$freisetzungen==0],na.rm = T),
  mean(matched_wk$kaufpreis[matched_wk$freisetzungen==0],na.rm = T),
  mean(matched_wk$price_sqm[matched_wk$freisetzungen==0],na.rm = T),
  mean(matched_wk$wohnflaeche[matched_wk$freisetzungen==0],na.rm = T),
  mean(matched_wk$zimmeranzahl[matched_wk$freisetzungen==0],na.rm = T),
  mean(matched_wk$pscore[matched_wk$freisetzungen==0],na.rm = T),
  mean(matched_wk$sell_year[matched_wk$freisetzungen==0],na.rm = T))

sd_1<-c( 
  sd(matched_wk$balkon[matched_wk$freisetzungen==1],na.rm = T),
  sd(matched_wk$baujahr[matched_wk$freisetzungen==1],na.rm = T),
  sd(matched_wk$kaufpreis[matched_wk$freisetzungen==1],na.rm = T),
  sd(matched_wk$price_sqm[matched_wk$freisetzungen==1],na.rm = T),
  sd(matched_wk$wohnflaeche[matched_wk$freisetzungen==1],na.rm = T),
  sd(matched_wk$zimmeranzahl[matched_wk$freisetzungen==1],na.rm = T),
  sd(matched_wk$pscore[matched_wk$freisetzungen==1],na.rm = T),
  sd(matched_wk$sell_year[matched_wk$freisetzungen==1],na.rm = T))

sd_0<-c( 
  sd(matched_wk$balkon[matched_wk$freisetzungen==0],na.rm = T),
  sd(matched_wk$baujahr[matched_wk$freisetzungen==0],na.rm = T),
  sd(matched_wk$price_sqm[matched_wk$freisetzungen==0],na.rm = T),
  sd(matched_wk$wohnflaeche[matched_wk$freisetzungen==0],na.rm = T),
  sd(matched_wk$wohnflaeche[matched_wk$freisetzungen==0],na.rm = T),
  sd(matched_wk$zimmeranzahl[matched_wk$freisetzungen==0],na.rm = T),
  sd(matched_wk$zimmeranzahl[matched_wk$freisetzungen==0],na.rm = T),
  sd(matched_wk$sell_year[matched_wk$freisetzungen==0],na.rm = T))

diff<- mean_1 - mean_0


#T-test on equivalence 

t_for_all_varibles<-function(){ t<-rep(NA,length(variables))

for ( i in 1:length(variables) ){
  test<-t.test(matched_wk[[variables[i]]][matched_wk$freisetzungen==1], 
               alternative = "two.sided", 
               matched_wk[[variables[i]]][matched_wk$freisetzungen == 0])
  
  t[i]<-test$statistic }

return(t)}

p_value_for_all_varibles<-function(){ t<-rep(NA,length(variables))

for ( i in 1:length(variables) ){
  test<-t.test(matched_wk[[variables[i]]][matched_wk$freisetzungen==1], 
               alternative = "two.sided",
               matched_wk[[variables[i]]][matched_wk$freisetzungen == 0])
  
  t[i]<-test$p.value }

return(t)}

t<-t_for_all_varibles()
p_value<-p_value_for_all_varibles()

(sum.st_ad<-data.frame(variables,mean_1,mean_0,sd_1,sd_0,
                            diff,t,p_value))

# ignore t and p-values for discrete variables, i.e. consider t and p values 
# only for static or binary variables 

rm(mean_1,mean_0,sd_1,sd_0,diff,t,p_value)

#############distribution of PS after matching without replacement##############

#create a color
## Convert hexadecimal colors to RGB
rgb_color0 <- col2rgb("#5AAE61")
rgb_color1 <- col2rgb("#C2A5CF")

# Create colors with 50% transparency using the rgb function
color0 <- rgb(rgb_color0[1], rgb_color0[2], rgb_color0[3], maxColorValue = 255, 
              alpha = 0.5 * 255)
color1 <- rgb(rgb_color1[1], rgb_color1[2], rgb_color1[3], maxColorValue = 255,
              alpha = 0.5 * 255)

#set graphical parameters 
par(mfrow=c(2,1),mai=c(0.5,0.8,0.2,0),oma=c(4,1,1,1),bg = "grey96")

#before matching 

#histogram in D=0
hist(wk$pscore[wk$freisetzungen==0],breaks=seq(0,1,0.05),
    freq = F,col=F,ylim=c(0,20),xlab=NULL,ylab="Density",main="Before matching")

grid(nx = NULL, ny = NULL, lty = 2,col = "gray",lwd = 1)

hist(wk$pscore[wk$freisetzungen==0],breaks=seq(0,1,0.05),
     freq = F,col=color0,main=NULL,border ="black",
     xlab=NULL,add=T)

#legend("topright",c("Control","Treatment"),fill=c(color0,color1),bty="n")

#histogram in D=1

hist(wk$pscore[wk$freisetzungen==1],breaks=seq(0,1,0.05),
     freq = F,col =color1,main=NULL,border ="black",
     xlab=NULL,add=T)

#after matching without replacment
#histogram in D=0
hist(matched_wk$pscore[matched_wk$freisetzungen==0],
     breaks=seq(0,1,0.05),freq = F, col=F,border ="black",ylim=c(0,5),
     xlab=NULL,ylab="Density",main="After matching without replacment")

grid(nx = NULL, ny = NULL, lty = 2,col = "gray",lwd = 1)

hist(matched_wk$pscore[matched_wk$freisetzungen==0],
     breaks=seq(0,1,0.05),freq = F,col=color0,main=NULL,border ="black",
     ylab ="Density",xlab=NULL,add=T)

legend(0.15,-1.8,c("Control","Treatment"),fill=c(color0,color1), bty="n",
        horiz=T,xpd=NA)

#histogram in D=1

hist(matched_wk$pscore[matched_wk$freisetzungen==1],
     breaks=seq(0,1,0.05),freq = F,col =color1,main=NULL,border ="black",
     ylab ="Density",xlab=NULL,add=T)

mtext("Propensity Score", outer = TRUE,side=1)

#save with width=800,height=700

#Density of propensity score in D=0 and D=1 after matching without replacement
par(mfrow=c(1,1),oma=c(1,1,1,1),mai=c(0.2,0.2,0.3,0.2))
plot(nearest.match, type = "jitter", interactive = F,col=col[1])
#save with width=550,height=550

# After 3-1 NN matching, the propensity score is more balanced.
#
# However, compered to control-apartments, larger share of treated-apartments 
# has a high probability of being treated. Thus we find few suitable
# controls for treated-apartments with height propensity score when matching.
# This is in line with the observation that our raw control group is different
# from the treatment group.



#####################ATT via adjusted sample without replacement################


#mean comparison between treated and control group

mdif_ad<-
  ( (mean(log(matched_wk$price_sqm[matched_wk$freisetzungen==1])))-
      (mean(log(matched_wk$price_sqm[matched_wk$freisetzungen==0]))) )    

#Regression estimation of the ATE

##simple univariate linear regression 

s.model_ad<-feols(log(price_sqm) ~ freisetzungen,
                       data = matched_wk)
summary(s.model_ad)

##multivariate regression with fixed effects and clustering by "plz"
model_ad<-feols(log(price_sqm) ~ freisetzungen + baujahr + wohnflaeche+
                    zimmeranzahl + balkon + objektzustand
                    + etage + as.factor(sell_year)
                    |as.factor(plz),
                    cluster = "plz",
                    weights = matched_wk$weights, data = matched_wk)

summary(model_ad)




##################### 3-to-1 NN matching with replacement#######################

nearest.match_r <- matchit(freisetzungen ~ baujahr + wohnflaeche +
                           balkon + objektzustand+as.factor(plz)+ sell_year,
                           data = wk, 
                           method = "nearest",
                           ratio = 3,
                           replace = T, 
                           distance = wk$pscore,
                           discard = "both")


#MatchIt's summary statistics
summary(nearest.match_r)

#Sample Sizes:
              #Control      Treated

#All           297076.      5362
#Matched (ESS)   4977.61    5337
#Matched         8966.      5337
#Unmatched      57655.         0
#Discarded     230455.        25


###############sample balance after matching without replacment#################

# match.data() creates a data frame after matching

matched_r_wk<-match.data(nearest.match_r)

nrow(subset(matched_r_wk,matched_r_wk$freisetzungen==1))
nrow(subset(matched_r_wk,matched_r_wk$freisetzungen==0))
nrow(subset(matched_r_wk,matched_r_wk$freisetzungen==1))+nrow(
  subset(matched_r_wk,matched_r_wk$freisetzungen==0))
nrow(matched_r_wk)



variables<-c("balkon","baujahr","kaufpreis","price_sqm","wohnflaeche",
             "zimmeranzahl","pscore","sell_year")

mean_1<-c( 
  mean(matched_r_wk$balkon[matched_r_wk$freisetzungen==1],na.rm = T),
  mean(matched_r_wk$baujahr[matched_r_wk$freisetzungen==1],na.rm = T),
  mean(matched_r_wk$kaufpreis[matched_r_wk$freisetzungen==1],na.rm = T),
  mean(matched_r_wk$price_sqm[matched_r_wk$freisetzungen==1],na.rm = T),
  mean(matched_r_wk$wohnflaeche[matched_r_wk$freisetzungen==1],na.rm = T),
  mean(matched_r_wk$zimmeranzahl[matched_r_wk$freisetzungen==1],na.rm = T),
  mean(matched_r_wk$pscore[matched_r_wk$freisetzungen==1],na.rm = T),
  mean(matched_r_wk$sell_year[matched_r_wk$freisetzungen==1],na.rm = T))

mean_0<-c( 
  mean(matched_r_wk$balkon[matched_r_wk$freisetzungen==0],na.rm = T),
  mean(matched_r_wk$baujahr[matched_r_wk$freisetzungen==0],na.rm = T),
  mean(matched_r_wk$kaufpreis[matched_r_wk$freisetzungen==0],na.rm = T),
  mean(matched_r_wk$price_sqm[matched_r_wk$freisetzungen==0],na.rm = T),
  mean(matched_r_wk$wohnflaeche[matched_r_wk$freisetzungen==0],na.rm = T),
  mean(matched_r_wk$zimmeranzahl[matched_r_wk$freisetzungen==0],na.rm = T),
  mean(matched_r_wk$pscore[matched_r_wk$freisetzungen==0],na.rm = T),
  mean(matched_r_wk$sell_year[matched_r_wk$freisetzungen==0],na.rm = T))

sd_1<-c( 
  sd(matched_r_wk$balkon[matched_r_wk$freisetzungen==1],na.rm = T),
  sd(matched_r_wk$baujahr[matched_r_wk$freisetzungen==1],na.rm = T),
  sd(matched_r_wk$kaufpreis[matched_r_wk$freisetzungen==1],na.rm = T),
  sd(matched_r_wk$price_sqm[matched_r_wk$freisetzungen==1],na.rm = T),
  sd(matched_r_wk$wohnflaeche[matched_r_wk$freisetzungen==1],na.rm = T),
  sd(matched_r_wk$zimmeranzahl[matched_r_wk$freisetzungen==1],na.rm = T),
  sd(matched_r_wk$pscore[matched_r_wk$freisetzungen==1],na.rm = T),
  sd(matched_r_wk$sell_year[matched_r_wk$freisetzungen==1],na.rm = T))

sd_0<-c( 
  sd(matched_r_wk$balkon[matched_r_wk$freisetzungen==0],na.rm = T),
  sd(matched_r_wk$baujahr[matched_r_wk$freisetzungen==0],na.rm = T),
  sd(matched_r_wk$price_sqm[matched_r_wk$freisetzungen==0],na.rm = T),
  sd(matched_r_wk$wohnflaeche[matched_r_wk$freisetzungen==0],na.rm = T),
  sd(matched_r_wk$wohnflaeche[matched_r_wk$freisetzungen==0],na.rm = T),
  sd(matched_r_wk$zimmeranzahl[matched_r_wk$freisetzungen==0],na.rm = T),
  sd(matched_r_wk$zimmeranzahl[matched_r_wk$freisetzungen==0],na.rm = T),
  sd(matched_r_wk$sell_year[matched_r_wk$freisetzungen==0],na.rm = T))


diff<- mean_1 - mean_0


#T-test on equivalence 

t_for_all_varibles<-function(){ t<-rep(NA,length(variables))

for ( i in 1:length(variables) ){
  test<-
    t.test(matched_r_wk[[variables[i]]][matched_r_wk$freisetzungen==1], 
           alternative = "two.sided", 
           matched_r_wk[[variables[i]]][matched_r_wk$freisetzungen == 0])
  
  t[i]<-test$statistic }

return(t)}

p_value_for_all_varibles<-function(){ t<-rep(NA,length(variables))

for ( i in 1:length(variables) ){
  test<-
    t.test(matched_r_wk[[variables[i]]][matched_r_wk$freisetzungen==1], 
           alternative = "two.sided",
           matched_r_wk[[variables[i]]][matched_r_wk$freisetzungen == 0])
  
  t[i]<-test$p.value }

return(t)}

t<-t_for_all_varibles()
p_value<-p_value_for_all_varibles()

(sum.st_ad_r<-data.frame(variables,mean_1,mean_0,sd_1,sd_0,
                              diff,t,p_value,variables))

# ignore t and p-values for discrete variables, i.e. consider t and p values 
# only for static or binary variables

rm(mean_1,mean_0,sd_1,sd_0,diff,t,p_value)

#############distribution of PS after matching with replacement#################

#create a color
## Convert hexadecimal colors to RGB
rgb_color0 <- col2rgb("#5AAE61")
rgb_color1 <- col2rgb("#C2A5CF")

# Create colors with 50% transparency using the rgb function
color0 <- rgb(rgb_color0[1], rgb_color0[2], rgb_color0[3], maxColorValue = 255, 
              alpha = 0.5 * 255)
color1 <- rgb(rgb_color1[1], rgb_color1[2], rgb_color1[3], maxColorValue = 255,
              alpha = 0.5 * 255)

#set graphical parameters 
par(mfrow=c(2,1),mai=c(0.5,0.8,0.2,0),oma=c(4,1,1,1),bg = "grey96")

#before matching 

#histogram in D=0
hist(wk$pscore[wk$freisetzungen==0],breaks=seq(0,1,0.05),
     freq = F,col=F,ylim=c(0,20),xlab=NULL,ylab="Density",main="Before matching")

grid(nx = NULL, ny = NULL, lty = 2,col = "gray",lwd = 1)

hist(wk$pscore[wk$freisetzungen==0],breaks=seq(0,1,0.05),
     freq = F,col=color0,main=NULL,border ="black",
     xlab=NULL,add=T)

#legend("topright",c("Control","Treatment"),fill=c(color0,color1),bty="n")

#histogram in D=1

hist(wk$pscore[wk$freisetzungen==1],breaks=seq(0,1,0.05),
     freq = F,col =color1,main=NULL,border ="black",
     xlab=NULL,add=T)

#after matching without replacment
#histogram in D=0
hist(matched_r_wk$pscore[matched_r_wk$freisetzungen==0],
     breaks=seq(0,1,0.05),freq = F, col=F,border ="black",ylim=c(0,5),
     xlab=NULL,ylab="Density",main="After matching with replacment")

grid(nx = NULL, ny = NULL, lty = 2,col = "gray",lwd = 1)

hist(matched_r_wk$pscore[matched_r_wk$freisetzungen==0],
     breaks=seq(0,1,0.05),freq = F, col=color0,main=NULL,border ="black",
     ylab ="Density",xlab=NULL,add=T)

legend(0.15,-1.5,c("Control","Treatment"),fill=c(color0,color1), bty="n",
       horiz=T,xpd=NA)

#histogram in D=1

hist(matched_r_wk$pscore[matched_r_wk$freisetzungen==1],
     breaks=seq(0,1,0.05),freq = F,col =color1,main=NULL,border ="black",
     ylab ="Density",xlab=NULL,add=T)

mtext("Propensity Score", outer = TRUE,side=1)

#save with width=800,height=700

#Density of propensity score in D=0 and D=1 after matching without replacement

par(mfrow=c(1,1),oma=c(1,1,1,1),mai=c(0.2,0.2,0.3,0.2))
plot(nearest.match_r, type = "jitter", interactive = F,col=col[1])

#save with width=550,height=550
# Note: The more often a control is matched the larger is its circle

# the PS are more balanced relative to matching without  replacement. But since
# controls are matched several times the estimate becomes less precise (higher
# variance). I.e. the treatment and control groups are better balanced, thus we 
# reduce bias, but these estimates rely on very few observations.

#####################ATT via adjusted sample with replacement###################

#mean comparison between treated and control group

mdif_ad_r<-(mean
                 (log(matched_r_wk$price_sqm[matched_r_wk$freisetzungen==1])))-
            (mean(log(matched_r_wk$price_sqm[matched_r_wk$freisetzungen==0])))    

#Regression estimation of the ATE

##simple univariate linear regression 

s.model_ad_r<-feols(log(price_sqm) ~ freisetzungen,
                         data = matched_r_wk)
summary(s.model_ad_r)

##multivariate regression with fixed effects and clustering by "plz"

model_ad_r<-feols(log(price_sqm) ~ freisetzungen + baujahr + wohnflaeche+
                     zimmeranzahl + balkon + objektzustand
                    +etage + as.factor(sell_year)
                    |as.factor(plz),
                     cluster = c("plz"),
                     weights = matched_r_wk$weights, data = matched_r_wk)

summary(model_ad_r)



# Note that the ATET is insignificant on the 5% level due 
# to the large standard errors of the estimate.

# But we can steel infer statistically significant effect of the treatment,
# on th 10% significance level 


rm(list=ls()[!(ls() %in% c("logit","freisetzungen","install_if_missing",
"s.model","model","s.model_ad","model_ad","s.model_ad_r","model_ad_r",
"matched_r_wk","matched_wk","wk", "sum.st","sum.st_ad","sum.st_ad_r","df")
)])


#summary of all models 

summary(s.model)
summary(model)

summary(s.model_ad)
summary(model_ad)

summary(s.model_ad_r)
summary(model_ad_r)


'#############################END OF ESTIMATION################################'
'##############################################################################'

