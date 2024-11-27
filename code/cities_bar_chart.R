packages <- c("cobalt","MatchIt","tidyverse","dplyr","ggplot2","fixest",
              "sf","webshot","RColorBrewer")
install_if_missing(packages)

library(RColorBrewer)
library(webshot)
library(fixest)
library(dplyr)
library(ggplot2)

library(tidyverse)
library(MatchIt)
library(cobalt)




##################before vs after matching without replqcement##################

#create a color
## Convert hexadecimal colors to RGB
rgb_color0 <- col2rgb("#1B7837")
rgb_color1 <- col2rgb("#762A83")

# Create colors with 50% transparency using the rgb function
color0 <- rgb(rgb_color0[1], rgb_color0[2], rgb_color0[3], maxColorValue = 255, 
              alpha = 0.5 * 255)
color1 <- rgb(rgb_color1[1], rgb_color1[2], rgb_color1[3], maxColorValue = 255,
              alpha = 0.5 * 255)


'#city'

#befor matching
par(mfrow=c(2,1),mai=c(0.3,0.8,0.2,0),oma=c(2.2,1,2,1),bg = "grey96")

#barplot in D=0
barplot(prop.table(table(wk$city_name[wk$freisetzungen==0])),
        ylim=c(0,0.25),col=F,names.arg=F,yaxt = "n",ylab= "Relative frequency",
        space =c(0.2,rep(0.35,14)) )

grid(nx = NULL, ny = NULL, lty = 2,col = "gray",lwd = 1)

barplot(prop.table(table(wk$city_name[wk$freisetzungen==0])),
        ylim=c(0,0.25),col=color0,names.arg=F, yaxt = "n",add= T,
        space =c(0.2,rep(0.35,14)))


legend(x=2,y=0.25,legend=c("Control","Treatment"),fill=c(color0,color1),bty="n",
       horiz=T,xpd=T, x.intersp = 0.2, text.width = strwidth("Control"))
       

#barplot in D=1

vec<-prop.table(table(wk$city_name[wk$freisetzungen==1]))

barplot(vec,ylim=c(0,0.25),col=color1,yaxt = "n",cex.names = 0.8,add=T, 
        space =c(0.35,rep(0.35,14)))


axis(2,at=seq(0,0.25,0.05),labels=c("0","0.05","0.10","0.15","0.20","0.25"))



#after matching without replacement
#barplot in D=0

vec<-
  prop.table(table(matched_wk$city_name[matched_wk$freisetzungen==0]))


barplot(vec,ylim=c(0,0.25),col=F,names.arg=F,yaxt = "n",
        ylab= "Relative frequency", space =c(0.2,rep(0.35,14)))

grid(nx = NULL, ny = NULL, lty = 2,col = "gray",lwd = 1)

barplot(vec, ylim=c(0,0.25),col=color0,names.arg=F, yaxt = "n",add= T,
        space =c(0.2,rep(0.35,14)))


#legend("top",legend=c("Control","Treatment"),fill=c(color0,color1),bty="n",
#       list(w=1,h=1),horiz=T)

#barplot in D=1

vec<-
  prop.table(table(matched_wk$city_name[matched_wk$freisetzungen==1]))


barplot(vec,ylim=c(0,0.25),col=color1,yaxt = "n",cex.names = 0.8,add=T,
        space =c(0.35,rep(0.35,14)))


axis(2,at=seq(0,0.25,0.05),labels=c("0","0.05","0.10","0.15","0.20","0.25"))
#save with width=1190,height=830

##################before vs after matching with replqcement#####################

#create a color
## Convert hexadecimal colors to RGB
rgb_color0 <- col2rgb("#1B7837")
rgb_color1 <- col2rgb("#762A83")

# Create colors with 50% transparency using the rgb function
color0 <- rgb(rgb_color0[1], rgb_color0[2], rgb_color0[3], maxColorValue = 255, 
              alpha = 0.5 * 255)
color1 <- rgb(rgb_color1[1], rgb_color1[2], rgb_color1[3], maxColorValue = 255,
              alpha = 0.5 * 255)


'#city'

#befor matching
par(mfrow=c(2,1),mai=c(0.3,0.8,0.2,0),oma=c(2.2,1,2,1),bg = "grey96")

#barplot in D=0
barplot(prop.table(table(wk$city_name[wk$freisetzungen==0])),
        ylim=c(0,0.25),col=F,names.arg=F,yaxt = "n",ylab= "Relative frequency",
        space =c(0.2,rep(0.35,14)) )

grid(nx = NULL, ny = NULL, lty = 2,col = "gray",lwd = 1)

barplot(prop.table(table(wk$city_name[wk$freisetzungen==0])),
        ylim=c(0,0.25),col=color0,names.arg=F, yaxt = "n",add= T,
        space =c(0.2,rep(0.35,14)))


legend(x=2,y=0.25,legend=c("Control","Treatment"),fill=c(color0,color1),bty="n",
       horiz=T,xpd=T, x.intersp = 0.2, text.width = strwidth("Control"))


#barplot in D=1

vec<-prop.table(table(wk$city_name[wk$freisetzungen==1]))

barplot(vec,ylim=c(0,0.25),col=color1,yaxt = "n",cex.names = 0.8,add=T, 
        space =c(0.35,rep(0.35,14)))


axis(2,at=seq(0,0.25,0.05),labels=c("0","0.05","0.10","0.15","0.20","0.25"))



#after matching without replacement
#barplot in D=0

vec<-
  prop.table(table(matched_r_wk$city_name[matched_r_wk$freisetzungen==0]))


barplot(vec,ylim=c(0,0.25),col=F,names.arg=F,yaxt = "n",
        ylab= "Relative frequency", space =c(0.2,rep(0.35,14)))

grid(nx = NULL, ny = NULL, lty = 2,col = "gray",lwd = 1)

barplot(vec, ylim=c(0,0.25),col=color0,names.arg=F, yaxt = "n",add= T,
        space =c(0.2,rep(0.35,14)))

vec<-
  prop.table(table(matched_r_wk$city_name[matched_r_wk$freisetzungen==1]))


barplot(vec,ylim=c(0,0.25),col=color1,yaxt = "n",cex.names = 0.8,add=T,
        space =c(0.35,rep(0.35,14)))


axis(2,at=seq(0,0.25,0.05),labels=c("0","0.05","0.10","0.15","0.20","0.25"))
#save with width=1190,height=830

rm(list=ls()[!(ls() %in% c("logit","freisetzungen","install_if_missing",
      "s.model","model","s.model_ad","model_ad","s.model_ad_r","model_ad_r",
      "matched_r_wk","matched_wk","wk", "sum.st","sum.st_ad","sum.st_ad_r","df")
)])
