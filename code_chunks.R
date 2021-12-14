#----------------------------------------------------------------------------------------------------------------------------
add_bear_shade<-function(st_date,ed_date,shade_color="darkgray",threshold=0.1,mode="runmax",days=252)
{
  #st_date<-"2000-01-01"
  #ed_date<-Sys.Date()
  library(fredr)
  library(ecm)
  library(ggplot2)
  library(dplyr)
  library(tidyverse)
  library(zoo)
  library(data.table)
  
  mb <- read_csv("https://cloud.amadeusquantamental.lu/owncloud/index.php/s/gka11HDskZE2rPA/download")
  
  mb<-mb[mb$date>st_date & mb$date<ed_date,]
  
  if(mode=="runmax")
  {
    mb$max_drawdown<-mb$PX_LAST/runmax(mb$PX_LAST,days,align="right")-1    
  }else{
    mb$max_drawdown<-mb$PX_LAST/cummax(mb$PX_LAST)-1    
  }
  #plot(mb$PX_LAST/runmax(mb$PX_LAST,250)-1)
  #plot(mb$PX_LAST/cummax(mb$PX_LAST)-1)
  
  mb$dddummy<-ifelse(mb$max_drawdown<0,1,0)
  mb$ddcount<-ifelse(mb$dddummy==1 & lagpad(mb$dddummy,k=1)==0,1,0)
  mb$ddcount[1]<-0
  mb$dds<-cumsum(mb$ddcount)
  mb$dds<-ifelse(mb$dddummy==0,0,mb$dds)
  
  mb<-mb %>% group_by(dds) %>% mutate(through=min(max_drawdown))
  
  mb$dd10<-ifelse(mb$through<(-threshold),1,0)
  mb$regime<-ifelse(mb$dddummy==1 & mb$max_drawdown==mb$through,"recovery",NA)
  mb$regime<-ifelse(mb$dddummy==1 & lagpad(mb$dddummy,k=1)==0,"bear",mb$regime)
  mb<-mb%>%group_by(dds)%>%mutate(regime=na.locf(regime,na.rm=F))
  mb$dd10_bear<-ifelse(mb$through<(-threshold) & mb$regime=="bear",1,0)
  
  mb$bear_start<-ifelse(mb$dd10_bear==1 & lagpad(mb$dd10_bear,k=1)==0,1,NA)
  mb$bear_end<-ifelse(mb$dd10_bear==0 & lagpad(mb$dd10_bear,k=1)==1,1,NA)
  mb<-as.data.table(mb)
  
  bear_starts<-(mb[bear_start==1,]$date)
  bear_ends<-(mb[bear_end==1,]$date)
  if(length(bear_starts)>length(bear_starts))
  {
    bear_ends<-c(bear_ends,Sys.Date())
  }
  if(length(bear_starts)>length(bear_starts))
  {
    bear_starts<-tail(bear_starts,length(bear_starts)-1)
  }
  recs<-as.data.frame(cbind(bear_starts,bear_ends),stringsAsFactors=F)
  names(recs)<-c("recession.start","recession.end")
  recs$recession.start<-as.Date(recs$recession.start)
  recs$recession.end<-as.Date(recs$recession.end)
  
  if(nrow(recs)>0)
  {
    rec_shade<-geom_rect(data=recs, inherit.aes=F, aes(xmin=recession.start, xmax=recession.end, ymin=-Inf, ymax=+Inf), fill=shade_color, alpha=0.5)  
    rec_shade<-list("rec_shade"=rec_shade,"mb"=mb)
    return(rec_shade)
  }
}

#Example Output
mb <- read_csv("https://cloud.amadeusquantamental.lu/owncloud/index.php/s/gka11HDskZE2rPA/download")
cols <- c("Drawdowns (vs 252D High)" = "grey","% Members >200D SMA" = "#04103b")

ggplot(data=mb,aes(x=as.Date(date), y=PX_LAST/runmax(PX_LAST,252,align="right")))+
  add_bear_shade(as.Date(min(mb$date)),as.Date(Sys.Date()),threshold=0.07,mode="runmax",days=252)$rec_shade+
  #geom_line(size=1,aes(y=PX_LAST/cummax(PX_LAST),color="Drawdowns"))+
  geom_line(size=1,aes(y=PX_LAST/runmax(PX_LAST,252,align="right"),color="Drawdowns (vs 252D High)"))+
  geom_line(size=1,aes(y=PCT_MEMB_ABOVE_MOV_AVG_200D/100,color="% Members >200D SMA"))+
  scale_colour_manual(values = cols)+
  #size 22 for overleaf
  labs(color='')+
  labs(title="Market Breadth & Max Drawdowns",subtitle="S&P 500 Price Index (Shading for Drawdowns > 7%)",x ="")+
  labs(caption = paste0('Source: Bloomberg, Amadeus ', Sys.Date()))+
  guides(colour = guide_legend(nrow = 1))+
  scale_x_date(labels = date_format("%m-%Y"))+
  theme(legend.position = "bottom",legend.margin=margin(-20,-20,-20,-20),legend.box.margin=margin(0,0,30,0))+
  ylab("")+
  theme(plot.margin=margin(l=5,r=20,b=5,t=5))+
  scale_y_continuous(labels = percent)
