#Theme set############
library(ggplot2)
theme_set(theme_classic()+
            theme(axis.line = element_line(colour = "white")))

#Library#################
library(dvmisc)
library(plotrix)
library(incidence)
library(ggpubr)
library(gridExtra)
library(deSolve)
library(tidyverse)
library(gridExtra)
library(spread)
library(fhidata)
library(viridis)
library(gganimate)
library(tidyverse)
library(plotrix)
library(ggridges)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(dygraphs)
library(xts)
library(tm)
library(deSolve)
library(wesanderson)


#World situation 24/03/2020#####
confermati=read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

ggplot(data = confermati,aes(x = Long, y = Lat,fill=X3.24.20)) +
  borders("legacy_world",  colour = "grey60",fill="antiquewhite",alpha=0.5) +
  scale_fill_distiller(type = 'seq', palette = "Spectral",direction=1)+
  theme_map()+
  geom_point(colour = "black", alpha = 0.40,shape=21,aes(size=X3.24.20)) +
  labs(title = "24/03/20 Infected",fill="Infetti",size="Ampiezza")+
  theme(panel.background = element_rect(fill = "aliceblue"))

complete=confermati %>%
  gather(X2.22.20:X4.29.20,key="date",value="n") %>%
  select(Country.Region,date,n,Long,Lat)%>%
  arrange(Country.Region)
head(complete)


# complete$date=str_replace(complete$date, "X", "0")
# complete$date=as.Date(complete$date,format = c("%m.%d.%y"))


# #dynamic problem
# ggplot(data = complete,aes(x = Long, y = Lat)) +
#   borders("legacy_world",  colour = "grey60",fill="antiquewhite",alpha=0.5) +
#   scale_fill_distiller(type = 'seq', palette = "Spectral",direction=1)+ 
#   geom_point(colour = "black", alpha = 0.70,shape=21,aes(size=n,grou)) +
#   labs(title = "Date: {frame_time}", size = "Infetti") +
#   transition_time(date) +
#   coord_fixed(xlim = c(-10,160),ylim=c(-40,100))+
#   theme(panel.background = element_rect(fill = "aliceblue"))+
#   labs(title= "Date: {frame_time}",subtitle = "INFETTI")

#ITALY DYNAMIC SITUATION REGION###############
dat_csv<-read.csv("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv",header=T)
colnames(dat_csv)
dat_csv$data=as.Date(dat_csv$data)
italy_map <- map_data("italy")
print(unique(italy_map$region))

set.seed(1492)
choro_dat <- data_frame(region=unique(italy_map$    region),
                        value=sample(100, length(region)))

italy_proj <- "+proj=aea +lat_1=38.15040684902542
+lat_2=44.925490198742295 +lon_0=12.7880859375"


ggplot(data = dat_csv,aes(x = long, y = lat,fill=totale_casi,size=totale_casi))+
  geom_map(data=italy_map, map=italy_map,
           aes(long, lat, map_id=region),
           fill="antiquewhite",, size=0.4, color="grey")+
  scale_fill_distiller(type = 'seq', palette = "Spectral",direction=1)+ 
  geom_point(colour = "black", alpha = 0.90,shape=21) +
  labs(title = "Date: {frame_time}", size = "Infetti",subtitle = "INFETTI ITALIA LIVELLO REGIONALE") +
  theme(panel.background = element_rect(fill = "aliceblue"))+
  transition_time(data)


#italy dynamic situation province
dat_csv=read.csv("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-province/dpc-covid19-ita-province.csv")
dat_csv=dat_csv %>%
  filter(lat!=0 & long !=0)
myDateTimeStr1 <- as.vector(dat_csv$data)
myDateTimeStr1= gsub("T"," ",myDateTimeStr1)
myPOSIXct1 <- as.POSIXct(myDateTimeStr1, format="%Y-%m-%d")
dat_csv$data=myPOSIXct1
myPoSIXct1 =removeWords(as.character(dat_csv$data),"CET")
dat_csv$data=as.Date(myPoSIXct1)

ggplot(data = dat_csv,aes(x = long, y = lat,fill=totale_casi,size=totale_casi))+
  geom_map(data=italy_map, map=italy_map,
           aes(long, lat, map_id=region),
           fill="antiquewhite",, size=0.4, color="grey")+
  scale_fill_distiller(type = 'seq', palette = "Spectral",direction=1)+ 
  geom_point(colour = "black", alpha = 0.90,shape=21) +
  labs(title = "Date: {frame_time}", size = "Infetti",subtitle = "INFETTI ITALIA LIVELLO PROVINCIALE") +
  theme(panel.background = element_rect(fill = "aliceblue"))+
  transition_time(data)



#EPIDEMIC CURVE italy vs china ##############
cases=read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
death=read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
recovered=read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")

region_confirmed=cases %>% 
  gather(X1.22.20:X6.10.20,key="date",value="n") %>%
  filter(Province.State=="Hubei" | Country.Region=="Italy" ) %>%
  select(Province.State,date,n)

region_death=death %>% 
  gather(X1.22.20:X6.10.20,key="date",value="n_morti") %>%
  filter(Province.State=="Hubei" | Country.Region=="Italy" ) %>%
  select(Province.State,date,n_morti)

region_recovered=recovered %>% 
  gather(X1.22.20:X6.10.20,key="date",value="n_guariti") %>%
  filter(Province.State=="Hubei" | Country.Region=="Italy" ) %>%
  select(Province.State,date,n_guariti)

region=inner_join(region_confirmed,region_death)
region=inner_join(region,region_recovered) %>%
  mutate(casi=n-n_morti-n_guariti) %>%
  select(Province.State,date,casi)


region$date=str_replace(region$date, "X", "0")
region$date=as.Date(region$date,format = c("%m.%d.%y"))
val=which(region$Province.State==c(""))
region$Province.State=as.character(region$Province.State)
region$Province.State[val]=c("Italia")

picchi=region%>%
  group_by(Province.State)%>%
  summarize(massimo=max(casi),date=date[which((casi)==max(casi))])


ggplot()+ 
  geom_density_line(region,mapping=aes(x=date,y=casi,group=Province.State,fill=Province.State),stat = "identity", size=.7, alpha=0.75,colour="Blue4") +
  scale_fill_manual(values=c("Green4","Yellow3"))+
  geom_point(picchi,mapping=aes(x=date,y=massimo,group=Province.State),col="Red",size=3)+
  labs(title="La prevalenza",y="Attualmente positivi",x="Date",fill="Stati")+
  scale_x_date(breaks="7 days")+
  theme(axis.text.x = element_text(angle=45,hjust=1))+
  guides(colour = guide_legend(override.aes = list(size=1)))+
  theme(legend.justification=c(1,0), legend.position=c(0.147,0.65),legend.text=element_text(size=10))



#CINA-->Download and creation Hubei incidence##############

data=read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
colnames(data)

infected_a=data %>% 
  filter(Province.State=="Hubei") %>%
  select(-c("Lat","Long","Country.Region","Province.State"))
infected_a=as.numeric(infected_a[1,])

#Incidence
incidence_1=vector()
for(i in 2:length(infected_a)){
  incidence_1[i-1]=infected_a[i]-infected_a[i-1]
}
incidence_cina=c(incidence_1)
incidence_cina=incidence_cina[1:50]
time=length(incidence_cina)

#Incidence in output

date=seq.Date(from = as.Date("2020-01-23"),length.out = time , by = "day")
length(date)
inc=data.frame(date=date,incidence=incidence_cina)
ince<-xts(inc,order.by = date)
ince=data.frame(date=ince$date,incidence=incidence_cina)
ince$incidence=as.numeric(ince$incidence)
ince$date=as.Date(ince$date)

g1=ggplot(ince,aes(x=date,y=incidence))+
  geom_bar(stat="identity",fill="springgreen3",col="white")+
  geom_line()+
  theme(axis.text.x = element_text(angle=45,hjust=1))+
  theme(legend.position = "None")+
  labs(title = "Incidenza Cina-Hubei",y="Incidenza")+
  scale_x_date(breaks="7 days")+
  theme(axis.text.x = element_text(angle=45,hjust=1))

#costruzione finestra mobile
input = 1:length(ince$incidence)
multiple_of_3 = (input %% 3) == 0
valori=input[multiple_of_3]

somma=function (x,len){
  aggregate(x,by=list(rep(1:(length(x)/len),each=len) ),FUN=sum)
}


mobile=data.frame(incidence=somma(incidence_cina[-c(49,50)],3),date=ince$date[valori])

g2=ggplot(mobile,aes(x=date,y=incidence.x))+ 
  geom_bar(stat="identity",fill="springgreen3",col="white")+
  geom_line()+
  theme(axis.text.x = element_text(angle=45,hjust=1))+
  theme(legend.position = "None")+
  labs(title = "Incidenza Cina-Hubei",subtitle = "Finestra mobile 3 giorni",y="Incidenza")+
  scale_x_date(breaks="7 days")+
  theme(axis.text.x = element_text(angle=45,hjust=1))
g2

ggarrange(g1, g2,ncol=2, nrow=1)

#morti###########

dat_csv<-read.csv("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv",header=T)
myDateTimeStr1 <- as.vector(dat_csv$data)
myDateTimeStr1= gsub("T"," ",myDateTimeStr1)
myPOSIXct1 <- as.POSIXct(myDateTimeStr1, format="%Y-%m-%d")
dat_csv$data=myPOSIXct1
myPoSIXct1 =removeWords(as.character(dat_csv$data),"CET")
dat_csv$data=as.Date(myPoSIXct1)

dat_csv%>%
  filter(data=="2020-05-29")%>%
  summarize(morti=sum(deceduti),guariti=sum(dimessi_guariti))%>%
  mutate(tasso=(morti/guariti))

dat_csv%>%
  summarize(positivi=sum(nuovi_positivi))

dat_csv%>%
  filter(data=="2020-03-10")%>%
  summarize(morti=sum(deceduti),guariti=sum(dimessi_guariti))%>%
  mutate(tasso=(morti/guariti))


dat_csv%>%
  filter(data=="2020-05-10")%>%
  summarize(morti=sum(deceduti),guariti=sum(dimessi_guariti))%>%
  mutate(tasso=(morti/guariti))

10000/(60.36*10^6)
#Same work for italy##########################
data=read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
colnames(data)
infected=data %>% 
  filter(Country.Region=="Italy") %>%
  select(-c("Lat","Long","Country.Region","Province.State"))
infected=as.numeric(infected[1,-c(1:29)])
infected


incidence_italia=vector()
for(i in 2:length(infected)){
  incidence_italia[i-1]=infected[i]-infected[i-1]}
length(incidence_italia)
incidence_italia

date=seq.Date(from = as.Date("2020-02-21"), length.out = length(incidence_italia), by = "day")
length(date)

inc=data.frame(date=date,incidence=incidence_italia)
ince<-xts(inc,order.by = date)

ince=data.frame(date=ince$date,incidence=incidence_italia)
ince$incidence=as.numeric(ince$incidence)
ince$date=as.Date(ince$date)

tamponi=vector()
for(i in 2:length(dat_csv$tamponi)){
  tamponi[i-1]=dat_csv$tamponi[i]-dat_csv$tamponi[i-1]}
tamponi

ince=cbind(ince[-c(1:4),],tamponi=tamponi)
g1=ggplot(ince,aes(x=date,y=incidence,fill=tamponi))+
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle=45,hjust=1))+
  labs(title = "Incidence Italia",fill="Tamponi")+
  scale_x_date(breaks="7 days")+
  theme(axis.text.x = element_text(angle=45,hjust=1))+
  scale_fill_viridis(discrete = F,option = "C")
g1

#costruzione mobile
input = 1:length(ince$incidence)
multiple_of_3 = (input %% 3) == 0
valori=input[multiple_of_3]

mobile=data.frame(incidence=somma(incidence_italia[-c(76,77,78,79,80)],3),date=ince$date[valori],tamponi=somma(tamponi[-c(76,77,78,79,80)],3))
g2=ggplot(mobile,aes(x=date,y=incidence.x,fill=tamponi.x))+
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle=45,hjust=1))+
  labs(title = "Incidence Italia",subtitle = "Finestra mobile 3 giorni",fill="incidence",y="incidence")+
  scale_x_date(breaks="7 days")+
  theme(axis.text.x = element_text(angle=45,hjust=1))+
  scale_fill_viridis(discrete = F,option = "C")
g2

ggarrange(g1, g2,ncol=2, nrow=1)


#R0 estimation for china##################
china=region%>%
  filter(Province.State=="Hubei")

library(R0)

?est.R0.EG
GT = generation.time ("lognormal",c(4.7,2.6))

est.R0.EG(epid = china$casi, GT, begin =1, end =15,reg.met = "poisson")


est.R0.ML(china$casi,GT,begin = 2,end=15)

#R0 estimation for italy################

GT = generation.time ("lognormal",c(4.7,2.6))

order(incidence_italia,decreasing = T)

#same time china's period 

italia=region%>%
  filter(Province.State=="Italia")

est.R0.EG(epid = infected, GT, begin =1, end =15,reg.met = "poisson")

est.R0.ML(infected,GT,begin =1,end=15)


#parameters choice########################

R0=4
Incubazione=3 ##5 medi - 2 medi dove sono contagioso
Periodo_infetto=10  ##10 medi
gamma=1/Periodo_infetto #recovered rate
sigma=1/Incubazione 
beta=gamma*R0 
beta


#SIR MODEL LOMBARDY 23/02/20##############

sir <- function(time, state, parameters) {
  
  with(as.list(c(state, parameters)), {
    
    dS <- -beta * S * I
    dI <-  beta * S * I - gamma * I
    dR <-                 gamma * I
    
    return(list(c(dS, dI, dR)))
  })
}

N_Lombardia=10.04*10^6

#OFFICIAL NUMBER IN 24/02

init       <- c(S =(N_Lombardia-20-1)/N_Lombardia , I =20/N_Lombardia , R =0/N_Lombardia )

parameters <- c(beta = beta, gamma = gamma)

times      <- seq(0, 200, by = 1)

out <- ode(y = init, times = times, func = sir, parms = parameters)

out <- as.data.frame(out)
out$I=out$I
out$R=out$R

out$time <- NULL

head(out, 10)

out
title <- bquote("SIR MODEL")
subtit <- bquote(list(beta==.(parameters[1]),~gamma==.(parameters[2])))

sir<-ggplot(out,aes(x=0:200))+
  ggtitle(bquote(atop(.(title),atop(bold(.(subtit))))))+
  geom_line(aes(y=S,colour="Suscettibili"))+
  geom_line(aes(y=I,colour="Infetti"))+
  geom_line(aes(y=R,colour="Guariti"))+
  ylab(label="Proporzione pop.")+
  xlab(label="Giorni")+
  labs(color="Legenda : ")+
  theme(legend.justification=c(1,0), legend.position=c(1,0.3))
sir

#CASO LOMBARDO variazione di R0###############
detach("package:R0", unload=TRUE)
library(tidyverse)

dat_csv<-read.csv("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv",header=T)
colnames(dat_csv)
dat_csv=dat_csv %>%
  filter(denominazione_regione=="Lombardia")

view(dat_csv)
days<-dim(dat_csv)[1]


colnames(dat_csv)
dat_csv$t<-1:days
days

myDateTimeStr1 <- dat_csv$data
myDateTimeStr1= gsub("T"," ",myDateTimeStr1)
myPOSIXct1 <- as.POSIXct(myDateTimeStr1, format="%Y-%m-%d %H:%M:%S")
days_dy<-as.Date(myPOSIXct1)

colnames(dat_csv)

#need to update manually
dat_csv_dy<-xts(dat_csv[,-c(1:6,18:20)], order.by = days_dy, frequency = 7)
length(dat_csv_dy$ricoverati_con_sintomi)
gruppi=c(rep(1,10),rep(2,10),rep(3,10),rep(4,10),rep(5,10),rep(6,10),rep(7,10))
dat_csv_dy=cbind(dat_csv_dy,log=log(as.numeric(dat_csv_dy$totale_positivi)),as.numeric(gruppi))


p1=ggplot(dat_csv_dy)+
  geom_point(aes(x=t,y=log),size=1)+
  scale_color_brewer(palette="Dark2")+
  geom_line(aes(x=t,y=log,group=gruppi,col=as.factor(gruppi)),stat="smooth",method="lm",se = F,alpha=1,size=1)+
  theme(legend.position = "none")+
  labs(y="Log_tot_positivi",x="Tempo",title = "Log-Lineare")

p2=ggplot(dat_csv_dy)+
  geom_point(aes(x=t,y=totale_positivi),size=1)+
  scale_color_brewer(palette="Dark2")+
  geom_smooth(aes(x=t,y=totale_positivi,group=gruppi,col=as.factor(gruppi)),method="glm",method.args = list(family = "poisson"),se = F,size=1)+
  theme(legend.position = "none")+
  labs(y="Tot_positivi",x="Tempo",title="Poisson")
  

grid.arrange(p1,p2,ncol=2)

duration<-15
beta_vec<-NULL
sd_vec<-NULL
mse_POIS=NULL
mse_LM=NULL
mse_LOGLM=NULL

tamponi=vector()
for(i in 2:length(dat_csv$tamponi)){
  tamponi[i]=dat_csv$tamponi[i]-dat_csv$tamponi[i-1]}
tamponi[1]=dat_csv$tamponi[1]
tamponi[3]=492

dat_csv$tamponi=tamponi


for (i in 3:(days-2)){
  fit <- glm((totale_positivi)~t,weights = 1/tamponi,family="poisson",data=dat_csv[(i-2):(i+2),])
  mse_POIS=c(mse_POIS,get_mse(fit))
  fit <- glm((totale_positivi)~t,weights = 1/tamponi,family="gaussian",data=dat_csv[(i-2):(i+2),])
  mse_LM=c(mse_LM,get_mse(fit))
  fit <- glm(log(totale_positivi)~t,weights = 1/tamponi,family="gaussian",data=dat_csv[(i-2):(i+2),])
  mse_LOGLM=c(mse_POIS,get_mse(fit))
  beta_vec<-c(beta_vec,coef(fit)[2])
  sd_vec<-c(sd_vec,coef(summary(fit))[2,2])
}
mean(mse_LM)
mean(mse_LOGLM) #migliore
mean(mse_POIS)

label<-as.Date(substr(dat_csv$data,1,10))[3:(days-2)]
mean  <- 1+(beta_vec*duration)
lower <- 1+((beta_vec-1.96*sd_vec)*duration)
upper <- 1+((beta_vec+1.96*sd_vec)*duration)
df <- data.frame(label, mean, lower, upper,tamponi=tamponi[-c(1,2,length(mean),length(mean)-1)])
fp <- ggplot(data=df, aes(x=label, y=mean, ymin=lower, ymax=upper,col=tamponi)) +
  geom_pointrange(size=0.2) +
  scale_color_gradientn(colours = rainbow(5))+
  geom_hline(yintercept=1, size=0.5,color="black",linetype="dashed") +
  xlab("Date") + ylab("Rt")+
  labs(title = "Lombardia",subtitle = "Intervallo di confidenza 95%",col="Tamponi" )+
  scale_x_date(breaks="7 days")+
  theme(axis.text.x = element_text(angle=45,hjust=1))+
  theme(legend.justification=c(1,0), legend.position=c(1,0.3))
fp


#MODELLO SEIR LOMBARDIA BASATO SU CASI 24/02/20#############

seir_model = function (current_timepoint, state_values, parameters)
{
  S = state_values [1]        # susceptibles
  E = state_values [2]        # exposed
  I = state_values [3]        # infectious
  R = state_values [4]        # recovered
  
  with ( 
    as.list (parameters),     
    {
      
      dS = (-beta * S * I)
      dE = (beta * S * I) - (delta * E)
      dI = (delta * E) - (gamma * I)
      dR = (gamma * I)
      
      
      results = c (dS, dE, dI, dR)
      list (results)
    }
  )
}


parameter_list = c (beta = beta, gamma = gamma, delta=sigma)

W = N_Lombardia-1        
Y = 0     
Z = 0     
X = 1    

N = W + X + Y + Z

initial_values = c (S = W/N, E = X/N, I = Y/N, R = Z/N)
timepoints = seq (0, 300, by=1)
output = lsoda (initial_values, timepoints, seir_model, parameter_list)

title <- bquote("SEIR MODEL")
subtit <- bquote(list(beta==.(parameter_list[1]),~gamma==.(parameter_list[2]),~delta==.(parameter_list[3])))

output=as.data.frame(output)


seir<-ggplot(output,aes(x=time))+
  ggtitle(bquote(atop(.(title),atop(bold(.(subtit))))))+
  geom_line(aes(y=S,colour="Suscettibili"))+
  geom_line(aes(y=I,colour="Infetti"))+
  geom_line(aes(y=R,colour="Guariti"))+
  geom_line(aes(y=E,colour="Esposti"))+
  ylab(label="Proporzione pop.")+
  xlab(label="Giorni")+
  labs(color="Legenda : ")+
theme(legend.justification=c(1,0), legend.position=c(1,0.15))

seir
# OD MATRIX SOME GENERAL ANALYSIS#############

matrice_od <- read.csv("C:/Users/matte/Desktop/Lavori tesi/File_Excel/matrice_od.csv")
od=matrice_od
colnames(matrice_od)

#maggiori spostamenti
colnames(od)
val=od%>%
  filter(ZONA_ORIG!=ZONA_DEST) %>%
  summarise_each(funs(sum), -c(ZONA_ORIG,PROV_ORIG,PROV_DEST,ZONA_DEST,FASCIA_ORARIA))%>%
  gather(LAV_COND:RIT_ALTRO,key="Tipo",value="N") 
for(i in 1:length(val$N)){
  val$N[i]=round(val$N[i],0)
}

val%>%
  filter(N>100000)%>%
  mutate(pct = round(prop.table(N),3))%>%
  ggplot(aes(x=N,y=Tipo,fill=N))+
  geom_bar(stat="Identity")+
  scale_fill_distiller(type = 'seq', palette = "Spectral",direction=1)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),legend.position ="NULL")+
  geom_text(aes(label=scales::percent(pct)), angle = 360, 
            position=position_dodge(width=0.2), vjust=0.2)+
  labs(y="Tipo di spostamento")

val$Tipo
gruppi=c(rep("LAVORO",8),rep("STUDIO",8),rep("OCCASIONALI",8),rep("AFFARI",8),rep("RIENTRO",8))
new_val=cbind(val,gruppi)

new_val%>%
  filter(gruppi!="RIENTRO")%>%
  group_by(gruppi)%>%
  summarize(N=sum(N))%>%
  mutate(pct = prop.table(N))%>%
  ggplot(aes(x=gruppi,y=N,fill=N))+
  geom_bar(stat="Identity")+
  scale_fill_distiller(palette =4)+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),legend.position ="NULL")+
  geom_text(aes(label=scales::percent(pct)),position=position_dodge(width=0.5), vjust=0.9)+
  labs(title ="Motivo di spostamento",x="Motivo")

val$Tipo
gruppi=gsub("[^;]*_(.*)", "\\1", val$Tipo)
new_val_2=cbind(val,gruppi)

new_val_2=new_val_2%>%
  group_by(gruppi)%>%
  summarize(N=sum(N))

new_val_2$gruppi=as.character(new_val_2$gruppi)
new_val_2$gruppi[3]=c("AUTO")
new_val_2$gruppi[4]=c("TRENO/METRO/TRAM")
new_val_2$gruppi[5]=c("BUS/PULLMAN")
new_val_2$gruppi[7]=c("PASSEGGERO AUTO")

fig <- plot_ly(new_val_2, labels = ~gruppi, values = ~N, type = 'pie')
fig <- fig %>% layout(xaxis = list(showgrid = T, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = T, zeroline = FALSE, showticklabels = FALSE))

fig


#CLEAN OD_MATRIX TO BUILDING PUBLIC TRANSPORT MATRIX################
matrix_od=od %>% 
  filter(ZONA_ORIG!=ZONA_DEST) %>%
  select(ZONA_ORIG,PROV_ORIG,ZONA_DEST,PROV_DEST,LAV_FERRO,LAV_GOMMA,OCC_FERRO,OCC_GOMMA,RIT_GOMMA,RIT_FERRO,STU_GOMMA,STU_FERRO) %>%
  mutate(all=LAV_FERRO+LAV_GOMMA+OCC_FERRO+OCC_GOMMA+RIT_GOMMA+RIT_FERRO+STU_GOMMA+STU_FERRO) %>%
  select(ZONA_ORIG,PROV_ORIG,ZONA_DEST,PROV_DEST,all) %>%
  filter(all>0)

#SICCOME NELLA MATRICE OD I DATI ERANO PER FASCE ORARIE, VADO A UNIRLI PER NON AVERE RIPETIZIONI
#new_file=aggregate(matrix_od$all, by=list(matrix_od$ZONA_ORIG,matrix_od$ZONA_DEST), FUN=sum)

new_file=matrix_od%>%
  group_by(ZONA_ORIG,ZONA_DEST,PROV_ORIG,PROV_DEST) %>%
  summarize(all=sum(all))%>%
  rename(from=ZONA_ORIG,to=ZONA_DEST,n=all)

#SISTEMAZIONE NOMI
#funzione per eliminare numeri nelle strighe
new_file$from=as.character(new_file$from)
new_file$to=as.character(new_file$to)

split=function(val){
  x = unlist(strsplit(val, split = '\\s+')) # split your string
  val=paste0(x[Reduce(`|`, lapply(c('[A-Za-z]', '67'), grepl, x))], collapse = ' ')
  return(val)
}

for(i in 1:length(new_file$from)){
  new_file$from[i]=split(new_file$from[i])
  new_file$to[i]=split(new_file$to[i])
}

new_file$from=as.factor(new_file$from)
new_file$to=as.factor(new_file$to)
costruzione_province=new_file

new_file$from=as.character(new_file$from)
new_file$to=as.character(new_file$to)
new_file=new_file[,c(1:2,5)]
new_file=new_file %>% 
  filter(from!=to)

new_file$from=as.factor(new_file$from)
new_file$to=as.factor(new_file$to)

new_file=aggregate(new_file$n, by=list(new_file$from,new_file$to), FUN=sum)
colnames(new_file)=c("from","to","n")



#write.csv(new_file,file = "OD.csv") 

# DATAFRAME with city and population#############

`11125_comuni_Popolazione.residente.totale.per.classe.di.eta.Totali.al.11...Comun` <-`11125_comuni_Popolazione.residente.totale.per.classe.di.eta.Totali.al.11...Comun` <- read.csv("C:/Users/matte/Desktop/Lavori tesi/File_excel/11125_comuni_Popolazione-residente-totale-per-classe-di-eta-Totali-al-11---Comun.csv", sep=";")
popolazione=as_tibble(`11125_comuni_Popolazione.residente.totale.per.classe.di.eta.Totali.al.11...Comun`)
colnames(popolazione)

popolazione=popolazione %>%
  filter(Anno==2019 & Livello.territoriale=="Comune") %>%
  select(Territorio,Totale) %>%
  transmute(Territorio=toupper(Territorio),Totale=Totale)

popolazione$Territorio

citta=levels(new_file$from)
pop=vector()
v=0
for(i in citta){
  v=v+1
  if(i %in% popolazione$Territorio){
    a=which(popolazione$Territorio==i)
    a=a[length(a)]
    pop[v]=popolazione$Totale[a]
    
  }
  else {
    pop[v]="Na"
  }
}
length(citta)
length(pop)

regioni=c("LOMBARDIA","LAZIO","CAMPANIA","SICILIA","VENETO","EMILIA-ROMAGNA","PIEMONTE","PUGLIA","TOSCANA","CALABRIA","SARDEGNA","LIGURIA","MARCHE","ABRUZZO","FRIULI VENEZIA GIULIA","TRENTINO ALTO ADIGE","UMBRIA","BASILICATA","MOLISE","VALLE D'AOSTA","ASTI","BIELLA","BOLOGNA","BOLZANO","CANTU","CASSANO D`ADDA","CASTIGLIONE D`ADDA","CUNEO","FERRARA","FORLI - CESENA","MODENA","MUGGIO`","NOVARA","PADOVA","PARMA","PEREGO","PIACENZA","RAVENNA","REVERE","RIMINI","TORINO","TRENTO","TREVISO","VENEZIA","VICENZA")
regioni_pop=c(10018806,5898124,5839084,5056641,5056641,4448841	,4392526	,4063888	,3742437,1965128,1653135,1565307	,1538055,1322247,1217872,1062860,888908,570365,310449,126883,76164,44616,388367,106951,40007,18911,4665,56124,132009,117946,184727,23579,104284,936887,194417,1757,102355,159057,2508,148908,886837,117417,885447,261905,112198)

for(i in 1:length(regioni)){
  val=which(citta==regioni[i])
  pop[val]=regioni_pop[i]
}

for(i in 1:length(citta)){
  val=which(pop=="Na")
  pop[val]=1500
}



# Started values#############


E=rep(0,length(pop))
R=rep(0,length(pop))
I=rep(0,length(pop))
Ia=rep(0,length(pop))

citta_completo=data.frame(location_code=citta,S=as.numeric(pop),E=E,I=I,Ia=Ia,R=R)

#MODEL###############


#sTARTED VALUES FROM MILANO

which(citta_completo$location_code=="MILANO")
citta_completo[830,]
citta_completo[830,]$I=1
citta_completo[830,]$R=0
citta_completo[830,]$E=0
citta_completo[830,]
citta_completo

set.seed(123)
which(new_file$from=="TOSCANA" & new_file$to=="AFRICA")
new_file=new_file[-423,]
d <- commuter(
  seiiar=citta_completo,
  commuters=new_file,
  r0=4.0,
  latent_period = 3,
  infectious_period = 12,
  asymptomatic_prob=2/3,
  asymptomatic_relative_infectiousness=0.10,
  days_simulation=200,
  N=1
)
a=which(d$location_code=="MILANO")
d[a]

#BUILD A PROVINCE/CITY DATAFRAME#######################

location_code=levels(citta_completo$location_code)
provincia=vector()
v=0

for(i in location_code){
  v=v+1
  a=which(costruzione_province$from==i)
  a=a[1]
  provincia[v]=as.character(costruzione_province$PROV_ORIG[a])
}



provincia=replace(provincia, provincia=="BG", "BERGAMO")
provincia=replace(provincia, provincia=="BS", "BRESCIA")
provincia=replace(provincia, provincia=="CO", "COMO")
provincia=replace(provincia, provincia=="CR", "CREMONA")
provincia=replace(provincia, provincia=="LC", "LECCO")
provincia=replace(provincia, provincia=="LO", "LODI")
provincia=replace(provincia, provincia=="MN", "MANTOVA")
provincia=replace(provincia, provincia=="MI", "MILANO")
provincia=replace(provincia, provincia=="MB", "MONZA")
provincia=replace(provincia, provincia=="PV", "PAVIA")
provincia=replace(provincia, provincia=="SO", "SONDRIO")
provincia=replace(provincia, provincia=="VA", "VARESE")

province=data.frame(location_code=location_code,provincia=provincia)

##MODEL,BUILD WITH PROVINCE########################

d <- merge(d,province, by.x="location_code",by.y="location_code")
county <- d[,.(
  S=sum(S),
  E=sum(E),
  I=sum(I),
  Ia=sum(Ia),
  R=sum(R),
  incidence=sum(incidence),
  pop=sum(pop)
),
keyby=.(provincia,location_code,week,day,is_6pm)]

#select only lombardy's province
lombardia=which(county$provincia=="BERGAMO" | county$provincia=="BRESCIA" | county$provincia=="COMO" | county$provincia=="CREMONA" | county$provincia=="LECCO" | county$provincia=="LODI" | county$provincia=="MANTOVA" | county$provincia=="MILANO" | county$provincia=="MONZA" | county$provincia=="CREMONA" | county$provincia=="PAVIA" | county$provincia=="SONDRIO" | county$provincia=="VARESE")

county_2=county[lombardia,]
colnames(county_2)

#write.csv2(county_2,file = "infetti lombardia.csv")

#model output##########

pop_province=county_2%>%
  group_by(provincia)%>%
  filter(day==1)%>%
  summarize(pop=sum(S))


somme=county_2%>%
  group_by(provincia,day)%>%
  summarize(S=sum(S),I=sum(I),R=sum(R),E=sum(E),Ia=sum(Ia))

popprov=left_join(somme,pop_province)

somme=popprov%>%
  mutate(Ss=S/pop,Ii=I/pop,Rr=R/pop,Er=E/pop,Iaa=Ia/sum(Ia))
  
ggplot(somme,aes(x=day))+
  geom_line(aes(y=Ss,colour="Suscettibili"))+
  geom_line(aes(y=Ii,colour="Infetti"))+
  geom_line(aes(y=Rr,colour="Guariti"))+
  geom_line(aes(y=Er,colour="Esposti"))+
  geom_line(aes(y=Iaa,colour="Asintomatici"))+
  facet_wrap(~provincia)+
  ylab(label="Proporzione pop.")+
  xlab(label="Giorni")+
  labs(color="Legenda : ")+
  theme(legend.justification=c(2,0), legend.position=c(1,0.3))


colnames(county_2)
p <- ggplot(county_2, aes(x=day, y=incidence))+
  geom_col()+
  facet_wrap(~provincia)+
  labs(x="Giorni",y="Curva incidence")+
  theme(legend.position = "None")+
  theme(axis.text.x = element_text(angle=45,hjust=1))
p
#Save plot
ggsave(p,device = "png",width = 10,height = 7,filename = "Andamento incidence.png")

