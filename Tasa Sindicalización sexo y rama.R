## Generacion de cuadro tasa de sindicalizacion por sexo y rama 2016-2018
## Andreas Lafferte 


#1. Paquetes ---- 
pacman::p_load(dplyr, 
               car, 
               summarytools, 
               ggplot2,
               magrittr,
               tidyverse, 
               lubridate,
               ggpubr, 
               sjmisc,
               sjlabelled, 
               stargazer)
library(openxlsx)
library(readxl)
library(tidyverse)
library(lubridate)
library(ggrepel)
library(tibble)
library(writexl)

#### Cuadro 4. y 5. Cantidad de afiliados ####

## 1990 a 2001
cuadro4_1<-cbind(
  read.xlsx("1. OOSS.xlsx",sheet = 10,rows=c(1:12),cols=c(1:7),na.strings = "**"),
  read.xlsx("1. OOSS.xlsx",sheet = 11,rows=c(2:13),cols=c(2:7),na.strings = "**")
)

## 2002 a 2010

d<-read.xlsx("1. OOSS.xlsx",sheet = 12,rows=c(4:14),cols=c(1:10),na.strings = "**",colNames = FALSE)
e<-read.xlsx("1. OOSS.xlsx",sheet = 13,rows=c(3:13),cols=c(2:10),na.strings = "**",colNames = FALSE)
f<-read.xlsx("1. OOSS.xlsx",sheet = 14,rows=c(3:13),cols=c(2:10),na.strings = "**",colNames = FALSE)
names(d)<-c("Rama.Actividad.Económica","h2","m2","t2","h3","m3","t3","h4","m4","t4")
names(e)<-c("h5","m5","t5","h6","m6","t6","h7","m7","t7")
names(f)<-c("h8","m8","t8","h9","m9","t9","h10","m10","t10")
cuadro4_2_sexo<-cbind(d,e,f)

cuadro4_2<-cuadro4_2_sexo %>% select(1,4,7,10,13,16,19,22,25,28)

## 2011 a 2017
cuadro4_3_sexo<-cbind(
  read.xlsx("1. OOSS.xlsx",sheet = 15,rows=c(4:22),cols=c(1:7),na.strings = "**",colNames = FALSE),
  read.xlsx("1. OOSS.xlsx",sheet = 16,rows=c(4:22),cols=c(2:7),na.strings = "**",colNames = FALSE),
  read.xlsx("1. OOSS.xlsx",sheet = 17,rows=c(4:22),cols=c(2:7),na.strings = "**",colNames = FALSE),
  read.xlsx("1. OOSS.xlsx",sheet = 18,rows=c(4:22),cols=c(2:4),na.strings = "**",colNames = FALSE)
)

names(cuadro4_3_sexo)<-c("Rama.Actividad.Económica","h11","m11","t11","h12","m12","t12","h13","m13","t13","h14","m14","t14",
                         "h15","m15","t15","h16","m16","t16","h17","m17","t17")

cuadro4_3<-cuadro4_3_sexo %>% select(1,4,7,10,13,16,19,22)


## 2018
cuadro4_4_sexo<-read.xlsx("1. OOSS.xlsx",sheet = 19,rows=c(3:26),cols=c(1:4),na.strings = "**")
names(cuadro4_4_sexo)<-c("Rama.Actividad.Económica","h18","m18","t18")
cuadro4_4<-cuadro4_4_sexo %>% select(1,4)

## A. Primero sin sexo

cuadro4_1
cuadro4_2
colnames(cuadro4_2)<-c("Rama.Actividad.Económica",2002,2003,2004,2005,2006,2007,2008,2009,2010)

colnames(cuadro4_3)<-c("Rama.Actividad.Económica",2011,2012,2013,2014,2015,2016,2017)

cuadro4_3<-cuadro4_3 %>% add_row(Rama.Actividad.Económica="A Agriculture",
                                 `2011`=sum(.$`2011`[1]+.$`2011`[2]),
                                 `2012`=sum(.$`2012`[1]+.$`2012`[2]),
                                 `2013`=sum(.$`2013`[1]+.$`2013`[2]),
                                 `2014`=sum(.$`2014`[1]+.$`2014`[2]),
                                 `2015`=sum(.$`2015`[1]+.$`2015`[2]),
                                 `2016`=sum(.$`2016`[1]+.$`2016`[2]),
                                 `2017`=sum(.$`2017`[1]+.$`2017`[2])) %>% 
  
  add_row(Rama.Actividad.Económica="B Mining",
          `2011`=sum(.$`2011`[3]),
          `2012`=sum(.$`2012`[3]),
          `2013`=sum(.$`2013`[3]),
          `2014`=sum(.$`2014`[3]),
          `2015`=sum(.$`2015`[3]),
          `2016`=sum(.$`2016`[3]),
          `2017`=sum(.$`2017`[3])) %>%
  
  add_row(Rama.Actividad.Económica="C Manufacturing industry",
          `2011`=sum(.$`2011`[4]),
          `2012`=sum(.$`2012`[4]),
          `2013`=sum(.$`2013`[4]),
          `2014`=sum(.$`2014`[4]),
          `2015`=sum(.$`2015`[4]),
          `2016`=sum(.$`2016`[4]),
          `2017`=sum(.$`2017`[4])) %>%
  
  add_row(Rama.Actividad.Económica="D-E Electricity, Water and Sanitary Services",
          `2011`=sum(.$`2011`[5]),
          `2012`=sum(.$`2012`[5]),
          `2013`=sum(.$`2013`[5]),
          `2014`=sum(.$`2014`[5]),
          `2015`=sum(.$`2015`[5]),
          `2016`=sum(.$`2016`[5]),
          `2017`=sum(.$`2017`[5])) %>%
  
  add_row(Rama.Actividad.Económica="F Construction",
          `2011`=sum(.$`2011`[6]),
          `2012`=sum(.$`2012`[6]),
          `2013`=sum(.$`2013`[6]),
          `2014`=sum(.$`2014`[6]),
          `2015`=sum(.$`2015`[6]),
          `2016`=sum(.$`2016`[6]),
          `2017`=sum(.$`2017`[6])) %>%
  
  add_row(Rama.Actividad.Económica="G-I Commerce",
          `2011`=sum(.$`2011`[7]+.$`2011`[8]),
          `2012`=sum(.$`2012`[7]+.$`2012`[8]),
          `2013`=sum(.$`2013`[7]+.$`2013`[8]),
          `2014`=sum(.$`2014`[7]+.$`2014`[8]),
          `2015`=sum(.$`2015`[7]+.$`2015`[8]),
          `2016`=sum(.$`2016`[7]+.$`2016`[8]),
          `2017`=sum(.$`2017`[7]+.$`2017`[8])) %>%
  
  add_row(Rama.Actividad.Económica="H-J Transportation and Communication",
          `2011`=sum(.$`2011`[9]),
          `2012`=sum(.$`2012`[9]),
          `2013`=sum(.$`2013`[9]),
          `2014`=sum(.$`2014`[9]),
          `2015`=sum(.$`2015`[9]),
          `2016`=sum(.$`2016`[9]),
          `2017`=sum(.$`2017`[9])) %>%
  
  add_row(Rama.Actividad.Económica="L-K Banks and Financial Services",
          `2011`=sum(.$`2011`[10]+.$`2011`[11]),
          `2012`=sum(.$`2012`[10]+.$`2012`[11]),
          `2013`=sum(.$`2013`[10]+.$`2013`[11]),
          `2014`=sum(.$`2014`[10]+.$`2014`[11]),
          `2015`=sum(.$`2015`[10]+.$`2015`[11]),
          `2016`=sum(.$`2016`[10]+.$`2016`[11]),
          `2017`=sum(.$`2017`[10]+.$`2017`[11])) %>%
  
  add_row(Rama.Actividad.Económica="O Central, Regional and Municipal Government",
          `2011`=sum(.$`2011`[12]),
          `2012`=sum(.$`2012`[12]),
          `2013`=sum(.$`2013`[12]),
          `2014`=sum(.$`2014`[12]),
          `2015`=sum(.$`2015`[12]),
          `2016`=sum(.$`2016`[12]),
          `2017`=sum(.$`2017`[12])) %>%
  
  add_row(Rama.Actividad.Económica="P Education (private, public and municipalized)",
          `2011`=sum(.$`2011`[13]),
          `2012`=sum(.$`2012`[13]),
          `2013`=sum(.$`2013`[13]),
          `2014`=sum(.$`2014`[13]),
          `2015`=sum(.$`2015`[13]),
          `2016`=sum(.$`2016`[13]),
          `2017`=sum(.$`2017`[13])) %>%
  
  add_row(Rama.Actividad.Económica="Q Health (private, public and municipalized)",
          `2011`=sum(.$`2011`[14]),
          `2012`=sum(.$`2012`[14]),
          `2013`=sum(.$`2013`[14]),
          `2014`=sum(.$`2014`[14]),
          `2015`=sum(.$`2015`[14]),
          `2016`=sum(.$`2016`[14]),
          `2017`=sum(.$`2017`[14])) %>% 
  
  add_row(Rama.Actividad.Económica="Q Other Community, Social and Personal Services",
          `2011`=sum(.$`2011`[15]),
          `2012`=sum(.$`2012`[15]),
          `2013`=sum(.$`2013`[15]),
          `2014`=sum(.$`2014`[15]),
          `2015`=sum(.$`2015`[15]),
          `2016`=sum(.$`2016`[15]),
          `2017`=sum(.$`2017`[15])) %>% 
  
  add_row(Rama.Actividad.Económica="Unknown or Other Activities",
          `2011`=sum(.$`2011`[16]+.$`2011`[17]+.$`2011`[18]),
          `2012`=sum(.$`2012`[16]+.$`2012`[17]+.$`2012`[18]),
          `2013`=sum(.$`2013`[16]+.$`2013`[17]+.$`2013`[18]),
          `2014`=sum(.$`2014`[16]+.$`2014`[17]+.$`2014`[18]),
          `2015`=sum(.$`2015`[16]+.$`2015`[17]+.$`2015`[18]),
          `2016`=sum(.$`2016`[16]+.$`2016`[17]+.$`2016`[18]),
          `2017`=sum(.$`2017`[16]+.$`2017`[17]+.$`2017`[18])) 


colnames(cuadro4_4)<-c("Rama.Actividad.Económica",2018)

cuadro4_4<-cuadro4_4 %>% add_row(Rama.Actividad.Económica="A Agriculture",
                                 `2018`=sum(.$`2018`[1])) %>% 
  
  add_row(Rama.Actividad.Económica="B Mining",
          `2018`=sum(.$`2018`[2])) %>%
  
  add_row(Rama.Actividad.Económica="C Manufacturing industry",
          `2018`=sum(.$`2018`[3])) %>%
  
  add_row(Rama.Actividad.Económica="D-E Electricity, Water and Sanitary Services",
          `2018`=sum(.$`2018`[4]+.$`2018`[5])) %>%
  
  add_row(Rama.Actividad.Económica="F Construction",
          `2018`=sum(.$`2018`[6])) %>%
  
  add_row(Rama.Actividad.Económica="G-I Commerce",
          `2018`=sum(.$`2018`[7]+.$`2018`[9])) %>%
  
  add_row(Rama.Actividad.Económica="H-J Transportation and Communication",
          `2018`=sum(.$`2018`[8]+.$`2018`[10])) %>%
  
  add_row(Rama.Actividad.Económica="L-K Banks and Financial Services",
          `2018`=sum(.$`2018`[11]+.$`2018`[12])) %>%
  
  add_row(Rama.Actividad.Económica="Q Other Community, Social and Personal Services",
          `2018`=sum(.$`2018`[13]+.$`2018`[14]+.$`2018`[18]+.$`2018`[19])) %>%
  
  add_row(Rama.Actividad.Económica="O Central, Regional and Municipal Government",
          `2018`=sum(.$`2018`[15]))%>%
  
  add_row(Rama.Actividad.Económica="P Education (private, public and municipalized)",
          `2018`=sum(.$`2018`[16]))%>%
  
  add_row(Rama.Actividad.Económica="Q Health (private, public and municipalized)",
          `2018`=sum(.$`2018`[17]))%>%
  
  add_row(Rama.Actividad.Económica="Unknown or Other Activities",
          `2018`=sum(.$`2018`[20]+.$`2018`[21]+.$`2018`[22]))

#cuadro4<-merge(cuadro4_1,cuadro4_2,by="Rama.Actividad.EconÃ³mica")

cuadro4_3 <- cuadro4_3[-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18),]
cuadro4_4 <- cuadro4_4[-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22),]

cuadro4<-merge(cuadro4_3,cuadro4_4,by="Rama.Actividad.Económica",all.x = TRUE)

#cuadro4<-merge(cuadro4,cuadro4_4,by="Rama.Actividad.EconÃ³mica",all.x = TRUE)

cuadro4<-unique( cuadro4 )
#cuadro4<-cuadro4[-3,]

## B. Con sexo (cuadro 5) (solo desde 2002)

# hombres
cuadro4_5<-cuadro4_2_sexo %>% select(1,4-2,7-2,10-2,13-2,16-2,19-2,22-2,25-2,28-2)
cuadro4_6<-cuadro4_3_sexo %>% select(1,4-2,7-2,10-2,13-2,16-2,19-2,22-2)
cuadro4_7<-cuadro4_4_sexo %>% select(1,4-2)

colnames(cuadro4_5)<-c("Rama.Actividad.Económica",2002,2003,2004,2005,2006,2007,2008,2009,2010)

colnames(cuadro4_6)<-c("Rama.Actividad.Económica",2011,2012,2013,2014,2015,2016,2017)

cuadro4_6<-cuadro4_6 %>% add_row(Rama.Actividad.Económica="A Agriculture",
                                 `2011`=sum(.$`2011`[1]+.$`2011`[2]),
                                 `2012`=sum(.$`2012`[1]+.$`2012`[2]),
                                 `2013`=sum(.$`2013`[1]+.$`2013`[2]),
                                 `2014`=sum(.$`2014`[1]+.$`2014`[2]),
                                 `2015`=sum(.$`2015`[1]+.$`2015`[2]),
                                 `2016`=sum(.$`2016`[1]+.$`2016`[2]),
                                 `2017`=sum(.$`2017`[1]+.$`2017`[2])) %>% 
  
  add_row(Rama.Actividad.Económica="B Mining",
          `2011`=sum(.$`2011`[3]),
          `2012`=sum(.$`2012`[3]),
          `2013`=sum(.$`2013`[3]),
          `2014`=sum(.$`2014`[3]),
          `2015`=sum(.$`2015`[3]),
          `2016`=sum(.$`2016`[3]),
          `2017`=sum(.$`2017`[3])) %>%
  
  add_row(Rama.Actividad.Económica="C Manufacturing industry",
          `2011`=sum(.$`2011`[4]),
          `2012`=sum(.$`2012`[4]),
          `2013`=sum(.$`2013`[4]),
          `2014`=sum(.$`2014`[4]),
          `2015`=sum(.$`2015`[4]),
          `2016`=sum(.$`2016`[4]),
          `2017`=sum(.$`2017`[4])) %>%
  
  add_row(Rama.Actividad.Económica="D-E Electricity, Water and Sanitary Services",
          `2011`=sum(.$`2011`[5]),
          `2012`=sum(.$`2012`[5]),
          `2013`=sum(.$`2013`[5]),
          `2014`=sum(.$`2014`[5]),
          `2015`=sum(.$`2015`[5]),
          `2016`=sum(.$`2016`[5]),
          `2017`=sum(.$`2017`[5])) %>%
  
  add_row(Rama.Actividad.Económica="F Construction",
          `2011`=sum(.$`2011`[6]),
          `2012`=sum(.$`2012`[6]),
          `2013`=sum(.$`2013`[6]),
          `2014`=sum(.$`2014`[6]),
          `2015`=sum(.$`2015`[6]),
          `2016`=sum(.$`2016`[6]),
          `2017`=sum(.$`2017`[6])) %>%
  
  add_row(Rama.Actividad.Económica="G-I Commerce",
          `2011`=sum(.$`2011`[7]+.$`2011`[8]),
          `2012`=sum(.$`2012`[7]+.$`2012`[8]),
          `2013`=sum(.$`2013`[7]+.$`2013`[8]),
          `2014`=sum(.$`2014`[7]+.$`2014`[8]),
          `2015`=sum(.$`2015`[7]+.$`2015`[8]),
          `2016`=sum(.$`2016`[7]+.$`2016`[8]),
          `2017`=sum(.$`2017`[7]+.$`2017`[8])) %>%
  
  add_row(Rama.Actividad.Económica="H-J Transportation and Communication",
          `2011`=sum(.$`2011`[9]),
          `2012`=sum(.$`2012`[9]),
          `2013`=sum(.$`2013`[9]),
          `2014`=sum(.$`2014`[9]),
          `2015`=sum(.$`2015`[9]),
          `2016`=sum(.$`2016`[9]),
          `2017`=sum(.$`2017`[9])) %>%
  
  add_row(Rama.Actividad.Económica="L-K Banks and Financial Services",
          `2011`=sum(.$`2011`[10]+.$`2011`[11]),
          `2012`=sum(.$`2012`[10]+.$`2012`[11]),
          `2013`=sum(.$`2013`[10]+.$`2013`[11]),
          `2014`=sum(.$`2014`[10]+.$`2014`[11]),
          `2015`=sum(.$`2015`[10]+.$`2015`[11]),
          `2016`=sum(.$`2016`[10]+.$`2016`[11]),
          `2017`=sum(.$`2017`[10]+.$`2017`[11])) %>%
  
  add_row(Rama.Actividad.Económica="O Central, Regional and Municipal Government",
          `2011`=sum(.$`2011`[12]),
          `2012`=sum(.$`2012`[12]),
          `2013`=sum(.$`2013`[12]),
          `2014`=sum(.$`2014`[12]),
          `2015`=sum(.$`2015`[12]),
          `2016`=sum(.$`2016`[12]),
          `2017`=sum(.$`2017`[12])) %>%
  
  add_row(Rama.Actividad.Económica="P Education (private, public and municipalized)",
          `2011`=sum(.$`2011`[13]),
          `2012`=sum(.$`2012`[13]),
          `2013`=sum(.$`2013`[13]),
          `2014`=sum(.$`2014`[13]),
          `2015`=sum(.$`2015`[13]),
          `2016`=sum(.$`2016`[13]),
          `2017`=sum(.$`2017`[13])) %>%
  
  add_row(Rama.Actividad.Económica="Q Health (private, public and municipalized)",
          `2011`=sum(.$`2011`[14]),
          `2012`=sum(.$`2012`[14]),
          `2013`=sum(.$`2013`[14]),
          `2014`=sum(.$`2014`[14]),
          `2015`=sum(.$`2015`[14]),
          `2016`=sum(.$`2016`[14]),
          `2017`=sum(.$`2017`[14])) %>% 
  
  add_row(Rama.Actividad.Económica="Q Other Community, Social and Personal Services",
          `2011`=sum(.$`2011`[15]),
          `2012`=sum(.$`2012`[15]),
          `2013`=sum(.$`2013`[15]),
          `2014`=sum(.$`2014`[15]),
          `2015`=sum(.$`2015`[15]),
          `2016`=sum(.$`2016`[15]),
          `2017`=sum(.$`2017`[15])) %>% 
  
  add_row(Rama.Actividad.Económica="Unknown or Other Activities",
          `2011`=sum(.$`2011`[16]+.$`2011`[17]+.$`2011`[18]),
          `2012`=sum(.$`2012`[16]+.$`2012`[17]+.$`2012`[18]),
          `2013`=sum(.$`2013`[16]+.$`2013`[17]+.$`2013`[18]),
          `2014`=sum(.$`2014`[16]+.$`2014`[17]+.$`2014`[18]),
          `2015`=sum(.$`2015`[16]+.$`2015`[17]+.$`2015`[18]),
          `2016`=sum(.$`2016`[16]+.$`2016`[17]+.$`2016`[18]),
          `2017`=sum(.$`2017`[16]+.$`2017`[17]+.$`2017`[18])) 

colnames(cuadro4_7)<-c("Rama.Actividad.Económica",2018)

cuadro4_7<-cuadro4_7 %>% add_row(Rama.Actividad.Económica="A Agriculture",
                                 `2018`=sum(.$`2018`[1])) %>% 
  
  add_row(Rama.Actividad.Económica="B Mining",
          `2018`=sum(.$`2018`[2])) %>%
  
  add_row(Rama.Actividad.Económica="C Manufacturing industry",
          `2018`=sum(.$`2018`[3])) %>%
  
  add_row(Rama.Actividad.Económica="D-E Electricity, Water and Sanitary Services",
          `2018`=sum(.$`2018`[4]+.$`2018`[5])) %>%
  
  add_row(Rama.Actividad.Económica="F Construction",
          `2018`=sum(.$`2018`[6])) %>%
  
  add_row(Rama.Actividad.Económica="G-I Commerce",
          `2018`=sum(.$`2018`[7]+.$`2018`[9])) %>%
  
  add_row(Rama.Actividad.Económica="H-J Transportation and Communication",
          `2018`=sum(.$`2018`[8]+.$`2018`[10])) %>%
  
  add_row(Rama.Actividad.Económica="L-K Banks and Financial Services",
          `2018`=sum(.$`2018`[11]+.$`2018`[12])) %>%
  
  add_row(Rama.Actividad.Económica="Q Other Community, Social and Personal Services",
          `2018`=sum(.$`2018`[13]+.$`2018`[14]+.$`2018`[18]+.$`2018`[19])) %>%
  
  add_row(Rama.Actividad.Económica="O Central, Regional and Municipal Government",
          `2018`=sum(.$`2018`[15]))%>%
  
  add_row(Rama.Actividad.Económica="P Education (private, public and municipalized)",
          `2018`=sum(.$`2018`[16]))%>%
  
  add_row(Rama.Actividad.Económica="Q Health (private, public and municipalized)",
          `2018`=sum(.$`2018`[17]))%>%
  
  add_row(Rama.Actividad.Económica="Unknown or Other Activities",
          `2018`=sum(.$`2018`[20]+.$`2018`[21]+.$`2018`[22]))



cuadro4_6 <- cuadro4_6[-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18),]
cuadro4_7 <- cuadro4_7[-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22),]

cuadro5_hombres<-merge(cuadro4_6,cuadro4_7,by="Rama.Actividad.Económica",all.x = TRUE)
#cuadro5_hombres<-merge(cuadro5_hombres,cuadro4_4,by="Rama.Actividad.EconÃ³mica",all.x = TRUE)

cuadro5_hombres<-unique( cuadro5_hombres )
#cuadro5_hombres<-cuadro5_hombres[-3,]


# mujeres
cuadro4_8<-cuadro4_2_sexo %>% select(1,4-1,7-1,10-1,13-1,16-1,19-1,22-1,25-1,28-1)
cuadro4_9<-cuadro4_3_sexo %>% select(1,4-1,7-1,10-1,13-1,16-1,19-1,22-1)
cuadro4_10<-cuadro4_4_sexo %>% select(1,4-1)

colnames(cuadro4_8)<-c("Rama.Actividad.Económica",2002,2003,2004,2005,2006,2007,2008,2009,2010)

colnames(cuadro4_9)<-c("Rama.Actividad.Económica",2011,2012,2013,2014,2015,2016,2017)

cuadro4_9<-cuadro4_9 %>% add_row(Rama.Actividad.Económica="A Agriculture",
                                 `2011`=sum(.$`2011`[1]+.$`2011`[2]),
                                 `2012`=sum(.$`2012`[1]+.$`2012`[2]),
                                 `2013`=sum(.$`2013`[1]+.$`2013`[2]),
                                 `2014`=sum(.$`2014`[1]+.$`2014`[2]),
                                 `2015`=sum(.$`2015`[1]+.$`2015`[2]),
                                 `2016`=sum(.$`2016`[1]+.$`2016`[2]),
                                 `2017`=sum(.$`2017`[1]+.$`2017`[2])) %>% 
  
  add_row(Rama.Actividad.Económica="B Mining",
          `2011`=sum(.$`2011`[3]),
          `2012`=sum(.$`2012`[3]),
          `2013`=sum(.$`2013`[3]),
          `2014`=sum(.$`2014`[3]),
          `2015`=sum(.$`2015`[3]),
          `2016`=sum(.$`2016`[3]),
          `2017`=sum(.$`2017`[3])) %>%
  
  add_row(Rama.Actividad.Económica="C Manufacturing industry",
          `2011`=sum(.$`2011`[4]),
          `2012`=sum(.$`2012`[4]),
          `2013`=sum(.$`2013`[4]),
          `2014`=sum(.$`2014`[4]),
          `2015`=sum(.$`2015`[4]),
          `2016`=sum(.$`2016`[4]),
          `2017`=sum(.$`2017`[4])) %>%
  
  add_row(Rama.Actividad.Económica="D-E Electricity, Water and Sanitary Services",
          `2011`=sum(.$`2011`[5]),
          `2012`=sum(.$`2012`[5]),
          `2013`=sum(.$`2013`[5]),
          `2014`=sum(.$`2014`[5]),
          `2015`=sum(.$`2015`[5]),
          `2016`=sum(.$`2016`[5]),
          `2017`=sum(.$`2017`[5])) %>%
  
  add_row(Rama.Actividad.Económica="F Construction",
          `2011`=sum(.$`2011`[6]),
          `2012`=sum(.$`2012`[6]),
          `2013`=sum(.$`2013`[6]),
          `2014`=sum(.$`2014`[6]),
          `2015`=sum(.$`2015`[6]),
          `2016`=sum(.$`2016`[6]),
          `2017`=sum(.$`2017`[6])) %>%
  
  add_row(Rama.Actividad.Económica="G-I Commerce",
          `2011`=sum(.$`2011`[7]+.$`2011`[8]),
          `2012`=sum(.$`2012`[7]+.$`2012`[8]),
          `2013`=sum(.$`2013`[7]+.$`2013`[8]),
          `2014`=sum(.$`2014`[7]+.$`2014`[8]),
          `2015`=sum(.$`2015`[7]+.$`2015`[8]),
          `2016`=sum(.$`2016`[7]+.$`2016`[8]),
          `2017`=sum(.$`2017`[7]+.$`2017`[8])) %>%
  
  add_row(Rama.Actividad.Económica="H-J Transportation and Communication",
          `2011`=sum(.$`2011`[9]),
          `2012`=sum(.$`2012`[9]),
          `2013`=sum(.$`2013`[9]),
          `2014`=sum(.$`2014`[9]),
          `2015`=sum(.$`2015`[9]),
          `2016`=sum(.$`2016`[9]),
          `2017`=sum(.$`2017`[9])) %>%
  
  add_row(Rama.Actividad.Económica="L-K Banks and Financial Services",
          `2011`=sum(.$`2011`[10]+.$`2011`[11]),
          `2012`=sum(.$`2012`[10]+.$`2012`[11]),
          `2013`=sum(.$`2013`[10]+.$`2013`[11]),
          `2014`=sum(.$`2014`[10]+.$`2014`[11]),
          `2015`=sum(.$`2015`[10]+.$`2015`[11]),
          `2016`=sum(.$`2016`[10]+.$`2016`[11]),
          `2017`=sum(.$`2017`[10]+.$`2017`[11])) %>%
  
  add_row(Rama.Actividad.Económica="O Central, Regional and Municipal Government",
          `2011`=sum(.$`2011`[12]),
          `2012`=sum(.$`2012`[12]),
          `2013`=sum(.$`2013`[12]),
          `2014`=sum(.$`2014`[12]),
          `2015`=sum(.$`2015`[12]),
          `2016`=sum(.$`2016`[12]),
          `2017`=sum(.$`2017`[12])) %>%
  
  add_row(Rama.Actividad.Económica="P Education (private, public and municipalized)",
          `2011`=sum(.$`2011`[13]),
          `2012`=sum(.$`2012`[13]),
          `2013`=sum(.$`2013`[13]),
          `2014`=sum(.$`2014`[13]),
          `2015`=sum(.$`2015`[13]),
          `2016`=sum(.$`2016`[13]),
          `2017`=sum(.$`2017`[13])) %>%
  
  add_row(Rama.Actividad.Económica="Q Health (private, public and municipalized)",
          `2011`=sum(.$`2011`[14]),
          `2012`=sum(.$`2012`[14]),
          `2013`=sum(.$`2013`[14]),
          `2014`=sum(.$`2014`[14]),
          `2015`=sum(.$`2015`[14]),
          `2016`=sum(.$`2016`[14]),
          `2017`=sum(.$`2017`[14])) %>% 
  
  add_row(Rama.Actividad.Económica="Q Other Community, Social and Personal Services",
          `2011`=sum(.$`2011`[15]),
          `2012`=sum(.$`2012`[15]),
          `2013`=sum(.$`2013`[15]),
          `2014`=sum(.$`2014`[15]),
          `2015`=sum(.$`2015`[15]),
          `2016`=sum(.$`2016`[15]),
          `2017`=sum(.$`2017`[15])) %>% 
  
  add_row(Rama.Actividad.Económica="Unknown or Other Activities",
          `2011`=sum(.$`2011`[16]+.$`2011`[17]+.$`2011`[18]),
          `2012`=sum(.$`2012`[16]+.$`2012`[17]+.$`2012`[18]),
          `2013`=sum(.$`2013`[16]+.$`2013`[17]+.$`2013`[18]),
          `2014`=sum(.$`2014`[16]+.$`2014`[17]+.$`2014`[18]),
          `2015`=sum(.$`2015`[16]+.$`2015`[17]+.$`2015`[18]),
          `2016`=sum(.$`2016`[16]+.$`2016`[17]+.$`2016`[18]),
          `2017`=sum(.$`2017`[16]+.$`2017`[17]+.$`2017`[18]))

colnames(cuadro4_10)<-c("Rama.Actividad.Económica",2018)

cuadro4_10<-cuadro4_10 %>% add_row(Rama.Actividad.Económica="A Agriculture",
                                   `2018`=sum(.$`2018`[1])) %>% 
  
  add_row(Rama.Actividad.Económica="B Mining",
          `2018`=sum(.$`2018`[2])) %>%
  
  add_row(Rama.Actividad.Económica="C Manufacturing industry",
          `2018`=sum(.$`2018`[3])) %>%
  
  add_row(Rama.Actividad.Económica="D-E Electricity, Water and Sanitary Services",
          `2018`=sum(.$`2018`[4]+.$`2018`[5])) %>%
  
  add_row(Rama.Actividad.Económica="F Construction",
          `2018`=sum(.$`2018`[6])) %>%
  
  add_row(Rama.Actividad.Econ?mica="G-I Commerce",
          `2018`=sum(.$`2018`[7]+.$`2018`[9])) %>%
  
  add_row(Rama.Actividad.Económica="H-J Transportation and Communication",
          `2018`=sum(.$`2018`[8]+.$`2018`[10])) %>%
  
  add_row(Rama.Actividad.Económica="L-K Banks and Financial Services",
          `2018`=sum(.$`2018`[11]+.$`2018`[12])) %>%
  
  add_row(Rama.Actividad.Económica="Q Other Community, Social and Personal Services",
          `2018`=sum(.$`2018`[13]+.$`2018`[14]+.$`2018`[18]+.$`2018`[19])) %>%
  
  add_row(Rama.Actividad.Económica="O Central, Regional and Municipal Government",
          `2018`=sum(.$`2018`[15]))%>%
  
  add_row(Rama.Actividad.Económica="P Education (private, public and municipalized)",
          `2018`=sum(.$`2018`[16]))%>%
  
  add_row(Rama.Actividad.Económica="Q Health (private, public and municipalized)",
          `2018`=sum(.$`2018`[17]))%>%
  
  add_row(Rama.Actividad.Económica="Unknown or Other Activities",
          `2018`=sum(.$`2018`[20]+.$`2018`[21]+.$`2018`[22]))

cuadro4_9 <- cuadro4_9[-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18),]
cuadro4_10 <- cuadro4_10[-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22),]

cuadro5_mujeres<-merge(cuadro4_9,cuadro4_10,by="Rama.Actividad.Económica",all.x = TRUE)
#cuadro5_mujeres<-merge(cuadro5_mujeres,cuadro4_4,by="Rama.Actividad.EconÃ³mica",all.x = TRUE)

cuadro5_mujeres<-unique( cuadro5_mujeres )
#cuadro5_mujeres<-cuadro5_mujeres[-3,]

## combinar y crear cuadro 5

cuadro5_hombres<-cuadro5_hombres %>% mutate(sexo="hombres")
cuadro5_mujeres<-cuadro5_mujeres %>% mutate(sexo="mujeres")
cuadro5<-rbind(cuadro5_hombres,cuadro5_mujeres)

write_xlsx(cuadro5,"cuadro5.xlsx", col_names = TRUE,format_headers = TRUE)

#2. Cargar datos INE (censo 2017) ------------
pob_ocup_rama<-read.xlsx("Copia de serie-ocupados-según-rama-de-actividad-económica-ciiu-rev4.cl.xlsx",
                             sheet = 2,startRow = 7)

#3. Recodificar base y nombres cuadro 6 (ocupados totales) --------------------
nombres<-names(pob_ocup_rama)

pob_ocup_rama <-pob_ocup_rama[1:(nrow(pob_ocup_rama)-3), c(1,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48)]
nombres<-nombres[c(1,2,4-1,6-1,8-1,10-1,12-1,14-1,16-1,18-1,20-1,22-1,24-1,26-1,28-1,30-1,32-1,34-1,36-1,38-1,40-1,42-1,44-1,46-1,48-1)]
names(pob_ocup_rama)<-nombres

pob_ocup_rama <- pob_ocup_rama[-c(93,94,95,96),]

names(pob_ocup_rama)<-c("ano","trimestre","total_ocup","A_Agriculture","B_Mining","C_Manufacturing_industry", "D_Electricity_gas", "E_Water", "F_Construction", "G_Commerce", "H_Transport", "I_Hotel", "J_Comunications", "K_Financal", "L_Real_estate", "M_Professional",
                         "N_Admnistrative","O_Public_Adm","P_Education", "Q_Health", "R_Arts", "S_Other_Service", "T_Houseohlds", "U_Extraterritorial", "None")


#4. Filter and mutate ---------- 
a <- pob_ocup_rama %>% filter(trimestre=="Oct - Dic" & ano>=2016&ano<2019)%>% select(ano, total_ocup, A_Agriculture, B_Mining,
                                                                                     C_Manufacturing_industry, D_Electricity_gas,
                                                                                     E_Water, F_Construction, G_Commerce, H_Transport,
                                                                                     I_Hotel, J_Comunications, K_Financal, L_Real_estate,
                                                                                     M_Professional, N_Admnistrative, O_Public_Adm, P_Education,
                                                                                     Q_Health, R_Arts, S_Other_Service, T_Houseohlds, U_Extraterritorial,
                                                                                     None)

cuadro_a <- a %>% group_by(ano) %>% 
  mutate(total_ocup=as.numeric(total_ocup), 
         A_Agriculture=as.numeric(A_Agriculture), 
         B_Mining=as.numeric(B_Mining),
         C_Manufacturing_industry=as.numeric(C_Manufacturing_industry),
         'D-E_Electricity_Water_and_Sanitary_Services'=D_Electricity_gas+E_Water,
         F_Construction=as.numeric(F_Construction),
         'G_I_Commerce'=G_Commerce+I_Hotel,
         'H_J_Transportation_and_Communication'=H_Transport+J_Comunications,
         'L_K_Banks_and_Financial_Services'=K_Financal+L_Real_estate,
         'Q_Other_Community_Social_and_Personal_Services'=M_Professional+N_Admnistrative+R_Arts+S_Other_Service,
         O_Central_Regional_and_Municipal_Government=O_Public_Adm,
         P_Education=as.numeric(P_Education),
         Q_Health=as.numeric(Q_Health),
         'Unknown_or_Other_Activities'=T_Houseohlds+U_Extraterritorial) %>% as.data.frame()


cuadro_a <- cuadro_a[-c(6,7,9,10,11,12,13,14,15,16,17,20,21,22,23,24)]

#5.Crear cuadro 6 ------------

cuadro6 <- rbind(
  c("A Agriculture", 722.4282, 724.8103, 721.5891),
  c("B Mining", 203.6867, 205.9247, 216.4950),
  c("C Manufacturing industry", 901.3723, 942.1697, 887.6124),
  c("D-E Electricity, Water and Sanitary Services", 95.29307, 98.76027, 93.68633),
  c("F Construction", 713.6906, 711.0358, 769.4652), 
  c("G-I Commerce", 2050.093, 2048.900, 2123.043),
  c("H-J Transportation and Communication", 707.1425, 742.9791, 774.5569),
  c("L-K Banks and Financial Services", 234.0996, 259.3169, 252.0655),
  c("O Central, Regional and Municipal Government", 460.9567, 483.1438, 491.4204),
  c("P Education (private, public and municipalized)", 725.4361, 777.7899, 787.2497),
  c("Q Health (private, public and municipalized)", 452.0837, 469.8346, 521.0453),
  c("Q Other Community, Social and Personal Services", 850.8740, 929.5964, 916.7496),
  c("Unknown or Other Activities", 383.5538, 374.4051, 359.2706),
  c("Total", 8500.710, 8768.667, 8914.248))%>% as.data.frame()


names(cuadro6)<-c("Rama.Actividad.Económica","ocup_2016","ocup_2017","ocup_2018")

#6. Cuadros por sexo ---------------
#6.1 Hombres ------------

homb_ocup_rama <-read.xlsx("Copia de serie-ocupados-según-rama-de-actividad-económica-ciiu-rev4.cl.xlsx",
                           sheet = 3,startRow = 7)

nombres<-names(homb_ocup_rama)

homb_ocup_rama <-homb_ocup_rama[1:(nrow(homb_ocup_rama)-3), c(1,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48)]
nombres<-nombres[c(1,2,4-1,6-1,8-1,10-1,12-1,14-1,16-1,18-1,20-1,22-1,24-1,26-1,28-1,30-1,32-1,34-1,36-1,38-1,40-1,42-1,44-1,46-1,48-1)]
names(homb_ocup_rama)<-nombres

homb_ocup_rama <- homb_ocup_rama[-c(93,94,95,96),]

names(homb_ocup_rama)<-c("ano","trimestre","total_ocup","A_Agriculture","B_Mining","C_Manufacturing_industry", "D_Electricity_gas", "E_Water", "F_Construction", "G_Commerce", "H_Transport", "I_Hotel", "J_Comunications", "K_Financal", "L_Real_estate", "M_Professional",
                        "N_Admnistrative","O_Public_Adm","P_Education", "Q_Health", "R_Arts", "S_Other_Service", "T_Houseohlds", "U_Extraterritorial", "None")


b <- homb_ocup_rama %>% filter(trimestre=="Oct - Dic" & ano>=2016&ano<2019)%>% select(ano, total_ocup, A_Agriculture, B_Mining,
                                                                                     C_Manufacturing_industry, D_Electricity_gas,
                                                                                     E_Water, F_Construction, G_Commerce, H_Transport,
                                                                                     I_Hotel, J_Comunications, K_Financal, L_Real_estate,
                                                                                     M_Professional, N_Admnistrative, O_Public_Adm, P_Education,
                                                                                     Q_Health, R_Arts, S_Other_Service, T_Houseohlds, U_Extraterritorial,
                                                                                     None)

cuadro_b <- b %>% group_by(ano) %>% 
  mutate(total_ocup=as.numeric(total_ocup), 
         A_Agriculture=as.numeric(A_Agriculture), 
         B_Mining=as.numeric(B_Mining),
         C_Manufacturing_industry=as.numeric(C_Manufacturing_industry),
         'D-E_Electricity_Water_and_Sanitary_Services'=D_Electricity_gas+E_Water,
         F_Construction=as.numeric(F_Construction),
         'G_I_Commerce'=G_Commerce+I_Hotel,
         'H_J_Transportation_and_Communication'=H_Transport+J_Comunications,
         'L_K_Banks_and_Financial_Services'=K_Financal+L_Real_estate,
         'Q_Other_Community_Social_and_Personal_Services'=M_Professional+N_Admnistrative+R_Arts+S_Other_Service,
         O_Central_Regional_and_Municipal_Government=O_Public_Adm,
         P_Education=as.numeric(P_Education),
         Q_Health=as.numeric(Q_Health),
         'Unknown_or_Other_Activities'=T_Houseohlds+U_Extraterritorial) %>% as.data.frame()


cuadro_b <- cuadro_b[-c(6,7,9,10,11,12,13,14,15,16,17,20,21,22,23,24)]

# Cuadro 6 para sexo hombres

cuadro6_homb <- rbind(
  c("A Agriculture", 536.2180, 542.4714, 539.6693),
  c("B Mining", 185.9390, 187.9241, 190.3159),
  c("C Manufacturing industry", 598.0049, 631.1619, 614.0183),
  c("D-E Electricity, Water and Sanitary Services", 76.40419, 84.06328, 79.06487),
  c("F Construction", 669.6761, 662.9351, 720.3188), 
  c("G-I Commerce", 1059.144, 1059.251, 1065.295),
  c("H-J Transportation and Communication", 577.7433, 613.0182, 616.6072),
  c("L-K Banks and Financial Services", 115.4385, 136.1086, 124.4531),
  c("O Central, Regional and Municipal Government", 281.9876, 280.9498, 293.1033),
  c("P Education (private, public and municipalized)", 226.3161, 227.5072, 232.6717),
  c("Q Health (private, public and municipalized)", 116.8076, 123.7488, 134.1476),
  c("Q Other Community, Social and Personal Services", 458.3188, 477.3333, 500.0691),
  c("Unknown or Other Activities", 30.16408, 31.35151, 26.56438),
  c("Total", 4932.162, 5057.824, 5136.299))%>% as.data.frame()


names(cuadro6_homb)<-c("Rama.Actividad.Económica","homb_ocup_2016","homb_ocup_2017","homb_ocup_2018")

#6.2 Mujeres ------------

muj_ocup_rama <-read.xlsx("Copia de serie-ocupados-según-rama-de-actividad-económica-ciiu-rev4.cl.xlsx",
                           sheet = 4,startRow = 7)

nombres<-names(muj_ocup_rama)

muj_ocup_rama <-muj_ocup_rama[1:(nrow(muj_ocup_rama)-3), c(1,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48)]
nombres<-nombres[c(1,2,4-1,6-1,8-1,10-1,12-1,14-1,16-1,18-1,20-1,22-1,24-1,26-1,28-1,30-1,32-1,34-1,36-1,38-1,40-1,42-1,44-1,46-1,48-1)]
names(muj_ocup_rama)<-nombres

muj_ocup_rama <- muj_ocup_rama[-c(93,94,95,96),]

names(muj_ocup_rama)<-c("ano","trimestre","total_ocup","A_Agriculture","B_Mining","C_Manufacturing_industry", "D_Electricity_gas", "E_Water", "F_Construction", "G_Commerce", "H_Transport", "I_Hotel", "J_Comunications", "K_Financal", "L_Real_estate", "M_Professional",
                         "N_Admnistrative","O_Public_Adm","P_Education", "Q_Health", "R_Arts", "S_Other_Service", "T_Houseohlds", "U_Extraterritorial", "None")

c <- muj_ocup_rama %>% filter(trimestre=="Oct - Dic" & ano>=2016&ano<2019)%>% select(ano, total_ocup, A_Agriculture, B_Mining,
                                                                                      C_Manufacturing_industry, D_Electricity_gas,
                                                                                      E_Water, F_Construction, G_Commerce, H_Transport,
                                                                                      I_Hotel, J_Comunications, K_Financal, L_Real_estate,
                                                                                      M_Professional, N_Admnistrative, O_Public_Adm, P_Education,
                                                                                      Q_Health, R_Arts, S_Other_Service, T_Houseohlds, U_Extraterritorial,
                                                                                      None)

cuadro_c <- c %>% group_by(ano) %>% 
  mutate(total_ocup=as.numeric(total_ocup), 
         A_Agriculture=as.numeric(A_Agriculture), 
         B_Mining=as.numeric(B_Mining),
         C_Manufacturing_industry=as.numeric(C_Manufacturing_industry),
         'D-E_Electricity_Water_and_Sanitary_Services'=D_Electricity_gas+E_Water,
         F_Construction=as.numeric(F_Construction),
         'G_I_Commerce'=G_Commerce+I_Hotel,
         'H_J_Transportation_and_Communication'=H_Transport+J_Comunications,
         'L_K_Banks_and_Financial_Services'=K_Financal+L_Real_estate,
         'Q_Other_Community_Social_and_Personal_Services'=M_Professional+N_Admnistrative+R_Arts+S_Other_Service,
         O_Central_Regional_and_Municipal_Government=O_Public_Adm,
         P_Education=as.numeric(P_Education),
         Q_Health=as.numeric(Q_Health),
         'Unknown_or_Other_Activities'=T_Houseohlds+U_Extraterritorial) %>% as.data.frame()


cuadro_c <- cuadro_c[-c(6,7,9,10,11,12,13,14,15,16,17,20,21,22,23,24)]

# Cuadro 6 para sexo mujeres

cuadro6_muj <- rbind(
  c("A Agriculture", 186.2102, 182.3389, 181.9198),
  c("B Mining", 17.74763, 18.00063, 26.17903),
  c("C Manufacturing industry", 303.3675, 311.0078, 273.5942),
  c("D-E Electricity, Water and Sanitary Services", 18.88887, 14.69699, 14.62146),
  c("F Construction", 44.01451, 48.10071, 49.14644), 
  c("G-I Commerce", 990.9487, 989.6494, 1057.7473),
  c("H-J Transportation and Communication", 129.3992, 129.9608, 157.9498),
  c("L-K Banks and Financial Services", 118.6610, 123.2084, 127.6124),
  c("O Central, Regional and Municipal Government", 178.9691, 202.1940, 198.3171),
  c("P Education (private, public and municipalized)", 499.1200, 550.2827, 554.5780),
  c("Q Health (private, public and municipalized)", 335.2761, 346.0858, 386.8977),
  c("Q Other Community, Social and Personal Services", 392.5552, 452.2631, 416.6805),
  c("Unknown or Other Activities", 353.3897, 343.0536, 332.7062),
  c("Total", 3568.548, 3710.843, 3777.950))%>% as.data.frame()


names(cuadro6_muj)<-c("Rama.Actividad.Económica","muj_ocup_2016","muj_ocup_2017","muj_ocup_2018")

# 7. Unir cuadros ---------------- 

# cuadros sexo 

cuadro6_sexo <-merge(cuadro6_homb, cuadro6_muj, by="Rama.Actividad.Económica",all.x = TRUE)

cuadro6_sexo <-merge(cuadro6_sexo, cuadro6, by="Rama.Actividad.Económica",all.x = TRUE)

# cuadro sexo y cuadros 5 

cuadro_tasa_sex_hombres <- merge(cuadro6_homb, cuadro5_hombres, by="Rama.Actividad.Económica",all.x = TRUE)
cuadro_tasa_sex_mujeres <- merge(cuadro6_muj, cuadro5_mujeres, by="Rama.Actividad.Económica",all.x = TRUE)

cuadro_tasa_sex_hombres <- cuadro_tasa_sex_hombres[-c(5,6,7,8,9)]
cuadro_tasa_sex_mujeres <- cuadro_tasa_sex_mujeres[-c(5,6,7,8,9)]

names(cuadro_tasa_sex_hombres)<-c("Rama.Actividad.Económica","ocup_2016","ocup_2017","ocup_2018","afi_2016","afi_2017","afi_2018","sexo")

names(cuadro_tasa_sex_mujeres)<-c("Rama.Actividad.Económica","ocup_2016","ocup_2017","ocup_2018","afi_2016","afi_2017","afi_2018","sexo")

cuadro_tasa_sex<-rbind(cuadro_tasa_sex_hombres,cuadro_tasa_sex_mujeres)
cuadro_tasa_sex$ocup_2016 <- as.numeric(cuadro_tasa_sex$ocup_2016)
cuadro_tasa_sex$ocup_2017 <- as.numeric(cuadro_tasa_sex$ocup_2017)
cuadro_tasa_sex$ocup_2018 <- as.numeric(cuadro_tasa_sex$ocup_2018)
cuadro_tasa_sex <- as.data.frame(cuadro_tasa_sex)


write_xlsx(cuadro_tasa_sex,"cuadro_tasa_sex.xlsx", col_names = TRUE,format_headers = TRUE)
write_xlsx(cuadro_tasa_sex_hombres,"cuadro_tasa_sex_hombres.xlsx", col_names = TRUE,format_headers = TRUE)
write_xlsx(cuadro_tasa_sex_mujeres,"cuadro_tasa_sex_mujeres.xlsx", col_names = TRUE,format_headers = TRUE)

library(readxl)
tasa_sindi_sex <- read_excel("cuadro_tasa_sex2.xlsx") #se carga desde comuptador local puesto que se edita en excel
View(tasa_sindi_sex)

tasa_sindi_sex <- tasa_sindi_sex %>% mutate(ocup_2016=as.numeric(ocup_2016),
                                            ocup_2017=as.numeric(ocup_2017),
                                            ocup_2018=as.numeric(ocup_2018),
                                            tasa_sind_2016=afi_2016*100/ocup_2016,
                                            tasa_sind_2017=afi_2017*100/ocup_2017,
                                            tasa_sind_2018=afi_2018*100/ocup_2018)%>% as.data.frame()

write_xlsx(tasa_sindi_sex,"tasa_sindi_sex.xlsx", col_names = TRUE,format_headers = TRUE)
