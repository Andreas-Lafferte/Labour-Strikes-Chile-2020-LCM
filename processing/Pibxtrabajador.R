## 02: Generacion de cuadro pibxtrabajador según rama 2016-2018
## Andreas Lafferte y Emi 


#1. Librerias ---- 
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

#2. Data (pib nominal) ------------
options(scipen=999)
pib_nominal <- read_excel("input/03_PIB_nominal_por_actividad.xlsx")
View(pib_nominal)

#3. Recode pib nominal ------
pib_nominal <- pib_nominal %>% add_row(Rama.Actividad.Económica="A Agriculture",
                                        `2016`=sum(.$`2016`[1]+.$`2016`[2]),
                                        `2017`=sum(.$`2017`[1]+.$`2017`[2]),
                                        `2018`=sum(.$`2018`[1]+.$`2018`[2])) %>% 
  
  add_row(Rama.Actividad.Económica="B Mining",
          `2016`=sum(.$`2016`[3]),
          `2017`=sum(.$`2017`[3]),
          `2018`=sum(.$`2018`[3])) %>%
  
  add_row(Rama.Actividad.Económica="C Manufacturing industry",
          `2016`=sum(.$`2016`[4]),
          `2017`=sum(.$`2017`[4]),
          `2018`=sum(.$`2018`[4])) %>%
  
  add_row(Rama.Actividad.Económica="D-E Electricity, Water and Sanitary Services",
          `2016`=sum(.$`2016`[5]),
          `2017`=sum(.$`2017`[5]),
          `2018`=sum(.$`2018`[5])) %>%
  
  add_row(Rama.Actividad.Económica="F Construction",
          `2016`=sum(.$`2016`[6]),
          `2017`=sum(.$`2017`[6]),
          `2018`=sum(.$`2018`[6])) %>%
  
  add_row(Rama.Actividad.Económica="G-I Commerce",
          `2016`=sum(.$`2016`[7]+.$`2016`[8]),
          `2017`=sum(.$`2017`[7]+.$`2017`[8]),
          `2018`=sum(.$`2018`[7]+.$`2018`[8])) %>%
  
  add_row(Rama.Actividad.Económica="H-J Transportation and Communication",
          `2016`=sum(.$`2016`[9]+.$`2016`[10]),
          `2017`=sum(.$`2017`[9]+.$`2017`[10]),
          `2018`=sum(.$`2018`[9]+.$`2018`[10])) %>%
  
  add_row(Rama.Actividad.Económica="L-K Banks and Financial Services",
          `2016`=sum(.$`2016`[11]+.$`2016`[12]+.$`2016`[13]),
          `2017`=sum(.$`2017`[11]+.$`2017`[12]+.$`2017`[13]),
          `2018`=sum(.$`2018`[11]+.$`2018`[12]+.$`2018`[13])) %>% #se añade servicios empresariales (evaluar)
  
  add_row(Rama.Actividad.Económica="O Central, Regional and Municipal Government",
          `2016`=sum(.$`2016`[17]),
          `2017`=sum(.$`2017`[17]),
          `2018`=sum(.$`2018`[17])) %>%
  
  add_row(Rama.Actividad.Económica="P Education (private, public and municipalized)",
          `2016`=sum(.$`2016`[15]),
          `2017`=sum(.$`2017`[15]),
          `2018`=sum(.$`2018`[15])) %>%
  
  add_row(Rama.Actividad.Económica="Q Health (private, public and municipalized)",
          `2016`=sum(.$`2016`[16]),
          `2017`=sum(.$`2017`[16]),
          `2018`=sum(.$`2018`[16])) %>% 
  
  add_row(Rama.Actividad.Económica="Q Other Community, Social and Personal Services",
          `2016`=sum(.$`2016`[14]),
          `2017`=sum(.$`2017`[14]),
          `2018`=sum(.$`2018`[14])) %>% 
  
  add_row(Rama.Actividad.Económica="Unknown or Other Activities",
          `2016`=sum(.$`2016`[18]),
          `2017`=sum(.$`2017`[18]),
          `2018`=sum(.$`2018`[18])) %>% 
  
  add_row(Rama.Actividad.Económica="Total",
          `2016`=sum(.$`2016`[1]+.$`2016`[2]+.$`2016`[3]+.$`2016`[4]+.$`2016`[5]+.$`2016`[6]+.$`2016`[7]+.$`2016`[8]+.$`2016`[9]+.$`2016`[10]+.$`2016`[11]+.$`2016`[12]+.$`2016`[13]+.$`2016`[14]+.$`2016`[15]+.$`2016`[16]+.$`2016`[17]+.$`2016`[18]),
          `2017`=sum(.$`2017`[1]+.$`2017`[2]+.$`2017`[3]+.$`2017`[4]+.$`2017`[5]+.$`2017`[6]+.$`2017`[7]+.$`2017`[8]+.$`2017`[9]+.$`2017`[10]+.$`2017`[11]+.$`2017`[12]+.$`2017`[13]+.$`2017`[14]+.$`2017`[15]+.$`2017`[16]+.$`2017`[17]+.$`2017`[18]),
          `2018`=sum(.$`2018`[1]+.$`2018`[2]+.$`2018`[3]+.$`2018`[4]+.$`2018`[5]+.$`2018`[6]+.$`2018`[7]+.$`2018`[8]+.$`2018`[9]+.$`2018`[10]+.$`2018`[11]+.$`2018`[12]+.$`2018`[13]+.$`2018`[14]+.$`2018`[15]+.$`2018`[16]+.$`2018`[17]+.$`2018`[18]))

pib_nominal <- pib_nominal[-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18),]
names(pib_nominal)<-c("Rama.Actividad.Económica","pib_2016","pib_2017","pib_2018")

# 4. Cargar cuadro 6 (ocupados por rama) -----
cuadro6 <- read_excel("output/cuadro6.xlsx")
View(cuadro6)
# 5. Unir bases -------
pib_x_trab <- merge(pib_nominal,cuadro6, by="Rama.Actividad.Económica",all.x = TRUE)

pib_x_trab <- pib_x_trab %>% mutate(ocup_2016=as.numeric(ocup_2016),
                                    ocup_2017=as.numeric(ocup_2017),
                                    ocup_2018=as.numeric(ocup_2018),
                                    pib_2016=as.numeric(pib_2016),
                                    pib_2017=as.numeric(pib_2017),
                                    pib_2018=as.numeric(pib_2018),
                                    pibxtrab_2016=pib_2016/ocup_2016,
                                    pibxtrab_2017=pib_2017/ocup_2017,
                                    pibxtrab_2018=pib_2018/ocup_2018)%>% as.data.frame()

pib_x_trab <- pib_x_trab[-c(2,3,4,5,6,7)]
write_xlsx(pib_x_trab,"output/pib_x_trab.xlsx", col_names = TRUE,format_headers = TRUE)
