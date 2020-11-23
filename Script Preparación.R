## 00: Script preparacion

# ---- 1. Librerias y datos ----
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
ohl<-readWorkbook("C:/Users/USUARIO/Desktop/8vo semestre/Taller de Investigación del Trabajo/Labor_Strikes_Dataset_1979_2018_Public.xlsx", detectDates=TRUE)

# ---- 2. Procesamiento ---- 
ohl$ciuur2<-as.factor(ohl$ciuur2)
ohl$ciuur3<-as.factor(ohl$ciuur3)
ohl$leg<-as.factor(ohl$leg)

ohl<-ohl %>% mutate(sector=case_when(ciuur2==1 ~ "A Agriculture",
                                     ciuur2==2 ~ "B Mining",
                                     ciuur2==3 ~ "C Manufacturing industry",
                                     ciuur2==4 ~ "D-E Electricity, Water and Sanitary Services",
                                     ciuur2==5 ~ "F Construction",
                                     ciuur2==6 ~ "G-I Commerce",
                                     ciuur2==7 ~ "H-J Transportation and Communication",
                                     ciuur2==8 ~ "L-K Banks and Financial Services",
                                     ciuur2==9 ~  "O Central, Regional and Municipal Government",
                                     ciuur2==10 ~ "P Education (private, public and municipalized)",
                                     ciuur2==11 ~ "Q Health (private, public and municipalized)",
                                     ciuur2==12 ~ "Q Social and Personal Services",
                                     ciuur2==13 ~ "Unknown or Other Activities",
                                     ciuur2==14 ~ "National Strikes",
                                     is.na(ciuur2)&ciuur4==1 ~ "A Agriculture",
                                     is.na(ciuur2)&ciuur4==2  ~ "B Mining",
                                     is.na(ciuur2)&ciuur4==3  ~ "C Manufacturing industry",
                                     is.na(ciuur2)&ciuur4==4  ~ "D-E Electricity, Water and Sanitary Services",
                                     is.na(ciuur2)&ciuur4==5  ~ "D-E Electricity, Water and Sanitary Services",
                                     is.na(ciuur2)&ciuur4==6  ~ "F Construction",
                                     is.na(ciuur2)&ciuur4==7  ~ "G-I Commerce",
                                     is.na(ciuur2)&ciuur4==8  ~ "H-J Transportation and Communication",
                                     is.na(ciuur2)&ciuur4==9  ~ "G-I Commerce",
                                     is.na(ciuur2)&ciuur4==10 ~ "H-J Transportation and Communication",
                                     is.na(ciuur2)&ciuur4==11 ~ "L-K Banks and Financial Services",
                                     is.na(ciuur2)&ciuur4==12 ~ "L-K Banks and Financial Services",
                                     is.na(ciuur2)&ciuur4==13 ~ "M Professional, scientific and technical activities",
                                     is.na(ciuur2)&ciuur4==14 ~ "N Activities of administrative and support services",
                                     is.na(ciuur2)&ciuur4==15 ~ "O Central, Regional and Municipal Government",
                                     is.na(ciuur2)&ciuur4==16 ~ "P Education (private, public and municipalized)",
                                     is.na(ciuur2)&ciuur4==17 ~ "Q Health (private, public and municipalized)",
                                     is.na(ciuur2)&ciuur4==18 ~ "R Artistic, entertainment and recreational activities",
                                     is.na(ciuur2)&ciuur4==19 ~ "Unknown or Other Activities",
                                     is.na(ciuur2)&ciuur4==20 ~ "Unknown or Other Activities",
                                     is.na(ciuur2)&ciuur4==21 ~ "Unknown or Other Activities"))

ohl %>% subset(is.na(ciuur2)) %>% select(yr,ciuur2,ciuur3,ciuur4,sector)

# Pasos a seguir: 
# 1. Seleccionar las variables de la base procesada
ohl$num <- as.numeric(row.names(ohl))
options(scipen=999) # valores sin notación científica

proc_ohl <- ohl%>%select(organizacion = org, 
                            legalidad= leg, 
                            ano= yr, 
                            motivos= dem1, 
                            ddpp =ddpp,
                            sector = sector,
                            tot_trabajadores = trabemp,
                            rango_empresa = rangoemp,
                            trab_comprometidos = tc,
                            tactica = tactica1,
                            aliado = aliado1,
                            central = central1,
                            dptp = dhtp,
                            number = num, 
                         autoridad = autoridad)

# Comprobar
names(proc_ohl)

# 2. Reducir la base al año 2017 y 2018 
proc_ohl$ano = as_numeric(proc_ohl$ano)
class(proc_ohl$ano)

proc_ohl$ano[proc_ohl$ano < 2017] <- NA
proc_ohl <- proc_ohl[!is.na(proc_ohl$ano),]
view(proc_ohl)

# 3. Recodificar, renombrar y etiquetar las variables a utilizar 
# 4. Evaluar NA's 
# 5. Descriptivos de variables de interés y tablas de contingencia 
# 6. Añadir datos sobre sindicalización por sexo y PIB por rama para evaluar poder asociativo y poder estructural respectivamente
# 7. Finiquitar base de datos procesada para el análisis 