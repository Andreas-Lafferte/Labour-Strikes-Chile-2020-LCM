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
ohl<-readWorkbook("input/Labor_Strikes_Dataset_1979_2018_Public.xlsx", detectDates=TRUE)

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
                                     ciuur2==12 ~ "Q Other Community, Social and Personal Services",
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
                                     is.na(ciuur2)&ciuur4==13 ~ "Q Other Community, Social and Personal Services",
                                     is.na(ciuur2)&ciuur4==14 ~ "Q Other Community, Social and Personal Services",
                                     is.na(ciuur2)&ciuur4==15 ~ "O Central, Regional and Municipal Government",
                                     is.na(ciuur2)&ciuur4==16 ~ "P Education (private, public and municipalized)",
                                     is.na(ciuur2)&ciuur4==17 ~ "Q Health (private, public and municipalized)",
                                     is.na(ciuur2)&ciuur4==18 ~ "Q Other Community, Social and Personal Services",
                                     is.na(ciuur2)&ciuur4==19 ~ "Q Other Community, Social and Personal Services",
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
                            sector = sector,
                            tot_trabajadores = trabemp,
                            rango_empresa = rangoemp,
                            trab_comprometidos = tc,
                            tactica = tactica1,
                            number = num, 
                         autoridad = autoridad)

# Comprobar
names(proc_ohl)

# 2. Reducir la base al año 2016, 2017 y 2018 
proc_ohl$ano = as_numeric(proc_ohl$ano)
class(proc_ohl$ano)

proc_ohl$ano[proc_ohl$ano < 2016] <- NA
proc_ohl <- proc_ohl[!is.na(proc_ohl$ano),]
view(proc_ohl)

# ---- 3. Descriptivos por variable -----
freq(proc_ohl$organizacion)
freq(proc_ohl$legalidad)
freq(proc_ohl$sector)
freq(proc_ohl$tot_trabajadores) # Alta cantidad de NA's, evaluar imputar promedio 
freq(proc_ohl$rango_empresa)  # Alta cantidad de NA's, evaluar imputar promedio de tamaño empresa por rama
freq(proc_ohl$trab_comprometidos)
freq(proc_ohl$tactica)
freq(proc_ohl$autoridad)

sjmisc::descr(proc_ohl) # tot_trabajadores tiene un 49% de NA´s y rango_empresa un 46% de NA´s

# ---- 4. Re-Codification ----

proc_ohl$organizacion <- car::recode(proc_ohl$organizacion,"c(1,2,3,4,5,6,7,8) = 1; 0 = 0; 9 = 0; 10 = 0; 11 = 1; NA = NA", as.factor = T) # 1=Presencia 0=Ausencia
proc_ohl$legalidad <- car::recode(proc_ohl$legalidad,"1 = 0; 2 = 1", as.factor = T) #1=Legal 2=Extralegal
proc_ohl$tactica <- car::recode(proc_ohl$tactica,"c(1,2,3,4) = 1; c(5,6,7) = 0; 8 = 1; c(9,10) = 0; c(11,12) = 1; c(13,14) = 0;
                                15 = 1; c(16,17) = 0; c(18,19,20,21,22,23,24) = 2; c(25,26,27,28,29,30,31,32,33) = 3;
                                c(34,35) = 1; 36 = 0; c(37,38,39) = 2; 40 = 0; 41 = 2; 42 = 1; 43 = 2; 45 = 0; 46 = 2; 
                                c(47,48,49) = 1", as.factor = T) # 0=Públicas, 1=Convencionales y culturales, 2=Disruptivas y 3=Violentas (Fuente: OHL)
proc_ohl$ano <- as.factor(proc_ohl$ano)                                    

# Posterior al tratamiento de las variables con mucho NA y el joint con las variables exógenas, se pueden renombrar las categorías
# Recodificar, renombrar y etiquetar las variables a utilizar 
# Evaluar NA's 
# Descriptivos de variables de interés, tablas de contingencia y gráficos
# Añadir datos sobre sindicalización por sexo y PIB por rama para evaluar poder asociativo y poder estructural respectivamente
# Finiquitar base de datos procesada para el análisis 