## 00: Script Preparación

# ---- 1. Librerias y datos ----
pacman::p_load(dplyr, car, summarytools, ggplot2, magrittr, tidyverse, lubridate, ggpubr, sjmisc,
               sjlabelled, stargazer, sjPlot, devtools)
#install_github("cran/MissMech", force = TRUE)
library(MissMech)
#install.packages("finalfit")
library(finalfit)
#install.packages("mice")
library(mice)
#install.packages("missForest")
library(missForest)
library(readxl)
library(writexl)
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
                                     ciuur2==14 ~ "Unknown or Other Activities",
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

proc_ohl$organizacion <- car::recode(proc_ohl$organizacion,"0 = 0; c(1,2,3) = 1; 4 = 2; 5 = 1; c(6,7) = 2; 8 = 1; 9 = 0; 10 = 0; 11 = 1; NA = NA", as.factor = T) # 1=Presencia 0=Ausencia
proc_ohl$legalidad <- car::recode(proc_ohl$legalidad,"1 = 0; 2 = 1", as.factor = T) #0=Legal 1=Extralegal
proc_ohl$tactica <- car::recode(proc_ohl$tactica,"c(1,2,3,4) = 1; c(5,6,7) = 0; 8 = 1; c(9,10) = 0; c(11,12) = 1; c(13,14) = 0;
                                15 = 1; c(16,17) = 0; c(18,19,20,21,22,23,24) = 2; c(25,26,27,28,29,30,31,32,33) = 3;
                                c(34,35) = 1; 36 = 0; c(37,38,39) = 2; 40 = 0; 41 = 2; 42 = 1; 43 = 2; 44 = 1; 45 = 0; 46 = 2; 
                                c(47,48,49) = 1", as.factor = T) # 0=Públicas, 1=Convencionales y culturales, 2=Disruptivas y 3=Violentas (Fuente: OHL)
proc_ohl$rango_empresa <- car::recode(proc_ohl$rango_empresa, "1 = NA; c(2,3,4) = 0; c(5,6,7) = 1; c(8,9) = 2; c(10,11,12,13) = 3", as.factor = T) # 0=Micro, 1=Pequena, 2=Mediana y 3=Gran
proc_ohl$ano <- as.factor(proc_ohl$ano)

# 4.1. Variable representatividad sindical ---- 
proc_ohl <- proc_ohl %>% mutate(tot_trabajadores=as.numeric(tot_trabajadores),
                                trab_comprometidos=as.numeric(trab_comprometidos),
                                representatividad=trab_comprometidos/tot_trabajadores) %>% as.data.frame()
descr(proc_ohl$representatividad)
summary(proc_ohl$representatividad)
proc_ohl <- proc_ohl[-c(9)]
freq(proc_ohl$representatividad)

proc_ohl$representatividad <- car::recode(proc_ohl$representatividad, "0.0007421260:0.3000000 = 1; 0.3008130:0.5000000 = 2; 0.5008333:0.991189427312775 = 3; 1.0000000:15.8000000 = 4", as.factor = T)
proc_ohl$representatividad <- car::recode(proc_ohl$representatividad, "0.991189427312775 = 3", as.factor = T) # 1= Baja representacion, 2= Mediana representacion, 3= Alta representacion y 4= Sobre representacion

# 4.2. Variable trabajadores comprometidos ----
proc_ohl$trab_comprometidos <- as.numeric(proc_ohl$trab_comprometidos)
proc_ohl$trab_comprometidos <- car::recode(proc_ohl$trab_comprometidos, "1:100 = 1; 101:750 = 2; 755:9673 = 3; 10000:500000 = 4", as.factor = T) #1 = Baja cantidad TC, 2 = Mediana Cantidad TC, 3 = Alta Cantidad TC y 4 = Muy Alta Cantidad TC
summary(proc_ohl$trab_comprometidos)

  
# ---- 5. Tratamiento missing values ----
descr(proc_ohl$rango_empresa)
descr(proc_ohl$tot_trabajadores)
summary(proc_ohl$tot_trabajadores)
summary(proc_ohl$rango_empresa)

# Tablas de contingencia
# Rango empresa
ct <- tab_xtab(var.row = proc_ohl$rango_empresa, proc_ohl$sector,
         show.cell.prc = T,show.summary = F, show.na = T)
t1 <- table(proc_ohl$rango_empresa, proc_ohl$sector, useNA = "always")

chisq.test(t1)

# Detección de NA's en rango_empresa
proc_ohl[is.na(proc_ohl$rango_empresa),]
sum(is.na(proc_ohl$rango_empresa))
round(sum(is.na(proc_ohl$rango_empresa))/nrow(proc_ohl)*100,2)

# 5.1. Test para patrones de missing ----- 
df_select <- proc_ohl %>% select(tot_trabajadores,
                                 rango_empresa,
                                 trab_comprometidos) %>% as.data.frame()
df_select <- as_numeric(df_select)
res <- TestMCARNormality(data=df_select)
print(res)

df_select2 <- proc_ohl%>%select(tot_trabajadores,
                                rango_empresa,
                                trab_comprometidos,
                                sector, representatividad) %>% as.data.frame()

df_select2$rango_empresa <- as.factor(df_select2$rango_empresa)
df_select2$rango_empresa_MAR <- df_select2$rango_empresa
df_select2$tot_trabajadores_MAR <- df_select2$tot_trabajadores
df_select2$representatividad_MAR <- df_select2$representatividad

explanatory = c("tot_trabajadores", "trab_comprometidos", "sector", "representatividad")
dependent = "rango_empresa_MAR"
df_select2 %>% 
  missing_compare(dependent, explanatory) %>% 
  knitr::kable(row.names=FALSE, align = c("l", "l", "r", "r", "r"), 
               caption = "Mean comparisons between values of responders (Not missing) and 
        non-responders (Missing) on the Rango Empresa variable.")


explanatory = c("tot_trabajadores", "trab_comprometidos", "sector", "rango_empresa")
dependent = "representatividad_MAR"
df_select2 %>% 
  missing_compare(dependent, explanatory) %>% 
  knitr::kable(row.names=FALSE, align = c("l", "l", "r", "r", "r"), 
               caption = "Mean comparisons between values of responders (Not missing) and 
        non-responders (Missing) on the Representatividad Sindical variable.")

# 5.2. Imputation rango empresa ----
proc_ohl_mis <- proc_ohl
# Imputation con mice
md.pattern(proc_ohl, rotate.names = T)
imputed_Data <- mice(proc_ohl, maxit = 5, method = 'pmm')
complete_Data <- complete(imputed_Data)
proc_ohl$rango_empresa_imp <- complete_Data$rango_empresa
sum(is.na(proc_ohl$rango_empresa_imp))

# Imputation con missForest
sapply(proc_ohl_mis, class)
proc_ohl_mis$sector <- as.factor(proc_ohl_mis$sector)
proc_ohl_mis$autoridad <- as.factor(proc_ohl_mis$autoridad)
imp <- missForest(proc_ohl_mis, verbose = T, variablewise = F)
imp$OOBerror

dflimpio <- as.data.frame(imp$ximp)
proc_ohl$rango_empresa_forest <- dflimpio$rango_empresa

# ---- 6. Union de bases ----
pib_x_trab <- read_excel("output/pib_x_trab.xlsx")
tasa_sindi_sex <- read_excel("output/tasa_sindi_sex.xlsx")

pib_x_trab <- pib_x_trab[-c(13),]
tasa_sindi_sex <- tasa_sindi_sex[-c(13,27),]

pib_x_trab <- pib_x_trab %>% mutate(pibxtrab_acum=pibxtrab_2016+pibxtrab_2017+pibxtrab_2018)%>% as.data.frame()
tasa_sindi_sex <- tasa_sindi_sex %>% mutate(tasa_sind_acum=(tasa_sind_2016+tasa_sind_2017+tasa_sind_2018)/3)%>%as.data.frame()

# pibxtrabajador
names(pib_x_trab) <- c("sector", "pibxtrab_2016", "pibxtrab_2017", "pibxtrab_2018", "pibxtrab_acum")
proc_ohl <- merge(proc_ohl, pib_x_trab, by="sector",all.x = TRUE)

# tasa sindicalización por sexo 
tasa_sindi_sex_homb <- tasa_sindi_sex[-c(14,15,16,17,18,19,20,21,22,23,24,25,26),]
tasa_sindi_sex_muj <- tasa_sindi_sex[-c(1,2,3,4,5,6,7,8,9,10,11,12,13),]

tasa_sindi_sex_homb <- tasa_sindi_sex_homb[-c(8)]
tasa_sindi_sex_muj <- tasa_sindi_sex_muj[-c(8)]

names(tasa_sindi_sex_homb) <- c("sector", "ocup_homb_2016", "ocup_homb_2017", "ocup_homb_2018", "afi_homb_2016", "afi_homb_2017", "afi_homb_2018", "tasa_sind_homb_2016", "tasa_sind_homb_2017", "tasa_sind_homb_2018", "tasa_sind_homb_acum")
names(tasa_sindi_sex_muj) <- c("sector", "ocup_muj_2016", "ocup_muj_2017", "ocup_muj_2018", "afi_muj_2016", "afi_muj_2017", "afi_muj_2018", "tasa_sind_muj_2016", "tasa_sind_muj_2017", "tasa_sind_muj_2018", "tasa_sind_muj_acum")

tasalimpia <- merge(tasa_sindi_sex_homb, tasa_sindi_sex_muj, by="sector",all.x = TRUE)
proc_ohl <- merge(proc_ohl, tasalimpia, by="sector",all.x = TRUE)

# recodificar 

# pibxtrab
proc_ohl$pibxtrab_2016 <- car::recode(proc_ohl$pibxtrab_2016, "0:13440452.7944397 = 1; 20644094.3464882:43597708.7216876 = 2; 67026090.5882947:169175403.406413 = 3", as.factor = T) # 1= Bajo PESTRC, 2= Mediano PESTRC y 3= Alto PESTRC 
proc_ohl$pibxtrab_2016 <- car::recode(proc_ohl$pibxtrab_2016, "13440452.7944397 = 1; 67026090.5882947 = 3; 169175403.406413= 3", as.factor = T)
proc_ohl$pibxtrab_2017 <- car::recode(proc_ohl$pibxtrab_2017, "0:15201465.8050697 = 1; 16278989.718967:29663846.3977053 = 2; 84403495.9479072:154368748.64943 = 3", as.factor = T)
proc_ohl$pibxtrab_2017 <- car::recode(proc_ohl$pibxtrab_2017, "15201465.8050697= 1; 84403495.9479072 = 3; 154368748.64943 = 3", as.factor = T)
proc_ohl$pibxtrab_2018 <- car::recode(proc_ohl$pibxtrab_2018, "0:17458587.6262695 = 1; 18576867.3139131:44137961.0073318 = 2; 85502427.9299947:171735781.742852 = 3", as.factor = T)
proc_ohl$pibxtrab_2018 <- car::recode(proc_ohl$pibxtrab_2018, "17458587.6262695 = 1; 85502427.9299947 = 3", as.factor = T)
proc_ohl$pibxtrab_acum <- car::recode(proc_ohl$pibxtrab_acum, "0:47475758.8828155 = 1; 57937039.3587867:112901007.846686 = 2; 236932014.466197:481083838.793922 = 3", as.factor = T)
proc_ohl$pibxtrab_acum <- car::recode(proc_ohl$pibxtrab_acum, "236932014.466197 = 3", as.factor = T)

# tasa sindicalización por sexo y rama
proc_ohl$tasa_sind_homb_2016 <- car::recode(proc_ohl$tasa_sind_homb_2016, "0.0124899109038837:0.0999779940248183 = 1; 0.107992515253575:0.272922840286331 = 2; 0.516101647197425 = 3; 0.587287926566963 = 3", as.factor = T)
proc_ohl$tasa_sind_homb_2016 <- car::recode(proc_ohl$tasa_sind_homb_2016, "0.107992515253575 = 2; 0.516101647197425 = 3", as.factor = T)
proc_ohl$tasa_sind_homb_2017 <- car::recode(proc_ohl$tasa_sind_homb_2017, "0.0108916254790002:0.0852783326754007 = 1; 0.102501256287428:0.267804927627696 = 2; 0.4303622254582 = 3; 0.69744647068036 = 3", as.factor = T)
proc_ohl$tasa_sind_homb_2017 <- car::recode(proc_ohl$tasa_sind_homb_2017, "0.0108916254790002 = 1; 0.267804927627696 = 2; 0.4303622254582 = 3; 0.69744647068036 = 3", as.factor = T)
proc_ohl$tasa_sind_homb_2018 <- car::recode(proc_ohl$tasa_sind_homb_2018, "0.0150868311615734:0.0947598835064363 = 1; 0.102051544407887:0.204253650571982 = 2; 0.274932362456316:1.83463720967702 = 3", as.factor = T)
proc_ohl$tasa_sind_homb_2018 <- car::recode(proc_ohl$tasa_sind_homb_2018, "0.204253650571982 = 2", as.factor = T)
proc_ohl$tasa_sind_homb_acum <- car::recode(proc_ohl$tasa_sind_homb_acum, "0.0128227891814858:0.0990797112728943 = 1; 0.10587887440465:0.271886710123448 = 2; 0.383572507742536:1.03979053564145 = 3", as.factor = T)
proc_ohl$tasa_sind_homb_acum <- car::recode(proc_ohl$tasa_sind_homb_acum, "0.0128227891814858 = 1", as.factor = T)

proc_ohl$tasa_sind_muj_2016 <- car::recode(proc_ohl$tasa_sind_muj_2016, "0.0304968846577426:0.0977039589677833 = 1; 0.100030491071061:0.223664443056835 = 2; 0.462106336538543: 0.518033712064499 = 3", as.factor = T)
proc_ohl$tasa_sind_muj_2016 <- car::recode(proc_ohl$tasa_sind_muj_2016, "0.100030491071061 = 2; 0.462106336538543 = 3", as.factor = T)
proc_ohl$tasa_sind_muj_2017 <- car::recode(proc_ohl$tasa_sind_muj_2017, "0.0256239057538799:0.0968657377724932 = 1; 0.10620882324068:0.237191424910815 = 2; 0.455204352949961:0.568257888751671 = 3", as.factor = T)
proc_ohl$tasa_sind_muj_2017 <- car::recode(proc_ohl$tasa_sind_muj_2017, "0.10620882324068 = 2; 0.237191424910815 = 2; 0.568257888751671 = 3", as.factor = T)
proc_ohl$tasa_sind_muj_2018 <- car::recode(proc_ohl$tasa_sind_muj_2018, "0.0324127369752785:0.0928376130580618 = 1; 0.102268752096189:0.255069256592619 = 2; 0.260302322750259:0.403911069279496 = 3", as.factor = T)
proc_ohl$tasa_sind_muj_2018 <- car::recode(proc_ohl$tasa_sind_muj_2018, "0.0324127369752785 = 1; 0.0928376130580618 = 1; 0.102268752096189 = 2; 0.255069256592619 = 2; 0.260302322750259 = 3", as.factor = T)
proc_ohl$tasa_sind_muj_acum <- car::recode(proc_ohl$tasa_sind_muj_acum, "0.0295111757956337:0.0987883278725658 = 1; 0.100979926113027:0.218146854917988 = 2; 0.390793315360374:0.421244940450385 = 3", as.factor = T)
proc_ohl$tasa_sind_muj_acum <- car::recode(proc_ohl$tasa_sind_muj_acum, "0.0295111757956337 = 1; 0.218146854917988 = 2; 0.421244940450385 = 3", as.factor = T)

proc_ohl <- proc_ohl[-c(17,18,19,20,21,22,27,28,29,30,31,32)]
proc_ohl<- proc_ohl[-c(335,941),]

# ---- 7. Data por año y agregado ---- 
proc_ohl_2016 <- proc_ohl %>% filter(ano==2016)%>%as.data.frame()
proc_ohl_2017 <- proc_ohl %>% filter(ano==2017)%>%as.data.frame()
proc_ohl_2018 <- proc_ohl %>% filter(ano==2018)%>%as.data.frame()
 
proc_ohl_2016 <- proc_ohl_2016[-c(14,15,18,19,22,23)]
proc_ohl_2017 <- proc_ohl_2017[-c(13,15,17,19,21,23)]
proc_ohl_2018 <- proc_ohl_2018[-c(13,14,17,18,21,22)]

sapply(proc_ohl, class)
proc_ohl$sector <- as.factor(proc_ohl$sector)
proc_ohl$autoridad <- as.factor(proc_ohl$autoridad)

sapply(proc_ohl_2016, class)
proc_ohl_2016$sector <- as.factor(proc_ohl_2016$sector)
proc_ohl_2016$autoridad <- as.factor(proc_ohl_2016$autoridad)

sapply(proc_ohl_2017, class)
proc_ohl_2017$sector <- as.factor(proc_ohl_2017$sector)
proc_ohl_2017$autoridad <- as.factor(proc_ohl_2017$autoridad)

sapply(proc_ohl_2018, class)
proc_ohl_2018$sector <- as.factor(proc_ohl_2018$sector)
proc_ohl_2018$autoridad <- as.factor(proc_ohl_2018$autoridad)

# ---- 8. Export ---- 
save(proc_ohl, file= "data/proc-ohl.RData")
save(proc_ohl_2016, file= "data/proc-ohl-2016.RData")
save(proc_ohl_2017, file= "data/proc-ohl-2017.RData")
save(proc_ohl_2018, file= "data/proc-ohl-2018.RData")