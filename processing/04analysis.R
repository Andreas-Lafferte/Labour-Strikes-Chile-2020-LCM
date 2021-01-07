## 04: Analisis 

# ---- 1. Librerias -----
pacman::p_load(dplyr, car, summarytools, ggplot2, magrittr, tidyverse, sjmisc, sjlabelled, 
               stargazer, sjPlot, devtools, ggmosaic, texreg, kableExtra, webshot, readxl,
               openxlsx, psych, MASS, scatterplot3d, poLCA, reshape, writexl, readr)
options(scipen=999) # valores sin notación científica

# ---- 2. Datos ---- 
proc-ohl <- load("data/proc-ohl.RData")
proc-ohl-2016 <- load("data/proc-ohl-2016.RData")
proc-ohl-2017 <- load("data/proc-ohl-2017.RData")
proc-ohl-2018 <- load("data/proc-ohl-2018.RData")

# ---- 3. Analisis descriptivo ---- 
proc_ohl <- proc_ohl[-c(17,18,19,20,21,22,27,28,29,30,31,32)]
proc_ohl<- proc_ohl[-c(335,941),] 
proc_ohl$tactica <- car::recode(proc_ohl$tactica,"44 = 1", as.factor = T) 
proc_ohl_2016$tactica <- car::recode(proc_ohl_2016$tactica,"44 = 1", as.factor = T)
proc_ohl_2017$tactica <- car::recode(proc_ohl_2017$tactica,"44 = 1", as.factor = T)
proc_ohl_2018$tactica <- car::recode(proc_ohl_2018$tactica,"44 = 1", as.factor = T)# 0=Públicas, 1=Convencionales y culturales, 2=Disruptivas y 3=Violentas (Fuente: OHL)
proc_ohl_2016 <- proc_ohl_2016[-c(15,16,19,20)]
proc_ohl_2017 <- proc_ohl_2017[-c(15,16,19,20)]
proc_ohl_2018 <- proc_ohl_2018[-c(15,16,19,20)]

print(summarytools::dfSummary(proc_ohl),  method="viewer")
print(summarytools::dfSummary(proc_ohl_2016),  method="viewer")
print(summarytools::dfSummary(proc_ohl_2017),  method="viewer")
print(summarytools::dfSummary(proc_ohl_2018),  method="viewer")

# ---- 4. Analisis bivariado ---- 
tab_xtab(var.row = proc_ohl$rango_empresa_imp, proc_ohl$sector,
               show.cell.prc = T,show.summary = F, show.na = F)
table(proc_ohl$rango_empresa_imp, proc_ohl$sector, useNA = "always")

tab_xtab(var.row = proc_ohl$pibxtrab_acum, proc_ohl$rango_empresa_imp, 
         show.cell.prc = T,show.summary = F, show.na = F)

tab_xtab(var.row = proc_ohl$organizacion, proc_ohl$representatividad, 
         show.cell.prc = T,show.summary = F, show.na = T)

tab_xtab(var.row = proc_ohl$tactica, proc_ohl$trab_comprometidos, 
         show.cell.prc = T,show.summary = F, show.na = T)
table(proc_ohl$tactica, proc_ohl$trab_comprometidos, useNA = "always")

# ---- 5. Matriz ---- 
x1 <- proc_ohl_2016$organizacion
x2 <- proc_ohl_2016$rango_empresa_imp
x3 <- proc_ohl_2016$representatividad
x4 <- proc_ohl_2016$pibxtrab_2016
x5 <- proc_ohl_2016$tactica
x6 <- proc_ohl_2016$trab_comprometidos 
x7 <- proc_ohl_2016$tasa_sind_homb_2016
x8 <- proc_ohl_2016$tasa_sind_muj_2016

polychoric(data.frame(x1, x2, x3, x4, x5, x6, x7, x8), na.rm = T)

ohl_2016 <- data.frame(x2,x3,x4,x5,x6,x7,x8)

f <-cbind(x2, x3, x4, x5, x6, x7, x8)~1

# ---- 6. Modelos ----

# 6.1. Año 2016 ----

M1<-poLCA(formula = f, data = ohl_2016, nclass = 1, maxiter = 2000, nrep = 1, na.rm = TRUE)


M2<-poLCA(formula = f, data = seguridad, nclass = 2, maxiter = 2000, nrep = 1, na.rm = TRUE)
M3<-poLCA(formula = f, data = seguridad, nclass = 3, maxiter = 2000, nrep = 3, na.rm = TRUE) 
M4<-poLCA(formula = f, data = seguridad, nclass = 4, maxiter = 2000, nrep = 3, na.rm = TRUE) # nrep =1 falso máximo 
M5<-poLCA(formula = f, data = seguridad, nclass = 5, maxiter = 2000, nrep = 3, na.rm = TRUE) #ALERT: iterations finished, MAXIMUM LIKELIHOOD NOT FOUND, tengo que suber el maxiter y el nrep 
M5<-poLCA(formula = f, data = seguridad, nclass = 5, maxiter = 16000, nrep = 5, na.rm = TRUE)



# ---- 7. Ajuste ---- 

# ---- 8. Export ---- 