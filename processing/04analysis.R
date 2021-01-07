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

ohl_2016 <- data.frame(x1,x2,x3,x4,x5,x6,x7,x8)

f <-cbind(x1, x2, x3, x4, x5, x6, x7, x8)~1

ncol(ohl_2016)

# ---- 6. Modelos ----

# 6.1. Año 2016 ----

M1<-poLCA(formula = f, data = ohl_2016, nclass = 1, maxiter = 2000, nrep = 1, na.rm = F)
M2<-poLCA(formula = f, data = ohl_2016, nclass = 2, maxiter = 2000, nrep = 1, na.rm = F)
M3<-poLCA(formula = f, data = ohl_2016, nclass = 3, maxiter = 2000, nrep = 1, na.rm = F)
M4<-poLCA(formula = f, data = ohl_2016, nclass = 4, maxiter = 2000, nrep = 1, na.rm = F)
 
#M4<-poLCA(formula = f, data = seguridad, nclass = 4, maxiter = 2000, nrep = 3, na.rm = TRUE) # nrep =1 falso máximo 
#M5<-poLCA(formula = f, data = seguridad, nclass = 5, maxiter = 2000, nrep = 3, na.rm = TRUE) #ALERT: iterations finished, MAXIMUM LIKELIHOOD NOT FOUND, tengo que suber el maxiter y el nrep 
#M5<-poLCA(formula = f, data = seguridad, nclass = 5, maxiter = 16000, nrep = 5, na.rm = TRUE)

# 6.2. Año 2017

y1 <- proc_ohl_2017$organizacion
y2 <- proc_ohl_2017$rango_empresa_imp
y3 <- proc_ohl_2017$representatividad
y4 <- proc_ohl_2017$pibxtrab_2017
y5 <- proc_ohl_2017$tactica
y6 <- proc_ohl_2017$trab_comprometidos 
y7 <- proc_ohl_2017$tasa_sind_homb_2017
y8 <- proc_ohl_2017$tasa_sind_muj_2017

polychoric(data.frame(y1, y2, y3, y4, y5, y6, y7, y8), na.rm = T)

ohl_2017 <- data.frame(y1, y2, y3, y4, y5, y6, y7, y8)

t <-cbind(y1, y2, y3, y4, y5, y6, y7, y8)~1

M1<-poLCA(formula = t, data = ohl_2017, nclass = 1, maxiter = 2000, nrep = 1, na.rm = F)
M2<-poLCA(formula = t, data = ohl_2017, nclass = 2, maxiter = 2000, nrep = 1, na.rm = F)
M3<-poLCA(formula = t, data = ohl_2017, nclass = 3, maxiter = 2000, nrep = 1, na.rm = F)
M4<-poLCA(formula = t, data = ohl_2017, nclass = 4, maxiter = 2000, nrep = 1, na.rm = F)
M5<-poLCA(formula = t, data = ohl_2017, nclass = 4, maxiter = 2000, nrep = 5, na.rm = F)

# 6.3. Año 2018 
z1 <- proc_ohl_2018$organizacion
z2 <- proc_ohl_2018$rango_empresa_imp
z3 <- proc_ohl_2018$representatividad
z4 <- proc_ohl_2018$pibxtrab_2018
z5 <- proc_ohl_2018$tactica
z6 <- proc_ohl_2018$trab_comprometidos 
z7 <- proc_ohl_2018$tasa_sind_homb_2018
z8 <- proc_ohl_2018$tasa_sind_muj_2018

polychoric(data.frame(z1, z2, z3, z4, z5, z6, z7, z8), na.rm = T)

ohl_2018 <- data.frame(z1, z2, z3, z4, z5, z6, z7, z8)

r <-cbind(z1, z2, z3, z4, z5, z6, z7, z8)~1


M1<-poLCA(formula = r, data = ohl_2018, nclass = 1, maxiter = 2000, nrep = 1, na.rm = F)
M2<-poLCA(formula = r, data = ohl_2018, nclass = 2, maxiter = 2000, nrep = 1, na.rm = F)
M3<-poLCA(formula = r, data = ohl_2018, nclass = 3, maxiter = 2000, nrep = 1, na.rm = F)
M4<-poLCA(formula = r, data = ohl_2018, nclass = 4, maxiter = 2000, nrep = 1, na.rm = F) # M
M5<-poLCA(formula = r, data = ohl_2018, nclass = 4, maxiter = 2000, nrep = 5, na.rm = F)# 
M6<-poLCA(formula = r, data = ohl_2018, nclass = 5, maxiter = 2000, nrep = 5, na.rm = F)
M6<-poLCA(formula = r, data = ohl_2018, nclass = 3, maxiter = 16000, nrep = 5, na.rm = F)

# 6.4. Acumulado 
a1 <- proc_ohl$organizacion
a2 <- proc_ohl$rango_empresa_imp
a3 <- proc_ohl$representatividad
a4 <- proc_ohl$pibxtrab_acum
a5 <- proc_ohl$tactica
a6 <- proc_ohl$trab_comprometidos 
a7 <- proc_ohl$tasa_sind_homb_acum
a8 <- proc_ohl$tasa_sind_muj_acum

polychoric(data.frame(a1, a2, a3, a4, a5, a6, a7, a8), na.rm = T)

ohl_acum <- data.frame(a1, a2, a3, a4, a5, a6, a7, a8)

k <-cbind(a1, a2, a3, a4, a5, a6, a7, a8)~1

M1<-poLCA(formula = k, data = ohl_acum, nclass = 1, maxiter = 2000, nrep = 1, na.rm = F)
M2<-poLCA(formula = k, data = ohl_acum, nclass = 2, maxiter = 2000, nrep = 1, na.rm = F)
M3<-poLCA(formula = k, data = ohl_acum, nclass = 3, maxiter = 2000, nrep = 1, na.rm = F) # M Parsimonia
M4<-poLCA(formula = k, data = ohl_acum, nclass = 4, maxiter = 2000, nrep = 1, na.rm = F) 
M5<-poLCA(formula = k, data = ohl_acum, nclass = 4, maxiter = 2000, nrep = 5, na.rm = F) # M 
M6<-poLCA(formula = k, data = ohl_acum, nclass = 5, maxiter = 2000, nrep = 5, na.rm = F)


# ---- 7. Ajuste ---- 
M3$predcell
M3$predclass


p1<-1-pchisq(M1$Chisq, M1$resid.df)
p2<-1-pchisq(M2$Chisq, M2$resid.df)
p3<-1-pchisq(M3$Chisq, M3$resid.df)
p4<-1-pchisq(M4$Chisq, M4$resid.df)
p5<-1-pchisq(M5$Chisq, M5$resid.df)

AjusteM1<-data.frame(c("M1", "M2", "M3", "M4", "M5"),
                     c(M1$llik, M2$llik, M3$llik, M4$llik, M5$llik),
                     c(M1$Chisq, M2$Chisq,M3$Chisq, M4$Chisq, M5$Chisq),
                     c(M1$Gsq, M2$Gsq, M3$Gsq, M4$Gsq, M5$Gsq),
                     c(M1$npar, M2$npar, M3$npar, M4$npar, M5$npar),
                     c(M1$aic, M2$aic, M3$aic, M4$aic, M5$aic),
                     c(M1$bic, M2$bic, M3$bic, M4$bic, M5$bic),
                     c(p1, p2, p3, p4, p5))
colnames(AjusteM1)<-c("Modelo", "Loglike", "X2", "G2", "DF", "AIC", "BIC", "P-value")

View(AjusteM1)
# ---- 8. Export ----