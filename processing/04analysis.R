## 04: Analisis 

# ---- 1. Librerias -----
pacman::p_load(dplyr, car, summarytools, ggplot2, magrittr, tidyverse, sjmisc, sjlabelled, 
               stargazer, sjPlot, devtools, ggmosaic, texreg, kableExtra, webshot, readxl,
               openxlsx, psych, MASS, scatterplot3d, poLCA, reshape, writexl, readr, corrplot)
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

# 5.1. Matriz año 2016
x1 <- proc_ohl_2016$organizacion
x2 <- proc_ohl_2016$representatividad # evaluar por NA's
x3 <- proc_ohl_2016$rango_empresa_imp
x4 <- proc_ohl_2016$pibxtrab_2016
x5 <- proc_ohl_2016$tactica
x6 <- proc_ohl_2016$trab_comprometidos 
x7 <- proc_ohl_2016$tasa_sind_homb_2016
x8 <- proc_ohl_2016$tasa_sind_muj_2016

polychoric(data.frame(x1, x2, x3, x4, x5, x6, x7, x8), na.rm = T)

cor01<-polychoric(data.frame(x1, x2, x3, x4, x5, x6, x7, x8), na.rm = T)
rownames(cor01$rho) <- c("A. organización", "B. representatividad", "C. rango.empresa", "D. pibxtrab", "E. tactica", "F. trab.comprometidos", "G. sindi.h", "H. sindi.m")
colnames(cor01$rho) <- c(" (A)", "(B)", "(C)", "(D)", "(E)", "(F)", "(G)", "(H)")
tab_corr(cor01$rho,
         triangle = "lower")

ohl_2016 <- data.frame(x1,x2,x3,x4,x5,x6,x7,x8)

f <-cbind(x1, x2, x3, x4, x5, x6, x7, x8)~1

# 5.2. Matriz año 2017 
y1 <- proc_ohl_2017$organizacion
y2 <- proc_ohl_2017$representatividad
y3 <- proc_ohl_2017$rango_empresa_imp
y4 <- proc_ohl_2017$pibxtrab_2017
y5 <- proc_ohl_2017$tactica
y6 <- proc_ohl_2017$trab_comprometidos 
y7 <- proc_ohl_2017$tasa_sind_homb_2017
y8 <- proc_ohl_2017$tasa_sind_muj_2017

polychoric(data.frame(y1, y2, y3, y4, y5, y6, y7, y8), na.rm = T)

cor02<-polychoric(data.frame(y1, y2, y3, y4, y5, y6, y7, y8), na.rm = T)
rownames(cor02$rho) <- c("A. organización", "B. representatividad", "C. rango.empresa", "D. pibxtrab", "E. tactica", "F. trab.comprometidos", "G. sindi.h", "H. sindi.m")
colnames(cor02$rho) <- c(" (A)", "(B)", "(C)", "(D)", "(E)", "(F)", "(G)", "(H)")
tab_corr(cor02$rho,
         triangle = "lower")

ohl_2017 <- data.frame(y1, y2, y3, y4, y5, y6, y7, y8)

t <-cbind(y1, y2, y3, y4, y5, y6, y7, y8)~1

# 5.3. Matriz año 2018 
z1 <- proc_ohl_2018$organizacion
z2 <- proc_ohl_2018$representatividad
z3 <- proc_ohl_2018$rango_empresa_imp
z4 <- proc_ohl_2018$pibxtrab_2018
z5 <- proc_ohl_2018$tactica
z6 <- proc_ohl_2018$trab_comprometidos 
z7 <- proc_ohl_2018$tasa_sind_homb_2018
z8 <- proc_ohl_2018$tasa_sind_muj_2018

polychoric(data.frame(z1, z2, z3, z4, z5, z6, z7, z8), na.rm = T)

cor03<-polychoric(data.frame(z1, z2, z3, z4, z5, z6, z7, z8), na.rm = T)
rownames(cor03$rho) <- c("A. organización", "B. representatividad", "C. rango.empresa", "D. pibxtrab", "E. tactica", "F. trab.comprometidos", "G. sindi.h", "H. sindi.m")
colnames(cor03$rho) <- c(" (A)", "(B)", "(C)", "(D)", "(E)", "(F)", "(G)", "(H)")
tab_corr(cor03$rho,
         triangle = "lower")

ohl_2018 <- data.frame(z1, z2, z3, z4, z5, z6, z7, z8)

r <-cbind(z1, z2, z3, z4, z5, z6, z7, z8)~1

# 5.4. Matriz acumulada 
a1 <- proc_ohl$organizacion
a2 <- proc_ohl$representatividad
a3 <- proc_ohl$rango_empresa_imp
a4 <- proc_ohl$pibxtrab_acum
a5 <- proc_ohl$tactica
a6 <- proc_ohl$trab_comprometidos 
a7 <- proc_ohl$tasa_sind_homb_acum
a8 <- proc_ohl$tasa_sind_muj_acum

polychoric(data.frame(a1, a2, a3, a4, a5, a6, a7, a8), na.rm = T)

cor04<-polychoric(data.frame(a1, a2, a3, a4, a5, a6, a7, a8), na.rm = T)
rownames(cor04$rho) <- c("A. organización", "B. representatividad", "C. rango.empresa", "D. pibxtrab", "E. tactica", "F. trab.comprometidos", "G. sindi.h", "H. sindi.m")
colnames(cor04$rho) <- c(" (A)", "(B)", "(C)", "(D)", "(E)", "(F)", "(G)", "(H)")
tab_corr(cor04$rho,
         triangle = "lower")

ohl_acum <- data.frame(a1, a2, a3, a4, a5, a6, a7, a8)

k <-cbind(a1, a2, a3, a4, a5, a6, a7, a8)~1

# ---- 6. Modelos ----

# 6.1. Año 2016 ----
M1<-poLCA(formula = f, data = ohl_2016, nclass = 1, maxiter = 2000, nrep = 1, na.rm = F)
M2<-poLCA(formula = f, data = ohl_2016, nclass = 2, maxiter = 2000, nrep = 1, na.rm = F)
M3<-poLCA(formula = f, data = ohl_2016, nclass = 3, maxiter = 2000, nrep = 1, na.rm = F)
M4<-poLCA(formula = f, data = ohl_2016, nclass = 3, maxiter = 2000, nrep = 2, na.rm = F)
M5<-poLCA(formula = f, data = ohl_2016, nclass = 3, maxiter = 2000, nrep = 3, na.rm = F)# nrep = 1 falso máximo
M6<-poLCA(formula = f, data = ohl_2016, nclass = 4, maxiter = 2000, nrep = 1, na.rm = F)
M7<-poLCA(formula = f, data = ohl_2016, nclass = 4, maxiter = 2000, nrep = 5, na.rm = F)
M8<-poLCA(formula = f, data = ohl_2016, nclass = 5, maxiter = 2000, nrep = 3, na.rm = F)
M9<-poLCA(formula = f, data = ohl_2016, nclass = 6, maxiter = 2000, nrep = 3, na.rm = F)

# M5 el mejor modelo con 3 clases latentes y ajuste alto/ M8 mejor modelo con 5 clases latentes y ajuste bajo


# 6.2. Año 2017


# 6.3. Año 2018 


# 6.4. Acumulado 


# ---- 7. Ajuste ---- 

# 7.1. Ajuste año 2016 
M1$predcell
M5$predcell
M8$predcell

p1<-1-pchisq(M1$Chisq, M1$resid.df)
p2<-1-pchisq(M2$Chisq, M2$resid.df)
p3<-1-pchisq(M5$Chisq, M5$resid.df)
p4<-1-pchisq(M7$Chisq, M7$resid.df)
p5<-1-pchisq(M8$Chisq, M8$resid.df)

AjusteM_2016<-data.frame(c("M1", "M2", "M5", "M7", "M8"),
                         c(M1$llik, M2$llik, M5$llik, M7$llik, M8$llik),
                         c(M1$Chisq, M2$Chisq,M5$Chisq, M7$Chisq, M8$Chisq),
                         c(M1$Gsq, M2$Gsq, M5$Gsq, M7$Gsq, M8$Gsq),
                         c(M1$npar, M2$npar, M5$npar, M7$npar, M8$npar),
                         c(M1$aic, M2$aic, M5$aic, M7$aic, M8$aic),
                         c(M1$bic, M2$bic, M5$bic, M7$bic, M8$bic),
                         c(p1, p2, p3, p4, p5))
colnames(AjusteM_2016)<-c("Modelo", "Loglike", "X2", "G2", "DF", "AIC", "BIC", "P-value")

View(AjusteM_2016)

# Gráfico año 2016 

plotdatos <- melt(M8$probs) # función de reshape que me permite 'dar vuelta' una tabla
plotdatos2 <- plotdatos[plotdatos$X2=="1",]
# el probs son las probabilidades condicionales 

ggplot(plotdatos2, aes(x=L1, y = value, group = X1, colour = X1)) +
  geom_point() + geom_line() + theme_classic() + 
  labs(title = "Gráfico 1. Predicciones de clase año 2016.",
       x="Variables",
       y = "Valores",
       caption = "Fuente: Elaboración propia con base en datos de huelgas laborales OHL-COES (1979-2018)") + 
  theme(legend.position = "bottom",plot.title = element_text(size=12),
        plot.subtitle = element_text(size=10),
        plot.caption = element_text(size=8)) 


# ---- 8. Export ----