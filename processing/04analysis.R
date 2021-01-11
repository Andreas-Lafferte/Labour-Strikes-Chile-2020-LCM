## 04: Analisis 

# ---- 1. Librerias -----
pacman::p_load(dplyr, car, summarytools, ggplot2, magrittr, tidyverse, sjmisc, sjlabelled, 
               stargazer, sjPlot, devtools, ggmosaic, texreg, kableExtra, webshot, readxl,
               openxlsx, psych, MASS, scatterplot3d, poLCA, reshape, writexl, readr, corrplot, webshot)
options(scipen=999) # valores sin notación científica

# ---- 2. Datos ---- 
proc-ohl <- load("data/proc-ohl.RData")
proc-ohl-2016 <- load("data/proc-ohl-2016.RData")
proc-ohl-2017 <- load("data/proc-ohl-2017.RData")
proc-ohl-2018 <- load("data/proc-ohl-2018.RData")

# ---- 3. Analisis descriptivo ---- 
proc_ohl <- proc_ohl[-c(3,6)]
proc_ohl_2016 <- proc_ohl_2016[-c(3,6)]
proc_ohl_2017 <- proc_ohl_2017[-c(3,6)]
proc_ohl_2018 <- proc_ohl_2018[-c(3,6)]

print(summarytools::dfSummary(proc_ohl),  method="viewer")
print(summarytools::dfSummary(proc_ohl_2016),  method="viewer")
print(summarytools::dfSummary(proc_ohl_2017),  method="viewer")
print(summarytools::dfSummary(proc_ohl_2018),  method="viewer")

webshot(url = "http://localhost:22383/session/file522853c2722.html", file = "output/tables/summary-acum.png", vwidth = 992,
        vheight = 744, cliprect = NULL, selector = NULL, expand = NULL,
        delay = 0.2, zoom = 1, eval = NULL, debug = FALSE,
        useragent = NULL)

# ---- 4. Analisis bivariado ---- 
tab_xtab(var.row = proc_ohl$rango_empresa_imp, proc_ohl$sector,
               show.cell.prc = T,show.summary = F, show.na = F)

webshot(url = "http://localhost:22383/session/file522811b3136.html", file = "output/tables/rangoempresa-sector.png", vwidth = 992,
        vheight = 744, cliprect = NULL, selector = NULL, expand = NULL,
        delay = 0.2, zoom = 1, eval = NULL, debug = FALSE,
        useragent = NULL)

tab_xtab(var.row = proc_ohl$pibxtrab_acum, proc_ohl$sector,
         show.cell.prc = T,show.summary = F, show.na = F)

webshot(url = "http://localhost:22383/session/file522851b32602.html", file = "output/tables/pibxtrab-sector.png", vwidth = 992,
        vheight = 744, cliprect = NULL, selector = NULL, expand = NULL,
        delay = 0.2, zoom = 1, eval = NULL, debug = FALSE,
        useragent = NULL)

tab_xtab(var.row = proc_ohl$organizacion, proc_ohl$sector,
         show.cell.prc = T,show.summary = F, show.na = F)

webshot(url = "http://localhost:22383/session/file522827aa26f3.html", file = "output/tables/org-sector.png", vwidth = 992,
        vheight = 744, cliprect = NULL, selector = NULL, expand = NULL,
        delay = 0.2, zoom = 1, eval = NULL, debug = FALSE,
        useragent = NULL)

tab_xtab(var.row = proc_ohl$representatividad, proc_ohl$sector,
         show.cell.prc = T,show.summary = F, show.na = F)

webshot(url = "http://localhost:22383/session/file5228555147d5.html", file = "output/tables/represent-sector.png", vwidth = 992,
        vheight = 744, cliprect = NULL, selector = NULL, expand = NULL,
        delay = 0.2, zoom = 1, eval = NULL, debug = FALSE,
        useragent = NULL)

tab_xtab(var.row = proc_ohl$tactica, proc_ohl$sector,
         show.cell.prc = T,show.summary = F, show.na = F)

webshot(url = "http://localhost:22383/session/file52284ea42411.html", file = "output/tables/tactica-sector.png", vwidth = 992,
        vheight = 744, cliprect = NULL, selector = NULL, expand = NULL,
        delay = 0.2, zoom = 1, eval = NULL, debug = FALSE,
        useragent = NULL)

tab_xtab(var.row = proc_ohl$pibxtrab_acum, proc_ohl$rango_empresa_imp, 
         show.cell.prc = T,show.summary = F, show.na = F) #poder estructural

webshot(url = "http://localhost:22383/session/file5228ef45522.html", file = "output/tables/structural-power.png", vwidth = 992,
        vheight = 744, cliprect = NULL, selector = NULL, expand = NULL,
        delay = 0.2, zoom = 1, eval = NULL, debug = FALSE,
        useragent = NULL)

tab_xtab(var.row = proc_ohl$organizacion, proc_ohl$representatividad, 
         show.cell.prc = T,show.summary = F, show.na = T) #poder asociativo

webshot(url = "http://localhost:22383/session/file52282fbc19ce.html", file = "output/tables/associative-power.png", vwidth = 992,
        vheight = 744, cliprect = NULL, selector = NULL, expand = NULL,
        delay = 0.2, zoom = 1, eval = NULL, debug = FALSE,
        useragent = NULL)

tab_xtab(var.row = proc_ohl$tactica, proc_ohl$trab_comprometidos, 
         show.cell.prc = T,show.summary = F, show.na = T) #poder de movilización

webshot(url = "http://localhost:22383/session/file5228171e6c2c.html", file = "output/tables/movilization-power.png", vwidth = 992,
        vheight = 744, cliprect = NULL, selector = NULL, expand = NULL,
        delay = 0.2, zoom = 1, eval = NULL, debug = FALSE,
        useragent = NULL)

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
colnames(cor01$rho) <- c("(A)", "(B)", "(C)", "(D)", "(E)", "(F)", "(G)", "(H)")
tab_corr(cor01$rho,
         triangle = "lower")

webshot(url = "http://localhost:22383/session/file522832524b3.html", file = "output/tables/cor01.png", vwidth = 992,
        vheight = 744, cliprect = NULL, selector = NULL, expand = NULL,
        delay = 0.2, zoom = 1, eval = NULL, debug = FALSE,
        useragent = NULL)

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
colnames(cor02$rho) <- c("(A)", "(B)", "(C)", "(D)", "(E)", "(F)", "(G)", "(H)")
tab_corr(cor02$rho,
         triangle = "lower")

webshot(url = "http://localhost:22383/session/file522820467ae8.html", file = "output/tables/cor02.png", vwidth = 992,
        vheight = 744, cliprect = NULL, selector = NULL, expand = NULL,
        delay = 0.2, zoom = 1, eval = NULL, debug = FALSE,
        useragent = NULL)

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
colnames(cor03$rho) <- c("(A)", "(B)", "(C)", "(D)", "(E)", "(F)", "(G)", "(H)")
tab_corr(cor03$rho,
         triangle = "lower")

webshot(url = "http://localhost:22383/session/file5228cb4786a.html", file = "output/tables/cor03.png", vwidth = 992,
        vheight = 744, cliprect = NULL, selector = NULL, expand = NULL,
        delay = 0.2, zoom = 1, eval = NULL, debug = FALSE,
        useragent = NULL)

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

webshot(url = "http://localhost:13515/session/file2d3059233930.html", file = "output/tables/cor04.png", vwidth = 992,
        vheight = 744, cliprect = NULL, selector = NULL, expand = NULL,
        delay = 0.2, zoom = 1, eval = NULL, debug = FALSE,
        useragent = NULL)

ohl_acum <- data.frame(a1, a2, a3, a4, a5, a6, a7, a8)

k <-cbind(a1, a2, a3, a4, a5, a6, a7, a8)~1

# ---- 6. Modelos ----

# 6.1. Año 2016 
M1<-poLCA(formula = f, data = ohl_2016, nclass = 1, maxiter = 2000, nrep = 1, na.rm = F)
M2<-poLCA(formula = f, data = ohl_2016, nclass = 2, maxiter = 2000, nrep = 1, na.rm = F)
M3<-poLCA(formula = f, data = ohl_2016, nclass = 3, maxiter = 2000, nrep = 1, na.rm = F) 
M4<-poLCA(formula = f, data = ohl_2016, nclass = 3, maxiter = 2000, nrep = 2, na.rm = F) # nrep = 1 falso máximo
M5<-poLCA(formula = f, data = ohl_2016, nclass = 3, maxiter = 2000, nrep = 3, na.rm = F) # este
M6<-poLCA(formula = f, data = ohl_2016, nclass = 4, maxiter = 2000, nrep = 1, na.rm = F) 
M7<-poLCA(formula = f, data = ohl_2016, nclass = 4, maxiter = 2000, nrep = 5, na.rm = F)
M8<-poLCA(formula = f, data = ohl_2016, nclass = 5, maxiter = 2000, nrep = 3, na.rm = F)
M9<-poLCA(formula = f, data = ohl_2016, nclass = 6, maxiter = 2000, nrep = 3, na.rm = F)

# M6 el mejor modelo con 4 clases latentes y ajuste bajo/ M5 mejor modelo con 3 clases latentes y ajuste alto

# 6.2. Año 2017
M1_1<-poLCA(formula = t, data = ohl_2017, nclass = 1, maxiter = 2000, nrep = 1, na.rm = F)
M2_1<-poLCA(formula = t, data = ohl_2017, nclass = 2, maxiter = 2000, nrep = 1, na.rm = F)
M3_1<-poLCA(formula = t, data = ohl_2017, nclass = 3, maxiter = 2000, nrep = 1, na.rm = F)
M4_1<-poLCA(formula = t, data = ohl_2017, nclass = 3, maxiter = 2000, nrep = 2, na.rm = F) 
M5_1<-poLCA(formula = t, data = ohl_2017, nclass = 3, maxiter = 2000, nrep = 3, na.rm = F)  # este
M6_1<-poLCA(formula = t, data = ohl_2017, nclass = 4, maxiter = 2000, nrep = 1, na.rm = F)  
M7_1<-poLCA(formula = t, data = ohl_2017, nclass = 4, maxiter = 2000, nrep = 3, na.rm = F)
M8_1<-poLCA(formula = t, data = ohl_2017, nclass = 5, maxiter = 16000, nrep = 3, na.rm = F)
M9_1<-poLCA(formula = t, data = ohl_2017, nclass = 6, maxiter = 16000, nrep = 3, na.rm = F)

# M5_1 mejor modelo con ajuste con 3 clases latentes

# 6.3. Año 2018 
M1_2<-poLCA(formula = r, data = ohl_2018, nclass = 1, maxiter = 2000, nrep = 1, na.rm = F)
M2_2<-poLCA(formula = r, data = ohl_2018, nclass = 2, maxiter = 2000, nrep = 1, na.rm = F)
M3_2<-poLCA(formula = r, data = ohl_2018, nclass = 3, maxiter = 2000, nrep = 1, na.rm = F) # este
M4_2<-poLCA(formula = r, data = ohl_2018, nclass = 3, maxiter = 2000, nrep = 3, na.rm = F) # nrep = 1 falso máximo 
M5_2<-poLCA(formula = r, data = ohl_2018, nclass = 4, maxiter = 2000, nrep = 3, na.rm = F) # este igual
M6_2<-poLCA(formula = r, data = ohl_2018, nclass = 5, maxiter = 2000, nrep = 3, na.rm = F)
M7_2<-poLCA(formula = r, data = ohl_2018, nclass = 6, maxiter = 2000, nrep = 3, na.rm = F)

# M4_2 es el que mejor ajusta con 3 clases latentes/ M5_2 ajusta bien pero con 4 clases latentes

# 6.4. Acumulado 
M1_3<-poLCA(formula = k, data = ohl_acum, nclass = 1, maxiter = 2000, nrep = 1, na.rm = F)
M2_3<-poLCA(formula = k, data = ohl_acum, nclass = 2, maxiter = 2000, nrep = 1, na.rm = F) 
M3_3<-poLCA(formula = k, data = ohl_acum, nclass = 3, maxiter = 2000, nrep = 1, na.rm = F) #  
M4_3<-poLCA(formula = k, data = ohl_acum, nclass = 3, maxiter = 16000, nrep = 3, na.rm = F)
M5_3<-poLCA(formula = k, data = ohl_acum, nclass = 4, maxiter = 2000, nrep = 5, na.rm = F) # 
M6_3<-poLCA(formula = k, data = ohl_acum, nclass = 5, maxiter = 2000, nrep = 3, na.rm = F)

# M3_3 es el que mejor ajusta con 3 clases latentes/ M5_3 ajusta bien pero con 4 clases latentes

# ---- 7. Ajuste ---- 

# 7.1. Ajuste año 2016 
M1$predcell
M2$predcell
M5$predcell
M6$predcell
M8$predcell

p1<-1-pchisq(M1$Chisq, M1$resid.df)
p2<-1-pchisq(M2$Chisq, M2$resid.df)
p3<-1-pchisq(M5$Chisq, M5$resid.df)
p4<-1-pchisq(M6$Chisq, M7$resid.df)
p5<-1-pchisq(M8$Chisq, M8$resid.df)

AjusteM_2016<-data.frame(c("M1", "M2", "M3", "M4", "M5"),
                         c(M1$llik, M2$llik, M5$llik, M6$llik, M8$llik),
                         c(M1$Chisq, M2$Chisq, M5$Chisq, M6$Chisq, M8$Chisq),
                         c(M1$Gsq, M2$Gsq, M5$Gsq, M6$Gsq, M8$Gsq),
                         c(M1$npar, M2$npar, M5$npar, M6$npar, M8$npar),
                         c(M1$aic, M2$aic, M5$aic, M6$aic, M8$aic),
                         c(M1$bic, M2$bic, M5$bic, M6$bic, M8$bic),
                         c(p1, p2, p3, p4, p5))
colnames(AjusteM_2016)<-c("Modelo", "Loglike", "X2", "G2", "DF", "AIC", "BIC", "P-value")

View(AjusteM_2016)

# Gráfico año 2016 

# Pr(1)
plotdatos <- melt(M5$probs) 
plotdatos2 <- plotdatos[plotdatos$X2=="1",]

ggplot(plotdatos2, aes(x=L1, y = value, group = X1, colour = X1)) +
  geom_point() + geom_line() + theme_classic() + 
  labs(title = "Gráfico 4. Predicciones de clase año 2016",
       x="Variables",
       y = "Valores",
       caption = "Fuente: Elaboración propia con base en datos de huelgas laborales OHL-COES (1979-2018)") + 
  theme(legend.position = "bottom",plot.title = element_text(size=12),
        plot.subtitle = element_text(size=10),
        plot.caption = element_text(size=8)) 

ggsave(plot = last_plot(),
       filename = "Output/graph/predclass1-2016.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 33,
       height = 20)

# Pr(2)
plotdatos <- melt(M5$probs) 
plotdatos2 <- plotdatos[plotdatos$X2=="2",]

ggplot(plotdatos2, aes(x=L1, y = value, group = X1, colour = X1)) +
  geom_point() + geom_line() + theme_classic() + 
  labs(title = "Gráfico 4. Predicciones de clase año 2016",
       x="Variables",
       y = "Valores",
       caption = "Fuente: Elaboración propia con base en datos de huelgas laborales OHL-COES (1979-2018)") + 
  theme(legend.position = "bottom",plot.title = element_text(size=12),
        plot.subtitle = element_text(size=10),
        plot.caption = element_text(size=8)) 

ggsave(plot = last_plot(),
       filename = "Output/graph/predclass2-2016.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 33,
       height = 20)

# Pr(3)
plotdatos <- melt(M5$probs) 
plotdatos2 <- plotdatos[plotdatos$X2=="3",]

ggplot(plotdatos2, aes(x=L1, y = value, group = X1, colour = X1)) +
  geom_point() + geom_line() + theme_classic() + 
  labs(title = "Gráfico 4. Predicciones de clase año 2016",
       x="Variables",
       y = "Valores",
       caption = "Fuente: Elaboración propia con base en datos de huelgas laborales OHL-COES (1979-2018)") + 
  theme(legend.position = "bottom",plot.title = element_text(size=12),
        plot.subtitle = element_text(size=10),
        plot.caption = element_text(size=8)) 

ggsave(plot = last_plot(),
       filename = "Output/graph/predclass3-2016.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 33,
       height = 20)

# 7.2. Ajuste año 2017 
M1_1$predcell
M2_1$predcell
M5_1$predcell 
M7_1$predcell
M8_1$predcell

p1_1<-1-pchisq(M1_1$Chisq, M1_1$resid.df)
p2_1<-1-pchisq(M2_1$Chisq, M2_1$resid.df)
p3_1<-1-pchisq(M5_1$Chisq, M5_1$resid.df)
p4_1<-1-pchisq(M7_1$Chisq, M7_1$resid.df)
p5_1<-1-pchisq(M8_1$Chisq, M8_1$resid.df)

AjusteM_2017<-data.frame(c("M1", "M2", "M3", "M4", "M5"),
                         c(M1_1$llik, M2_1$llik, M5_1$llik, M7_1$llik, M8_1$llik),
                         c(M1_1$Chisq, M2_1$Chisq, M5_1$Chisq, M7_1$Chisq, M8_1$Chisq),
                         c(M1_1$Gsq, M2_1$Gsq, M5_1$Gsq, M7_1$Gsq, M8_1$Gsq),
                         c(M1_1$npar, M2_1$npar, M5_1$npar, M7_1$npar, M8_1$npar),
                         c(M1_1$aic, M2_1$aic, M5_1$aic, M7_1$aic, M8_1$aic),
                         c(M1_1$bic, M2_1$bic, M5_1$bic, M7_1$bic, M8_1$bic),
                         c(p1_1, p2_1, p3_1, p4_1, p5_1))
colnames(AjusteM_2017)<-c("Modelo", "Loglike", "X2", "G2", "DF", "AIC", "BIC", "P-value")

View(AjusteM_2017)

# Gráfico año 2017

# Pr(1)
plotdatos <- melt(M5_1$probs) 
plotdatos2 <- plotdatos[plotdatos$X2=="1",]

ggplot(plotdatos2, aes(x=L1, y = value, group = X1, color = X1)) +
  geom_point() + geom_line() + theme_classic() + 
  labs(title = "Gráfico 4. Predicciones de clase año 2017",
       x="Variables",
       y = "Valores",
       caption = "Fuente: Elaboración propia con base en datos de huelgas laborales OHL-COES (1979-2018)") + 
  theme(legend.position = "bottom",plot.title = element_text(size=12),
        plot.subtitle = element_text(size=10),
        plot.caption = element_text(size=8))      

ggsave(plot = last_plot(),
       filename = "Output/graph/predclass1-2017.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 33,
       height = 20)

# Pr(2)
plotdatos <- melt(M5_1$probs) 
plotdatos2 <- plotdatos[plotdatos$X2=="2",]

ggplot(plotdatos2, aes(x=L1, y = value, group = X1, color = X1)) +
  geom_point() + geom_line() + theme_classic() + 
  labs(title = "Gráfico 4. Predicciones de clase año 2017",
       x="Variables",
       y = "Valores",
       caption = "Fuente: Elaboración propia con base en datos de huelgas laborales OHL-COES (1979-2018)") + 
  theme(legend.position = "bottom",plot.title = element_text(size=12),
        plot.subtitle = element_text(size=10),
        plot.caption = element_text(size=8))    

ggsave(plot = last_plot(),
       filename = "Output/graph/predclass2-2017.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 33,
       height = 20)

# Pr(3)
plotdatos <- melt(M5_1$probs) 
plotdatos2 <- plotdatos[plotdatos$X2=="3",]

ggplot(plotdatos2, aes(x=L1, y = value, group = X1, color = X1)) +
  geom_point() + geom_line() + theme_classic() + 
  labs(title = "Gráfico 4. Predicciones de clase año 2017",
       x="Variables",
       y = "Valores",
       caption = "Fuente: Elaboración propia con base en datos de huelgas laborales OHL-COES (1979-2018)") + 
  theme(legend.position = "bottom",plot.title = element_text(size=12),
        plot.subtitle = element_text(size=10),
        plot.caption = element_text(size=8)) 

ggsave(plot = last_plot(),
       filename = "Output/graph/predclass3-2017.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 33,
       height = 20)

# 7.3. Ajuste año 2018 
M1_2$predcell
M2_2$predcell
M3_2$predcell
M5_2$predcell
M6_2$predcell

p1_2<-1-pchisq(M1_2$Chisq, M1_2$resid.df)
p2_2<-1-pchisq(M2_2$Chisq, M2_2$resid.df)
p3_2<-1-pchisq(M3_2$Chisq, M3_2$resid.df)
p4_2<-1-pchisq(M5_2$Chisq, M5_2$resid.df)
p5_2<-1-pchisq(M6_2$Chisq, M6_2$resid.df)

AjusteM_2018<-data.frame(c("M1", "M2", "M3", "M4", "M5"),
                         c(M1_2$llik, M2_2$llik, M3_2$llik, M5_2$llik, M6_2$llik),
                         c(M1_2$Chisq, M2_2$Chisq, M3_2$Chisq, M5_2$Chisq, M6_2$Chisq),
                         c(M1_2$Gsq, M2_2$Gsq, M3_2$Gsq, M5_2$Gsq, M6_2$Gsq),
                         c(M1_2$npar, M2_2$npar, M3_2$npar, M5_2$npar, M6_2$npar),
                         c(M1_2$aic, M2_2$aic, M3_2$aic, M5_2$aic, M6_2$aic),
                         c(M1_2$bic, M2_2$bic, M3_2$bic, M5_2$bic, M6_2$bic),
                         c(p1_2, p2_2, p3_2, p4_2, p5_2))
colnames(AjusteM_2018)<-c("Modelo", "Loglike", "X2", "G2", "DF", "AIC", "BIC", "P-value")

View(AjusteM_2018)

# Gráfico año 2018

# Pr(1)
plotdatos <- melt(M3_2$probs) 
plotdatos2 <- plotdatos[plotdatos$X2=="1",]

ggplot(plotdatos2, aes(x=L1, y = value, group = X1, colour = X1)) +
  geom_point() + geom_line() + theme_classic() + 
  labs(title = "Gráfico 6. Predicciones de clase año 2018",
       x="Variables",
       y = "Valores",
       caption = "Fuente: Elaboración propia con base en datos de huelgas laborales OHL-COES (1979-2018)") + 
  theme(legend.position = "bottom",plot.title = element_text(size=12),
        plot.subtitle = element_text(size=10),
        plot.caption = element_text(size=8)) 

ggsave(plot = last_plot(),
       filename = "Output/graph/predclass1-2018.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 33,
       height = 20)

# Pr(2)
plotdatos <- melt(M3_2$probs) 
plotdatos2 <- plotdatos[plotdatos$X2=="2",]

ggplot(plotdatos2, aes(x=L1, y = value, group = X1, colour = X1)) +
  geom_point() + geom_line() + theme_classic() + 
  labs(title = "Gráfico 6. Predicciones de clase año 2018",
       x="Variables",
       y = "Valores",
       caption = "Fuente: Elaboración propia con base en datos de huelgas laborales OHL-COES (1979-2018)") + 
  theme(legend.position = "bottom",plot.title = element_text(size=12),
        plot.subtitle = element_text(size=10),
        plot.caption = element_text(size=8)) 

ggsave(plot = last_plot(),
       filename = "Output/graph/predclass2-2018.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 33,
       height = 20)

# Pr(3)
plotdatos <- melt(M3_2$probs) 
plotdatos2 <- plotdatos[plotdatos$X2=="3",]

ggplot(plotdatos2, aes(x=L1, y = value, group = X1, colour = X1)) +
  geom_point() + geom_line() + theme_classic() + 
  labs(title = "Gráfico 6. Predicciones de clase año 2018",
       x="Variables",
       y = "Valores",
       caption = "Fuente: Elaboración propia con base en datos de huelgas laborales OHL-COES (1979-2018)") + 
  theme(legend.position = "bottom",plot.title = element_text(size=12),
        plot.subtitle = element_text(size=10),
        plot.caption = element_text(size=8)) 

ggsave(plot = last_plot(),
       filename = "Output/graph/predclass3-2018.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 33,
       height = 20)

# 7.4. Ajuste acumulado
M1_3$predcell
M2_3$predcell
M3_3$predcell
M5_3$predcell
M6_3$predcell

p1_3<-1-pchisq(M1_3$Chisq, M1_3$resid.df)
p2_3<-1-pchisq(M2_3$Chisq, M2_3$resid.df)
p3_3<-1-pchisq(M3_3$Chisq, M3_3$resid.df)
p4_3<-1-pchisq(M5_3$Chisq, M5_3$resid.df)
p5_3<-1-pchisq(M6_3$Chisq, M6_3$resid.df)

AjusteM_acum<-data.frame(c("M1", "M2", "M3", "M4", "M5"),
                         c(M1_3$llik, M2_3$llik, M3_3$llik, M5_3$llik, M6_3$llik),
                         c(M1_3$Chisq, M2_3$Chisq, M3_3$Chisq, M5_3$Chisq, M6_3$Chisq),
                         c(M1_3$Gsq, M2_3$Gsq, M3_3$Gsq, M5_3$Gsq, M6_3$Gsq),
                         c(M1_3$npar, M2_3$npar, M3_3$npar, M5_3$npar, M6_3$npar),
                         c(M1_3$aic, M2_3$aic, M3_3$aic, M5_3$aic, M6_3$aic),
                         c(M1_3$bic, M2_3$bic, M3_3$bic, M5_3$bic, M6_3$bic),
                         c(p1_3, p2_3, p3_3, p4_3, p5_3))
colnames(AjusteM_acum)<-c("Modelo", "Loglike", "X2", "G2", "DF", "AIC", "BIC", "P-value")

View(AjusteM_acum)

# Gráfico acumulado

# Con Pr(1)
plotdatos <- melt(M3_3$probs) 
plotdatos2 <- plotdatos[plotdatos$X2=="1",]

ggplot(plotdatos2, aes(x=L1, y = value, group = X1, colour = X1)) +
  geom_point() + geom_line() + theme_classic() + 
  labs(title = "Gráfico 1. Predicciones de clase 2016 a 2018",
       x="Variables",
       y = "Valores",
       caption = "Fuente: Elaboración propia con base en datos de huelgas laborales OHL-COES (1979-2018)") + 
  theme(legend.position = "bottom",plot.title = element_text(size=12),
        plot.subtitle = element_text(size=10),
        plot.caption = element_text(size=8)) 

ggsave(plot = last_plot(),
       filename = "Output/graph/predclass1-acum.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 33,
       height = 20)

# Con Pr(2)
plotdatos <- melt(M3_3$probs) 
plotdatos2 <- plotdatos[plotdatos$X2=="2",]

ggplot(plotdatos2, aes(x=L1, y = value, group = X1, colour = X1)) +
  geom_point() + geom_line() + theme_classic() + 
  labs(title = "Gráfico 2. Predicciones de clase 2016 a 2018",
       x="Variables",
       y = "Valores",
       caption = "Fuente: Elaboración propia con base en datos de huelgas laborales OHL-COES (1979-2018)") + 
  theme(legend.position = "bottom",plot.title = element_text(size=12),
        plot.subtitle = element_text(size=10),
        plot.caption = element_text(size=8)) 

ggsave(plot = last_plot(),
       filename = "Output/graph/predclass2-acum.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 33,
       height = 20)

# Con Pr(3)
plotdatos <- melt(M3_3$probs) 
plotdatos2 <- plotdatos[plotdatos$X2=="3",]

ggplot(plotdatos2, aes(x=L1, y = value, group = X1, colour = X1)) +
  geom_point() + geom_line() + theme_classic() + 
  labs(title = "Gráfico 3. Predicciones de clase 2016 a 2018",
       x="Variables",
       y = "Valores",
       caption = "Fuente: Elaboración propia con base en datos de huelgas laborales OHL-COES (1979-2018)") + 
  theme(legend.position = "bottom",plot.title = element_text(size=12),
        plot.subtitle = element_text(size=10),
        plot.caption = element_text(size=8)) 

ggsave(plot = last_plot(),
       filename = "Output/graph/predclass3-acum.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 33,
       height = 20)

# 6.2. Descripcion de clases latentes ----

# Clases latentes año 2016
proc_ohl_2016<-cbind(proc_ohl_2016, M1$predclass, M2$predclass, M5$predclass, M6$predclass, M8$predclass)

sapply(proc_ohl_2016, class)
proc_ohl_2016$`M1$predclass` <- as.factor(proc_ohl_2016$`M1$predclass`)
proc_ohl_2016$`M2$predclass` <- as.factor(proc_ohl_2016$`M2$predclass`)
proc_ohl_2016$`M5$predclass` <- as.factor(proc_ohl_2016$`M5$predclass`)
proc_ohl_2016$`M6$predclass` <- as.factor(proc_ohl_2016$`M6$predclass`)
proc_ohl_2016$`M8$predclass` <- as.factor(proc_ohl_2016$`M8$predclass`)

freq(proc_ohl_2016$`M1$predclass`)
freq(proc_ohl_2016$`M2$predclass`)
freq(proc_ohl_2016$`M5$predclass`)
freq(proc_ohl_2016$`M6$predclass`)
freq(proc_ohl_2016$`M8$predclass`)

ctable(proc_ohl_2016$tasa_sind_homb_2016, proc_ohl_2016$`M1$predclass`) #tasa sindicalización masculina
ctable(proc_ohl_2016$tasa_sind_homb_2016, proc_ohl_2016$`M2$predclass`)
ctable(proc_ohl_2016$tasa_sind_homb_2016, proc_ohl_2016$`M5$predclass`)
ctable(proc_ohl_2016$tasa_sind_homb_2016, proc_ohl_2016$`M6$predclass`)
ctable(proc_ohl_2016$tasa_sind_homb_2016, proc_ohl_2016$`M8$predclass`)

ctable(proc_ohl_2016$tasa_sind_muj_2016, proc_ohl_2016$`M1$predclass`) #tasa sindicalización femenina
ctable(proc_ohl_2016$tasa_sind_muj_2016, proc_ohl_2016$`M2$predclass`)
ctable(proc_ohl_2016$tasa_sind_muj_2016, proc_ohl_2016$`M5$predclass`)
ctable(proc_ohl_2016$tasa_sind_muj_2016, proc_ohl_2016$`M6$predclass`)
ctable(proc_ohl_2016$tasa_sind_muj_2016, proc_ohl_2016$`M8$predclass`)

ctable(proc_ohl_2016$sector, proc_ohl_2016$`M5$predclass`)
ctable(proc_ohl_2016$tactica, proc_ohl_2016$`M5$predclass`)

tab_xtab(var.row = proc_ohl_2016$sector, proc_ohl_2016$`M5$predclass`,
         show.cell.prc = T,show.summary = F, show.na = F)

webshot(url = "http://localhost:14019/session/file1d9411ac5a46.html", file = "output/tables/sector-predclass2016.png", vwidth = 992,
        vheight = 744, cliprect = NULL, selector = NULL, expand = NULL,
        delay = 0.2, zoom = 1, eval = NULL, debug = FALSE,
        useragent = NULL)

tab_xtab(var.row = proc_ohl_2016$tactica, proc_ohl_2016$`M5$predclass`,
         show.cell.prc = T,show.summary = F, show.na = F)

webshot(url = "http://localhost:14019/session/file1d941af47d6f.html", file = "output/tables/tactica-predclass2016.png", vwidth = 992,
        vheight = 744, cliprect = NULL, selector = NULL, expand = NULL,
        delay = 0.2, zoom = 1, eval = NULL, debug = FALSE,
        useragent = NULL)

#Clases latentes año 2017
proc_ohl_2017<-cbind(proc_ohl_2017, M1_1$predclass, M2_1$predclass, M5_1$predclass, M7_1$predclass, M8_1$predclass)

sapply(proc_ohl_2017, class)
proc_ohl_2017$`M1_1$predclass` <- as.factor(proc_ohl_2017$`M1_1$predclass`)
proc_ohl_2017$`M2_1$predclass` <- as.factor(proc_ohl_2017$`M2_1$predclass`)
proc_ohl_2017$`M5_1$predclass` <- as.factor(proc_ohl_2017$`M5_1$predclass`)
proc_ohl_2017$`M7_1$predclass` <- as.factor(proc_ohl_2017$`M7_1$predclass`)
proc_ohl_2017$`M8_1$predclass` <- as.factor(proc_ohl_2017$`M8_1$predclass`)

freq(proc_ohl_2017$`M1_1$predclass`)
freq(proc_ohl_2017$`M2_1$predclass`)
freq(proc_ohl_2017$`M5_1$predclass`)
freq(proc_ohl_2017$`M7_1$predclass`)
freq(proc_ohl_2017$`M8_1$predclass`)

ctable(proc_ohl_2017$tasa_sind_homb_2017, proc_ohl_2017$`M1_1$predclass`) #tasa sindicalización masculina
ctable(proc_ohl_2017$tasa_sind_homb_2017, proc_ohl_2017$`M2_1$predclass`)
ctable(proc_ohl_2017$tasa_sind_homb_2017, proc_ohl_2017$`M5_1$predclass`)
ctable(proc_ohl_2017$tasa_sind_homb_2017, proc_ohl_2017$`M7_1$predclass`)
ctable(proc_ohl_2017$tasa_sind_homb_2017, proc_ohl_2017$`M8_1$predclass`)

ctable(proc_ohl_2017$tasa_sind_muj_2017, proc_ohl_2017$`M1_1$predclass`) #tasa sindicalización femenina
ctable(proc_ohl_2017$tasa_sind_muj_2017, proc_ohl_2017$`M2_1$predclass`)
ctable(proc_ohl_2017$tasa_sind_muj_2017, proc_ohl_2017$`M5_1$predclass`)
ctable(proc_ohl_2017$tasa_sind_muj_2017, proc_ohl_2017$`M7_1$predclass`)
ctable(proc_ohl_2017$tasa_sind_muj_2017, proc_ohl_2017$`M8_1$predclass`)

ctable(proc_ohl_2017$sector, proc_ohl_2017$`M5_1$predclass`)
ctable(proc_ohl_2017$tactica, proc_ohl_2017$`M5_1$predclass`)

tab_xtab(var.row = proc_ohl_2017$sector, proc_ohl_2017$`M5_1$predclass`,
         show.cell.prc = T,show.summary = F, show.na = F)

webshot(url = "http://localhost:14019/session/file1d9458a81bea.html", file = "output/tables/sector-predclass2017.png", vwidth = 992,
        vheight = 744, cliprect = NULL, selector = NULL, expand = NULL,
        delay = 0.2, zoom = 1, eval = NULL, debug = FALSE,
        useragent = NULL)

tab_xtab(var.row = proc_ohl_2017$tactica, proc_ohl_2017$`M5_1$predclass`,
         show.cell.prc = T,show.summary = F, show.na = F)

webshot(url = "http://localhost:14019/session/file1d9464023efc.html", file = "output/tables/tactica-predclass2017.png", vwidth = 992,
        vheight = 744, cliprect = NULL, selector = NULL, expand = NULL,
        delay = 0.2, zoom = 1, eval = NULL, debug = FALSE,
        useragent = NULL)

#Clases latentes año 2018
proc_ohl_2018<-cbind(proc_ohl_2018, M1_2$predclass, M2_2$predclass, M3_2$predclass, M5_2$predclass, M6_2$predclass)

sapply(proc_ohl_2018, class)
proc_ohl_2018$`M1_2$predclass` <- as.factor(proc_ohl_2018$`M1_2$predclass`)
proc_ohl_2018$`M2_2$predclass` <- as.factor(proc_ohl_2018$`M2_2$predclass`)
proc_ohl_2018$`M3_2$predclass` <- as.factor(proc_ohl_2018$`M3_2$predclass`)
proc_ohl_2018$`M5_2$predclass` <- as.factor(proc_ohl_2018$`M5_2$predclass`)
proc_ohl_2018$`M6_2$predclass` <- as.factor(proc_ohl_2018$`M6_2$predclass`)

freq(proc_ohl_2018$`M1_2$predclass`)
freq(proc_ohl_2018$`M2_2$predclass`)
freq(proc_ohl_2018$`M3_2$predclass`)
freq(proc_ohl_2018$`M5_2$predclass`)
freq(proc_ohl_2018$`M6_2$predclass`)

ctable(proc_ohl_2018$tasa_sind_homb_2018, proc_ohl_2018$`M1_2$predclass`) #tasa sindicalización masculina
ctable(proc_ohl_2018$tasa_sind_homb_2018, proc_ohl_2018$`M2_2$predclass`)
ctable(proc_ohl_2018$tasa_sind_homb_2018, proc_ohl_2018$`M3_2$predclass`)
ctable(proc_ohl_2018$tasa_sind_homb_2018, proc_ohl_2018$`M5_2$predclass`)
ctable(proc_ohl_2018$tasa_sind_homb_2018, proc_ohl_2018$`M6_2$predclass`)

ctable(proc_ohl_2018$tasa_sind_muj_2018, proc_ohl_2018$`M1_2$predclass`) #tasa sindicalización femenina
ctable(proc_ohl_2018$tasa_sind_muj_2018, proc_ohl_2018$`M2_2$predclass`)
ctable(proc_ohl_2018$tasa_sind_muj_2018, proc_ohl_2018$`M3_2$predclass`)
ctable(proc_ohl_2018$tasa_sind_muj_2018, proc_ohl_2018$`M5_2$predclass`)
ctable(proc_ohl_2018$tasa_sind_muj_2018, proc_ohl_2018$`M6_2$predclass`)

ctable(proc_ohl_2018$sector, proc_ohl_2018$`M3_2$predclass`)
ctable(proc_ohl_2018$tactica, proc_ohl_2018$`M3_2$predclass`)

tab_xtab(var.row = proc_ohl_2018$sector, proc_ohl_2018$`M3_2$predclass`,
         show.cell.prc = T,show.summary = F, show.na = F)

webshot(url = "http://localhost:14019/session/file1d9472b423dd.html", file = "output/tables/sector-predclass2018.png", vwidth = 992,
        vheight = 744, cliprect = NULL, selector = NULL, expand = NULL,
        delay = 0.2, zoom = 1, eval = NULL, debug = FALSE,
        useragent = NULL)

tab_xtab(var.row = proc_ohl_2018$tactica, proc_ohl_2018$`M3_2$predclass`,
         show.cell.prc = T,show.summary = F, show.na = F)

webshot(url = "http://localhost:14019/session/file1d9470604966.html", file = "output/tables/tactica-predclass2018.png", vwidth = 992,
        vheight = 744, cliprect = NULL, selector = NULL, expand = NULL,
        delay = 0.2, zoom = 1, eval = NULL, debug = FALSE,
        useragent = NULL)

#Clases latentes acumulado
proc_ohl<-cbind(proc_ohl, M1_3$predclass, M2_3$predclass, M3_3$predclass, M5_3$predclass, M6_3$predclass)

sapply(proc_ohl, class)
proc_ohl$`M1_3$predclass` <- as.factor(proc_ohl$`M1_3$predclass`)
proc_ohl$`M2_3$predclass` <- as.factor(proc_ohl$`M2_3$predclass`)
proc_ohl$`M3_3$predclass` <- as.factor(proc_ohl$`M3_3$predclass`)
proc_ohl$`M5_3$predclass` <- as.factor(proc_ohl$`M5_3$predclass`)
proc_ohl$`M6_3$predclass` <- as.factor(proc_ohl$`M6_3$predclass`)

freq(proc_ohl$`M1_3$predclass`)
freq(proc_ohl$`M2_3$predclass`)
freq(proc_ohl$`M3_3$predclass`)
freq(proc_ohl$`M5_3$predclass`)
freq(proc_ohl$`M6_3$predclass`)

ctable(proc_ohl$tasa_sind_homb_acum, proc_ohl$`M1_3$predclass`) #tasa sindicalización masculina
ctable(proc_ohl$tasa_sind_homb_acum, proc_ohl$`M2_3$predclass`)
ctable(proc_ohl$tasa_sind_homb_acum, proc_ohl$`M3_3$predclass`)
ctable(proc_ohl$tasa_sind_homb_acum, proc_ohl$`M5_3$predclass`)
ctable(proc_ohl$tasa_sind_homb_acum, proc_ohl$`M6_3$predclass`)

ctable(proc_ohl$tasa_sind_muj_acum, proc_ohl$`M1_3$predclass`) #tasa sindicalización femenina
ctable(proc_ohl$tasa_sind_muj_acum, proc_ohl$`M2_3$predclass`)
ctable(proc_ohl$tasa_sind_muj_acum, proc_ohl$`M3_3$predclass`)
ctable(proc_ohl$tasa_sind_muj_acum, proc_ohl$`M5_3$predclass`)
ctable(proc_ohl$tasa_sind_muj_acum, proc_ohl$`M6_3$predclass`)

ctable(proc_ohl$sector, proc_ohl$`M3_3$predclass`)
ctable(proc_ohl$tactica, proc_ohl$`M3_3$predclass`)

tab_xtab(var.row = proc_ohl$`M3_3$predclass`,proc_ohl$sector,
         show.cell.prc = T,show.summary = F, show.na = F)

webshot(url = "http://localhost:20377/session/file3be0e372ab6.html", file = "output/tables/sector-predclassacum.png", vwidth = 992,
        vheight = 744, cliprect = NULL, selector = NULL, expand = NULL,
        delay = 0.2, zoom = 1, eval = NULL, debug = FALSE,
        useragent = NULL)

tab_xtab(var.row = proc_ohl$tactica, proc_ohl$`M3_3$predclass`,
         show.cell.prc = T,show.summary = F, show.na = F)

webshot(url = "http://localhost:22383/session/file52288e51060.html", file = "output/tables/tactica-predclassacum.png", vwidth = 992,
        vheight = 744, cliprect = NULL, selector = NULL, expand = NULL,
        delay = 0.2, zoom = 1, eval = NULL, debug = FALSE,
        useragent = NULL)

# ---- 8. Export ----
write_xlsx(AjusteM_2016,"output/ajuste2016.xlsx", col_names = TRUE,format_headers = TRUE)
write_xlsx(AjusteM_2017,"output/ajuste2017.xlsx", col_names = TRUE,format_headers = TRUE)
write_xlsx(AjusteM_2018,"output/ajuste2018.xlsx", col_names = TRUE,format_headers = TRUE)
write_xlsx(AjusteM_acum,"output/ajusteacum.xlsx", col_names = TRUE,format_headers = TRUE)