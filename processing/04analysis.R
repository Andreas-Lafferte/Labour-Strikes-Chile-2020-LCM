## 04: Analisis 

# ---- 1. Librerias -----
pacman::p_load(dplyr, car, summarytools, ggplot2, magrittr, tidyverse, sjmisc, sjlabelled, 
               stargazer, sjPlot, devtools, ggmosaic, texreg, kableExtra, webshot, readxl,
               openxlsx, psych, MASS, scatterplot3d, poLCA, reshape, writexl, readr)
options(scipen=999) # valores sin notación científica

# ---- 2. Datos ---- 
proc_ohl <- load("data/proc_ohl.RData")  %>% as.data.frame()
proc_ohl_2016 <- load("data/proc_ohl_2016.RData") %>% as.data.frame()
proc_ohl_2017 <- load("data/proc_ohl_2017.RData") %>% as.data.frame()
proc_ohl_2018 <- load("data/proc_ohl_2018.RData") %>% as.data.frame()

# ---- 3. Analisis descriptivo ---- 

# ---- 4. Analisis bivariado ---- 

# ---- 5. Matriz ---- 

# ---- 6. Modelos ----

# ---- 7. Ajuste ---- 

# ---- 8. Export ---- 