install.packages("esquisse")
library(esquisse)
library(writexl)
library(kableExtra)

# Set options for browser 
options("esquisse.display.mode" = "browser")

# Set option for usual RStudio dialog window
options("esquisse.display.mode" = "dialog")

base3 <- proc_ohl %>% select(organizacion, 
                             legalidad,
                             ano,
                             motivos, 
                             ddpp,
                             sector,
                             tot_trabajadores,
                             rango_empresa,
                             trab_comprometidos,
                             tactica,
                             aliado,
                             central,
                             dptp,
                             number, 
                             autoridad)


freq(base3$organizacion)
table_freq_organizacion <-freq(base3$organizacion)
table_freq_organizacion2 <- as.data.frame(table_freq_organizacion)
write_xlsx (table_freq_organizacion2, path = "table_freq_organizacion2.xlsx")

base3$organizacion[base3$organizacion==0] <- 12
base3$organizacion[base3$organizacion==10] <- 12
base3$organizacion[base3$organizacion==9] <- 12
base3$organizacion[base3$organizacion==8] <- 12
base3$organizacion[base3$organizacion==7] <- 11
base3$organizacion[base3$organizacion==6] <- 11
base3$organizacion[base3$organizacion==5] <- 11
base3$organizacion[base3$organizacion==4] <- 11
base3$organizacion[base3$organizacion==3] <- 11
base3$organizacion[base3$organizacion==2] <- 11
base3$organizacion[base3$organizacion==1] <- 11

base3$organizacion[base3$organizacion==11] <- 1
base3$organizacion[base3$organizacion==12] <- 0

freq(base3$organizacion)
View(base3)

#Renombrar la variable
base3 <- rename(base3, presencia_sindicato=organizacion)

View (base3$presencia_sindicato)

base3$presencia_sindicato = as_factor(base3$presencia_sindicato)
base3$ano = as_factor((base3$ano))

base3$presencia_sindicato <- factor(base3$presencia_sindicato, labels= c("No Sindicato", "Sindicato"))

base3 %>%
  filter(!is.na(presencia_sindicato)) %>%
  filter(!(tactica %in% " ") | is.na(tactica)) %>%
  ggplot() +
  aes(x = presencia_sindicato, fill = ano) +
  geom_bar(aes(y = ..count..*100/sum(..count..) ) ) +
  scale_fill_brewer(palette = "PuRd") +
  labs(x = "Presencia de sindicato", y = "Frecuencia", subtitle = "Gráfico 1. Presencia de organizaciónn sindical en huelgas 2016 a 2018", caption = "Elaboración propia con base en datos de huelgas laborales OHL-COES (1979-2018)", fill = "Año") +
  theme_classic()


geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%"), y=ypos))




ggplot(base3, aes(x = presencia_sindicato, fill = ano) ) +  
  geom_bar( aes(y = ..count..*100/sum(..count..) ) ) +
  xlab("Fruit") +
  ylab("Would like this to be percentage") + 
  scale_fill_manual("Condition", values = alpha( c("firebrick", "dodgerblue4", "black"), 1) ) 





freq(base3$presencia_sindicato)



freq(base3)








# Prueba
base4 <- base3 %>% select(presencia_sindicato, 
                          ano)

base4 <- na.omit(base4)



base4$ano = as_numeric(base4$ano)
class(base4$ano)

base4$ano[base4$ano < 2016] <- NA
base4 <- base4[!is.na(base4$ano),]
view(base4)

ctable(base4$presencia_sindicato, base4$ano)


freq(base4$presencia_sindicato)




base3 %>% gather(categoria,presencia_sindicato,-ano) %>% 
  filter(categoria %in% c("Sindicato","No_Sindicatos")) %>% 
  ggplot(aes(x=ano,y=presencia_sindicato,fill=categoria,color=categoria))+geom_line() + 
  scale_x_continuous(limits = c(2016,2018),breaks = c(2016,2017,2018)) +
  labs(title="Grafo",
       subtitle = " 12.079 sindicatos activos y 38.278 sindicatos constituidos",y="sindicatos",x="AÃ±o",
       caption = "Observatorio Sindical") + 
  theme_bw() +
  scale_color_manual("Sindicatos",values=c("red","black"), labels=c("Si","No")) + 
  theme(legend.position = "bottom")


