install.packages("readr")
install.packages("rlang")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("agricolae")
install.packages("modeest")
install.packages("rio")
install.packages("caret")
#Librerias
library(readr)
library(rlang)
library(ggplot2)
library(dplyr)
library(tidyverse) #Para Manipulación de datos
library(agricolae) # Sirve para desarrollar tabla de frecuencia
library(modeest) # Para sacar la moda
library(rio) # Exportar datos 
library(caret) # Poder de trabajar para flujo de datos en machine learning

# Para conocer la ruta del archivo #
file.choose()# Determina la Ruta del Archivo
cf_final <- read_delim("Carlos/Maestria/11Estadística-para-científicos-de-datos/Casos-Practicos/Caso-Practico-Final/Caso-Practico-Estad-Carlos-Uyaguari.csv")
summary(cf_final)# Resumen  calidad por cada linea 
View(cf_final)#Permite ver los datos en forma tabular


                            #Proceso de limpieza #

# Selecciono la variable a trabajar #
var_est_1 <- cf_final[,c("id","name","dob","age","sex","race","is_recid","r_offense_date",
                        "decile_score","score_text","is_violent_recid","vr_offense_date",
                        "v_decile_score","v_score_text")]
dim(var_est_1)

# Valida una unica entrada para evitar duplicados #

var_est_2<-var_est_1[!duplicated(var_est_1$id),]
dim(var_est_2)#Valida la dimension de la data

# Cambio el formato a fecha #
var_est_2$dob<-strptime(var_est_2$dob,format="%d/%m/%Y")
var_est_2$r_offense_date<-strptime(var_est_2$r_offense_date, format= "%d/%m/%Y")
var_est_2$vr_offense_date<-strptime(var_est_2$vr_offense_date, format= "%d/%m/%Y")
dim(var_est_2)

# Verifica los datos validos y se lo excluye del analisis (-1) de cada variable

var_est_3<-subset(var_est_2,var_est_2$is_recid != "-1"
                 & var_est_2$is_violent_recid != "-1"
                 & var_est_2$v_decile_score != "-1"
                 & var_est_2$v_decile_score != "-1" 
                 & var_est_2$decile_score != "-1")
dim(var_est_3)
summary(var_est_3)
View(var_est_3)

                          ###Grafico de Sexo###
frec_sex<- table(var_est_3$sex)# Tabla de sexo
frec_sex

color<- c("pink", "blue")
color1<- c("blue", "pink")

pie(frec_sex, labels = c(frec_sex),
    main = "TOTAL HOMBRES Y MUJERES 11.027",col=(color))
legend("topright", c( "HOMBRES:     79.48%","MUJERES :     20.52%"),
       cex = 0.7, fill = color1)

                        ###Grafico de Raza###
frec_race<- table(var_est_3$race)# Tabla de sexo
frec_race

color<- c("Black", "Yellow", "Green", "Blue", "Orange", "Pink")
color1<- c("Black", "Yellow", "Green", "Blue", "Orange", "Pink")

pie(frec_race, labels = c(frec_race),
    main = "TOTAL CATEGORIAS POR RAZAS 11.027",col=(color))
legend("topright", c( "AFRO-AMERICANO:   49.86%",
                      "ASIATICOS:                 0.48%", 
                      "CAUCASICOS:           34.56%", 
                      "HISPANOS:                  9.15%", 
                      "NATIVO-AMERICANOS: 0.33%", 
                      "OTROS:                        5.62%"),
       cex = 0.7, fill = color1)
              
                ### Regresion lineal ###

rl_var_est_4<-lm(is_violent_recid~sex+race,data=var_est_3)
summary(rl_var_est_4)

#Retirar ordenada al Origen##

rl_var_est_4<-lm(is_violent_recid~-1+race+sex,data=var_est_3)
summary(rl_var_est_4)

                      ### Graficos de BOXPLOT ###

boxplot(rl_var_est_4$residuals)
boxplot(var_est_3$is_recid ~ var_est_3$sex)


                            ### Analisis Anova ###
ANOVA<-aov(rl_var_est_4)
summary(ANOVA)
par(mfrow = c(2,2))
plot(ANOVA)


## Tabla de variable categorica de Frecuencia Relativa en porcentaje ##

table(var_est_3$score_text, var_est_3$race)

table(var_est_3$score_text)/sum(!is.na(var_est_3$score_text))*100
table(var_est_3$race)/sum(!is.na(var_est_3$race))*100

ggplot(var_est_3, aes(x=score_text, group=race)) +
  geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count") + 
  geom_text(size=3, aes( label = scales::percent(..prop..), y= ..prop.. ), 
            stat= "count", vjust = -0.7, hjust=0.4)+ 
  labs(y = "Porcentaje en Riesgo de Reincidencia en delitos Generales", x="Riesgo de Reincidencia", fill="Nivel de Reincidencia") +
  facet_grid(~race) + scale_y_continuous(labels=scales::percent)


      ## Riesgo de reincidencia en delitos Violentos vs raza ##

## Tabla de variable categorica de Frecuencia Relativa en porcentaje ##

table(var_est_3$v_score_text, var_est_3$race)

table(var_est_3$v_score_text)/sum(!is.na(var_est_3$score_text))*100
table(var_est_3$race)/sum(!is.na(var_est_3$race))*100

ggplot(var_est_3, aes(x=v_score_text, group=race,)) +
  geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count") + 
  geom_text(size=3, aes( label = scales::percent(..prop..), y= ..prop.. ), 
            stat= "count", vjust = -0.7, hjust=0.4) + 
  labs(y = "Porcentaje en Riesgo de Reincidencia en Delitos Violentos", x="Riesgo de Reincidencia", fill="Reincidencia") + 
  facet_grid(~race) + scale_y_continuous(labels=scales::percent)

                      ## RELACIÓN DE EDAD, SEXO Y RAZA ##

                  ### Histograma de frecuencia de Edad ###

summary(var_est_3$age)
sd(var_est_3$age)# Desviacion Estandar
density(var_est_3$age)

hist(var_est_3$age, prob=TRUE ,main = "HISTOGRAMA DE FRECUENCIA DE EDAD",
     ylab = "FRECUENCIA", col = "lightblue", border = "Red", lwd=3)
x <- seq(min(var_est_3$age), max(var_est_3$age), length = 40)
f <- dnorm(x, mean = mean(var_est_3$age), sd = sd(var_est_3$age))
lines(x, f, col = "red", lwd = 2)
lines(density(var_est_3$age), lwd = 2, col = 'Blue')
legend("topright", col= c("red", "Blue"), 
       legend = c("HISTOGRAMA CON CURVA NORMAL", "HISTOGRAMA CON CURVA DE DENSIDAD"), lwd = 2 )

                ### Histograma de frecuencia de Sexo ###

his_fr_sex<-table(var_est_3$sex)
his_fr_sex

mean(his_fr_sex)
median(his_fr_sex)
quantile(his_fr_sex)

mean(his_fr_sex$Female)
median(his_fr_sex)
quantile(his_fr_sex)
sd(his_fr_sex)# Desviacion Estandar
density(his_fr_sex)


hist(his_fr_sex, prob=TRUE ,main = "HISTOGRAMA DE FRECUENCIA DE SEXO",
     ylab = "FRECUENCIA", col = "Sky blue", border = "Red", lwd=3)
x <- seq(min(his_fr_sex), max(his_fr_sex), length = 90)
f <- dnorm(x, mean = mean(his_fr_sex), sd = sd(his_fr_sex))
lines(x, f, col = "red", lwd = 2)
lines(density(his_fr_sex), lwd = 2, col = 'Blue')
legend("topright", col= c("red", "Blue"), 
       legend = c("HISTOGRAMA CON CURVA NORMAL", "HISTOGRAMA CON CURVA DE DENSIDAD"), lwd = 3 )

              ### Histograma de frecuencia de Raza ###

his_fr_race<-table(var_est_3$race)
his_fr_race

mean(his_fr_race)
median(his_fr_race)
quantile(his_fr_race)

sd(his_fr_race)# Desviacion Estandar
density(his_fr_race)


hist(his_fr_race, prob=TRUE ,main = "HISTOGRAMA DE FRECUENCIA DE RAZA",
     ylab = "FRECUENCIA", col = "Sky blue", border = "Red", lwd=3)
x <- seq(min(his_fr_race), max(his_fr_race), length = 90)
f <- dnorm(x, mean = mean(his_fr_race), sd = sd(his_fr_race))
lines(x, f, col = "red", lwd = 2)
lines(density(his_fr_race), lwd = 2, col = 'Blue')
legend("topright", col= c("red", "Blue"), 
       legend = c("HISTOGRAMA CON CURVA NORMAL", "HISTOGRAMA CON CURVA DE DENSIDAD"), lwd = 3 )

               
    
  





   
