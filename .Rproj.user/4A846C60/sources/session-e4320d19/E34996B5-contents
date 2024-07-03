load(url("https://github.com/Kevin-carrasco/metod1-MCS/raw/main/files/data/external_data/latinobarometro2020.RData"))
View(latinobarometro2020)
names(latinobarometro2020)


pacman::p_load(dplyr, # Manipulacion datos
               sjmisc, # Descriptivos
               sjPlot, # Tablas
               sjlabelled, #etiquetas
               kableExtra, #Tablas
               GGally, # Correlaciones
               corrplot,
               screenreg,
               knitreg) # Correlaciones

options(scipen = 999) # para desactivar notacion cientifica


dim(latinobarometro2020) # Dimensiones

proc_lati <- latinobarometro2020 %>% select(edad=edad, conf_gob=p13st_e, conf_cong=p13st_d, conf_jud=p13st_f, confi_part=p13st_g, sexo=sexo, nivel=reeduc_1)
sjmisc::descr(proc_lati,
              show = c("label","range", "mean", "sd", "NA.prc", "n")) %>%
  kable(.,"markdown")

M <- cor(proc_lati, use = "complete.obs")
M

sjPlot::tab_corr(proc_lati, 
                 triangle = "lower")

sjPlot::tab_corr(proc_lati, 
                 na.deletion = "pairwise", # espeficicamos tratamiento NA
                 triangle = "lower")

corrplot.mixed(M)


proc_lati$nivel <- car::recode(proc_lati$nivel,"c(1)=0; c(2,3)=1; c(4,5)=2; c(6,7)=3")

proc_lati$nivel <- set_labels(proc_lati$nivel,
                                  labels=c( "Analfabeto"=0,
                                            "Educacion básica"=1,
                                            "Educación media"=2,
                                            "Educación superior"=3))

frq(proc_lati$nivel)


proc_lati$nivel <- as_factor(proc_lati$nivel)
proc_lati$sexo <- as_factor(proc_lati$sexo)

proc_lati <- na.omit(proc_lati)

reg1 <- lm(confi_part ~ 1, data=proc_lati)

stargazer(reg1, type="text")

#Variable dependiente: Participación Política
summary(proc_lati$confi_part)

proc_lati <- proc_lati%>%
  mutate(sexo = case_when(sexo == 1 ~ 1, #Para Hombres
                          sexo == 2 ~ 0)) #Para Mujeres
fit01 <- lm(confi_part ~ edad + nivel + sexo, data=proc_lati)
fit02 <- lm(confi_part ~ edad + nivel, data=proc_lati)
fit03 <- lm(confi_part ~ edad, data=proc_lati)

#Verificar el número de coeficientes en un modelo para asegurarse de que custom.coef.names tenga la longitud correcta
length(coef(fit01))

# Definir los nombres personalizados para los coeficientes
custom_coef_names <- c("Intercepto", "sexo (mujer)", "nivel", "edad")
install.packages("screenreg")
#Mostrar los resultados de los modelos con nombres personalizados para los modelos y coeficientes
screenreg(list(fit01, fit02, fit03),
          custom.model.names = c("Modelo 1", "Modelo 2", "Modelo 3", "Modelo 4", "Modelo 5"),
          custom.coef.names = custom_coef_names)





knitreg(list(fit01, fit02, fit03), 
        custom.model.names = c("Modelo 1",
                               "Modelo 2",
                               "Modelo 3"),
        custom.note = "*** p < 0.001; ** p < 0.01; * p < 0.05",
        custom.coef.names = c("Intercepto", 
                              "Edad",
                              "Educación media <br> <i>(Ref. Analfabeto)</i>", 
                              "Educación superior", 
                              "Mujer <br> <i>(Ref. Hombre)</i>"),
        caption = "Confianza a partidos politcos",
        caption.above = TRUE)
install.packages("Knitreg")







reg2 <- lm(confi_part ~ edad, data=proc_data)
reg3 <- lm(cohesion_barrial ~ educacion, data=proc_data)
reg4 <- lm(cohesion_barrial ~ sexo, data=proc_data)

knitreg(list(reg2, reg3, reg4), 
        custom.model.names = c("Modelo 1",
                               "Modelo 2",
                               "Modelo 3"),
        custom.note = "*** p < 0.001; ** p < 0.01; * p < 0.05",
        custom.coef.names = c("Intercepto", 
                              "Edad",
                              "Educación media <br> <i>(Ref. Ed. básica)</i>", 
                              "Educación superior", 
                              "Mujer <br> <i>(Ref. Hombre)</i>"),
        caption = "Cohesión barrial",
        caption.above = TRUE)
