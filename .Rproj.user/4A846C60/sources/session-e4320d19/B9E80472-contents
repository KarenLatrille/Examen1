rm(list=ls())       # borrar todos los objetos en el espacio de trabajo
options(scipen=999) # valores sin notación científica

pacman::p_load(dplyr, sjmisc, car, sjlabelled, stargazer, haven)

#cargamos la base de datos desde internet
load(url("https://github.com/Kevin-carrasco/metod1-MCS/raw/main/files/data/external_data/latinobarometro2020.RData"))
View(latinobarometro2020)
names(latinobarometro2020)

dim(latinobarometro2020)

find_var(data = latinobarometro2020,"Confianza")
find_var(data = latinobarometro2020,"eleccion")
find_var(data = latinobarometro2020,"desigualdad")
proc_data <- latinobarometro2020 %>% select(p13st_e, # Confianza en el Gobierno
                                            p13st_d, # Confianza en el congreso
                                            p13st_f, # Confianza en el Poder Judicial
                                            p13st_g,# Confianza en los partidos políticos
                                            p13st_i, #Confianza en el presidente
                                            p62st_b,# Elecciones
                                            p72npn,# Nivel de desigualdades
                                            P75NPN_11, #Acceso al poder
                                            reeduc_1,# nivel educacional
                                            sexo,# sexo
                                            edad,# edad
                                            idenpa) # pais 

# Comprobar
names(proc_data)


sjlabelled::get_label(proc_data)


proc_data <- proc_data %>% dplyr::filter(idenpa==152)



frq(proc_data$p13st_e)
frq(proc_data$p62st_b)
frq(proc_data$p72npn)
frq(proc_data$P75NPN_11)



proc_data$p13st_e <- recode(proc_data$p13st_e, "c(-2,-1)=NA")
proc_data$p13st_d <- recode(proc_data$p13st_d, "c(-2,-1)=NA")
proc_data$p13st_f <- recode(proc_data$p13st_f, "c(-2,-1)=NA")
proc_data$p13st_g <- recode(proc_data$p13st_g, "c(-2,-1)=NA")
proc_data$conf_pres <- recode(proc_data$conf_pres, "c(-5,-4,-3,-2,-1,0)=NA")
proc_data$p62st_b <- recode(proc_data$p62st_b, "c(-5,-4,-3,-2,-1)=NA")
proc_data$p72npn <- recode(proc_data$p72npn, "c(-5,-4,-3,-2,-1)=NA")

proc_data$p13st_e <- recode(proc_data$p13st_e, "1=3; 2=2; 3=1; 4=0")
proc_data$p13st_d <- recode(proc_data$p13st_d, "1=3; 2=2; 3=1; 4=0")
proc_data$p13st_f <- recode(proc_data$p13st_f, "1=3; 2=2; 3=1; 4=0")
proc_data$p13st_g <- recode(proc_data$p13st_g, "1=3; 2=2; 3=1; 4=0")
proc_data$p13st_i <- recode(proc_data$conf_pres, "0=0; 1=3; 2=2; 3=1; 4=0")


proc_data <- proc_data %>% rename("conf_gob"=p13st_e, # Confianza en el gobierno
                                  "conf_cong"=p13st_d, # Confianza en el congreso
                                  "conf_jud"=p13st_f, # Confianza en el Poder Judicial
                                  "conf_partpol"=p13st_g, # Confianza en los partidos políticos 
                                  "conf_pres"=p13st_i, #Confianza en el presidente
                                  "ofrec_elec"=p62st_b, #las elecciones ofrecen
                                  "nivel_desi"=p72npn) #Niveles de desigualdad 



proc_data$conf_gob <- set_label(x = proc_data$conf_gob,label = "Confianza: Gobierno")
get_label(proc_data$conf_gob)

proc_data$conf_cong  <- set_label(x = proc_data$conf_cong, label = "Confianza: Congreso")
get_label(proc_data$conf_cong)

proc_data$conf_jud  <- set_label(x = proc_data$conf_jud, label = "Confianza: Poder judicial")
get_label(proc_data$conf_jud)

proc_data$conf_partpol  <- set_label(x = proc_data$conf_partpol, label = "Confianza: Partidos politicos")
get_label(proc_data$conf_partpol)

proc_data$conf_pres  <- set_label(x = proc_data$conf_pres, label = "Confianza: Presidente")
get_label(proc_data$conf_pres)

proc_data$ofrec_elec  <- set_label(x = proc_data$ofrec_elec, label = "elecciones ofrecen")
get_label(proc_data$ofrec_elec)

proc_data$nivel_desi <- set_label(x = proc_data$nivel_desi, label = "Niveles de desigualdad")
get_label(proc_data$nivel_desi)




proc_data$conf_inst <- (proc_data$conf_gob+proc_data$conf_cong+proc_data$conf_jud+proc_data$conf_partpol+proc_data$conf_pres)
summary(proc_data$conf_inst)



get_label(proc_data$conf_inst)

frq(proc_data$conf_gob)
frq(proc_data$conf_pres)

frq(proc_data$conf_inst)
frq(proc_data$reeduc_1)



# recodificacion usando funcion 'recode' de la libreria car
proc_data$reeduc_1 <- car::recode(proc_data$reeduc_1, "c(1)=1; c(2,3)=2; c(4,5)=3; c(6,7)=4")

frq(proc_data$reeduc_1)


proc_data$reeduc_1 <- factor(proc_data$reeduc_1,
                             labels = c(#Analfabeto", "Educacion basica", "Educacion media", "Educacion superior"),
                             levels = c(1, 2, 3, 4))
#renombramiento
proc_data <- rename(proc_data,"educacion"=reeduc_1)

get_label(proc_data$educacion)

proc_data$educacion <- set_label(x = proc_data$educacion,label = "Educación")



frq(proc_data$sexo)
proc_data$sexo <- car::recode(proc_data$sexo, "1=0;2=1")
proc_data$sexo <- factor(proc_data$sexo,
                         labels=c( "Hombre",
                                   "Mujer"),
                         levels=c(0,1))
get_label(proc_data$sexo)
proc_data$sexo <- set_label(x = proc_data$sexo,label = "Sexo")
frq(proc_data$sexo)


frq(proc_data$edad)



proc_data <-as.data.frame(proc_data)
stargazer(proc_data, type="text")

pacman::p_load(sjlabelled,
               dplyr, #Manipulacion de datos
               stargazer, #Tablas
               sjmisc, # Tablas
               summarytools, # Tablas
               kableExtra, #Tablas
               sjPlot, #Tablas y gráficos
               corrplot, # Correlaciones
               sessioninfo, # Información de la sesión de trabajo
               ggplot2) # Para la mayoría de los gráficos
dim(proc_data) # Dimensiones

sjmisc::descr(proc_data,
              show = c("label","range", "mean", "sd", "NA.prc", "n"))%>%
  kable(.,"markdown")
summarytools::dfSummary(proc_data, plain.ascii = FALSE)
view(dfSummary(proc_data, headings=FALSE))


sjt.xtab(proc_data$educacion, proc_data$sexo)




tapply(proc_data$conf_inst, proc_data$educacion, mean)





graph <- ggplot(proc_data, aes(x =educacion, y = conf_inst)) +
  geom_boxplot() +
  labs(x = "Educación", y = "Confianza en instituciones") +
  theme_minimal()

graph

graph <- ggplot(proc_data, aes(x =educacion, y = conf_partpol)) +
  geom_boxplot() +
  labs(x = "Educación", y = "Confianza en partidos politicos") +
  theme_minimal()

graph

graph2 <- sjPlot::plot_stackfrq(dplyr::select(proc_data, conf_gob,
                                              conf_cong,
                                              conf_jud,
                                              conf_partpol),
                                title = "Confianza en instituciones políticas") +
  theme(legend.position="bottom")

graph2

graph3 <- proc_data %>% ggplot(aes(x = conf_inst, fill = sexo)) + 
  geom_bar() +
  xlab("Confianza en instituciones") +
  ylab("Cantidad") + 
  labs(fill="Sexo")+
  scale_fill_discrete(labels = c('Hombre','Mujer'))

graph3

graph4 <- ggplot(proc_data, aes(x = as.numeric(edad))) +
  geom_histogram(binwidth=0.6, colour="black", fill="yellow") +
  theme_bw() +
  xlab("Edad") +
  ylab("Cantidad")

graph4 

summary(proc_data$edad)
proc_data <- proc_data %>% 
  mutate(edad_groups = case_when(edad >=18 & edad<=25 ~ "Entre 18 y 25 años",
                                 edad >=26 & edad<=39 ~ "Entre 26 y 39 años",
                                 edad >=40 & edad<=65 ~ "Entre 40 y 65 años",
                                 edad >65 ~ "Más de 65 años"))

table(proc_data$edad_groups)

datos <- proc_data %>% group_by(educacion, edad_groups) %>% 
  summarise(promedio = mean(conf_inst))

ggplot(datos, aes(x =educacion, y = promedio)) +
  geom_point() +
  labs(x = "Educación", y = "Confianza en instituciones") +
  theme_bw()+
  ylim(0, 7)+
  facet_wrap(~edad_groups)
install.packages("COR")

pacman::p_load(dplyr, # Manipulacion datos
               sjmisc, # Descriptivos
               sjPlot, # Tablas
               sjlabelled, #etiquetas
               kableExtra, #Tablas
               GGally, # Correlaciones
               corrplot) # Correlaciones

options(scipen = 999) # para desactivar notacion cientifica



dim(proc_data) # Dimensiones


sjmisc::descr(proc_data,
              show = c("label","range", "mean", "sd", "NA.prc", "n")) %>%
  kable(.,"markdown")

#Tratamiento casos perdidos 

sum(is.na(proc_data))
colSums(is.na(proc_data))

proc_data <- na.omit(proc_data)
dim(proc_data)

M <- cor(proc_data, use = "complete.obs")
M
install.packages("tab_corr")

sjPlot::tab_corr(latinobarometro2020, 
                 triangle = "lower")

pacman::p_load(dplyr, # Manipulacion datos
               sjmisc, # Descriptivos
               sjPlot, # Tablas
               sjlabelled, #etiquetas
               kableExtra, #Tablas
               GGally, # Correlaciones
               corrplot) # Correlaciones

options(scipen = 999) # para desactivar notacion cientifica

dim(proc_data)
M <- cor(proc_data, use = "complete.obs")
M
