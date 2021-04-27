#Procesamiento ENS
# 1.Cargar librerías ------------------------------------------------------
pacman::p_load(haven,
               tidyverse,
               sjmisc,
               srvyr,
               survey)

# Cargar base de datos

#ENS Formulario 1, 2 y Examenes
ensA <- read_dta("input/data/ENS/ENS2017_1_2_EX.dta")
ensA2 <- read_spss("input/data/ENS/ENS2017_1_2_EX.sav")

# Factor expansion
#Fexp_F1p_Corr (f1)
#Fexp_F2p_Corr (f2)

#ENS Formulario 4
ensB <- read_dta("input/data/ENS/ENS2017_4.dta")
#Factor expansion
#Fexp_F4

#ENS Base Medicamentos
ensC <- read_spss("input/data/ENS/Base_medicamentos_2016/ENS2017_MED.sav")
ensD <- read_spss("input/data/ENS/Base_medicamentos_2016/ENS2017_MED2.sav")


#Indicaciones expansion
#Si las variables que se están analizando son cruces de F1 conF2
#se debe usar ???Fexp_F1F2p_Corr

## Pre_cardio (v1) ----

#Ambas del formulario 1
sjmisc::find_var(ensA, "d1")
table(ensA$d1_f1)

sjmisc::find_var(ensA, "d4")
table(ensA$d4)

#Recodificar

ensA$d1_f1 <- car::recode(ensA$d1_f1, "1=1;2=0; c(-8888,-9999)=NA")
ensA$d4 <- car::recode(ensA$d4, "1=1;2=0;c(-8888,-9999)=NA")

#Calcular n y p 

ensA <- ensA %>% mutate(pre_cardio = case_when(d1_f1 == 1 ~ 1,
                                               d4 == 1 ~ 1,
                                               TRUE~0))
table(ensA$pre_cardio)

ensA %>% select(pre_cardio,d1_f1,d4) %>% subset(pre_cardio == 1)

ensA$pre_cardio <- as.character(ensA$pre_cardio)
str(ensA$pre_cardio)

#Expansion
ensA$fexp_f1p_corr

ensA_cardio <- ensA %>% filter(!is.na(fexp_f1p_corr))%>%  as_survey_design(ids = 1, weights = fexp_f1p_corr)

options(survey.lonely.psu = "certainty" )

v1 <- ensA_cardio %>% group_by(pre_cardio) %>% summarize(proportion = survey_mean(vartype = "ci",na.rm = TRUE),
                                                         total = survey_total(vartype = "ci",na.rm = TRUE))



## Pre_resp (v2) ----

#Ambas del formulario 2
sjmisc::find_var(ensA, "m9p17a")
table(ensA$m9p17a) #-8888


sjmisc::find_var(ensA, "m9p18a")
table(ensA$m9p18a)

#Recodificar

ensA$m9p17a <- car::recode(ensA$m9p17a, "1=1;2=0; -8888=NA")
ensA$m9p18a <- car::recode(ensA$m9p18a, "1=1;2=0;-8888=NA")

#Calcular n y p 

ensA <- ensA %>% mutate(pre_resp = case_when(m9p17a == 1 ~ 2,
                                             m9p18a == 1 ~ 2,
                                             TRUE~0))
table(ensA$pre_resp)

ensA %>% select(pre_resp,m9p17a,m9p18a) %>% subset(pre_resp == 1)

ensA$pre_resp <- as.character(ensA$pre_resp)

str(ensA$pre_resp)

#Expansion
ensA$fexp_f2p_corr #Formulario 2

ensA_resp <- ensA %>% filter(!is.na(fexp_f2p_corr))%>%  as_survey_design(ids = 1, weights = fexp_f2p_corr)

options(survey.lonely.psu = "certainty" )

v2 <- ensA_resp %>% group_by(pre_resp) %>% summarize(proportion = survey_mean(vartype = "ci",na.rm = TRUE),
                                                     total = survey_total(vartype = "ci",na.rm = TRUE))

## Pre_hta (v3) ----

#Formulario 1
sjmisc::find_var(ensA, "h2")
table(ensA$h2)

#1 y 2 = 1
#3 y 4 = 0

#Recodificar

ensA$h2 <- car::recode(ensA$h2, "c(1,2)=1;c(3,4)=0")

#Calcular n y p 

ensA <- ensA %>% mutate(pre_hta = case_when(h2 == 1 ~ 3,
                                            TRUE~0))
table(ensA$pre_hta)

ensA %>% select(pre_hta,h2) %>% subset(pre_hta == 1)

ensA$pre_hta <- as.character(ensA$pre_hta)
str(ensA$pre_hta)

#Expansion
ensA$fexp_f1p_corr

ensA_hta <- ensA %>% filter(!is.na(fexp_f1p_corr))%>%  as_survey_design(ids = 1, weights = fexp_f1p_corr)

options(survey.lonely.psu = "certainty" )

v3 <- ensA_hta %>% group_by(pre_hta) %>% summarize(proportion = survey_mean(vartype = "ci",na.rm = TRUE),
                                                   total = survey_total(vartype = "ci",na.rm = TRUE))

## Pre_diabetes (v4)----

#Formulario 1
sjmisc::find_var(ensA, "di3")

table(ensA$di3)


#Recodificar

ensA$di3 <- car::recode(ensA$di3, "1=1;c(2,3)=0")

#Calcular n y p 

ensA <- ensA %>% mutate(pre_diabetes = case_when(di3 == 1 ~ 4,
                                                 TRUE~0))
table(ensA$pre_diabetes)

ensA %>% select(pre_diabetes,di3) %>% subset(pre_diabetes == 4)

ensA$pre_diabetes <- as.character(ensA$pre_diabetes)
str(ensA$pre_diabetes)

#Expansion
ensA$fexp_f1p_corr #Formulario 1

ensA_diabetes <- ensA %>% filter(!is.na(fexp_f1p_corr)) %>%
  as_survey_design(ids = 1, weights = fexp_f1p_corr)

options(survey.lonely.psu = "certainty" )

v4 <- ensA_diabetes %>% group_by(pre_diabetes) %>% summarize(proportion = survey_mean(vartype = "ci",na.rm = TRUE),
                                                             total = survey_total(vartype = "ci",na.rm = TRUE))

## Pre_imuno (v5) ----
ensC2 <- merge(ensA2, ensC, by = "IdEncuesta")

#m9p19a - #Formulario 2
#se ocupa spss por error dta
sjmisc::find_var(ensA2, "m9p19A")

table(ensA2$m9p19A) #-8888

ensC2$m9p19A <- car::recode(ensC2$m9p19A, "1=5;2=0; -8888=NA")

table(ensC2$m9p19A)


#m9p13a_esp --> "VIH VIH/SIDA vih VIH/Sida " - Formulario 2
sjmisc::find_var(ensC2, "m9p13A")

table(ensC2$m9p13A_esp, useNA = "ifany") 

ensC2 <- ensC2 %>% mutate(m9p13A_esp = case_when(m9p13A_esp == "VIH" ~ 5,
                                                 m9p13A_esp == "VIH/SIDA" ~ 5,
                                                 TRUE~0)) 

# Glucocorticoides - ENS C (base1 medicamentos)
#H02AB01
# H02AB02
# H02AB04
# H02AB07
# H02AB08
# H02AB09
# H02AB13

table(ensC$H02AB01)
table(ensC$H02AB13)

ensC2 <- ensC2 %>% mutate(corticoide = case_when(H02AB01 == 1 ~ 5,
                                                 H02AB02 == 1 ~ 5,
                                                 H02AB04 == 1 ~ 5,
                                                 H02AB07 == 1 ~ 5,
                                                 H02AB08 == 1 ~ 5,
                                                 H02AB09 == 1 ~ 5,
                                                 H02AB13 == 1 ~ 5,
                                                 TRUE~0)) 
table(ensC2$corticoide)

#Inmunosupresores
# L04AA06
# L04AA13
# L04AB06
# L04AD01
# L04AX01
# L04AX02
# L04AX03

table(ensC2$L04AA06)

ensC2 <- ensC2 %>% mutate(inmunosupresor = case_when(L04AA06 == 1 ~ 5,
                                                     L04AA13 == 1 ~ 5,
                                                     L04AB06 == 1 ~ 5,
                                                     L04AD01 == 1 ~ 5,
                                                     L04AX01 == 1 ~ 5,
                                                     L04AX02 == 1 ~ 5,
                                                     L04AX03 == 1 ~ 5,
                                                     TRUE~0)) 
table(ensC2$inmunosupresor)

#Base de medicamentos ¿Que factor ocupar?
summary(ensC$Fexp_F2p_Corr)
summary(ensC$Fexp_F1p_Corr)

#Calcular n y p 

ensC2 <- ensC2 %>% mutate(pre_inmuno = case_when(m9p13A_esp == 5 ~ 5,
                                                 m9p19A == 5 ~ 5,
                                                 corticoide == 5 ~ 5,
                                                 inmunosupresor == 5 ~ 5,
                                                 TRUE~0))


table(ensC2$pre_inmuno)


ensC2$pre_inmuno <- as.character(ensC2$pre_inmuno)

str(ensC2$pre_inmuno)

#Expansion
ensC2$Fexp_F2p_Corr.x #Formulario 2 (factor de expansion mas resctictvo manda)

ensC_inmuno <- ensC2 %>% filter(!is.na(Fexp_F2p_Corr.x))%>%  as_survey_design(ids = 1, weights = Fexp_F2p_Corr.x)

options(survey.lonely.psu = "certainty" )

v5 <- ensC_inmuno %>% group_by(pre_inmuno) %>% summarize(proportion = survey_mean(vartype = "ci",na.rm = TRUE),
                                                         total = survey_total(vartype = "ci",na.rm = TRUE))
## Pre_onco (v6) ----
#Formulario 2
# m9p2e
sjmisc::find_var(ensA, "m9p2e")
table(ensA$m9p2e) #1 y 2
ensA$m9p2e <- car::recode(ensA$m9p2e, "1=6;c(NA,2)=0") #NA como 0para poder calc prop

# m9p3e
table(ensA$m9p3e) #1 y 2
ensA$m9p3e <- car::recode(ensA$m9p3e, "1=6;c(NA,2)=0") #NA como 0para poder calc prop

# m9p4e
table(ensA$m9p4e) #-8888 y 2
ensA$m9p4e <- car::recode(ensA$m9p4e, "c(NA,2)=0; -8888 = NA") #NA como 0para poder calc prop

# m9p5e
table(ensA$m9p5e) #1, 2, -8888
ensA$m9p5e <- car::recode(ensA$m9p5e, "1=6;c(NA,2)=0;-8888 = NA") 

# m9p6e
table(ensA$m9p6e) #1, 2, -8888
ensA$m9p6e <- car::recode(ensA$m9p6e, "1=6;c(NA,2)=0;-8888 = NA") 

# m9p7e
table(ensA$m9p7e) #1, 2 y -8888
ensA$m9p7e <- car::recode(ensA$m9p7e, "1=6;c(NA,2)=0;-8888 = NA") 

# m9p8e
table(ensA$m9p8e) #1 y 2, -8888
ensA$m9p8e <- car::recode(ensA$m9p8e, "1=6;c(NA,2)=0;-8888 = NA") 


#Calcular n y p 

ensA <- ensA %>% mutate(pre_onco = case_when(m9p2e == 6 ~ 6,
                                             m9p3e == 6 ~ 6,
                                             m9p4e == 6 ~ 6,
                                             m9p5e == 6 ~ 6,
                                             m9p6e == 6 ~ 6,
                                             m9p7e == 6 ~ 6,
                                             m9p8e == 6 ~ 6,
                                             TRUE~0))
table(ensA$pre_onco)


ensA$pre_onco <- as.character(ensA$pre_onco)
str(ensA$pre_onco)

#Expansion
ensA$fexp_f2p_corr # Formulario 2

ensA_onco <- ensA %>% filter(!is.na(fexp_f2p_corr))%>%  as_survey_design(ids = 1, weights = fexp_f2p_corr)

options(survey.lonely.psu = "certainty" )


v6 <- ensA_onco %>% group_by(pre_onco) %>% summarize(proportion = survey_mean(vartype = "ci",na.rm = TRUE),
                                                     total = survey_total(vartype = "ci",na.rm = TRUE))

## Pre_renal (v7)----
#fg_ckdschwartz_diminuido_30 --> EXAMENES 2 FORMULARIO 2

#Ambas del formulario 1
sjmisc::find_var(ensA, "fg_ckdschwartz_diminuido_30")

table(ensA$fg_ckdschwartz_diminuido_30)


#Calcular n y p 

ensA <- ensA %>% mutate(pre_renal = case_when(fg_ckdschwartz_diminuido_30 == 1 ~ 7,
                                              TRUE~0))
table(ensA$pre_renal)


ensA$pre_renal <- as.character(ensA$pre_renal)

str(ensA$pre_renal)

#Expansion
ensA$fexp_ex2p_corr #factor examenes 2 y formulario2

ensA_renal <- ensA %>% filter(!is.na(fexp_ex2p_corr))%>%  as_survey_design(ids = 1, weights = fexp_ex2p_corr)

options(survey.lonely.psu = "certainty" )


v7 <- ensA_renal %>% group_by(pre_renal) %>% summarize(proportion = survey_mean(vartype = "ci",na.rm = TRUE),
                                                       total = survey_total(vartype = "ci",na.rm = TRUE))


## Pre_fuma (v8)----
#ta3 --> FORMULARIO 1

#Ambas del formulario 1
sjmisc::find_var(ensA, "ta3")

table(ensA$ta3)


#Calcular n y p 

ensA <- ensA %>% mutate(pre_fuma = case_when(ta3 == 1 ~ 8,
                                             TRUE~0))
table(ensA$pre_fuma)

ensA$pre_fuma <- as.character(ensA$pre_fuma)

str(ensA$pre_fuma)

#Expansion
ensA$fexp_f1p_corr #factor examenes 2 y formulario2

ensA_fuma <- ensA %>% filter(!is.na(fexp_f1p_corr))%>%  as_survey_design(ids = 1, weights = fexp_f1p_corr)

options(survey.lonely.psu = "certainty" )

v8 <- ensA_fuma %>% group_by(pre_fuma) %>% summarize(proportion = survey_mean(vartype = "ci",na.rm = TRUE),
                                                     total = survey_total(vartype = "ci",na.rm = TRUE))


## Pre_obeso (v10)----
#ta3 --> FORMULARIO 1

#Ambas del formulario 1
sjmisc::find_var(ensA, "n2")

table(ensA$n2)


#Calcular n y p 

ensA <- ensA %>% mutate(pre_obesidad = case_when(n2 == 4 ~ 10,
                                             TRUE~0))
table(ensA$pre_obesidad)

ensA$pre_obesidad <- as.character(ensA$pre_obesidad)

str(ensA$pre_obesidad)

#Expansion
ensA$fexp_f1p_corr #factor examenes 2 y formulario2

ensA_obesidad <- ensA %>% filter(!is.na(fexp_f1p_corr))%>%  as_survey_design(ids = 1, weights = fexp_f1p_corr)

options(survey.lonely.psu = "certainty" )

v10 <- ensA_obesidad %>% group_by(pre_obesidad) %>%
  summarize(proportion = survey_mean(vartype = "ci",na.rm = TRUE),
            total = survey_total(vartype = "ci",na.rm = TRUE))


##### Ninguna de las anteriores (v9)----

# Formularios todos --> se cruza f1,f2 y ex2
#Si las variables que se están analizando son cruces de Ex2 con F2, se debe usar
#Fexp_F1F2EX2p_Corr

#Construir variable de nada
table(ensA$pre_cardio)
table(ensA$pre_resp)
table(ensA$pre_hta)
table(ensA$pre_diabetes)
table(ensC2$pre_inmuno) #ensC2
table(ensA$pre_onco)
table(ensA$pre_renal)
table(ensA$pre_fuma)

#Merge
ens_sano <- merge(ensA, ensC2, by.x = "idencuesta", by.y = "IdEncuesta")

table(ens_sano$pre_cardio)

#Crear variable
#¿Condicional?

ens_sano <- ens_sano %>% mutate(pre_ninguna = case_when(pre_cardio == 0 &
                                                          pre_resp == 0 &
                                                          pre_hta == 0 &
                                                          pre_diabetes == 0 &
                                                          pre_inmuno == 0 &
                                                          pre_onco == 0 &
                                                          pre_renal == 0 &
                                                          pre_obesidad == 0 &
                                                          pre_fuma == 0 ~ 9,
                                                        TRUE~0))


table(ens_sano$pre_ninguna)


ens_sano$pre_ninguna <- as.character(ens_sano$pre_ninguna)

str(ens_sano$pre_ninguna)

#Expansion
ens_sano$fexp_f1f2ex2p_corr #Fexp_F1F2EX2p_Corr

ensA_sano <- ens_sano %>% filter(!is.na(fexp_f1f2ex2p_corr))%>%  as_survey_design(ids = 1, weights = fexp_f1f2ex2p_corr)

options(survey.lonely.psu = "certainty" )

v9 <- ensA_sano %>% group_by(pre_ninguna) %>% summarize(proportion = survey_mean(vartype = "ci",na.rm = TRUE),
                                                        total = survey_total(vartype = "ci",na.rm = TRUE))

#### Tabla de preexistencias

v1<-v1[2,2:7]
v2<-v2[2,2:7]
v3<-v3[2,2:7]
v4<-v4[2,2:7]
v5<-v5[2,2:7]
v6<-v6[2,2:7]
v7<-v7[2,2:7]
v8<-v8[2,2:7]
v9<-v9[2,2:7]
v10<-v10[2,2:7]

v<-rbind(v1,v2,v3,v4,v5,v6,v7,v8,v10,v9)
v$variable<-c("Enfermedades Cardiovasculares",
              "Enfermedad Respiratoria Crónica",
              "Hipertensión Arterial",
              "Diabetes",
              "Inmunodeficiencia",
              "Paciente Oncológico",
              "Enfermedad Renal",
              "Fumador",
              "Obesidad",
              "Ninguna de las anteriores")
v$preexistencia <- c("pre_cv",
                     "pre_respiratoria",
                     "pre_hipertension",
                     "pre_diabetes",
                     "pre_inmuno",
                     "pre_oncologico",
                     "pre_renal",
                     "pre_fuma",
                     "pre_obesidad",
                     "pre_ninguna")

ens_pre<-v %>% select(c(1,2,3,4,5,6,7,8))
ens_pre

# Guardar base de datos

save(ens_pre, file = "output/data/ENS2017-preexistencias.RData")


## ENS + cruces

## Pre_obeso (v10)----
#ta3 --> FORMULARIO 1

#Ambas del formulario 1
sjmisc::find_var(ensA, "as5")

table(ensA$as5_1)


#Calcular n y p 
ensA$as5_1 <- as.numeric(ensA$as5_1)
ensA$prevision <- car::recode(ensA$as5_1, recodes = c("1='FONASA A';
                                                       2='FONASA B';
                                                       3='FONASA C';
                                                       4='FONASA D';
                                                       5='FONASA no sabe';
                                                       6='FFAA y Orden';
                                                       7='ISAPRE';
                                                       8='Ninguno';
                                                       9='Otro'"), as.factor = T,
                              levels = c('FONASA A',
                                         'FONASA B',
                                         'FONASA C',
                                         'FONASA D',
                                         'FONASA no sabe',
                                         'FFAA y Orden',
                                         'ISAPRE',
                                         'Ninguno',
                                         'Otro'))
  
table(ensA$prevision)


ensA$prevision_cat <- car::recode(ensA$as5_1, recodes = c("c(1,2,3,4,5)='FONASA';
                                                       6='FFAA y Orden';
                                                       7='ISAPRE';
                                                       8='Ninguno';
                                                       9='Otro'"), as.factor = T,
                              levels = c('FONASA',
                                         'FFAA y Orden',
                                         'ISAPRE',
                                         'Ninguno',
                                         'Otro'))

table(ensA$prevision_cat)


ensA$prevision <- as.character(ensA$prevision)
ensA$prevision_cat <- as.character(ensA$prevision_cat)

#Expansion
ensA$fexp_f1p_corr #factor examenes 2 y formulario2

ensA <- ensA %>% filter(!is.na(fexp_f1p_corr))%>%  as_survey_design(ids = 1, weights = fexp_f1p_corr)

options(survey.lonely.psu = "certainty" )

v11 <- ensA %>% group_by(prevision,pre_cardio) %>%
  summarize(proportion = survey_mean(vartype = "ci",na.rm = TRUE),
            total = survey_total(vartype = "ci",na.rm = TRUE)) %>% 
  mutate(preexistencia = pre_cardio)

v12 <- ensA %>% group_by(prevision,pre_resp) %>%
  summarize(proportion = survey_mean(vartype = "ci",na.rm = TRUE),
            total = survey_total(vartype = "ci",na.rm = TRUE)) %>% 
  mutate(preexistencia = pre_resp)

v13 <- ensA %>% group_by(prevision,pre_hta) %>%
  summarize(proportion = survey_mean(vartype = "ci",na.rm = TRUE),
            total = survey_total(vartype = "ci",na.rm = TRUE)) %>% 
  mutate(preexistencia = pre_hta)


v14 <- ensA %>% group_by(prevision,pre_diabetes) %>%
  summarize(proportion = survey_mean(vartype = "ci",na.rm = TRUE),
            total = survey_total(vartype = "ci",na.rm = TRUE)) %>% 
  mutate(preexistencia = pre_diabetes)


v15 <- ensA %>% group_by(prevision,pre_inmuno) %>%
  summarize(proportion = survey_mean(vartype = "ci",na.rm = TRUE),
            total = survey_total(vartype = "ci",na.rm = TRUE)) %>% 
  mutate(preexistencia = pre_inmuno)

v16 <- ensA %>% group_by(prevision,pre_onco) %>%
  summarize(proportion = survey_mean(vartype = "ci",na.rm = TRUE),
            total = survey_total(vartype = "ci",na.rm = TRUE)) %>% 
  mutate(preexistencia = pre_onco)

v17 <- ensA %>% group_by(prevision,pre_renal) %>%
  summarize(proportion = survey_mean(vartype = "ci",na.rm = TRUE),
            total = survey_total(vartype = "ci",na.rm = TRUE))%>% 
  mutate(preexistencia = pre_renal)

v18 <- ensA %>% group_by(prevision,pre_fuma) %>%
  summarize(proportion = survey_mean(vartype = "ci",na.rm = TRUE),
            total = survey_total(vartype = "ci",na.rm = TRUE)) %>% 
  mutate(preexistencia = pre_fuma)

v19 <- ensA %>% group_by(prevision,pre_obesidad) %>%
  summarize(proportion = survey_mean(vartype = "ci",na.rm = TRUE),
            total = survey_total(vartype = "ci",na.rm = TRUE)) %>% 
  mutate(preexistencia = pre_obesidad)

v20 <- ensA %>% group_by(prevision,pre_ninguna) %>%
  summarize(proportion = survey_mean(vartype = "ci",na.rm = TRUE),
            total = survey_total(vartype = "ci",na.rm = TRUE)) %>% 
  mutate(preexistencia = pre_ninguna)

v_prev<-rbind(v11,v12,v13,v14,v16,v17,v18,v19)


v_prev <- v_prev %>%
  select(prevision, preexistencia, total, proportion, proportion_low, proportion_upp) %>% 
  mutate(preexistencia = case_when( preexistencia == 1 ~ "Enfermedades Cardiovasculares",
                                    preexistencia == 2 ~ "Enfermedad Respiratoria Crónica",
                                    preexistencia == 3 ~ "Hipertensión Arterial",
                                    preexistencia == 4 ~ "Diabetes",
                                    preexistencia == 5 ~ "Inmunodeficiencia",
                                    preexistencia == 6 ~ "Paciente Oncológico",
                                    preexistencia == 7 ~ "Enfermedad Renal",
                                    preexistencia == 8 ~ "Fumador",
                                    preexistencia == 10 ~ "Obesidad",
                                    preexistencia == 9 ~"Ninguna de las anteriores")) %>% 
  filter(!is.na(preexistencia))

# Guardar base de datos

save(ens_pre, v_prev, file = "output/data/ENS2017-preexistencias.RData")
