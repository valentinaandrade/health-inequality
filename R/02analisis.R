# Code 0: Procesamiento ---------------------------------------------------

# 1. Cargar paquetes ---------------------------------------------------------
pacman::p_load(dplyr, haven, tidyverse, stringr, sjmisc,sjlabelled)


# 2. Cargar bases de datos ------------------------------------------------
## ISSP Module Social Inequlities
# issp99 <- read_dta("input/data/original/ISSP1999.dta") #remplazar por cep39
issp99 <- read_spss("input/data/original/cep39mar-abr2000.sav") 
# issp09 <- read_dta("input/data/original/ISSP2009.dta") #remplazar por cep59
issp09 <- read_spss("input/data/original/cep59may-jun2009.sav") 
issp19 <- read_dta("input/data/original/ISSP2019.dta")
issp11 <- read_dta("input/data/original/ISSP2011.dta")

#  3. Explorar -----------------------------------------------------------------
names(issp99)
issp99 <- issp99 %>% mutate(year = 1999,factor = pond)
issp09 <- issp09 %>% mutate(year = 2009, factor = Fcorr)
issp19 <- issp19 %>% mutate(year = 2019, factor = FACTOR)
issp11 <- issp11 %>% mutate(year = 2011, factor = WEIGHT) %>% 
  filter(C_ALPHAN == "CL")

# 4. Substancial variables ------------------------------------------------------

# 4.1 Justicia educacion y salud ------------------------------------------------------------------

# 1999 ----------------------------------------------------------------------------------------#

just_vlabels<- get_labels(issp19$M2_P9A)[1:5]

sjmisc::frq(issp99$te10a)
issp99$justsalud<- issp99$te10a #salud
issp99$justsalud[issp99$justsalud %in% c(8,9)] <- NA
issp99$justsalud <- factor(issp99$justsalud,labels=just_vlabels)
issp99$justsalud <- fct_rev(issp99$justsalud)
sjmisc::frq(issp99$justsalud)


# 2009 ----------------------------------------------------------------------------------------#
sjmisc::frq(issp09$TE2P18_A)
issp09$justsalud<- issp09$TE2P18_A #salud
issp09$justsalud[issp09$justsalud %in% c(8,9)] <- NA
issp09$justsalud <- factor(issp09$justsalud,labels=just_vlabels)
issp09$justsalud <- fct_rev(issp09$justsalud)
sjmisc::frq(issp09$justsalud)


# 2019 ----------------------------------------------------------------------------------------#
sjmisc::frq(issp19$M2_P9A)
issp19$justsalud<- issp19$M2_P9A #salud
issp19$justsalud[issp19$justsalud %in% c(88,99)] <- NA
issp19$justsalud <- factor(issp19$justsalud,labels=just_vlabels)
issp19$justsalud <- fct_rev(issp19$justsalud)
sjmisc::frq(issp19$justsalud)

# 2011 --------------------------------------------------------------------
sjmisc::frq(issp11$V10)
issp11$justsalud<- issp11$V10 #salud
issp11$justsalud[issp11$justsalud %in% c(8,9)] <- NA
issp11$justsalud <- factor(issp11$justsalud,labels=just_vlabels)
issp11$justsalud <- fct_rev(issp11$justsalud)
sjmisc::frq(issp11$justsalud)


# 4.2 Justicia educacion --------------------------------------------------
# 1999 ----------------------------------------------------------------------------------------#
sjmisc::frq(issp99$te10b)
issp99$justeduca<- issp99$te10b #educacion
issp99$justeduca[issp99$justeduca %in% c(8,9)] <- NA
issp99$justeduca <- factor(issp99$justeduca,labels=just_vlabels)
issp99$justeduca <- fct_rev(issp99$justeduca)
sjmisc::frq(issp99$justeduca)

# 2009 ----------------------------------------------------------------------------------------#
sjmisc::frq(issp09$TE2P18_B)
issp09$justeduca<- issp09$TE2P18_B #educacion
issp09$justeduca[issp09$justeduca %in% c(8,9)] <- NA
issp09$justeduca <- factor(issp09$justeduca,labels=just_vlabels)
issp09$justeduca <- fct_rev(issp09$justeduca)
sjmisc::frq(issp09$justeduca)

# 2019 ----------------------------------------------------------------------------------------#
sjmisc::frq(issp19$M2_P9B)
issp19$justeduca<- issp19$M2_P9B
issp19$justeduca[issp19$justeduca %in% c(88,99)] <- NA
issp19$justeduca <- factor(issp19$justeduca,labels=just_vlabels)
issp19$justeduca <- fct_rev(issp19$justeduca)
sjmisc::frq(issp19$justeduca)




# 4.2 Redistribution (red) ----------------------------------------------------------

## 1999: Variable te7b
issp99$red<- as.numeric(issp99$te7b)

issp99 <- issp99 %>% mutate(red=ifelse(red %in% c(8,9),NA,red),
                            red=rec(red,rec = "rev"),
                            red=factor(red,labels = c('Muy en desacuerdo','En desacuerdo','Ni de acuerdo ni desacuerdo','De acuerdo','Muy de acuerdo'),ordered = T))
table(issp99$red)  

## 2009: Variable TE2P16_B
table(issp09$TE2P16_B)
issp09$red<- as.numeric(issp09$TE2P16_B)
issp09$red=ifelse(issp09$red %in% c(8,9),NA,issp09$red)
issp09 <- issp09 %>% mutate(red=rec(red,rec = "rev"),
                            red=factor(red,labels = c('Muy en desacuerdo','En desacuerdo','Ni de acuerdo ni desacuerdo','De acuerdo','Muy de acuerdo'),ordered = T))
table(issp09$red)  

## 2019: Variable M2_P4_1
sjmisc::find_var(issp19, "diferencias")
table(issp19$M2_P4_2)
issp19$red<- as.numeric(issp19$M2_P4_2)
issp19$red=ifelse(issp19$red %in% c(8,9),NA,issp19$red)
issp19 <- issp19 %>% mutate(red=rec(red,rec = "rev"),
                            red=factor(red,labels = c('Muy en desacuerdo','En desacuerdo','Ni de acuerdo ni desacuerdo','De acuerdo','Muy de acuerdo'),ordered = T))
table(issp19$red)  


# 4.4 Escala Izquierda Derecha --------------------------------------------------------------------
pospol_label<- c("Derecha","Centro","Izquierda","Independiente","Ninguna")

#1999
sjmisc::frq(issp99$p7)
issp99$pospol <- issp99$p7
issp99$pospol <- car::recode(issp99$pospol,"c(1,2)=1;3=2;c(4,5)=3;6=4;7=5;c(8,9)=NA")
issp99$pospol <- factor(issp99$pospol,labels = pospol_label)
table(issp99$pospol)

sjmisc::frq(issp09$MBP16)
issp09$pospol <- issp09$MBP16 
issp09$pospol <- car::recode(issp09$pospol,"c(1,2)=1;3=2;c(4,5)=3;6=4;7=5;c(8,9)=NA")
issp09$pospol <- factor(issp09$pospol,labels = pospol_label)
table(issp09$pospol)
# sjmisc::frq(issp09$POS_POL)

sjmisc::frq(issp19$MB_P14)
issp19$pospol <- issp19$MB_P14 
issp19$pospol <- car::recode(issp19$pospol,"c(1,2)=1;3=2;c(4,5)=3;6=4;7=5;c(8,9)=NA")
issp19$pospol <- factor(issp19$pospol,labels = pospol_label)
table(issp19$pospol)


# 5. Controls -------------------------------------------------------------
# 5.1 Income (pchhinc y pchhinc_a) --------------------------------------------------------------
# browseURL(url = "https://www.ine.gub.uy/indicadores?indicadorCategoryId=11421")
## 1999
### Variable hompop: How many persons in household 
### rincome (respondent income) and incomer (Family income by decile 1-10)
issp99$hompop <- as.numeric(issp99$dat_26a)
issp99$hompop[issp99$hompop == 99] <- NA
issp99$hompop[issp99$hompop == 0] <- 1

sjmisc::find_var(issp99,"income")
issp99$income <- as.numeric(issp99$dat_23)
issp99 <- issp99 %>% mutate(income = case_when(
  dat_23	 == 1 ~45000,
  dat_23	 == 2 ~105500,
  dat_23	 == 3 ~135500,
  dat_23	 == 4 ~165500,
  dat_23	 == 5 ~195500,
  dat_23	 == 6 ~225500,
  dat_23	 == 7 ~265500,
  dat_23	 == 8 ~340500,
  dat_23	 == 9 ~495500,
  dat_23	 == 10~800500,
  dat_23	 == 11~8000000,
  dat_23	 == 12~10750000,
  dat_23	 == 13~16000000,
  dat_23	 == 14~1500000,
  dat_23	 == 97 ~NA_real_,
  dat_23	 == 98 ~NA_real_,
  dat_23	 == 99 ~NA_real_),
  pchhinc=income/hompop,
  pchhinc_a = pchhinc*186.62/100) #Art. Politicas Publicas UC (linea 149 dofile)
# [cambiamos el IPC acumulado de 2009 por el de diciembre 2018 que es 186.62] - (JI - 13 nov 2020) 

summary(issp99$pchhinc)
## 2009
### Variable HOMPOP: How many persons in household 
### CL_RINC:income specific in Chile
issp09$HOMPOP <- as.numeric(issp09$DDP35)
issp09$HOMPOP[issp09$HOMPOP == 99] <- NA
issp09$HOMPOP[issp09$HOMPOP == 0] <- 1

sjmisc::find_var(issp09,"income")
issp09$income <- as.numeric(issp09$DDP34)
issp09 <- issp09 %>% mutate(income =  case_when(income == 1  ~17500,
                                                income == 2  ~45500,
                                                income == 3  ~67000,
                                                income == 4  ~89500,
                                                income == 5  ~117500,
                                                income == 6  ~158500,
                                                income == 7  ~201500,
                                                income == 8  ~257000,
                                                income == 9  ~324500,
                                                income == 10 ~403000,
                                                income == 11 ~724500,
                                                income == 12 ~1500000,
                                                income == 13 ~2500000,
                                                income == 14 ~3500000,
                                                income == 99 ~NA_real_,
                                                income == 9999998 ~NA_real_,
                                                income == 9999999 ~NA_real_),
                            pchhinc=income/HOMPOP,
                            pchhinc_a = pchhinc*186.62/100) # [cambiamos el IPC acumulado de 2009 por el de diciembre 2018 que es 186.62]


summary(issp09$pchhinc)
## 2019
### DS_P34 Numero personas en hogar
### DS_P38. 	De los siguientes tramos de ingresos mensuales
issp19$HOMPOP <- as.numeric(issp19$DS_P34)
issp19$HOMPOP[issp19$HOMPOP == 99] <- NA
issp19$HOMPOP[issp19$HOMPOP == 0] <- 1

issp19$income <- as.numeric(issp19$DS_P39)
issp19 <- issp19 %>% mutate(income = case_when(income == 1~17500,
                                               income == 2~45500,
                                               income == 3~67000,
                                               income == 4~89500,
                                               income == 5~117500,
                                               income == 6~158500,
                                               income == 7~201500,
                                               income == 8~257000,
                                               income == 9~324500,
                                               income == 10~403000,
                                               income == 11~724500,
                                               income == 12~1500000,
                                               income == 13~2500000,
                                               income == 14~3500000,
                                               income == 98 ~NA_real_,
                                               income == 99 ~NA_real_),
                            pchhinc=income/HOMPOP,
                            pchhinc_a = pchhinc) 
summary(issp19$pchhinc)

# 5. Educ. Level (educ) ---------------------------------------------------------

## 1999: Variable x_degr
sjmisc::frq(issp99$dat_6)
issp99$educ<- as.numeric(issp99$dat_6)
issp99$educ <- car::recode(issp99$educ, recodes = c("c(1,2)='No estudió';c(3,4)='Básica completa';c(5,6, 8)='Media completa';9='Superior no universitaria';7='Universitaria completa';99=NA"), as.factor = T, 
                           levels =c('No estudió','Básica completa','Media completa','Superior no universitaria','Universitaria completa'))
table(issp99$educ)  
## 2009: Variable CL_DGR
table(issp09$DDP06_ni)
issp09$educ<- as.numeric(issp09$DDP06_ni+1)

issp09$educ <- car::recode(issp09$educ, recodes = c("c(1,2)='No estudió';c(3,4)='Básica completa';c(5,6,8)='Media completa';9='Superior no universitaria';7='Universitaria completa';100=NA"), as.factor = T, 
                           levels =c('No estudió','Básica completa','Media completa','Superior no universitaria','Universitaria completa'))
table(issp09$educ)  

## 2019: Variable DS_P4
sjmisc::find_var(issp19, "DS_P4")
table(issp19$DS_P4)
issp19$educ<- as.numeric(issp19$DS_P4)
issp19$educ <- car::recode(issp19$educ, recodes = c("c(0,1)='No estudió';c(2,3)='Básica completa';c(4,5,7)='Media completa';6='Superior no universitaria';c(8,9)='Universitaria completa';99=NA"), as.factor = T, 
                           levels =c('No estudió','Básica completa','Media completa','Superior no universitaria','Universitaria completa'))
table(issp19$educ)  

# 5. Age (age) ------------------------------------------------------------------

## 1999: Variable age
table(issp99$dat_2)
issp99$age<- as.numeric(issp99$dat_2)
issp99$age <- car::recode(issp99$age, recodes = c("18:29='18-29';30:44='30-44';45:54='45-54';55:64='55-64';65:94='65 o más'"),
                          as.factor = T, 
                          levels =c('18-29','30-44','45-54','55-64','65 o más'))
table(issp99$age)  
## 2009: Variable CL_DGR
table(issp09$DDP02)
issp09$age<- as.numeric(issp09$DDP02)
issp09$age <- car::recode(issp09$age, recodes = c("18:29='18-29';30:44='30-44';45:54='45-54';55:64='55-64';65:94='65 o más'"),
                          as.factor = T, 
                          levels =c('18-29','30-44','45-54','55-64','65 o más'))
table(issp09$age)  

## 2019: Variable DS_P2
sjmisc::find_var(issp19, "edad")
table(issp19$DS_P2_EXACTA)
issp19$age<- as.numeric(issp19$DS_P2_EXACTA)
issp19$age <- car::recode(issp19$age, recodes = c("18:29='18-29';30:44='30-44';45:54='45-54';55:64='55-64';65:96='65 o más'"),
                          as.factor = T, 
                          levels =c('18-29','30-44','45-54','55-64','65 o más'))
table(issp19$age)  

# 5. Sex (sex) ------------------------------------------------------------------

## 1999: Variable sex
table(issp99$dat_1)
issp99$sex<- as.numeric(issp99$dat_1)
issp99$sex <- car::recode(issp99$sex, recodes = c("1='Hombre';2='Mujer'"),
                          as.factor = T, 
                          levels =c('Hombre','Mujer'))
table(issp99$sex)  
## 2009: Variable CL_DGR
table(issp09$DDP01)
issp09$sex<- as.numeric(issp09$DDP01)
issp09$sex <- car::recode(issp09$sex, recodes = c("1='Hombre';2='Mujer'"),
                          as.factor = T, 
                          levels =c('Hombre','Mujer'))
table(issp09$sex)  

## 2019: Variable DS_P1
sjmisc::find_var(issp19, "DS_P1")
table(issp19$DS_P1)
issp19$sex<- as.numeric(issp19$DS_P1)
issp19$sex <- car::recode(issp19$sex, recodes = c("1='Hombre';2='Mujer'"),
                          as.factor = T, 
                          levels =c('Hombre','Mujer'))
table(issp19$sex)  

# Region ------------------------------------------------------------------
find_var(issp19, "REGION")

### 1999
issp99 <- issp99 %>% mutate(region_rm = case_when(region %in% 1:12 ~ "No RM",
                                                  region %in% 13 ~ "RM",
                                                  TRUE ~ NA_character_))



table(issp99$region_rm)
### 2009
table(issp09$Fregion)
issp09 <- issp09 %>% mutate(region_rm = case_when(Fregion %in% 1:12 ~ "No RM",
                                                  Fregion %in% 13 ~ "RM",
                                                  TRUE ~ NA_character_))



table(issp09$region_rm)

### 2019
table(issp19$REGION)
issp19 <- issp19 %>% mutate(region_rm = case_when(REGION %in% 1:12 ~ "No RM",
                                                  REGION %in% 14:16 ~ "No RM",
                                                  REGION %in% 13 ~ "RM",
                                                  TRUE ~ NA_character_))



table(issp19$region_rm)


# 6. Merge ISSP 99-09-19-11 --------------------------------------------------

issp <- bind_rows(issp99,issp09,issp19,issp11)
issp <- issp %>% 
  select(year, sex, age, region_rm, pospol, educ, pchhinc, pchhinc_a,
         red,justsalud,justeduca,factor)


save(issp,file = "input/data/proc/issp.Rdata")

# ISSP 2011 ---------------------------------------------------------------
names(issp11)
issp11_w <- read_dta("input/data/original/ISSP2011.dta")

# Just --------------------------------------------------------------------
just_vlabels<- get_labels(issp11_w$V10)[1:5]

sjmisc::frq(issp11_w$V10)
issp11_w$justsalud<- issp11_w$V10 #salud
issp11_w$justsalud[issp11_w$justsalud %in% c(8,9)] <- NA
issp11_w$justsalud <- factor(issp11_w$justsalud,labels=just_vlabels)
issp11_w$justsalud <- fct_rev(issp11_w$justsalud)
sjmisc::frq(issp11_w$justsalud)

# Confianza ---------------------------------------------------------------
just_vlabels<- get_labels(issp11_w$V7)[2:6]

sjmisc::frq(issp11_w$V7)
issp11_w$confsalud<- issp11_w$V7 #salud
issp11_w$confsalud[issp11_w$confsalud %in% c(8,9,0)] <- NA
issp11_w$confsalud <- factor(issp11_w$confsalud,labels=just_vlabels)
issp11_w$confsalud <- fct_rev(issp11_w$confsalud)
sjmisc::frq(issp11_w$confsalud)

# Cambio ---------------------------------------------------------------
just_vlabels<- get_labels(issp11_w$V8)[2:5]

sjmisc::frq(issp11_w$V8)
issp11_w$cambiosalud<- issp11_w$V8 #salud
issp11_w$cambiosalud[issp11_w$cambiosalud %in% c(8,9,0)] <- NA
issp11_w$cambiosalud <- factor(issp11_w$cambiosalud,labels=just_vlabels)
issp11_w$cambiosalud <- fct_rev(issp11_w$cambiosalud)
sjmisc::frq(issp11_w$cambiosalud)

# Rol gobierno ---------------------------------------------------------------
just_vlabels<- get_labels(issp11_w$V13)[1:5]
sjmisc::frq(issp11_w$V13)
issp11_w$gobiernosalud<- issp11_w$V13 #salud
issp11_w$gobiernosalud[issp11_w$gobiernosalud %in% c(8,9)] <- NA
issp11_w$gobiernosalud <- factor(issp11_w$gobiernosalud,labels=just_vlabels)
issp11_w$gobiernosalud <- fct_rev(issp11_w$gobiernosalud)
sjmisc::frq(issp11_w$gobiernosalud)

# Ineficiencia ---------------------------------------------------------------
just_vlabels<- get_labels(issp11_w$V14)[1:5]

sjmisc::frq(issp11_w$V15)
issp11_w$inefsalud<- issp11_w$V14 #salud
issp11_w$inefsalud[issp11_w$inefsalud %in% c(8,9)] <- NA
issp11_w$inefsalud <- factor(issp11_w$inefsalud,labels=just_vlabels)
issp11_w$inefsalud <- fct_rev(issp11_w$inefsalud)
sjmisc::frq(issp11_w$inefsalud)

# Satisfaccion ---------------------------------------------------------------
just_vlabels<- get_labels(issp11_w$V51)[1:7]

sjmisc::frq(issp11_w$V51)
issp11_w$satsalud<- issp11_w$V51 #salud
issp11_w$satsalud[issp11_w$satsalud %in% c(98,99)] <- NA
issp11_w$satsalud <- factor(issp11_w$satsalud,labels=just_vlabels)
issp11_w$satsalud <- fct_rev(issp11_w$satsalud)
sjmisc::frq(issp11_w$satsalud)

# Prev ---------------------------------------------------------------
sjmisc::frq(issp11_w$V63)
issp11_w$prevsalud<- as.numeric(issp11_w$V63) #salud
issp11_w$prevsalud <- car::recode(issp11_w$prevsalud,
                                  c("c(2,5,6) = 'Público';
                                    c(3,4,7,8) = 'Privado';
                                    1 = 'Sin previsión';
                                    9 ='FF.AA y Orden';
                                    c(0,99,98) = NA"))

sjmisc::frq(issp11_w$prevsalud)


# Seleccionar -------------------------------------------------------------


issp_w <- issp11_w %>% select(contains("salud"), C_ALPHAN)

# Guardar -----------------------------------------------------------------

save(issp,issp11_w,file = "input/data/proc/issp.Rdata")

