# Code MOVID-IMPACT salud -------------------------------------------------

# 1. Packages -------------------------------------------------------------

pacman::p_load(haven, tidyverse, sjPlot, sjmisc, sjlabelled)

# 2. Load --------------------------------------------------------------------

data <- haven::read_dta(url("https://movid-impact.netlify.app/input/data/MOVID-IMPACT.dta"))

options(scipen = 999)

# 3. Explore --------------------------------------------------------------
# Las dependientes serían d6-d8-c1-e7-f4-f3-f4
# f5.1 y f5.2
# f6

data_proc <- data %>%  select(id_encuesta, region, factor_expansion,
                      sexo, edad,
                      a4, #jefehogar,
                      a8a,
                      a8b, #neduc
                      b2, #prev
                      d6,
                      d8,
                      starts_with("c1"),
                      e7,
                      starts_with("f4"),
                      starts_with("f3"),
                      f5_1,
                      f5_2,
                      f6,
                      f3_1)


# Recode ------------------------------------------------------------------
# Modulo C. Preexistencias ------------------------------------------------
data_proc <- data_proc %>% mutate_at(vars(starts_with("c"), -c1_6_esp), funs(as.numeric(.)))

# C1_* Enfermedades cronicas ----------------------------------------------
# C1_1 Diabetes -----------------------------------------------------------
data_proc$c1_1 <- ifelse(is.na(data_proc$c1_1), 0, 1)
# C1_2 Hipertension -----------------------------------------------------------
data_proc$c1_2 <- ifelse(is.na(data_proc$c1_2), 0, 1)
# C1_3 Cardiovascular -----------------------------------------------------------
data_proc$c1_3 <- ifelse(is.na(data_proc$c1_3), 0, 1)
# C1_4 Respiratoria -----------------------------------------------------------
data_proc$c1_4 <- ifelse(is.na(data_proc$c1_4), 0, 1)
# C1_5 Mental -----------------------------------------------------------
data_proc$c1_5 <- ifelse(is.na(data_proc$c1_5), 0, 1)
# C1_6 Otra -----------------------------------------------------------
data_proc$c1_6 <- ifelse(is.na(data_proc$c1_6), 0, 1)
# C1_7 Sano -----------------------------------------------------------
data_proc$c1_7 <- ifelse(is.na(data_proc$c1_7), 0, 1)
# C1_9 NA -----------------------------------------------------------
data_proc$c1_8 <- ifelse(is.na(data_proc$c1_8), 0, 1)
data_proc$c1_9 <- ifelse(is.na(data_proc$c1_9), 0, 1)


# Health risk: arterial hypertension, obesity, diabetes, chronic respiratory diseases (asthma, emphysema or other), cardiovascular diseases, active cancer, chronic kidney disease or immunodeficiencies
## Health risk General
table(data_proc$c1_9)
data_proc <- data_proc %>% mutate(cronicos = case_when(c1_1 == 1 ~ 1,
                                                   c1_2 == 1 ~ 1,
                                                   c1_3 == 1 ~ 1,
                                                   c1_4 == 1 ~ 1,
                                                   c1_5 == 1 ~ 1,
                                                   c1_6_esp %in% c("artritis", "artritis reumatoide, fibromialgia") ~ 1,
                                                   c1_7 == 1 ~ 0,
                                                   c1_8 == 1 ~ NA_real_,
                                                   c1_9 == 1 ~ NA_real_)) %>%
  mutate(cronicos = if_else(cronicos == 1, "Sí", "No"))

table(data_proc$cronicos) ## Artritis

data_proc$cronicos <- ifelse(data_proc$c1_1==0 &
                             data_proc$c1_2==0 &
                             data_proc$c1_3==0 &
                             data_proc$c1_4==0 &
                             data_proc$c1_5==0 &
                             data_proc$c1_6_esp!= "artritis reumatoide, fibromialgia" &
                             data_proc$c1_6_esp!="artritis", "No",
                           data_proc$cronicos)

data_proc$cronicos <- ifelse(is.na(data_proc$cronicos) &
                             is.na(data_proc$c1_6_esp) &
                             data_proc$c1_6==1, "No",
                           data_proc$cronicos)



data_proc <- data_proc %>% mutate_at(vars(starts_with("c1"),
                                          -c1_6_esp), funs(if_else(. ==  1, "Sí", "No")))

# Nivel Educacion 3 categorias --------------------------------------------------
# 3 categories: High school or less, Technical qualification and University degree
sjPlot::plot_frq(data_proc$a8a,  show.n = TRUE, weight.by = data_proc$factor_expansion) + theme_sjplot()

data_proc <- data_proc %>% mutate_at(vars(starts_with("a"), edad), funs(as.numeric(.)))

table(data_proc$a8a)
data_proc$educ_3cat <- car::recode(data_proc$a8a, c("c(1,2,3,4,5,6)='Media o menos';7='Técnica';c(8,9)='Profesional';99=NA"), as.factor = T,
                                 levels = c("Media o menos", 'Técnica', "Profesional"))

data_proc$educ_4cat <- car::recode(data_proc$a8a, c("c(1,2,3,4)='Básica o menos';c(5,6) = 'Media';7='Técnica';c(8,9)='Profesional';99=NA"), as.factor = T,
                                   levels = c("Básica o menos","Media", 'Técnica', "Profesional"))

table(data_proc$educ_3cat)
table(data_proc$educ_4cat)
sjPlot::plot_frq(data_proc$educ_4cat,  show.n = TRUE, weight.by = data_proc$factor_expansion) + theme_sjplot()


# Edad --------------------------------------------------------------------
## Edad en cuatro categorías
data_proc$edad_cat <- car::recode(data_proc$edad, c("0:29='18 a 29';30:39='30 a 39';40:49='40 a 49';50:59='50 a 59';60:69='60 a 69';70:hi='70+'"), as.factor = T,
                                levels = c("18 a 29", "30 a 39","40 a 49","50 a 59","60 a 69","70+"))

table(data_proc$edad_cat)

# Guardar proc ------------------------------------------------------------


save(data_proc, file = "input/data/movid-proc.RData")

# Analysis ----------------------------------------------------------------
sjPlot::plot_grpfrq(data_proc$d6, data_proc$sexo, show.n = TRUE) + theme_sjplot()
sjPlot::plot_grpfrq(data_proc$d6, data_proc$educ_3cat,show.n = TRUE) + theme_sjplot()
sjPlot::plot_grpfrq(data_proc$d6, data_proc$b2, show.n = TRUE) + theme_sjplot()

sjt.xtab(data_proc$d6, data_proc$sexo, weight.by = data_proc$factor_expansion, encoding = "UTF-8", show.col.prc=TRUE,
         show.summary=FALSE, file = "output/fig1.html")
sjt.xtab(data_proc$d6, data_proc$educ_3cat, weight.by = data_proc$factor_expansion, encoding = "UTF-8",
         show.col.prc=TRUE,
         show.summary=FALSE, file = "output/fig2.html")
sjt.xtab(data_proc$d6, data_proc$b2, weight.by = data_proc$factor_expansion, encoding = "UTF-8", show.col.prc=TRUE,
         show.summary=FALSE, file = "output/fig3.html")


# d8
sjPlot::plot_grpfrq(data_proc$d8, data_proc$sexo, show.n = TRUE) + theme_sjplot()
sjPlot::plot_grpfrq(data_proc$d8, data_proc$educ_3cat, show.n = TRUE) + theme_sjplot()
sjPlot::plot_grpfrq(data_proc$d8, data_proc$b2, show.n = TRUE) + theme_sjplot()

sjt.xtab(data_proc$d8, data_proc$sexo, weight.by = data_proc$factor_expansion, encoding = "UTF-8", show.col.prc=TRUE,
         show.summary=FALSE, file = "output/fig4.html")
sjt.xtab(data_proc$d8, data_proc$educ_3cat, weight.by = data_proc$factor_expansion, encoding = "UTF-8", show.col.prc=TRUE,
         show.summary=FALSE, file = "output/fig5.html")
sjt.xtab(data_proc$d8, data_proc$b2, weight.by = data_proc$factor_expansion, encoding = "UTF-8", show.col.prc=TRUE,
         show.summary=FALSE, file = "output/fig6.html")

## Cronicos
sjPlot::tab_stackfrq(data_proc[,c("c1_1", "c1_2", "c1_3","c1_4", "c1_5","c1_6", "c1_7", "c1_8", "c1_9", "cronicos")],  alternate.rows = TRUE,
                     show.n = TRUE,
                     weight.by = data_proc$factor_expansion,
                     file = "output/fig7.html")

sjPlot::tab_stackfrq(data_proc[,c("f3_1", "f3_2", "f3_3","f3_4", "f3_5")],  alternate.rows = TRUE,
                     show.n = TRUE, weight.by = data_proc$factor_expansion,
                     file = "output/fig8.html")


sjPlot::tab_stackfrq(data_proc[,c("f4_1", "f4_2", "f4_3","f4_4")],  alternate.rows = TRUE,
                     show.n = TRUE, weight.by = data_proc$factor_expansion,
                     file = "output/fig9.html")

sjPlot::tab_stackfrq(data_proc[,c("f5_1", "f5_2")],  alternate.rows = TRUE,
                     show.n = TRUE, weight.by = data_proc$factor_expansion,
                     file = "output/fig10.html")

sjPlot::tab_stackfrq(data_proc[,c("f6")],  alternate.rows = TRUE,
                     show.n = TRUE, weight.by = data_proc$factor_expansion,
                     file = "output/fig11.html")



sjPlot::plot_frq(data_proc$f6,  show.n = TRUE, weight.by = data_proc$factor_expansion) + theme_sjplot()
sjPlot::plot_frq(data_proc$f3_1,  show.n = TRUE, weight.by = data_proc$factor_expansion) + theme_sjplot()


## Otros

sjt.xtab(data_proc$f6, data_proc$educ_4cat, weight.by = data_proc$factor_expansion, encoding = "UTF-8", show.col.prc=TRUE,
         show.summary=FALSE)

sjt.xtab(data_proc$f6, data_proc$edad, weight.by = data_proc$factor_expansion, encoding = "UTF-8", show.col.prc=TRUE,
         show.summary=FALSE)

sjt.xtab(data_proc$f6, data_proc$edad_cat, weight.by = data_proc$factor_expansion, encoding = "UTF-8", show.col.prc=TRUE,
         show.summary=FALSE)

sjt.xtab(data_proc$f5_1, data_proc$educ_4cat, weight.by = data_proc$factor_expansion, encoding = "UTF-8", show.col.prc=TRUE,
         show.summary=FALSE)

sjt.xtab(data_proc$f3_1, data_proc$educ_4cat, weight.by = data_proc$factor_expansion, encoding = "UTF-8", show.col.prc=TRUE,
         show.summary=FALSE)

sjt.xtab(data_proc$f3_1, data_proc$sexo, weight.by = data_proc$factor_expansion, encoding = "UTF-8", show.col.prc=TRUE,
         show.summary=FALSE)

sjt.xtab(data_proc$f3_1, data_proc$edad_cat, weight.by = data_proc$factor_expansion, encoding = "UTF-8", show.col.prc=TRUE,
         show.summary=FALSE)
