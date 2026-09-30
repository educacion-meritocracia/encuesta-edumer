#*******************************************************************************************************************
#
# 0. Identification ---------------------------------------------------
# Title: Data preparation for EDUMER Teachers Survey Wave 1
# Author: Andreas Laffert            
# Overview: EDUMER Teachers Survey Wave 1         
# Date: 13-066-2024            
#
#******************************************************************************************************************

# 1. Packages ---------------------------------------------------------
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               sjlabelled, 
               sjmisc, 
               rio,
               here, 
               codebook,
               haven)

options(scipen=999)

# 2. Data --------------------------------------------------------------

db <- rio::import(file = here("input","data", "original", "310524_BDD_edumer.sav")) %>% 
  as_tibble()

names(db)

db_or_label <- sjlabelled::get_label(db)

# 3. Processing -----------------------------------------------------------

clean_labels <- function(x) {
  current_label <- sjlabelled::get_label(x)
  if (!is.null(current_label)) {
    new_label <- stringr::str_remove(current_label, "^\\s*\\d+\\.?\\d*\\s*-?\\s*")
    x <- sjlabelled::set_label(x, label = new_label)
  }
  return(x)
}

# Select ----
db2 <- db[,c(106:179)]  # only teachers variables 

# Colnames ----
names(db2)

db2 <- db2 %>% 
  rename(id_docente = SbjNum_docente,
         fecha_docente = Date_docente)

names(db2)
glimpse(db2)

names(which(colSums(is.na(db2)) > 0))

db2 <- db2 %>% 
  rename_with(~str_remove(.,"_docente")) # just for operate, then restored

# Manipulation ----

## ID 
frq(db2$id)
get_label(db2$id)
db2$id <- sjlabelled::set_label(db2$id, label = "Identificador único docente")

## Fecha
frq(db2$fecha)
class(db2$fecha)
db2$fecha <- lubridate::as_datetime(db2$fecha)
get_label(db2$fecha)
db2$fecha <- sjlabelled::set_label(db2$fecha, label = "Fecha de aplicación")
## p1_

db2 %>% 
  select(starts_with("p1_")) %>% 
  frq(.)

labels1 <- c("Muy en desacuerdo" = 1, 
             "En desacuerdo" = 2, 
             "De acuerdo" = 3, 
             "Muy de acuerdo" = 4, 
             "No sabe" = 88, 
             "No responde" = 99)

db2 <- db2 %>% 
  mutate(across(.cols = starts_with("p1_"),.fns = ~ sjlabelled::set_labels(., labels = labels1)))

db2 %>% 
  select(starts_with("p1_")) %>% 
  get_label()

db2$p1_1 <- sjlabelled::set_label(db2$p1_1, 
                                  label = "En Chile, las personas son recompensadas por sus esfuerzos")

db2$p1_2 <- sjlabelled::set_label(db2$p1_2, 
                                  label = "En Chile, las personas son recompensadas por su inteligencia y habilidad")

db2$p1_3 <- sjlabelled::set_label(db2$p1_3, 
                                  label = "En Chile, a quienes tienen padres ricos les va mucho mejor en la vida")

db2$p1_4 <- sjlabelled::set_label(db2$p1_4, 
                                  label = "En Chile, quienes tienen buenos contactos les va mejor en la vida")

db2$p1_5 <- sjlabelled::set_label(db2$p1_5, 
                                  label = "Quienes más se esfuerzan deberían obtener mayores recompensas que quienes se esfuerzan menos")

db2$p1_6 <- sjlabelled::set_label(db2$p1_6, 
                                  label = "Quienes poseen más talento deberían obtener mayores recompensas que quienes poseen menos talento")

db2$p1_7 <- sjlabelled::set_label(db2$p1_7, 
                                  label = "Está bien que quienes tienen padres ricos les vaya bien en la vida")

db2$p1_8 <- sjlabelled::set_label(db2$p1_8, 
                                  label = "Está bien que quienes tienen buenos contactos les vaya bien en la vida")

db2$p1_9 <- sjlabelled::set_label(db2$p1_9, 
                                  label = "En Chile, todas las personas tienen las mismas oportunidades para salir adelante")

db2$p1_10 <- sjlabelled::set_label(db2$p1_10, 
                                   label = "En Chile, todas las personas obtienen lo que merecen")

## p2_

db2 %>% 
  select(starts_with("p2_")) %>% 
  frq(.)

db2 <- db2 %>% 
  mutate(across(.cols = starts_with("p2_"),.fns = ~ sjlabelled::set_labels(., labels = labels1)))

db2 %>% 
  select(starts_with("p2_")) %>% 
  get_label()

db2$p2_1 <- sjlabelled::set_label(db2$p2_1, 
                                  label = "En esta escuela, quienes se esfuerzan obtienen buenas notas")

db2$p2_2 <- sjlabelled::set_label(db2$p2_2, 
                                  label = "En esta escuela, quienes son inteligentes obtienen buenas notas")

db2$p2_3 <- sjlabelled::set_label(db2$p2_3, 
                                  label = "En esta escuela, los/as estudiantes obtienen las notas que merecen")

## p3_0
frq(db2$p3_0)
get_label(db2$p3_0)

db2 <- db2 %>% 
  mutate(
    p3_0 = case_when(p3_0 == 0 ~ 1, 
                     p3_0 == 1 ~ 2, 
                     p3_0 == 2 ~ 3, 
                     p3_0 == 3 ~ 4, 
                     p3_0 == 4 ~ 5, 
                     p3_0 == 5 ~ 6, 
                     p3_0 == 6 ~ 7, 
                     p3_0 == 7 ~ 8, 
                     p3_0 == 8 ~ 9, 
                     p3_0 == 9 ~ 10, 
                     TRUE ~ NA_real_),
    p3_0 = sjlabelled::set_label(p3_0, label = "¿Qué es más importante para obtener buenas notas, el esfuerzo o la inteligencia? Escala 1 (esfuerzo) a 10 (inteligencia)")
  )

## p3_1 y p3_2

db2 %>% 
  select(p3_1, p3_2) %>% 
  frq(.)

db2 <- db2 %>% 
  mutate(across(.cols = c(p3_1, p3_2),.fns = ~ sjlabelled::set_labels(., labels = labels1)))

db2 %>% 
  select(p3_1, p3_2) %>% 
  get_label()

db2$p3_1 <- sjlabelled::set_label(db2$p3_1, 
                                  label = "Las personas pueden esforzarse más si lo intentan")

db2$p3_2 <- sjlabelled::set_label(db2$p3_2, 
                                  label = "Las personas pueden ser más inteligentes si lo intentan")

## p4_

db2 %>% 
  select(starts_with("p4_")) %>% 
  frq(.)

db2 <- db2 %>% 
  mutate(across(.cols = starts_with("p4_"),.fns = ~ sjlabelled::set_labels(., labels = labels1)))

db2 %>% 
  select(starts_with("p4_")) %>% 
  get_label()

db2$p4_1 <- sjlabelled::set_label(db2$p4_1, 
                                  label = "Las diferencias económicas entre ricos y pobres en Chile son demasiado grandes")

db2$p4_2 <- clean_labels(db2$p4_2)
db2$p4_3 <- clean_labels(db2$p4_3)
db2$p4_4 <- clean_labels(db2$p4_4)
db2$p4_5 <- clean_labels(db2$p4_5)
db2$p4_6 <- clean_labels(db2$p4_6)

## p5_

db2 %>% 
  select(starts_with("p5_")) %>% 
  frq(.)

labels2 <- c("Nada importante" = 1, 
             "Algo importante" = 2, 
             "Importante" = 3, 
             "Muy importante" = 4, 
             "No sabe" = 88, 
             "No responde" = 99)

db2 <- db2 %>% 
  mutate(across(.cols = starts_with("p5_"),.fns = ~ sjlabelled::set_labels(., labels = labels2)))

db2 %>% 
  select(starts_with("p5_")) %>% 
  get_label()

db2$p5_1 <- sjlabelled::set_label(db2$p5_1, label = "Votar en todas las elecciones nacionales")
db2$p5_2 <- clean_labels(db2$p5_2)
db2$p5_3 <- clean_labels(db2$p5_3)
db2$p5_4 <- clean_labels(db2$p5_4)
db2$p5_5 <- clean_labels(db2$p5_5)
db2$p5_6 <- clean_labels(db2$p5_6)
db2$p5_7 <- clean_labels(db2$p5_7)
db2$p5_8 <- clean_labels(db2$p5_8)

## p6_

db2 %>% 
  select(starts_with("p6_")) %>% 
  frq(.)

db2 <- db2 %>% 
  mutate(
    across(.cols = starts_with("p6_"),
           .fns = ~ sjlabelled::set_labels(., labels = c("Sí" = 1,
                                                         "No" = 2,
                                                         "No responde" = 99)))
  )

db2 %>% 
  select(starts_with("p6_")) %>% 
  get_label()

db2$p6_1 <- sjlabelled::set_label(db2$p6_1, label = "Participar en una marcha o manifestación pacífica")
db2$p6_2 <- clean_labels(db2$p6_2)
db2$p6_3 <- clean_labels(db2$p6_3)

## p7_

db2 %>% 
  select(starts_with("p7_")) %>% 
  frq(.)

labels3 <- c("Nunca" = 1, 
             "Una vez al año" = 2, 
             "Una vez al mes" = 3, 
             "Semanalmente" = 4,
             "Todos los días" = 5,
             "No sabe" = 88, 
             "No responde" = 99)

db2 <- db2 %>% 
  mutate(across(.cols = starts_with("p7_"),.fns = ~ sjlabelled::set_labels(., labels = labels3)))

db2 %>% 
  select(starts_with("p7_")) %>% 
  get_label()

db2$p7_1 <- sjlabelled::set_label(db2$p7_1, label = "Buscar sitios web de Internet para encontrar información sobre temas políticos o sociales")
db2$p7_2 <- clean_labels(db2$p7_2)
db2$p7_3 <- clean_labels(db2$p7_3)
db2$p7_4 <- clean_labels(db2$p7_4)
db2$p7_5 <- clean_labels(db2$p7_5)
db2$p7_6 <- clean_labels(db2$p7_6)

## p8_

db2 %>% 
  select(starts_with("p8_")) %>% 
  frq(.)

db2 <- db2 %>% 
  mutate(
    across(.cols = starts_with("p8_"),
           .fns = ~ sjlabelled::set_labels(., labels = c("Sí" = 1,
                                                         "No" = 2,
                                                         "No responde" = 99)))
  )

db2 %>% 
  select(starts_with("p8_")) %>% 
  get_label()

db2$p8_1 <- clean_labels(db2$p8_1)
db2$p8_2 <- clean_labels(db2$p8_2)
db2$p8_3 <- clean_labels(db2$p8_3)

## p9_

db2 %>% 
  select(starts_with("p9_")) %>% 
  frq(.)

labels4 <- c("Nunca" = 1, 
             "Casi nunca" = 2, 
             "Casi siempre" = 3, 
             "Siempre" = 4,
             "No sabe" = 88, 
             "No responde" = 99)

db2 <- db2 %>% 
  mutate(across(.cols = starts_with("p9_"),.fns = ~ sjlabelled::set_labels(., labels = labels4)))

db2 %>% 
  select(starts_with("p9_")) %>% 
  get_label()

db2$p9_1 <- sjlabelled::set_label(db2$p9_1, 
                                  label = "Los estudiantes pueden manifestar abiertamente su desacuerdo con sus profesores(as)")

db2$p9_2 <- clean_labels(db2$p9_2)
db2$p9_3 <- clean_labels(db2$p9_3)
db2$p9_4 <- clean_labels(db2$p9_4)
db2$p9_5 <- clean_labels(db2$p9_5)
db2$p9_6 <- clean_labels(db2$p9_6)

## p10
frq(db2$p10)
db2$p10 <- sjlabelled::set_labels(db2$p10, 
                                  labels = c("Ninguna importancia" = 1, 
                                             "Algo de importancia" = 2, 
                                             "Mediana importancia" = 3, 
                                             "Mucha importancia" = 4,
                                             "No sabe" = 88, 
                                             "No responde" = 99))

get_label(db2$p10)
db2$p10 <- clean_labels(db2$p10)

## check atencion
frq(db2$check)
db2$check <- sjlabelled::set_labels(db2$check, labels = labels1)

get_label(db2$check)
db2$check <- sjlabelled::set_label(db2$check, label = "Si está leyendo correctamente esta pregunta, marque la opción En desacuerdo")

## p11_

db2 %>% 
  select(starts_with("p11_")) %>% 
  frq()

db2 <- db2 %>% 
  mutate(across(.cols = starts_with("p11_"),.fns = ~ sjlabelled::set_labels(., labels = labels1)))

db2 %>% 
  select(starts_with("p11_")) %>% 
  get_label()

db2$p11_1 <- sjlabelled::set_label(db2$p11_1, 
                                  label = "Recompenso a los/as estudiantes que mejor trabajan")

db2$p11_2 <- clean_labels(db2$p11_2)
db2$p11_3 <- clean_labels(db2$p11_3)
db2$p11_4 <- clean_labels(db2$p11_4)
db2$p11_5 <- clean_labels(db2$p11_5)
db2$p11_6 <- clean_labels(db2$p11_6)
db2$p11_7 <- clean_labels(db2$p11_7)
db2$p11_8 <- clean_labels(db2$p11_8)

## p12_

db2 %>% 
  select(starts_with("p12_")) %>% 
  frq()

db2 <- db2 %>% 
  mutate(across(.cols = starts_with("p12_"),.fns = ~ sjlabelled::set_labels(., labels = labels4)))

db2 %>% 
  select(starts_with("p12_")) %>% 
  get_label()

db2$p12_1 <- sjlabelled::set_label(db2$p12_1, 
                                   label = "Sugerir a un/a estudiante que haya terminado su trabajo que ayude a otro/a que tenga dificultades")

db2$p12_2 <- clean_labels(db2$p12_2)
db2$p12_3 <- clean_labels(db2$p12_3)
db2$p12_4 <- clean_labels(db2$p12_4)
db2$p12_5 <- clean_labels(db2$p12_5)
db2$p12_6 <- clean_labels(db2$p12_6)
db2$p12_7 <- clean_labels(db2$p12_7)
db2$p12_8 <- clean_labels(db2$p12_8)
db2$p12_9 <- clean_labels(db2$p12_9)

## p13
frq(db2$p13)
get_label(db2$p13)
db2$p13 <- sjlabelled::set_label(db2$p13, label = "Género docente")

## p14
db2 %>% 
  select(starts_with("p14_")) %>% 
  frq()

db2 %>% 
  select(starts_with("p14_")) %>% 
  get_label()

db2$p14_mes <- clean_labels(db2$p14_mes)
db2$p14_ano <- clean_labels(db2$p14_ano)

## p15
frq(db2$p15)
db2$p15 <- sjlabelled::set_labels(db2$p15, 
                                  labels = c("Derecha" = 1, 
                                             "Centro Derecha" = 2, 
                                             "Centro" = 3, 
                                             "Centro Izquierda" = 4,
                                             "Izquierda" = 5,
                                             "Independiente" = 6,
                                             "Ninguna" = 7,
                                             "No sabe" = 88, 
                                             "No responde" = 99))

get_label(db2$p15)
db2$p15 <- sjlabelled::set_label(db2$p15, label = "Posición política (eje izq. y der.)")

## p16
frq(db2$p16)
db2$p16 <- sjlabelled::set_labels(db2$p16, 
                                  labels = c("Mapuche" = 1, 
                                             "Aimara" = 2, 
                                             "Rapa Nui" = 3, 
                                             "Quechua" = 4,
                                             "No, a ninguno de las anteriores" = 5,
                                             "No sabe" = 88, 
                                             "No responde" = 99))

get_label(db2$p16)
db2$p16 <- clean_labels(db2$p16)


#4. Save and remove ----------------------------------------------------

db_teachers <- db2 %>% 
  rename_with(~ paste0(., "_docente"), -id) %>% 
  janitor::clean_names() %>% 
  group_by(id) %>% 
  slice_head(n = 1) %>% 
  ungroup() %>% 
  as.data.frame()

base::save(db_teachers, file = here("output/data/db_proc_teachers.RData"))
haven::write_dta(db_teachers, path = here("output/data/db_proc_teachers.dta"))
haven::write_sav(db_teachers, path = here("output/data/db_proc_teachers.sav"))

rm(list = ls(pattern = "^labels[0-9]+$"))
