#############################################################
# Bioestadística con R (Forestal) 
# Objetivo: emtender funcionamiento y uso del tidyverse
# Ejemplo: experimento con déficit hídrico 
############################################################


# 1. Cargar paquetes ----

library(tidyverse)
library(janitor)
library(lubridate)

# 2. Importar archivo ----

datos_raw <- read.delim("~/GitHub/Bioestadica_R/Bioestadista_R/03_data_tidy.txt",
												header = TRUE,
												sep = "\t")
glimpse(datos_raw)
names(datos_raw)

# 3. Exploración inicial ----

head(datos_raw)
summary(datos_raw)
dim(datos_raw)

# 4. Limpiar nombres de columnas ----

datos <- datos_raw %>%
	clean_names()

names(datos)

# 5. Limpiar espacios en variables de texto ----

datos <- datos %>%
	mutate(across(where(is.character), str_trim))

count(datos, bloque, sort = TRUE)
count(datos, tratamiento, sort = TRUE)
count(datos, riego, sort = TRUE)
count(datos, observaciones, sort = TRUE)

# 6. Estandarizar texto ----

datos <- datos %>%
	mutate(
		tratamiento = str_to_lower(tratamiento),
		riego = str_to_lower(riego),
		observaciones = str_to_lower(observaciones)
	)
View(datos)

# 7. Recodificar categorías inconsistentes ----

datos <- datos %>%
	mutate(
		tratamiento = case_when(
			tratamiento %in% c("ctrl", "control") ~ "control",
			tratamiento %in% c("fert", "fertilizado") ~ "fertilizado",
			TRUE ~ tratamiento
		),
		riego = case_when(
			riego == "alto"  ~ "alto",
			riego == "bajo"  ~ "bajo",
			riego == "medio" ~ "medio",
			TRUE ~ NA_character_
		)
	)

count(datos, tratamiento, sort = TRUE)
count(datos, riego, sort = TRUE)

View(datos)

# 8. Convertir observaciones inconsistentes a NA ----

datos <- datos %>%
	mutate(
		observaciones = na_if(observaciones, ""),
		observaciones = na_if(observaciones, "."),
		observaciones = na_if(observaciones, "sin dato"),
		observaciones = na_if(observaciones, "na")
	)

count(datos, observaciones, sort = TRUE)
View(datos)
# 9. Convertir fecha ----

datos <- datos %>%
	mutate(
		fecha_medicion = parse_date_time(
			fecha_medicion,
			orders = c("ymd", "dmy", "d-b-Y", "Y/m/d")
		),
		fecha_medicion = as_date(fecha_medicion)
	)

glimpse(datos)


# 10. Crear variable de mortalidad ----
	# 0 = muerto; 1 = vivo
	# Regla: diámetro <= 0 --> muerto

datos <- datos %>%
	mutate(
		mortalidad = case_when(
			diametro_mm <= 0 ~ 0,
			diametro_mm > 0  ~ 1,
			TRUE ~ NA_real_
		)
	)

count(datos, mortalidad)


# 11. Revisar valores imposibles ----

datos %>% filter(altura_cm < 0)
datos %>% filter(diametro_mm <= 0)
datos %>% filter(biomasa_g < 0)
datos %>% filter(p_h_suelo < 0 | p_h_suelo > 14)


# 12. Reemplazar valores imposibles por NA ----

datos <- datos %>%
	mutate(
		altura_cm   = if_else(altura_cm < 0, NA_real_, altura_cm),
		diametro_mm = if_else(diametro_mm <= 0, NA_real_, diametro_mm),
		biomasa_g   = if_else(biomasa_g < 0, NA_real_, biomasa_g),
		p_h_suelo   = if_else(p_h_suelo < 0 | p_h_suelo > 14, NA_real_, p_h_suelo)
	)
View(datos)

# 13. Revisar duplicados ----

duplicados <- datos %>%
	count(id_planta) %>%
	filter(n > 1)

duplicados

datos %>%
	filter(id_planta %in% duplicados$id_planta) %>%
	arrange(id_planta)


# 14. Tratar duplicados ----

datos_con_duplicados <- datos

datos_sin_duplicados <- datos %>%
	distinct(id_planta, .keep_all = TRUE)


# 15. Resumen de faltantes ----

datos %>%
	summarise(across(everything(), ~ sum(is.na(.))))


# 16. Tabla resumen por tratamiento y riego ----

resumen <- datos_sin_duplicados %>%
	group_by(tratamiento, riego) %>%
	summarise(
		n = n(),
		vivos = sum(mortalidad == 1, na.rm = TRUE),
		muertos = sum(mortalidad == 0, na.rm = TRUE),
		altura_media = mean(altura_cm, na.rm = TRUE),
		diametro_medio = mean(diametro_mm, na.rm = TRUE),
		biomasa_media = mean(biomasa_g, na.rm = TRUE),
		ph_medio = mean(p_h_suelo, na.rm = TRUE),
		.groups = "drop"
	)

resumen


# 17. Exportar base limpia ----

write_csv(datos_sin_duplicados, "~/GitHub/Bioestadica_R/Bioestadista_R/data_class_04_limpia.csv")


# 18. Exportar resumen ----

write_csv(resumen, "~/GitHub/Bioestadica_R/Bioestadista_R/resumen_tratamiento_riego.csv")

## End ##
