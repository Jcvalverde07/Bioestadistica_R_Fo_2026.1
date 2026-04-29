# ============================================================
# Curso: Bioestadística con R
# Capítulo 9: Regresión múltiple, selección de modelos y GLM
# Caso de estudio: Ataque patógeno en Gmelina arborea
# Base: melina_patogeno_multivariado.xlsx
# ============================================================

# 1. Instalación y carga de paquetes ----

paquetes <- c(
	"readxl",
	"dplyr",
	"ggplot2",
	"tidyr",
	"car",
	"MASS",
	"MuMIn",
	"performance",
	"DHARMa",
	"broom",
	"ggeffects"
)

instalar <- paquetes[!(paquetes %in% installed.packages()[, "Package"])]

if(length(instalar) > 0){
	install.packages(instalar)
}

library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(car)
library(MASS)
library(MuMIn)
library(performance)
library(DHARMa)
library(broom)
library(ggeffects)


# 2. Importar base de datos ----

datos <- read_excel("melina_patogeno_multivariado.xlsx",
										sheet = "Datos")

# Revisar estructura
str(datos)
head(datos)
summary(datos)
names(datos)


# 3. Preparación de variables ----

datos <- datos %>%
	mutate(
		Plot_ID = as.factor(Plot_ID),
		Farm = as.factor(Farm),
		Region = as.factor(Region),
		Block = as.factor(Block),
		Disease_class = factor(Disease_class,
													 levels = c("Low", "Moderate", "Severe")),
		Drainage_class = factor(Drainage_class,
														levels = c("Good", "Moderate", "Poor"))
	)

# Revisar balance de clases
table(datos$Disease_class)
table(datos$Drainage_class)
table(datos$Region)

# Revisar datos faltantes
colSums(is.na(datos))


# 4. Exploración inicial ----
# Resumen general por clase sanitaria
resumen_clase <- datos %>%
	group_by(Disease_class) %>%
	summarise(
		n = n(),
		Volume_mean = mean(Volume_m3_ha, na.rm = TRUE),
		Volume_sd = sd(Volume_m3_ha, na.rm = TRUE),
		DBH_mean = mean(DBH_cm, na.rm = TRUE),
		Height_mean = mean(Height_m, na.rm = TRUE),
		Severity_mean = mean(Severity_index, na.rm = TRUE),
		Incidence_mean = mean(Incidence_pct, na.rm = TRUE),
		Mortality_mean = mean(Mortality_pct, na.rm = TRUE),
		LAI_mean = mean(LAI, na.rm = TRUE),
		Soil_moisture_mean = mean(Soil_moisture_pct, na.rm = TRUE),
		RH_mean = mean(Relative_humidity_pct, na.rm = TRUE)
	)

resumen_clase


# Gráfico: volumen por clase sanitaria
ggplot(datos, aes(x = Disease_class,
									y = Volume_m3_ha,
									fill = Disease_class)) +
	geom_boxplot(alpha = 0.70) +
	theme_minimal() +
	labs(
		x = "Clase sanitaria",
		y = expression("Volumen"~(m^3~ha^{-1})),
		title = "Volumen por clase sanitaria"
	) +
	theme(legend.position = "none")


# Gráfico: severidad vs volumen
ggplot(datos, aes(x = Severity_index,
									y = Volume_m3_ha,
									color = Disease_class)) +
	geom_point(size = 3, alpha = 0.80) +
	geom_smooth(method = "lm", se = TRUE) +
	theme_minimal() +
	labs(
		x = "Índice de severidad",
		y = expression("Volumen"~(m^3~ha^{-1})),
		title = "Relación entre severidad del ataque y volumen"
	)


# Gráfico: humedad del suelo vs severidad
ggplot(datos, aes(x = Soil_moisture_pct,
									y = Severity_index,
									color = Disease_class)) +
	geom_point(size = 3, alpha = 0.80) +
	geom_smooth(method = "lm", se = TRUE) +
	theme_minimal() +
	labs(
		x = "Humedad del suelo (%)",
		y = "Índice de severidad",
		title = "Relación entre humedad del suelo y severidad"
	)


# 5. Regresión lineal múltiple ----

# Pregunta:
# ¿Qué variables explican el volumen por hectárea en parcelas de Gmelina
# afectadas por ataque patógeno?

# Variable respuesta:
# Volume_m3_ha

# Predictores candidatos:
# DBH_cm, Height_m, Stand_age_yr, Severity_index,
# Soil_moisture_pct, Relative_humidity_pct


modelo_full <- lm(
	Volume_m3_ha ~ DBH_cm +
		Height_m +
		Stand_age_yr +
		Severity_index +
		Soil_moisture_pct +
		Relative_humidity_pct,
	data = datos
)

summary(modelo_full)


# 6. Interpretación ordenada de coeficientes ----

coef_modelo <- broom::tidy(modelo_full,
													 conf.int = TRUE)

coef_modelo


# Tabla más limpia
coef_modelo_limpio <- coef_modelo %>%
	select(term, estimate, std.error, conf.low, conf.high, statistic, p.value)

coef_modelo_limpio


# 7. Diagnóstico del modelo lineal ----

# Diagnóstico gráfico clásico
par(mfrow = c(2, 2))
plot(modelo_full)
par(mfrow = c(1, 1))


# Diagnóstico general con performance
check_model(modelo_full)


# Normalidad de residuos
shapiro.test(residuals(modelo_full))


# Residuos vs ajustados
datos_diag <- data.frame(
	fitted = fitted(modelo_full),
	residuals = residuals(modelo_full)
)

ggplot(datos_diag, aes(x = fitted,
											 y = residuals)) +
	geom_point(size = 3, alpha = 0.75) +
	geom_hline(yintercept = 0,
						 linetype = "dashed") +
	theme_minimal() +
	labs(
		x = "Valores ajustados",
		y = "Residuos",
		title = "Residuos vs valores ajustados"
	)


# QQ plot de residuos
ggplot(datos_diag, aes(sample = residuals)) +
	stat_qq() +
	stat_qq_line() +
	theme_minimal() +
	labs(
		title = "QQ plot de residuos del modelo lineal"
	)


# 8. Colinealidad entre predictores ----

vif(modelo_full)

# Interpretación práctica:
# VIF < 5: colinealidad baja o aceptable
# VIF entre 5 y 10: colinealidad moderada-alta
# VIF > 10: colinealidad severa


# 9. Modelos candidatos para selección ----
	# Modelo 1: tamaño del rodal
	m1 <- lm(
		Volume_m3_ha ~ DBH_cm + Height_m + Stand_age_yr,
		data = datos
	)
	
	# Modelo 2: tamaño + severidad
	m2 <- lm(
		Volume_m3_ha ~ DBH_cm + Height_m + Stand_age_yr +
			Severity_index,
		data = datos
	)
	
	# Modelo 3: tamaño + ambiente
	m3 <- lm(
		Volume_m3_ha ~ DBH_cm + Height_m + Stand_age_yr +
			Soil_moisture_pct + Relative_humidity_pct,
		data = datos
	)
	
	# Modelo 4: tamaño + severidad + ambiente
	m4 <- lm(
		Volume_m3_ha ~ DBH_cm + Height_m + Stand_age_yr +
			Severity_index +
			Soil_moisture_pct +
			Relative_humidity_pct,
		data = datos
	)
	
	# Modelo 5: tamaño + severidad + interacción simple
	m5 <- lm(
		Volume_m3_ha ~ DBH_cm + Height_m + Stand_age_yr +
			Severity_index * Soil_moisture_pct,
		data = datos
	)


# 10. Comparación de modelos por AIC y BIC ----

AIC(m1, m2, m3, m4, m5)

BIC(m1, m2, m3, m4, m5)


# Tabla de selección con MuMIn
modelos <- list(
	m1_tamano = m1,
	m2_tamano_severidad = m2,
	m3_tamano_ambiente = m3,
	m4_tamano_severidad_ambiente = m4,
	m5_interaccion = m5
)

tabla_modelos <- model.sel(modelos)

tabla_modelos


# Convertir tabla a data frame
tabla_modelos_df <- as.data.frame(tabla_modelos)

tabla_modelos_df


# 11. Comparación de modelos anidados -----

# m1 está anidado en m2
anova(m1, m2)

# m2 está anidado en m4
anova(m2, m4)

# m2 está anidado en m5 si se agregan términos relacionados
anova(m2, m5)


# 12. Selección del modelo final ----
# En este ejemplo se toma m4 como modelo final didáctico.
# La decisión real debe basarse en AIC, diagnóstico y sentido biológico.

modelo_final_lm <- m4

summary(modelo_final_lm)
check_model(modelo_final_lm)
vif(modelo_final_lm)


# Coeficientes del modelo final
coef_final_lm <- broom::tidy(modelo_final_lm,
														 conf.int = TRUE)

coef_final_lm


# R2 y R2 ajustado
broom::glance(modelo_final_lm)


# 13. Predicción con regresión múltiple ----

# Escenario:
# Estimar cómo cambia el volumen esperado conforme aumenta la severidad,
# manteniendo los demás predictores en su valor medio.

nuevos_lm <- data.frame(
	DBH_cm = mean(datos$DBH_cm, na.rm = TRUE),
	Height_m = mean(datos$Height_m, na.rm = TRUE),
	Stand_age_yr = mean(datos$Stand_age_yr, na.rm = TRUE),
	Severity_index = seq(
		min(datos$Severity_index, na.rm = TRUE),
		max(datos$Severity_index, na.rm = TRUE),
		length.out = 100
	),
	Soil_moisture_pct = mean(datos$Soil_moisture_pct, na.rm = TRUE),
	Relative_humidity_pct = mean(datos$Relative_humidity_pct, na.rm = TRUE)
)

pred_lm <- predict(
	modelo_final_lm,
	newdata = nuevos_lm,
	interval = "confidence"
)

nuevos_lm <- cbind(nuevos_lm, pred_lm)

head(nuevos_lm)


# Gráfico de predicción
ggplot(nuevos_lm,
			 aes(x = Severity_index,
			 		y = fit)) +
	geom_line(linewidth = 1) +
	geom_ribbon(aes(ymin = lwr,
									ymax = upr),
							alpha = 0.20) +
	theme_minimal() +
	labs(
		x = "Índice de severidad",
		y = expression("Volumen predicho"~(m^3~ha^{-1})),
		title = "Efecto estimado de la severidad sobre el volumen"
	)


# 14. Predicción comparando escenarios de humedad ----

nuevos_lm_humedad <- expand.grid(
	DBH_cm = mean(datos$DBH_cm, na.rm = TRUE),
	Height_m = mean(datos$Height_m, na.rm = TRUE),
	Stand_age_yr = mean(datos$Stand_age_yr, na.rm = TRUE),
	Severity_index = seq(
		min(datos$Severity_index, na.rm = TRUE),
		max(datos$Severity_index, na.rm = TRUE),
		length.out = 100
	),
	Soil_moisture_pct = quantile(
		datos$Soil_moisture_pct,
		probs = c(0.25, 0.50, 0.75),
		na.rm = TRUE
	),
	Relative_humidity_pct = mean(datos$Relative_humidity_pct, na.rm = TRUE)
)

nuevos_lm_humedad$Humedad_suelo <- factor(
	nuevos_lm_humedad$Soil_moisture_pct,
	labels = c("Baja", "Media", "Alta")
)

pred_lm_humedad <- predict(
	modelo_final_lm,
	newdata = nuevos_lm_humedad,
	interval = "confidence"
)

nuevos_lm_humedad <- cbind(nuevos_lm_humedad, pred_lm_humedad)

ggplot(nuevos_lm_humedad,
			 aes(x = Severity_index,
			 		y = fit,
			 		color = Humedad_suelo,
			 		fill = Humedad_suelo)) +
	geom_line(linewidth = 1) +
	geom_ribbon(aes(ymin = lwr,
									ymax = upr),
							alpha = 0.15,
							color = NA) +
	theme_minimal() +
	labs(
		x = "Índice de severidad",
		y = expression("Volumen predicho"~(m^3~ha^{-1})),
		color = "Humedad del suelo",
		fill = "Humedad del suelo",
		title = "Volumen predicho según severidad y humedad del suelo"
	)


# 4.2. MODELOS LINEALES GENERALIZADOS, GLM
# 15. GLM binomial: ataque severo sí/no
# Crear variable binaria:
# 1 = ataque severo
# 0 = ataque bajo o moderado

datos <- datos %>%
	mutate(
		Severe_attack = ifelse(Disease_class == "Severe", 1, 0),
		Severe_attack = as.numeric(Severe_attack)
	)

table(datos$Severe_attack)


# Modelo logístico
modelo_logit <- glm(
	Severe_attack ~ Soil_moisture_pct +
		Relative_humidity_pct +
		Mean_temp_C +
		Drainage_class,
	data = datos,
	family = binomial(link = "logit")
)

summary(modelo_logit)


# ------------------------------------------------------------
# 16. Interpretación del modelo logístico
# ------------------------------------------------------------

# Coeficientes en escala logit
broom::tidy(modelo_logit,
						conf.int = TRUE)


# Odds ratios
odds_ratios <- broom::tidy(modelo_logit,
													 conf.int = TRUE,
													 exponentiate = TRUE)

odds_ratios


# Diagnóstico general
check_model(modelo_logit)


# Residuos simulados con DHARMa
res_logit <- simulateResiduals(modelo_logit)
plot(res_logit)


# ------------------------------------------------------------
# 17. Predicción de probabilidad de ataque severo
# ------------------------------------------------------------

nuevos_logit <- data.frame(
	Soil_moisture_pct = seq(
		min(datos$Soil_moisture_pct, na.rm = TRUE),
		max(datos$Soil_moisture_pct, na.rm = TRUE),
		length.out = 100
	),
	Relative_humidity_pct = mean(datos$Relative_humidity_pct, na.rm = TRUE),
	Mean_temp_C = mean(datos$Mean_temp_C, na.rm = TRUE),
	Drainage_class = factor("Moderate",
													levels = levels(datos$Drainage_class))
)

pred_logit <- predict(
	modelo_logit,
	newdata = nuevos_logit,
	type = "link",
	se.fit = TRUE
)

nuevos_logit <- nuevos_logit %>%
	mutate(
		fit_link = pred_logit$fit,
		se_link = pred_logit$se.fit,
		prob = plogis(fit_link),
		lower = plogis(fit_link - 1.96 * se_link),
		upper = plogis(fit_link + 1.96 * se_link)
	)

head(nuevos_logit)


ggplot(nuevos_logit,
			 aes(x = Soil_moisture_pct,
			 		y = prob)) +
	geom_line(linewidth = 1) +
	geom_ribbon(aes(ymin = lower,
									ymax = upper),
							alpha = 0.20) +
	theme_minimal() +
	labs(
		x = "Humedad del suelo (%)",
		y = "Probabilidad de ataque severo",
		title = "Probabilidad predicha de ataque severo"
	)


# ------------------------------------------------------------
# 18. GLM binomial para proporciones
# ------------------------------------------------------------

# Ejemplo:
# Incidence_pct representa el porcentaje de árboles afectados.
# Para modelarlo como binomial, se requiere conocer número de árboles evaluados.
# Aquí se asume didácticamente que se evaluaron 25 árboles por parcela.

datos <- datos %>%
	mutate(
		Trees_evaluated = 25,
		Trees_infected = round(Incidence_pct / 100 * Trees_evaluated),
		Trees_healthy = Trees_evaluated - Trees_infected
	)

head(datos %>%
		 	select(Plot_ID, Incidence_pct,
		 				 Trees_evaluated, Trees_infected, Trees_healthy))


modelo_binom_prop <- glm(
	cbind(Trees_infected, Trees_healthy) ~ Soil_moisture_pct +
		Relative_humidity_pct +
		Mean_temp_C +
		Drainage_class,
	data = datos,
	family = binomial(link = "logit")
)

summary(modelo_binom_prop)


# Odds ratios para modelo binomial de proporciones
broom::tidy(modelo_binom_prop,
						conf.int = TRUE,
						exponentiate = TRUE)


# Diagnóstico
check_model(modelo_binom_prop)

res_binom <- simulateResiduals(modelo_binom_prop)
plot(res_binom)


# Predicción de incidencia esperada
nuevos_binom <- data.frame(
	Soil_moisture_pct = seq(
		min(datos$Soil_moisture_pct, na.rm = TRUE),
		max(datos$Soil_moisture_pct, na.rm = TRUE),
		length.out = 100
	),
	Relative_humidity_pct = mean(datos$Relative_humidity_pct, na.rm = TRUE),
	Mean_temp_C = mean(datos$Mean_temp_C, na.rm = TRUE),
	Drainage_class = factor("Moderate",
													levels = levels(datos$Drainage_class))
)

pred_binom <- predict(
	modelo_binom_prop,
	newdata = nuevos_binom,
	type = "link",
	se.fit = TRUE
)

nuevos_binom <- nuevos_binom %>%
	mutate(
		fit_link = pred_binom$fit,
		se_link = pred_binom$se.fit,
		prop = plogis(fit_link),
		lower = plogis(fit_link - 1.96 * se_link),
		upper = plogis(fit_link + 1.96 * se_link),
		incidence_pct_pred = prop * 100,
		lower_pct = lower * 100,
		upper_pct = upper * 100
	)

ggplot(nuevos_binom,
			 aes(x = Soil_moisture_pct,
			 		y = incidence_pct_pred)) +
	geom_line(linewidth = 1) +
	geom_ribbon(aes(ymin = lower_pct,
									ymax = upper_pct),
							alpha = 0.20) +
	theme_minimal() +
	labs(
		x = "Humedad del suelo (%)",
		y = "Incidencia predicha (%)",
		title = "Incidencia predicha del ataque patógeno"
	)


# ------------------------------------------------------------
# 19. GLM Poisson para conteos
# ------------------------------------------------------------

# Crear una variable de conteo didáctica:
# número de lesiones observadas por parcela.
# Se construye a partir de síntomas y una variación aleatoria pequeña.

set.seed(123)

datos <- datos %>%
	mutate(
		Lesion_count = round(
			Symptom_leaf_spot +
				Symptom_necrosis +
				Symptom_stem_lesion +
				Symptom_canker +
				rpois(n(), lambda = 3)
		)
	)

summary(datos$Lesion_count)

ggplot(datos, aes(x = Lesion_count)) +
	geom_histogram(bins = 15,
								 color = "black") +
	theme_minimal() +
	labs(
		x = "Número de lesiones",
		y = "Frecuencia",
		title = "Distribución del número de lesiones por parcela"
	)


modelo_pois <- glm(
	Lesion_count ~ Disease_class +
		Soil_moisture_pct +
		Relative_humidity_pct,
	data = datos,
	family = poisson(link = "log")
)

summary(modelo_pois)


# Coeficientes exponenciados:
# se interpretan como razón de tasas o razón de conteos esperados.

broom::tidy(modelo_pois,
						conf.int = TRUE,
						exponentiate = TRUE)


# Diagnóstico del modelo Poisson
check_model(modelo_pois)

res_pois <- simulateResiduals(modelo_pois)
plot(res_pois)


# ------------------------------------------------------------
# 20. Evaluación de sobredispersión
# ------------------------------------------------------------

sobredispersion_pois <- deviance(modelo_pois) / df.residual(modelo_pois)

sobredispersion_pois

# Interpretación práctica:
# Valor cercano a 1: dispersión compatible con Poisson.
# Valor > 1.5: posible sobredispersión.
# Valor > 2: sobredispersión importante.


# También se puede usar performance
check_overdispersion(modelo_pois)


# ------------------------------------------------------------
# 21. Modelo binomial negativo
# ------------------------------------------------------------

modelo_nb <- glm.nb(
	Lesion_count ~ Disease_class +
		Soil_moisture_pct +
		Relative_humidity_pct,
	data = datos
)

summary(modelo_nb)

# Coeficientes exponenciados
broom::tidy(modelo_nb,
						conf.int = TRUE,
						exponentiate = TRUE)


# Comparación Poisson vs binomial negativo
AIC(modelo_pois, modelo_nb)

BIC(modelo_pois, modelo_nb)


# Diagnóstico binomial negativo
check_model(modelo_nb)

res_nb <- simulateResiduals(modelo_nb)
plot(res_nb)

check_overdispersion(modelo_nb)


# ------------------------------------------------------------
# 22. Predicción de conteos
# ------------------------------------------------------------

nuevos_count <- expand.grid(
	Disease_class = levels(datos$Disease_class),
	Soil_moisture_pct = mean(datos$Soil_moisture_pct, na.rm = TRUE),
	Relative_humidity_pct = mean(datos$Relative_humidity_pct, na.rm = TRUE)
)

pred_count <- predict(
	modelo_nb,
	newdata = nuevos_count,
	type = "link",
	se.fit = TRUE
)

nuevos_count <- nuevos_count %>%
	mutate(
		fit_link = pred_count$fit,
		se_link = pred_count$se.fit,
		count_pred = exp(fit_link),
		lower = exp(fit_link - 1.96 * se_link),
		upper = exp(fit_link + 1.96 * se_link)
	)

nuevos_count


ggplot(nuevos_count,
			 aes(x = Disease_class,
			 		y = count_pred,
			 		fill = Disease_class)) +
	geom_col(alpha = 0.75) +
	geom_errorbar(aes(ymin = lower,
										ymax = upper),
								width = 0.15) +
	theme_minimal() +
	labs(
		x = "Clase sanitaria",
		y = "Conteo esperado de lesiones",
		title = "Conteo esperado de lesiones por clase sanitaria"
	) +
	theme(legend.position = "none")


# ------------------------------------------------------------
# 23. GLM Gamma para respuesta positiva continua
# ------------------------------------------------------------

# Variable ejemplo:
# Pathogen_score como índice continuo positivo de presión patogénica.

summary(datos$Pathogen_score)

# Asegurar que no existan ceros
datos <- datos %>%
	mutate(
		Pathogen_score_pos = ifelse(Pathogen_score <= 0,
																0.01,
																Pathogen_score)
	)

modelo_gamma <- glm(
	Pathogen_score_pos ~ Soil_moisture_pct +
		Relative_humidity_pct +
		Mean_temp_C +
		Drainage_class,
	data = datos,
	family = Gamma(link = "log")
)

summary(modelo_gamma)


# Coeficientes exponenciados para interpretar cambios multiplicativos
broom::tidy(modelo_gamma,
						conf.int = TRUE,
						exponentiate = TRUE)


# Diagnóstico
check_model(modelo_gamma)

res_gamma <- simulateResiduals(modelo_gamma)
plot(res_gamma)


# ------------------------------------------------------------
# 24. Predicción para GLM Gamma
# ------------------------------------------------------------

nuevos_gamma <- data.frame(
	Soil_moisture_pct = seq(
		min(datos$Soil_moisture_pct, na.rm = TRUE),
		max(datos$Soil_moisture_pct, na.rm = TRUE),
		length.out = 100
	),
	Relative_humidity_pct = mean(datos$Relative_humidity_pct, na.rm = TRUE),
	Mean_temp_C = mean(datos$Mean_temp_C, na.rm = TRUE),
	Drainage_class = factor("Moderate",
													levels = levels(datos$Drainage_class))
)

pred_gamma <- predict(
	modelo_gamma,
	newdata = nuevos_gamma,
	type = "link",
	se.fit = TRUE
)

nuevos_gamma <- nuevos_gamma %>%
	mutate(
		fit_link = pred_gamma$fit,
		se_link = pred_gamma$se.fit,
		fit = exp(fit_link),
		lower = exp(fit_link - 1.96 * se_link),
		upper = exp(fit_link + 1.96 * se_link)
	)

ggplot(nuevos_gamma,
			 aes(x = Soil_moisture_pct,
			 		y = fit)) +
	geom_line(linewidth = 1) +
	geom_ribbon(aes(ymin = lower,
									ymax = upper),
							alpha = 0.20) +
	theme_minimal() +
	labs(
		x = "Humedad del suelo (%)",
		y = "Índice patogénico predicho",
		title = "Predicción del índice de presión patogénica"
	)


# ------------------------------------------------------------
# 25. Comparación de modelos GLM binomiales
# ------------------------------------------------------------

# Comparación de modelos para probabilidad de ataque severo

g1 <- glm(
	Severe_attack ~ Soil_moisture_pct,
	data = datos,
	family = binomial(link = "logit")
)

g2 <- glm(
	Severe_attack ~ Soil_moisture_pct +
		Relative_humidity_pct,
	data = datos,
	family = binomial(link = "logit")
)

g3 <- glm(
	Severe_attack ~ Soil_moisture_pct +
		Relative_humidity_pct +
		Mean_temp_C,
	data = datos,
	family = binomial(link = "logit")
)

g4 <- glm(
	Severe_attack ~ Soil_moisture_pct +
		Relative_humidity_pct +
		Mean_temp_C +
		Drainage_class,
	data = datos,
	family = binomial(link = "logit")
)

AIC(g1, g2, g3, g4)

BIC(g1, g2, g3, g4)

model.sel(list(
	g1_humedad_suelo = g1,
	g2_humedad_suelo_humedad_relativa = g2,
	g3_ambiente = g3,
	g4_ambiente_drenaje = g4
))


# Comparación de modelos anidados
anova(g1, g2, g3, g4, test = "Chisq")


# ------------------------------------------------------------
# 26. Tablas finales para reporte
# ------------------------------------------------------------

# Tabla modelo lineal final
tabla_lm_final <- broom::tidy(modelo_final_lm,
															conf.int = TRUE)

tabla_lm_final


# Tabla odds ratios modelo logístico
tabla_logit_or <- broom::tidy(modelo_logit,
															conf.int = TRUE,
															exponentiate = TRUE)

tabla_logit_or


# Tabla modelo binomial proporciones
tabla_binom_prop <- broom::tidy(modelo_binom_prop,
																conf.int = TRUE,
																exponentiate = TRUE)

tabla_binom_prop


# Tabla modelo Poisson
tabla_pois <- broom::tidy(modelo_pois,
													conf.int = TRUE,
													exponentiate = TRUE)

tabla_pois


# Tabla modelo binomial negativo
tabla_nb <- broom::tidy(modelo_nb,
												conf.int = TRUE,
												exponentiate = TRUE)

tabla_nb


# Tabla modelo Gamma
tabla_gamma <- broom::tidy(modelo_gamma,
													 conf.int = TRUE,
													 exponentiate = TRUE)

tabla_gamma


# Resumen de ajuste de modelos principales
ajuste_modelos <- bind_rows(
	broom::glance(modelo_final_lm) %>%
		mutate(Modelo = "Regresión múltiple"),
	broom::glance(modelo_logit) %>%
		mutate(Modelo = "GLM binomial logístico"),
	broom::glance(modelo_binom_prop) %>%
		mutate(Modelo = "GLM binomial proporciones"),
	broom::glance(modelo_pois) %>%
		mutate(Modelo = "GLM Poisson"),
	broom::glance(modelo_nb) %>%
		mutate(Modelo = "GLM binomial negativo"),
	broom::glance(modelo_gamma) %>%
		mutate(Modelo = "GLM Gamma")
)

ajuste_modelos


# ------------------------------------------------------------
# 27. Exportar tablas si se desea
# ------------------------------------------------------------

# write.csv(tabla_lm_final,
#           "tabla_regresion_multiple.csv",
#           row.names = FALSE)

# write.csv(tabla_logit_or,
#           "tabla_glm_logistico_odds_ratios.csv",
#           row.names = FALSE)

# write.csv(tabla_binom_prop,
#           "tabla_glm_binomial_proporciones.csv",
#           row.names = FALSE)

# write.csv(tabla_nb,
#           "tabla_glm_binomial_negativo.csv",
#           row.names = FALSE)

# write.csv(ajuste_modelos,
#           "tabla_ajuste_modelos_capitulo9.csv",
#           row.names = FALSE)


# ============================================================
# 28. Guía de interpretación para clase
# ============================================================

# REGRESIÓN MÚLTIPLE
# 1. Revisar si los predictores explican volumen.
# 2. Interpretar coeficientes manteniendo constantes los demás predictores.
# 3. Revisar residuos, normalidad, homogeneidad y colinealidad.
# 4. Comparar modelos con AIC, BIC y pruebas de modelos anidados.
# 5. Presentar predicciones con intervalos de confianza.

# GLM BINOMIAL
# 1. Usar cuando la respuesta es binaria o proporción de éxitos/fracasos.
# 2. Interpretar coeficientes como odds ratios.
# 3. Presentar probabilidades predichas.

# GLM POISSON
# 1. Usar para conteos.
# 2. Revisar sobredispersión.
# 3. Si hay sobredispersión, considerar binomial negativo.

# GLM GAMMA
# 1. Usar para respuestas continuas positivas y asimétricas.
# 2. Interpretar efectos multiplicativos cuando se usa enlace log.

# PRINCIPIO GENERAL
# El modelo debe seleccionarse por:
# a) pregunta biológica,
# b) tipo de variable respuesta,
# c) diagnóstico estadístico,
# d) simplicidad,
# e) capacidad interpretativa.
