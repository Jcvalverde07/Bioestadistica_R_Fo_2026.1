# ============================================================
# Curso: Bioestadística con R
# Tema: Técnicas multivariadas
# Caso de estudio: Ataque patógeno en Gmelina arborea
# ============================================================


# 1. Instalación y carga de paquetes ----

# Instalar paquetes si no están instalados
paquetes <- c(
	"readxl",
	"dplyr",
	"ggplot2",
	"tidyr",
	"factoextra",
	"vegan",
	"MASS",
	"corrplot",
	"ggrepel"
)

instalar <- paquetes[!(paquetes %in% installed.packages()[, "Package"])]

if(length(instalar) > 0){
	install.packages(instalar)
}

# Cargar paquetes
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(factoextra)
library(vegan)
library(MASS)
library(corrplot)
library(ggrepel)


# 2. Importar base de datos ----

# Cambiar la ruta según la ubicación del archivo en su computadora
datos <- read_excel("melina_patogeno_multivariado.xlsx",
										sheet = "Datos")

# Revisar estructura general
str(datos)

# Primeras filas
head(datos)

# Nombres de variables
names(datos)

# Resumen general
summary(datos)


# 3. Preparación básica de la base -----

# Convertir variables categóricas a factor
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

# Revisar número de parcelas por clase sanitaria
table(datos$Disease_class)

# Revisar número de parcelas por región
table(datos$Region)

# Revisar datos faltantes
colSums(is.na(datos))


# 4. Definir grupos de variables ----

# Variables sanitarias
vars_disease <- c(
	"Incidence_pct",
	"Severity_index",
	"Defoliation_pct",
	"Leaf_spot_pct",
	"Canker_pct",
	"Dieback_pct",
	"Mortality_pct",
	"Pathogen_score"
)

# Variables de crecimiento y productividad
vars_growth <- c(
	"DBH_cm",
	"Height_m",
	"Basal_area_m2_ha",
	"Volume_m3_ha",
	"Biomass_Mg_ha",
	"MAI_m3_ha_yr",
	"Survival_pct"
)

# Variables fisiológicas o de vigor
vars_vigor <- c(
	"LAI",
	"Chlorophyll_index",
	"Leaf_N_pct",
	"Leaf_water_content_pct",
	"Crown_density_pct",
	"Crown_transparency_pct"
)

# Variables ambientales y de suelo
vars_env <- c(
	"Soil_pH",
	"Soil_OM_pct",
	"Soil_N_pct",
	"Soil_P_mg_kg",
	"Soil_K_cmol_kg",
	"Clay_pct",
	"Sand_pct",
	"Soil_moisture_pct",
	"Rainfall_mm",
	"Mean_temp_C",
	"Relative_humidity_pct"
)

# Variables de síntomas
vars_symptoms <- c(
	"Symptom_leaf_spot",
	"Symptom_chlorosis",
	"Symptom_necrosis",
	"Symptom_defoliation",
	"Symptom_canker",
	"Symptom_dieback",
	"Symptom_wilt",
	"Symptom_stem_lesion"
)


# 5. Exploración inicial de los datos ----

# Resumen por clase sanitaria
resumen_sanitario <- datos %>%
	group_by(Disease_class) %>%
	summarise(
		n = n(),
		Incidence_mean = mean(Incidence_pct, na.rm = TRUE),
		Severity_mean = mean(Severity_index, na.rm = TRUE),
		Defoliation_mean = mean(Defoliation_pct, na.rm = TRUE),
		Mortality_mean = mean(Mortality_pct, na.rm = TRUE),
		DBH_mean = mean(DBH_cm, na.rm = TRUE),
		Height_mean = mean(Height_m, na.rm = TRUE),
		Volume_mean = mean(Volume_m3_ha, na.rm = TRUE),
		LAI_mean = mean(LAI, na.rm = TRUE),
		Chlorophyll_mean = mean(Chlorophyll_index, na.rm = TRUE)
	)

resumen_sanitario


# Boxplot de severidad por clase sanitaria
ggplot(datos, aes(x = Disease_class,
									y = Severity_index,
									fill = Disease_class)) +
	geom_boxplot(alpha = 0.7) +
	theme_minimal() +
	labs(
		x = "Clase sanitaria",
		y = "Índice de severidad",
		title = "Severidad del ataque patógeno por clase sanitaria"
	) +
	theme(legend.position = "none")


# Relación entre severidad y volumen
ggplot(datos, aes(x = Severity_index,
									y = Volume_m3_ha,
									color = Disease_class)) +
	geom_point(size = 3, alpha = 0.8) +
	geom_smooth(method = "lm", se = TRUE) +
	theme_minimal() +
	labs(
		x = "Índice de severidad",
		y = "Volumen (m³ ha⁻¹)",
		title = "Relación entre severidad del ataque y volumen"
	)


# 6. Matriz de correlación ----

datos_corr <- datos %>%
	select(all_of(c(vars_disease, vars_growth, vars_vigor))) %>%
	na.omit()

matriz_cor <- cor(datos_corr)

corrplot(
	matriz_cor,
	method = "color",
	type = "upper",
	tl.cex = 0.7,
	tl.col = "black"
)


# 7. Análisis de Componentes Principales, PCA ----

# Objetivo:
# Reducir la dimensionalidad de variables sanitarias, productivas y de vigor.

datos_pca <- datos %>%
	select(
		Incidence_pct,
		Severity_index,
		Defoliation_pct,
		Mortality_pct,
		DBH_cm,
		Height_m,
		Volume_m3_ha,
		Biomass_Mg_ha,
		LAI,
		Chlorophyll_index,
		Crown_density_pct
	) %>%
	na.omit()

# Ejecutar PCA con estandarización
pca <- prcomp(datos_pca,
							center = TRUE,
							scale. = TRUE)

# Resumen del PCA
summary(pca)

# Importancia de componentes
pca_importancia <- summary(pca)$importance
pca_importancia

# Cargas de variables
pca$rotation

# Coordenadas de parcelas
pca_scores <- as.data.frame(pca$x)

# Agregar clase sanitaria
pca_scores$Disease_class <- datos$Disease_class[complete.cases(
	datos %>%
		select(
			Incidence_pct,
			Severity_index,
			Defoliation_pct,
			Mortality_pct,
			DBH_cm,
			Height_m,
			Volume_m3_ha,
			Biomass_Mg_ha,
			LAI,
			Chlorophyll_index,
			Crown_density_pct
		)
)]

# Gráfico de sedimentación
fviz_eig(pca,
				 addlabels = TRUE,
				 ylim = c(0, 60)) +
	labs(title = "Varianza explicada por los componentes principales")


# Biplot del PCA
fviz_pca_biplot(
	pca,
	habillage = pca_scores$Disease_class,
	addEllipses = TRUE,
	ellipse.level = 0.95,
	repel = TRUE,
	col.var = "black"
) +
	labs(title = "PCA de variables sanitarias, productivas y de vigor")


# PCA solo con individuos
fviz_pca_ind(
	pca,
	habillage = pca_scores$Disease_class,
	addEllipses = TRUE,
	repel = TRUE
) +
	labs(title = "Ordenación de parcelas según PCA")


# PCA solo con variables
fviz_pca_var(
	pca,
	col.var = "contrib",
	gradient.cols = c("gray70", "gray30", "black"),
	repel = TRUE
) +
	labs(title = "Contribución de variables al PCA")


# Contribución de variables al componente 1
fviz_contrib(pca,
						 choice = "var",
						 axes = 1,
						 top = 10) +
	labs(title = "Variables con mayor contribución al PC1")


# Contribución de variables al componente 2
fviz_contrib(pca,
						 choice = "var",
						 axes = 2,
						 top = 10) +
	labs(title = "Variables con mayor contribución al PC2")


# 8. Análisis de conglomerados ----

# Objetivo:
# Agrupar parcelas según perfiles sanitarios, productivos y fisiológicos.

datos_cluster <- datos %>%
	select(
		Severity_index,
		Defoliation_pct,
		Dieback_pct,
		Mortality_pct,
		DBH_cm,
		Height_m,
		Volume_m3_ha,
		LAI,
		Chlorophyll_index,
		Crown_density_pct
	) %>%
	na.omit()

# Estandarizar variables
datos_cluster_scaled <- scale(datos_cluster)

# Calcular distancia euclidiana
dist_cluster <- dist(datos_cluster_scaled,
										 method = "euclidean")

# Agrupamiento jerárquico
cluster_h <- hclust(dist_cluster,
										method = "ward.D2")

# Dendrograma base
plot(cluster_h,
		 main = "Dendrograma de parcelas según perfil multivariado",
		 xlab = "",
		 sub = "")

# Dendrograma con factoextra
fviz_dend(
	cluster_h,
	k = 3,
	rect = TRUE,
	show_labels = FALSE,
	main = "Agrupamiento jerárquico de parcelas"
)

# Cortar árbol en tres grupos
grupo_cluster <- cutree(cluster_h, k = 3)

# Agregar grupo a la base
datos_cluster_resultado <- datos %>%
	mutate(Cluster = as.factor(grupo_cluster))

# Resumen de variables por cluster
datos_cluster_resultado %>%
	group_by(Cluster) %>%
	summarise(
		n = n(),
		Severity_mean = mean(Severity_index, na.rm = TRUE),
		Defoliation_mean = mean(Defoliation_pct, na.rm = TRUE),
		Mortality_mean = mean(Mortality_pct, na.rm = TRUE),
		DBH_mean = mean(DBH_cm, na.rm = TRUE),
		Volume_mean = mean(Volume_m3_ha, na.rm = TRUE),
		LAI_mean = mean(LAI, na.rm = TRUE),
		Chlorophyll_mean = mean(Chlorophyll_index, na.rm = TRUE)
	)


# Visualizar clusters sobre el PCA
fviz_cluster(
	list(data = datos_cluster_scaled,
			 cluster = grupo_cluster),
	geom = "point",
	ellipse.type = "convex",
	main = "Clusters de parcelas en espacio multivariado"
)


# 9. Análisis de Coordenadas Principales, PCoA ----

# Objetivo:
# Ordenar parcelas usando una matriz de distancia basada en síntomas.

datos_symptoms <- datos %>%
	select(all_of(vars_symptoms)) %>%
	na.omit()

# Distancia Bray-Curtis
dist_bray <- vegdist(datos_symptoms,
										 method = "bray")

# PCoA
pcoa <- cmdscale(dist_bray,
								 eig = TRUE,
								 k = 2)

# Coordenadas
pcoa_scores <- as.data.frame(pcoa$points)
colnames(pcoa_scores) <- c("PCoA1", "PCoA2")

pcoa_scores$Disease_class <- datos$Disease_class[complete.cases(datos_symptoms)]
pcoa_scores$Plot_ID <- datos$Plot_ID[complete.cases(datos_symptoms)]

# Porcentaje de variación aproximada explicada
var_pcoa <- round(100 * pcoa$eig / sum(pcoa$eig[pcoa$eig > 0]), 2)

var_pcoa[1:5]

# Gráfico PCoA
ggplot(pcoa_scores,
			 aes(x = PCoA1,
			 		y = PCoA2,
			 		color = Disease_class)) +
	geom_point(size = 3, alpha = 0.8) +
	stat_ellipse(level = 0.95) +
	theme_minimal() +
	labs(
		x = paste0("PCoA1 (", var_pcoa[1], "%)"),
		y = paste0("PCoA2 (", var_pcoa[2], "%)"),
		title = "PCoA basado en síntomas del ataque patógeno",
		color = "Clase sanitaria"
	)


# 10. Escalamiento multidimensional no métrico, NMDS ----

# Objetivo:
# Explorar patrones no lineales de similitud entre parcelas según síntomas.

set.seed(123)

nmds <- metaMDS(
	datos_symptoms,
	distance = "bray",
	k = 2,
	trymax = 100,
	autotransform = FALSE
)

# Resultado del NMDS
nmds

# Valor de estrés
nmds$stress

# Coordenadas del NMDS
nmds_scores <- as.data.frame(scores(nmds, display = "sites"))
nmds_scores$Disease_class <- datos$Disease_class[complete.cases(datos_symptoms)]
nmds_scores$Plot_ID <- datos$Plot_ID[complete.cases(datos_symptoms)]

# Gráfico NMDS
ggplot(nmds_scores,
			 aes(x = NMDS1,
			 		y = NMDS2,
			 		color = Disease_class)) +
	geom_point(size = 3, alpha = 0.8) +
	stat_ellipse(level = 0.95) +
	theme_minimal() +
	labs(
		title = paste0("NMDS de síntomas patogénicos, estrés = ",
									 round(nmds$stress, 3)),
		color = "Clase sanitaria"
	)


# Ajustar variables ambientales al NMDS
env_nmds <- datos %>%
	select(
		Soil_pH,
		Soil_moisture_pct,
		Rainfall_mm,
		Mean_temp_C,
		Relative_humidity_pct
	) %>%
	na.omit()

fit_env <- envfit(nmds,
									env_nmds,
									permutations = 999)

fit_env

# Gráfico NMDS con variables ambientales
plot(nmds, display = "sites", type = "n")
points(nmds,
			 display = "sites",
			 col = as.numeric(datos$Disease_class),
			 pch = 19)
plot(fit_env,
		 p.max = 0.05,
		 col = "black")


# 11. MANOVA -----

# Objetivo:
# Evaluar si la clase sanitaria afecta simultáneamente variables
# de crecimiento y vigor.

modelo_manova <- manova(
	cbind(
		DBH_cm,
		Height_m,
		Volume_m3_ha,
		Biomass_Mg_ha,
		LAI,
		Chlorophyll_index
	) ~ Disease_class,
	data = datos
)

# Prueba multivariada con Pillai
summary(modelo_manova,
				test = "Pillai")

# Otras pruebas multivariadas
summary(modelo_manova,
				test = "Wilks")

summary(modelo_manova,
				test = "Hotelling-Lawley")

summary(modelo_manova,
				test = "Roy")

# ANOVA univariados posteriores
summary.aov(modelo_manova)


# Visualización de respuestas por clase sanitaria
datos_long_manova <- datos %>%
	select(
		Disease_class,
		DBH_cm,
		Height_m,
		Volume_m3_ha,
		Biomass_Mg_ha,
		LAI,
		Chlorophyll_index
	) %>%
	pivot_longer(
		cols = -Disease_class,
		names_to = "Variable",
		values_to = "Valor"
	)

ggplot(datos_long_manova,
			 aes(x = Disease_class,
			 		y = Valor,
			 		fill = Disease_class)) +
	geom_boxplot(alpha = 0.7) +
	facet_wrap(~ Variable,
						 scales = "free_y") +
	theme_minimal() +
	labs(
		x = "Clase sanitaria",
		y = "Valor observado",
		title = "Variables productivas y de vigor por clase sanitaria"
	) +
	theme(legend.position = "none")


# 12. PERMANOVA ----

# Objetivo:
# Evaluar si la estructura multivariada de síntomas difiere
# entre clases sanitarias.

permanova_symptoms <- adonis2(
	datos_symptoms ~ Disease_class,
	data = datos,
	method = "bray",
	permutations = 999
)

permanova_symptoms


# PERMANOVA incluyendo región
permanova_region <- adonis2(
	datos_symptoms ~ Disease_class + Region,
	data = datos,
	method = "bray",
	permutations = 999
)

permanova_region


# PERMANOVA con interacción
permanova_interaccion <- adonis2(
	datos_symptoms ~ Disease_class * Region,
	data = datos,
	method = "bray",
	permutations = 999
)

permanova_interaccion


# Evaluar homogeneidad de dispersión multivariada
dispersion <- betadisper(dist_bray,
												 datos$Disease_class)

anova(dispersion)

permutest(dispersion,
					permutations = 999)

plot(dispersion,
		 main = "Dispersión multivariada por clase sanitaria")


# Distancia al centroide por clase sanitaria
dist_centroid <- data.frame(
	Disease_class = datos$Disease_class,
	Distance = dispersion$distances
)

ggplot(dist_centroid,
			 aes(x = Disease_class,
			 		y = Distance,
			 		fill = Disease_class)) +
	geom_boxplot(alpha = 0.7) +
	theme_minimal() +
	labs(
		x = "Clase sanitaria",
		y = "Distancia al centroide",
		title = "Homogeneidad de dispersión multivariada"
	) +
	theme(legend.position = "none")


# 13. RDA: Análisis de redundancia ----

# Objetivo:
# Evaluar cuánto de la variación del daño patogénico puede ser
# explicado por variables de suelo y ambiente.

Y_disease <- datos %>%
	select(
		Incidence_pct,
		Severity_index,
		Defoliation_pct,
		Dieback_pct,
		Mortality_pct,
		Pathogen_score
	)

X_env <- datos %>%
	select(
		Soil_pH,
		Soil_OM_pct,
		Soil_moisture_pct,
		Rainfall_mm,
		Mean_temp_C,
		Relative_humidity_pct
	)

# Estandarizar matriz respuesta
Y_disease_scaled <- scale(Y_disease)

# RDA
rda_modelo <- rda(
	Y_disease_scaled ~ Soil_pH +
		Soil_OM_pct +
		Soil_moisture_pct +
		Rainfall_mm +
		Mean_temp_C +
		Relative_humidity_pct,
	data = X_env
)

# Resumen
summary(rda_modelo)

# Prueba global
anova(rda_modelo,
			permutations = 999)

# Prueba por ejes
anova(rda_modelo,
			by = "axis",
			permutations = 999)

# Prueba por términos
anova(rda_modelo,
			by = "terms",
			permutations = 999)

# Varianza explicada ajustada
RsquareAdj(rda_modelo)

# Gráfico RDA base
plot(rda_modelo,
		 scaling = 2,
		 main = "RDA: daño patogénico explicado por ambiente")


# Gráfico RDA con vegan
ordiplot(rda_modelo,
				 scaling = 2,
				 type = "text")


# 14. CCA: Análisis de correspondencia canónica ----

# Objetivo:
# Relacionar la matriz de síntomas con gradientes ambientales.
# Esta técnica es más apropiada cuando la respuesta tiene estructura
# de abundancia, conteo, intensidad o composición.

Y_symptoms <- datos %>%
	select(all_of(vars_symptoms))

X_cca <- datos %>%
	select(
		Soil_pH,
		Soil_moisture_pct,
		Rainfall_mm,
		Mean_temp_C,
		Relative_humidity_pct
	)

cca_modelo <- cca(
	Y_symptoms ~ Soil_pH +
		Soil_moisture_pct +
		Rainfall_mm +
		Mean_temp_C +
		Relative_humidity_pct,
	data = X_cca
)

# Resumen
summary(cca_modelo)

# Prueba global
anova(cca_modelo,
			permutations = 999)

# Prueba por ejes
anova(cca_modelo,
			by = "axis",
			permutations = 999)

# Prueba por términos
anova(cca_modelo,
			by = "terms",
			permutations = 999)

# Gráfico CCA
plot(cca_modelo,
		 scaling = 2,
		 main = "CCA: síntomas patogénicos y gradientes ambientales")


# 15. Análisis discriminante lineal, LDA ----

# Objetivo:
# Evaluar qué variables permiten separar las clases sanitarias.

datos_lda <- datos %>%
	select(
		Disease_class,
		Incidence_pct,
		Severity_index,
		Defoliation_pct,
		Mortality_pct,
		DBH_cm,
		Height_m,
		Volume_m3_ha,
		LAI,
		Chlorophyll_index
	) %>%
	na.omit()

lda_modelo <- lda(
	Disease_class ~ Incidence_pct +
		Severity_index +
		Defoliation_pct +
		Mortality_pct +
		DBH_cm +
		Height_m +
		Volume_m3_ha +
		LAI +
		Chlorophyll_index,
	data = datos_lda
)

lda_modelo

# Predicción
pred_lda <- predict(lda_modelo)

# Matriz de clasificación
tabla_clasificacion <- table(
	Observado = datos_lda$Disease_class,
	Predicho = pred_lda$class
)

tabla_clasificacion

# Precisión global
precision <- sum(diag(tabla_clasificacion)) / sum(tabla_clasificacion)
precision

# Coordenadas discriminantes
lda_scores <- as.data.frame(pred_lda$x)
lda_scores$Disease_class <- datos_lda$Disease_class

# Gráfico LDA
ggplot(lda_scores,
			 aes(x = LD1,
			 		y = LD2,
			 		color = Disease_class)) +
	geom_point(size = 3, alpha = 0.8) +
	stat_ellipse(level = 0.95) +
	theme_minimal() +
	labs(
		title = "Análisis discriminante de clases sanitarias",
		x = "LD1",
		y = "LD2",
		color = "Clase sanitaria"
	)


# 16. Tablas resumen para reporte ----

	# Tabla PCA: varianza explicada
	tabla_pca <- data.frame(
		Componente = paste0("PC", 1:length(pca$sdev)),
		Varianza = pca$sdev^2,
		Proporcion = summary(pca)$importance[2, ],
		Acumulada = summary(pca)$importance[3, ]
	)

tabla_pca


	# Tabla PERMANOVA
	tabla_permanova <- as.data.frame(permanova_symptoms)
	tabla_permanova
	
	
	# Tabla RDA por términos
	tabla_rda_terms <- anova(rda_modelo,
													 by = "terms",
													 permutations = 999)
	
tabla_rda_terms


	# Tabla CCA por términos
	tabla_cca_terms <- anova(cca_modelo,
													 by = "terms",
													 permutations = 999)

tabla_cca_terms
