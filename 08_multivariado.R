# 1. Paquetes ----

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

library(readxl)
library(ggplot2)
library(tidyr)
library(factoextra)
library(vegan)
library(corrplot)
library(ggrepel)
library(dplyr)


# 2. Importación de datos ----

file_path <- "C:/Users/jcval/OneDrive - TEC/Courses/2026_1st_R/Material/Cap_08_multivariados.xlsx"

if(!file.exists(file_path)){
	stop("No se encontró el archivo. Revise la ruta: ", file_path)
}

datos <- readxl::read_excel(
	file_path,
	sheet = "Datos"
)

str(datos)
head(datos)
names(datos)
summary(datos)


# 3. Preparación básica de la base ----

datos <- datos %>%
	dplyr::mutate(
		Plot_ID = as.factor(Plot_ID),
		Farm = as.factor(Farm),
		Region = as.factor(Region),
		Block = as.factor(Block),
		Disease_class = factor(
			Disease_class,
			levels = c("Baja", "Moderada", "Severa")
		),
		Drainage_class = factor(
			Drainage_class,
			levels = c("Bueno", "Moderado", "Deficiente")
		)
	)

table(datos$Disease_class)
table(datos$Region)
colSums(is.na(datos))


# 4. Definición de grupos de variables ----

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

vars_growth <- c(
	"DBH_cm",
	"Height_m",
	"Basal_area_m2_ha",
	"Volume_m3_ha",
	"Biomass_Mg_ha",
	"MAI_m3_ha_yr",
	"Survival_pct"
)

vars_vigor <- c(
	"LAI",
	"Chlorophyll_index",
	"Leaf_N_pct",
	"Leaf_water_content_pct",
	"Crown_density_pct",
	"Crown_transparency_pct"
)

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


# 5. Verificación de variables ----

vars_all <- c(
	vars_disease,
	vars_growth,
	vars_vigor,
	vars_env,
	vars_symptoms,
	"Disease_class",
	"Region",
	"Plot_ID"
)

vars_faltantes <- setdiff(vars_all, names(datos))

if(length(vars_faltantes) > 0){
	stop(
		"Estas variables no existen en la base: ",
		paste(vars_faltantes, collapse = ", ")
	)
}


# 6. Exploración inicial de los datos ----

resumen_sanitario <- datos %>%
	dplyr::group_by(Disease_class) %>%
	dplyr::summarise(
		n = dplyr::n(),
		Incidence_mean = mean(Incidence_pct, na.rm = TRUE),
		Severity_mean = mean(Severity_index, na.rm = TRUE),
		Defoliation_mean = mean(Defoliation_pct, na.rm = TRUE),
		Mortality_mean = mean(Mortality_pct, na.rm = TRUE),
		DBH_mean = mean(DBH_cm, na.rm = TRUE),
		Height_mean = mean(Height_m, na.rm = TRUE),
		Volume_mean = mean(Volume_m3_ha, na.rm = TRUE),
		LAI_mean = mean(LAI, na.rm = TRUE),
		Chlorophyll_mean = mean(Chlorophyll_index, na.rm = TRUE),
		.groups = "drop"
	)

resumen_sanitario


# 7. Gráfico de severidad por clase sanitaria ----

ggplot(datos, aes(x = Disease_class, y = Severity_index, fill = Disease_class)) +
	geom_boxplot(alpha = 0.7) +
	theme_minimal() +
	labs(
		x = "Clase sanitaria",
		y = "Índice de severidad",
		title = "Severidad del ataque patógeno por clase sanitaria"
	) +
	theme(legend.position = "none")


# 8. Relación entre severidad y volumen ----

ggplot(datos, aes(x = Severity_index, y = Volume_m3_ha, color = Disease_class)) +
	geom_point(size = 3, alpha = 0.8) +
	geom_smooth(method = "lm", se = TRUE) +
	theme_minimal() +
	labs(
		x = "Índice de severidad",
		y = "Volumen (m³ ha⁻¹)",
		title = "Relación entre severidad del ataque y volumen"
	)


# 9. Matriz de correlación ----

datos_corr <- datos %>%
	dplyr::select(dplyr::all_of(c(vars_disease, vars_growth, vars_vigor))) %>%
	stats::na.omit()

matriz_cor <- stats::cor(
	datos_corr,
	use = "complete.obs"
)

corrplot::corrplot(
	matriz_cor,
	method = "color",
	type = "upper",
	tl.cex = 0.7,
	tl.col = "black"
)


# 10. Preparación de datos para PCA ----

pca_vars <- c(
	"Incidence_pct",
	"Severity_index",
	"Defoliation_pct",
	"Mortality_pct",
	"DBH_cm",
	"Height_m",
	"Volume_m3_ha",
	"Biomass_Mg_ha",
	"LAI",
	"Chlorophyll_index",
	"Crown_density_pct"
)

datos_pca_full <- datos %>%
	dplyr::select(
		Plot_ID,
		Disease_class,
		dplyr::all_of(pca_vars)
	) %>%
	stats::na.omit()

datos_pca <- datos_pca_full %>%
	dplyr::select(dplyr::all_of(pca_vars))


# 11. Análisis de Componentes Principales, PCA ----

pca <- stats::prcomp(
	datos_pca,
	center = TRUE,
	scale. = TRUE
)

summary(pca)

pca_importancia <- summary(pca)$importance
pca_importancia

pca$rotation

pca_scores <- as.data.frame(pca$x)

pca_scores <- pca_scores %>%
	dplyr::mutate(
		Plot_ID = datos_pca_full$Plot_ID,
		Disease_class = datos_pca_full$Disease_class
	)


# 12. Gráficos del PCA ----

factoextra::fviz_eig(
	pca,
	addlabels = TRUE,
	ylim = c(0, 60)
) +
	labs(title = "Varianza explicada por los componentes principales")

factoextra::fviz_pca_biplot(
	pca,
	habillage = pca_scores$Disease_class,
	addEllipses = TRUE,
	ellipse.level = 0.95,
	repel = TRUE,
	col.var = "black"
) +
	labs(title = "PCA de variables sanitarias, productivas y de vigor")

factoextra::fviz_pca_ind(
	pca,
	habillage = pca_scores$Disease_class,
	addEllipses = TRUE,
	repel = TRUE
) +
	labs(title = "Ordenación de parcelas según PCA")

factoextra::fviz_pca_var(
	pca,
	col.var = "contrib",
	gradient.cols = c("gray70", "gray30", "black"),
	repel = TRUE
) +
	labs(title = "Contribución de variables al PCA")

factoextra::fviz_contrib(
	pca,
	choice = "var",
	axes = 1,
	top = 10
) +
	labs(title = "Variables con mayor contribución al PC1")

factoextra::fviz_contrib(
	pca,
	choice = "var",
	axes = 2,
	top = 10
) +
	labs(title = "Variables con mayor contribución al PC2")


# 13. Preparación de datos para conglomerados ----

cluster_vars <- c(
	"Severity_index",
	"Defoliation_pct",
	"Dieback_pct",
	"Mortality_pct",
	"DBH_cm",
	"Height_m",
	"Volume_m3_ha",
	"LAI",
	"Chlorophyll_index",
	"Crown_density_pct"
)

datos_cluster_full <- datos %>%
	dplyr::select(
		Plot_ID,
		Disease_class,
		dplyr::all_of(cluster_vars)
	) %>%
	stats::na.omit()

datos_cluster <- datos_cluster_full %>%
	dplyr::select(dplyr::all_of(cluster_vars))

datos_cluster_scaled <- scale(datos_cluster)


# 14. Análisis de conglomerados jerárquicos ----

dist_cluster <- stats::dist(
	datos_cluster_scaled,
	method = "euclidean"
)

cluster_h <- stats::hclust(
	dist_cluster,
	method = "ward.D2"
)

plot(
	cluster_h,
	main = "Dendrograma de parcelas según perfil multivariado",
	xlab = "",
	sub = ""
)

factoextra::fviz_dend(
	cluster_h,
	k = 3,
	rect = TRUE,
	show_labels = FALSE,
	main = "Agrupamiento jerárquico de parcelas"
)

grupo_cluster <- stats::cutree(
	cluster_h,
	k = 3
)

datos_cluster_resultado <- datos_cluster_full %>%
	dplyr::mutate(
		Cluster = as.factor(grupo_cluster)
	)

resumen_cluster <- datos_cluster_resultado %>%
	dplyr::group_by(Cluster) %>%
	dplyr::summarise(
		n = dplyr::n(),
		Severity_mean = mean(Severity_index, na.rm = TRUE),
		Defoliation_mean = mean(Defoliation_pct, na.rm = TRUE),
		Mortality_mean = mean(Mortality_pct, na.rm = TRUE),
		DBH_mean = mean(DBH_cm, na.rm = TRUE),
		Volume_mean = mean(Volume_m3_ha, na.rm = TRUE),
		LAI_mean = mean(LAI, na.rm = TRUE),
		Chlorophyll_mean = mean(Chlorophyll_index, na.rm = TRUE),
		.groups = "drop"
	)

resumen_cluster

factoextra::fviz_cluster(
	list(
		data = datos_cluster_scaled,
		cluster = grupo_cluster
	),
	geom = "point",
	ellipse.type = "convex",
	main = "Clusters de parcelas en espacio multivariado"
)


# 15. Preparación de datos para PCoA ----

datos_symptoms_full <- datos %>%
	dplyr::select(
		Plot_ID,
		Disease_class,
		Region,
		dplyr::all_of(vars_symptoms)
	) %>%
	stats::na.omit()

datos_symptoms <- datos_symptoms_full %>%
	dplyr::select(dplyr::all_of(vars_symptoms))

dist_bray <- vegan::vegdist(
	datos_symptoms,
	method = "bray"
)


# 16. Análisis de Coordenadas Principales, PCoA ----

pcoa <- stats::cmdscale(
	dist_bray,
	eig = TRUE,
	k = 2
)

pcoa_scores <- as.data.frame(pcoa$points)

colnames(pcoa_scores) <- c("PCoA1", "PCoA2")

pcoa_scores <- pcoa_scores %>%
	dplyr::mutate(
		Plot_ID = datos_symptoms_full$Plot_ID,
		Disease_class = datos_symptoms_full$Disease_class
	)

var_pcoa <- round(
	100 * pcoa$eig / sum(pcoa$eig[pcoa$eig > 0]),
	2
)

var_pcoa[1:5]

ggplot(
	pcoa_scores,
	aes(x = PCoA1, y = PCoA2, color = Disease_class)
) +
	geom_point(size = 3, alpha = 0.8) +
	stat_ellipse(level = 0.95) +
	theme_minimal() +
	labs(
		x = paste0("PCoA1 (", var_pcoa[1], "%)"),
		y = paste0("PCoA2 (", var_pcoa[2], "%)"),
		title = "PCoA basado en síntomas del ataque patógeno",
		color = "Clase sanitaria"
	)


# 17. Escalamiento multidimensional no métrico, NMDS ----

set.seed(123)

nmds <- vegan::metaMDS(
	datos_symptoms,
	distance = "bray",
	k = 2,
	trymax = 100,
	autotransform = FALSE
)

nmds
nmds$stress

nmds_scores <- as.data.frame(
	vegan::scores(nmds, display = "sites")
)

nmds_scores <- nmds_scores %>%
	dplyr::mutate(
		Plot_ID = datos_symptoms_full$Plot_ID,
		Disease_class = datos_symptoms_full$Disease_class,
		Region = datos_symptoms_full$Region
	)

ggplot(
	nmds_scores,
	aes(x = NMDS1, y = NMDS2, color = Disease_class)
) +
	geom_point(size = 3, alpha = 0.8) +
	stat_ellipse(level = 0.95) +
	theme_minimal() +
	labs(
		title = paste0(
			"NMDS de síntomas patogénicos, estrés = ",
			round(nmds$stress, 3)
		),
		color = "Clase sanitaria"
	)


# 18. NMDS con ajuste de variables ambientales ----

envfit_vars <- c(
	"Soil_pH",
	"Soil_moisture_pct",
	"Rainfall_mm",
	"Mean_temp_C",
	"Relative_humidity_pct"
)

datos_nmds_env_full <- datos %>%
	dplyr::select(
		Plot_ID,
		Disease_class,
		dplyr::all_of(vars_symptoms),
		dplyr::all_of(envfit_vars)
	) %>%
	stats::na.omit()

datos_nmds_env_symptoms <- datos_nmds_env_full %>%
	dplyr::select(dplyr::all_of(vars_symptoms))

datos_nmds_env <- datos_nmds_env_full %>%
	dplyr::select(dplyr::all_of(envfit_vars))

set.seed(123)

nmds_env <- vegan::metaMDS(
	datos_nmds_env_symptoms,
	distance = "bray",
	k = 2,
	trymax = 100,
	autotransform = FALSE
)

fit_env <- vegan::envfit(
	nmds_env,
	datos_nmds_env,
	permutations = 999
)

fit_env

plot(
	nmds_env,
	display = "sites",
	type = "n",
	main = "NMDS con variables ambientales"
)

points(
	nmds_env,
	display = "sites",
	col = as.numeric(datos_nmds_env_full$Disease_class),
	pch = 19
)

plot(
	fit_env,
	p.max = 0.05,
	col = "black"
)

legend(
	"topright",
	legend = levels(datos_nmds_env_full$Disease_class),
	col = seq_along(levels(datos_nmds_env_full$Disease_class)),
	pch = 19,
	bty = "n"
)


# 19. Preparación de datos para MANOVA ----

manova_vars <- c(
	"DBH_cm",
	"Height_m",
	"Volume_m3_ha",
	"Biomass_Mg_ha",
	"LAI",
	"Chlorophyll_index"
)

datos_manova <- datos %>%
	dplyr::select(
		Disease_class,
		dplyr::all_of(manova_vars)
	) %>%
	stats::na.omit()


# 20. Análisis multivariado de varianza, MANOVA ----

modelo_manova <- stats::manova(
	cbind(
		DBH_cm,
		Height_m,
		Volume_m3_ha,
		Biomass_Mg_ha,
		LAI,
		Chlorophyll_index
	) ~ Disease_class,
	data = datos_manova
)

summary(
	modelo_manova,
	test = "Pillai"
)

summary(
	modelo_manova,
	test = "Wilks"
)

summary(
	modelo_manova,
	test = "Hotelling-Lawley"
)

summary(
	modelo_manova,
	test = "Roy"
)

summary.aov(modelo_manova)


# 21. Visualización de respuestas del MANOVA ----

datos_long_manova <- datos_manova %>%
	tidyr::pivot_longer(
		cols = -Disease_class,
		names_to = "Variable",
		values_to = "Valor"
	)

ggplot(
	datos_long_manova,
	aes(x = Disease_class, y = Valor, fill = Disease_class)
) +
	geom_boxplot(alpha = 0.7) +
	facet_wrap(~ Variable, scales = "free_y") +
	theme_minimal() +
	labs(
		x = "Clase sanitaria",
		y = "Valor observado",
		title = "Variables productivas y de vigor por clase sanitaria"
	) +
	theme(legend.position = "none")


# 22. Análisis permutacional multivariado, PERMANOVA ----

permanova_symptoms <- vegan::adonis2(
	datos_symptoms ~ Disease_class,
	data = datos_symptoms_full,
	method = "bray",
	permutations = 999
)

permanova_symptoms

permanova_region <- vegan::adonis2(
	datos_symptoms ~ Disease_class + Region,
	data = datos_symptoms_full,
	method = "bray",
	permutations = 999
)

permanova_region

permanova_interaccion <- vegan::adonis2(
	datos_symptoms ~ Disease_class * Region,
	data = datos_symptoms_full,
	method = "bray",
	permutations = 999
)

permanova_interaccion


# 23. Dispersión multivariada ----

dispersion <- vegan::betadisper(
	dist_bray,
	datos_symptoms_full$Disease_class
)

anova(dispersion)

vegan::permutest(
	dispersion,
	permutations = 999
)

plot(
	dispersion,
	main = "Dispersión multivariada por clase sanitaria"
)

dist_centroid <- data.frame(
	Disease_class = datos_symptoms_full$Disease_class,
	Distance = dispersion$distances
)

ggplot(
	dist_centroid,
	aes(x = Disease_class, y = Distance, fill = Disease_class)
) +
	geom_boxplot(alpha = 0.7) +
	theme_minimal() +
	labs(
		x = "Clase sanitaria",
		y = "Distancia al centroide",
		title = "Homogeneidad de dispersión multivariada"
	) +
	theme(legend.position = "none")


# 24. Preparación de datos para RDA ----

rda_response_vars <- c(
	"Incidence_pct",
	"Severity_index",
	"Defoliation_pct",
	"Dieback_pct",
	"Mortality_pct",
	"Pathogen_score"
)

rda_explanatory_vars <- c(
	"Soil_pH",
	"Soil_OM_pct",
	"Soil_moisture_pct",
	"Rainfall_mm",
	"Mean_temp_C",
	"Relative_humidity_pct"
)

datos_rda <- datos %>%
	dplyr::select(
		dplyr::all_of(rda_response_vars),
		dplyr::all_of(rda_explanatory_vars)
	) %>%
	stats::na.omit()

Y_disease <- datos_rda %>%
	dplyr::select(dplyr::all_of(rda_response_vars))

X_env <- datos_rda %>%
	dplyr::select(dplyr::all_of(rda_explanatory_vars))

Y_disease_scaled <- scale(Y_disease)


# 25. Análisis de redundancia, RDA ----

rda_modelo <- vegan::rda(
	Y_disease_scaled ~ Soil_pH +
		Soil_OM_pct +
		Soil_moisture_pct +
		Rainfall_mm +
		Mean_temp_C +
		Relative_humidity_pct,
	data = X_env
)

summary(rda_modelo)

anova(
	rda_modelo,
	permutations = 999
)

anova(
	rda_modelo,
	by = "axis",
	permutations = 999
)

anova(
	rda_modelo,
	by = "terms",
	permutations = 999
)

vegan::RsquareAdj(rda_modelo)

plot(
	rda_modelo,
	scaling = 2,
	main = "RDA: daño patogénico explicado por ambiente"
)

vegan::ordiplot(
	rda_modelo,
	scaling = 2,
	type = "text"
)


# 26. Preparación de datos para CCA ----

cca_explanatory_vars <- c(
	"Soil_pH",
	"Soil_moisture_pct",
	"Rainfall_mm",
	"Mean_temp_C",
	"Relative_humidity_pct"
)

datos_cca <- datos %>%
	dplyr::select(
		dplyr::all_of(vars_symptoms),
		dplyr::all_of(cca_explanatory_vars)
	) %>%
	stats::na.omit()

Y_symptoms <- datos_cca %>%
	dplyr::select(dplyr::all_of(vars_symptoms))

X_cca <- datos_cca %>%
	dplyr::select(dplyr::all_of(cca_explanatory_vars))


# 27. Análisis de correspondencia canónica, CCA ----

cca_modelo <- vegan::cca(
	Y_symptoms ~ Soil_pH +
		Soil_moisture_pct +
		Rainfall_mm +
		Mean_temp_C +
		Relative_humidity_pct,
	data = X_cca
)

summary(cca_modelo)

anova(
	cca_modelo,
	permutations = 999
)

anova(
	cca_modelo,
	by = "axis",
	permutations = 999
)

anova(
	cca_modelo,
	by = "terms",
	permutations = 999
)

plot(
	cca_modelo,
	scaling = 2,
	main = "CCA: síntomas patogénicos y gradientes ambientales"
)


# 28. Preparación de datos para LDA ----

lda_vars <- c(
	"Incidence_pct",
	"Severity_index",
	"Defoliation_pct",
	"Mortality_pct",
	"DBH_cm",
	"Height_m",
	"Volume_m3_ha",
	"LAI",
	"Chlorophyll_index"
)

datos_lda <- datos %>%
	dplyr::select(
		Disease_class,
		dplyr::all_of(lda_vars)
	) %>%
	stats::na.omit()


# 29. Análisis discriminante lineal, LDA ----

lda_modelo <- MASS::lda(
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

pred_lda <- predict(lda_modelo)

tabla_clasificacion <- table(
	Observado = datos_lda$Disease_class,
	Predicho = pred_lda$class
)

tabla_clasificacion

precision <- sum(diag(tabla_clasificacion)) / sum(tabla_clasificacion)

precision

lda_scores <- as.data.frame(pred_lda$x)

lda_scores <- lda_scores %>%
	dplyr::mutate(
		Disease_class = datos_lda$Disease_class
	)

ggplot(
	lda_scores,
	aes(x = LD1, y = LD2, color = Disease_class)
) +
	geom_point(size = 3, alpha = 0.8) +
	stat_ellipse(level = 0.95) +
	theme_minimal() +
	labs(
		title = "Análisis discriminante de clases sanitarias",
		x = "LD1",
		y = "LD2",
		color = "Clase sanitaria"
	)


