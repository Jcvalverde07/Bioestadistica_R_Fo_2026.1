# =========================================================
# CAPÍTULO 7. ESTADÍSTICA NO PARAMÉTRICA CON R
# =========================================================

rm(list = ls())
cat("\014")

# 0. Paquetes ----

library(dplyr)
library(ggplot2)


# 1. GENERAR BASE DE DATOS SIMULADA ----

set.seed(123)

n <- 120

datos <- data.frame(
	Tratamiento = rep(c("Control", "Fertilizado", "Riego"), each = 40),
	Clon = rep(c("A", "B"), times = 60)
)

# Variable continua con distribución no normal
datos$DBH <- c(
	rgamma(40, shape = 8, scale = 0.7),   # Control
	rgamma(40, shape = 10, scale = 0.7),  # Fertilizado
	rgamma(40, shape = 12, scale = 0.7)   # Riego
)

# Otra variable continua asociada
datos$Altura <- datos$DBH * 0.65 + rlnorm(n, meanlog = 0.4, sdlog = 0.25)

# Variable ordinal
datos$Danio <- sample(
	x = c(1, 2, 3, 4, 5),
	size = n,
	replace = TRUE,
	prob = c(0.15, 0.25, 0.30, 0.20, 0.10)
)

# Variable categórica
datos$Supervivencia <- sample(
	x = c("Viva", "Muerta"),
	size = n,
	replace = TRUE,
	prob = c(0.82, 0.18)
)

# Crear datos pareados
set.seed(456)
altura_inicial <- round(rgamma(20, shape = 8, scale = 0.5), 2)
altura_final   <- round(altura_inicial + rnorm(20, mean = 0.8, sd = 0.6), 2)

# Ver estructura
str(datos)
summary(datos)
head(datos)

# 2. EXPLORACIÓN DESCRIPTIVA ----

# Resumen general
summary(datos$DBH)
median(datos$DBH)
IQR(datos$DBH)

# Resumen por tratamiento
datos %>%
	group_by(Tratamiento) %>%
	summarise(
		n = n(),
		mediana_DBH = median(DBH),
		Q1 = quantile(DBH, 0.25),
		Q3 = quantile(DBH, 0.75),
		min = min(DBH),
		max = max(DBH),
		.groups = "drop"
	)

# 3. GRÁFICOS EXPLORATORIOS ----

# Histograma
ggplot(datos, aes(x = DBH)) +
	geom_histogram(bins = 20) +
	theme_minimal() +
	labs(
		title = "Distribución de DBH",
		x = "DBH",
		y = "Frecuencia"
	)

# Boxplot por clon
ggplot(datos, aes(x = Clon, y = DBH)) +
	geom_boxplot() +
	theme_minimal() +
	labs(
		title = "DBH por clon",
		x = "Clon",
		y = "DBH"
	)

# Boxplot por tratamiento
ggplot(datos, aes(x = Tratamiento, y = DBH)) +
	geom_boxplot() +
	theme_minimal() +
	labs(
		title = "DBH por tratamiento",
		x = "Tratamiento",
		y = "DBH"
	)

# Dispersión
ggplot(datos, aes(x = Altura, y = DBH)) +
	geom_point() +
	geom_smooth(method = "loess", se = TRUE) +
	theme_minimal() +
	labs(
		title = "Relación entre Altura y DBH",
		x = "Altura",
		y = "DBH"
	)


# 4. WILCOXON / MANN-WHITNEY ----
# Comparación de dos grupos independientes


wilcox_ind <- wilcox.test(DBH ~ Clon, data = datos, exact = FALSE)
wilcox_ind

# Interpretación simple
if (wilcox_ind$p.value < 0.05) {
	cat("\nWilcoxon independiente: hay diferencias significativas entre clones.\n")
} else {
	cat("\nWilcoxon independiente: no se detectan diferencias significativas entre clones.\n")
}

# 5. WILCOXON PAREADO ----
# Comparación de dos mediciones relacionadas

wilcox_par <- wilcox.test(altura_inicial, altura_final, paired = TRUE, exact = FALSE)
wilcox_par

if (wilcox_par$p.value < 0.05) {
	cat("\nWilcoxon pareado: hay diferencias significativas entre altura inicial y final.\n")
} else {
	cat("\nWilcoxon pareado: no se detectan diferencias significativas entre altura inicial y final.\n")
}


# 6. KRUSKAL-WALLIS ----
# Comparación de tres o más grupos independientes

kruskal_res <- kruskal.test(DBH ~ Tratamiento, data = datos)
kruskal_res

if (kruskal_res$p.value < 0.05) {
	cat("\nKruskal-Wallis: al menos un tratamiento difiere significativamente.\n")
} else {
	cat("\nKruskal-Wallis: no se detectan diferencias significativas entre tratamientos.\n")
}


# 7. COMPARACIONES MÚLTIPLES POST HOC ----
# Después de Kruskal-Wallis

pairwise_res <- pairwise.wilcox.test(
	x = datos$DBH,
	g = datos$Tratamiento,
	p.adjust.method = "bonferroni"
)

pairwise_res


# 8. CORRELACIÓN DE SPEARMAN ----
# Asociación monotónica entre variables continuas

spearman_res <- cor.test(datos$Altura, datos$DBH, method = "spearman")
spearman_res

if (spearman_res$p.value < 0.05) {
	cat("\nSpearman: existe asociación monotónica significativa entre Altura y DBH.\n")
} else {
	cat("\nSpearman: no se detecta asociación monotónica significativa entre Altura y DBH.\n")
}

# 9. CHI-CUADRADO
# Asociación entre variables categóricas


tabla_chi <- table(datos$Tratamiento, datos$Supervivencia)
tabla_chi

chi_res <- chisq.test(tabla_chi)
chi_res

if (chi_res$p.value < 0.05) {
	cat("\nChi-cuadrado: existe asociación significativa entre Tratamiento y Supervivencia.\n")
} else {
	cat("\nChi-cuadrado: no se detecta asociación significativa entre Tratamiento y Supervivencia.\n")
}

# =========================================================
# 10. FISHER
# Alternativa cuando las frecuencias esperadas son bajas
# =========================================================

# Ejemplo con tabla 2x2
tabla_fisher <- matrix(c(12, 3, 7, 8), nrow = 2, byrow = TRUE)
rownames(tabla_fisher) <- c("Control", "Tratado")
colnames(tabla_fisher) <- c("Viva", "Muerta")

tabla_fisher

fisher_res <- fisher.test(tabla_fisher)
fisher_res

if (fisher_res$p.value < 0.05) {
	cat("\nFisher: existe asociación significativa en la tabla 2x2.\n")
} else {
	cat("\nFisher: no se detecta asociación significativa en la tabla 2x2.\n")
}

# =========================================================
# 11. RESÚMENES CONSISTENTES CON ANÁLISIS NO PARAMÉTRICO
# =========================================================

# Medianas e IQR por tratamiento
resumen_np <- datos %>%
	group_by(Tratamiento) %>%
	summarise(
		n = n(),
		mediana = median(DBH),
		IQR = IQR(DBH),
		Q1 = quantile(DBH, 0.25),
		Q3 = quantile(DBH, 0.75),
		.groups = "drop"
	)

print(resumen_np)

# Frecuencias para variable categórica
prop.table(table(datos$Tratamiento, datos$Supervivencia), margin = 1)

# =========================================================
# 12. MENSAJES FINALES
# =========================================================

cat("\n=========================================================\n")
cat("PRUEBAS NO PARAMÉTRICAS EJECUTADAS:\n")
cat("- Wilcoxon para grupos independientes\n")
cat("- Wilcoxon pareado\n")
cat("- Kruskal-Wallis\n")
cat("- Comparaciones múltiples con pairwise Wilcoxon\n")
cat("- Correlación de Spearman\n")
cat("- Chi-cuadrado\n")
cat("- Fisher exacta\n")
cat("=========================================================\n")
