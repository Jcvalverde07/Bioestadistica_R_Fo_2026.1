#############################################################
# Curso: Bioestadistica con R
# Practica de clase: imputacion de datos climaticos en R
# Base de datos: Cap_04_NA_data.xlsx
#############################################################

# 1. Cargar paquetes ----

library(readxl)
library(dplyr)
library(ggplot2)
library(naniar)
library(VIM)
library(mice)
library(writexl)

# 2. Importar archivo ----

datos <- read_excel(
  "~/GitHub/Bioestadica_R/Bioestadista_R/Cap_04_NA_data.xlsx",
  sheet = "DATA"
)

# Revisar estructura inicial
str(datos)
summary(datos)
head(datos)

# 3. Cuantificar valores faltantes ----

conteo_na <- sapply(datos, function(x) sum(is.na(x)))
porc_na   <- round(100 * sapply(datos, function(x) mean(is.na(x))), 2)

resumen_na <- data.frame(
  variable = names(conteo_na),
  n_faltantes = conteo_na,
  porcentaje = porc_na
)

print(resumen_na)

# 4. Visualizar patron de faltantes ----

vis_miss(datos)

# Otra opcion grafica
aggr(datos, numbers = TRUE, sortVars = TRUE, cex.axis = 0.7)

# Patron matricial de mice
md.pattern(datos)

# 5. Seleccionar variables para imputacion ----
# Se excluyen variables identificadoras o auxiliares
# como Fecha y Estacion del modelo de imputacion.

vars_imputar <- datos %>%
  select(
    Tmin_C,
    Tmax_C,
    Humedad_rel_pct,
    Precipitacion_mm,
    Radiacion_MJ_m2,
    Viento_m_s
  )

summary(vars_imputar)

# 5.1 Revisar variables con muy poca variacion ----

n_unicos <- sapply(vars_imputar, function(x) length(unique(na.omit(x))))
print(n_unicos)

# Eliminar variables con 1 o 0 valores distintos observados
vars_imputar <- vars_imputar[, n_unicos > 1, drop = FALSE]

# 5.2 Revisar correlacion entre variables numericas ----
# Esto ayuda a detectar colinealidad extrema.

cor_mat <- cor(vars_imputar, use = "pairwise.complete.obs")
print(round(cor_mat, 3))

# 6. Comparacion con una imputacion simple (media) ----

datos_media <- vars_imputar

for (i in seq_along(datos_media)) {
  if (is.numeric(datos_media[[i]])) {
    datos_media[[i]][is.na(datos_media[[i]])] <- mean(datos_media[[i]], na.rm = TRUE)
  }
}

summary(datos_media)

# Comparar distribucion original vs imputada por media
ggplot() +
  geom_density(
    data = vars_imputar,
    aes(x = Tmin_C),
    na.rm = TRUE,
    linewidth = 1
  ) +
  geom_density(
    data = datos_media,
    aes(x = Tmin_C),
    linetype = "dashed",
    linewidth = 1
  ) +
  labs(
    title = "Comparacion de Tmin_C",
    subtitle = "Linea continua: datos observados | linea discontinua: imputacion por media",
    x = "Temperatura minima (C)",
    y = "Densidad"
  ) +
  theme_minimal()

# 7. Imputacion multiple con PMM ----
# PMM (Predictive Mean Matching) reemplaza NA con valores observados
# tomados de casos con predicciones similares.
# Esto preserva mejor la distribucion empirica que la media.

# Definir metodo para todas las variables
metodos <- rep("pmm", ncol(vars_imputar))
names(metodos) <- names(vars_imputar)

# Construir matriz de predictores
pred <- make.predictorMatrix(vars_imputar)

# Evitar que cada variable se use a si misma
diag(pred) <- 0

# 7.1 Reducir riesgo de singularidad ----
# Si hay correlaciones extremadamente altas, se anulan esos predictores.
# Umbral sugerido: |r| > 0.98

cor_altas <- which(abs(cor_mat) > 0.98 & abs(cor_mat) < 1, arr.ind = TRUE)

if (nrow(cor_altas) > 0) {
  for (i in seq_len(nrow(cor_altas))) {
    fila <- rownames(cor_mat)[cor_altas[i, 1]]
    col  <- colnames(cor_mat)[cor_altas[i, 2]]
    pred[fila, col] <- 0
  }
}

print(pred)

# 7.2 Ejecutar imputacion ----
set.seed(123)

imp <- mice(
  vars_imputar,
  m = 5,                  # numero de bases imputadas
  method = metodos,
  predictorMatrix = pred,
  maxit = 10,
  seed = 123,
  printFlag = TRUE
)

# Resumen del objeto de imputacion
print(imp)

# Patron final de una de las bases imputadas
md.pattern(complete(imp, 1))

# 8. Diagnosticos de imputacion ----

# Convergencia
plot(imp)

# Comparacion de distribuciones observadas vs imputadas
densityplot(imp)

# Dispersion de valores imputados
stripplot(imp, pch = 20, cex = 0.8)

# 9. Extraer una base imputada ----

datos_pmm <- complete(imp, 1)

# Reintegrar variables originales no imputadas
# Se incluyen Fecha y Estacion  tal como estaban en la base original.
datos_final <- bind_cols(
  datos %>% select(Fecha, Estacion),
  datos_pmm
)


# Verificar faltantes despues de imputar
faltantes_finales <- sapply(datos_final, function(x) sum(is.na(x)))
print(faltantes_finales)

# 10. Comparar original vs PMM en una variable ----

ggplot() +
  geom_density(
    data = vars_imputar,
    aes(x = Radiacion_MJ_m2),
    na.rm = TRUE,
    linewidth = 1
  ) +
  geom_density(
    data = datos_pmm,
    aes(x = Radiacion_MJ_m2),
    linetype = "dashed",
    linewidth = 1
  ) +
  labs(
    title = "Radiacion_MJ_m2: observado vs PMM",
    subtitle = "Continua = observado | discontinua = imputado con PMM",
    x = "Radiacion solar (MJ m-2 dia-1)",
    y = "Densidad"
  ) +
  theme_minimal()

# Boxplot comparativo para precipitacion
bind_rows(
  vars_imputar %>% mutate(Metodo = "Observado"),
  datos_media %>% mutate(Metodo = "Media"),
  datos_pmm %>% mutate(Metodo = "PMM")
) %>%
  ggplot(aes(x = Metodo, y = Precipitacion_mm)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Comparacion de metodos de imputacion",
    y = "Precipitacion (mm)"
  )

# 11. Exportar resultados ----

write_xlsx(
  list(
    datos_originales = datos,
    resumen_na = resumen_na,
    imputacion_simple_media = datos_media,
    datos_imputados_PMM = datos_final
  ),
  path = "resultados_imputacion_climatica.xlsx"
)

# 12. Comentario final para clase ----
cat("\nImputacion completada.\n")
cat("Archivo exportado: resultados_imputacion_climatica.xlsx\n")
