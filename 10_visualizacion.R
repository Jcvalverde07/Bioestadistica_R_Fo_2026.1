###############################################################
# Curso: Bioestadística con R
# Capítulo 10: Visualización y comunicación de resultados en R
# Enfoque: ggplot2

###############################################################

# 1. Preparación del ambiente ----

paquetes <- c(
  "readxl", "dplyr", "tidyr", "ggplot2", "forcats",
  "scales", "patchwork", "broom", "ggrepel"
)

instalar <- paquetes[!(paquetes %in% installed.packages()[, "Package"])]
if (length(instalar) > 0) install.packages(instalar)

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(scales)
library(patchwork)
library(broom)
library(ggrepel)

# Crear carpeta para guardar figuras
if (!dir.exists("figuras_capitulo10")) dir.create("figuras_capitulo10")

# 2. Importación de la base de datos ----

# Coloque el archivo Excel en la misma carpeta del script.
datos <- read_excel("C:/Users/jcval/OneDrive - TEC/Courses/2026_1st_R/Material/Cap_10_visualizacion.xlsx", 
										sheet = "Data")

# Revisión inicial
str(datos)
summary(datos)
head(datos)

# Preparación de variables categóricas
datos <- datos %>%
  mutate(
    Region = as.factor(Region),
    Farm = as.factor(Farm),
    Block = as.factor(Block),
    Treatment = factor(
      Treatment,
      levels = c("Control", "Manejo preventivo", "Alta humedad")
    ),
    Drainage_class = factor(
      Drainage_class,
      levels = c("Good", "Moderate", "Poor")
    ),
    Disease_class = factor(
      Disease_class,
      levels = c("Low", "Moderate", "Severe")
    )
  )

# 3. Tema gráfico general para la clase ----


# Este tema se reutiliza en todas las figuras para mantener consistencia visual.
tema_clase <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

# Paleta discreta sobria para tratamientos
paleta_trat <- c(
  "Control" = "#4D4D4D",
  "Manejo preventivo" = "#1B9E77",
  "Alta humedad" = "#D95F02"
)

# 4. Gráfico 1: distribución de una variable continua ----


# Pregunta didáctica:
# ¿Cómo se distribuye el volumen por hectárea en las parcelas evaluadas?

p1 <- ggplot(datos, aes(x = Volume_m3_ha)) +
  geom_histogram(bins = 18, color = "white", fill = "grey45") +
  tema_clase +
  labs(
    title = "Distribución del volumen por hectárea",
    subtitle = "Histograma para identificar rango, concentración y asimetría",
    x = expression("Volumen"~(m^3~ha^{-1})),
    y = "Número de parcelas"
  )

p1

ggsave(
  filename = "figuras_capitulo11/figura_01_histograma_volumen.png",
  plot = p1,
  width = 7,
  height = 5,
  dpi = 300
)


# 5. Gráfico 2: comparación entre grupos con boxplot y puntos ----


# Pregunta didáctica:
# ¿El volumen difiere visualmente entre tratamientos?

p2 <- ggplot(datos, aes(x = Treatment, y = Volume_m3_ha, fill = Treatment)) +
  geom_boxplot(width = 0.55, alpha = 0.65, outlier.shape = NA) +
  geom_jitter(aes(color = Treatment), width = 0.12, alpha = 0.70, size = 2) +
  scale_fill_manual(values = paleta_trat) +
  scale_color_manual(values = paleta_trat) +
  tema_clase +
  guides(color = "none", fill = "none") +
  labs(
    title = "Volumen por hectárea según tratamiento",
    subtitle = "El boxplot resume la distribución; los puntos muestran las parcelas individuales",
    x = "Tratamiento",
    y = expression("Volumen"~(m^3~ha^{-1}))
  )

p2

ggsave(
  "figuras_capitulo11/figura_02_boxplot_volumen_tratamiento.png",
  p2,
  width = 7,
  height = 5,
  dpi = 300
)


# 6. Gráfico 3: relación entre dos variables continuas ----


# Pregunta didáctica:
# ¿Cómo se relaciona la severidad del ataque con el volumen?

p3 <- ggplot(datos, aes(x = Severity_index, y = Volume_m3_ha, color = Treatment)) +
  geom_point(alpha = 0.75, size = 2.4) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.9) +
  scale_color_manual(values = paleta_trat) +
  tema_clase +
  labs(
    title = "Relación entre severidad sanitaria y volumen",
    subtitle = "La línea representa una tendencia lineal con intervalo de confianza",
    x = "Índice de severidad sanitaria",
    y = expression("Volumen"~(m^3~ha^{-1})),
    color = "Tratamiento"
  )

p3

ggsave(
  "figuras_capitulo11/figura_03_dispersion_severidad_volumen.png",
  p3,
  width = 7.5,
  height = 5.2,
  dpi = 300
)


# 7. Gráfico 4: uso de facetas ----


# Pregunta didáctica:
# ¿La relación severidad-volumen cambia visualmente entre regiones?

p4 <- ggplot(datos, aes(x = Severity_index, y = Volume_m3_ha, color = Treatment)) +
  geom_point(alpha = 0.75, size = 2) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
  facet_wrap(~ Region) +
  scale_color_manual(values = paleta_trat) +
  tema_clase +
  labs(
    title = "Relación severidad-volumen por región",
    subtitle = "Las facetas permiten comparar patrones entre subconjuntos de datos",
    x = "Índice de severidad sanitaria",
    y = expression("Volumen"~(m^3~ha^{-1})),
    color = "Tratamiento"
  )

p4

ggsave(
  "figuras_capitulo11/figura_04_facetas_region.png",
  p4,
  width = 9,
  height = 5.5,
  dpi = 300
)


# 8. Gráfico 5: promedios e intervalos de error ----


# En comunicación científica, es común resumir tratamientos usando media ± error estándar.
# El error estándar describe la incertidumbre de la media, no la variabilidad total de los datos.

resumen_trat <- datos %>%
  group_by(Treatment) %>%
  summarise(
    n = n(),
    Volume_mean = mean(Volume_m3_ha, na.rm = TRUE),
    Volume_sd = sd(Volume_m3_ha, na.rm = TRUE),
    Volume_se = Volume_sd / sqrt(n),
    Severity_mean = mean(Severity_index, na.rm = TRUE),
    .groups = "drop"
  )

resumen_trat

p5 <- ggplot(resumen_trat, aes(x = Treatment, y = Volume_mean, fill = Treatment)) +
  geom_col(width = 0.62, alpha = 0.75) +
  geom_errorbar(
    aes(ymin = Volume_mean - Volume_se, ymax = Volume_mean + Volume_se),
    width = 0.15,
    linewidth = 0.8
  ) +
  scale_fill_manual(values = paleta_trat) +
  tema_clase +
  guides(fill = "none") +
  labs(
    title = "Volumen medio por tratamiento",
    subtitle = "Barras = media; líneas verticales = error estándar",
    x = "Tratamiento",
    y = expression("Volumen medio"~(m^3~ha^{-1}))
  )

p5

ggsave(
  "figuras_capitulo11/figura_05_media_error_estandar.png",
  p5,
  width = 7,
  height = 5,
  dpi = 300
)


# 9. Gráfico 6: varias variables en formato largo ----


# Para graficar varias respuestas con ggplot2, conviene usar formato largo.

datos_largo <- datos %>%
  select(Plot_ID, Treatment, Disease_class, LAI, Chlorophyll_index, Crown_density_pct) %>%
  pivot_longer(
    cols = c(LAI, Chlorophyll_index, Crown_density_pct),
    names_to = "Variable",
    values_to = "Valor"
  )

p6 <- ggplot(datos_largo, aes(x = Disease_class, y = Valor, fill = Disease_class)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.12, alpha = 0.45, size = 1.5) +
  facet_wrap(~ Variable, scales = "free_y") +
  tema_clase +
  guides(fill = "none") +
  labs(
    title = "Indicadores fisiológicos según clase sanitaria",
    subtitle = "El formato largo facilita comparar varias variables en una sola figura",
    x = "Clase sanitaria",
    y = "Valor observado"
  )

p6

ggsave(
  "figuras_capitulo11/figura_06_formato_largo_facetas.png",
  p6,
  width = 9,
  height = 5.5,
  dpi = 300
)


# 10. Visualización de resultados de un modelo simple ----


# Modelo lineal didáctico:
# Volumen como función de severidad, edad y tratamiento.

modelo <- lm(Volume_m3_ha ~ Severity_index + Stand_age_yr + Treatment, data = datos)
summary(modelo)

# Tabla de coeficientes para revisar estimaciones e intervalos de confianza.
coeficientes <- broom::tidy(modelo, conf.int = TRUE)
coeficientes

# Datos nuevos para predicción manteniendo la edad en su promedio.
nuevo <- expand.grid(
  Severity_index = seq(
    min(datos$Severity_index, na.rm = TRUE),
    max(datos$Severity_index, na.rm = TRUE),
    length.out = 100
  ),
  Stand_age_yr = mean(datos$Stand_age_yr, na.rm = TRUE),
  Treatment = levels(datos$Treatment)
)

pred <- predict(modelo, newdata = nuevo, interval = "confidence")
nuevo_pred <- bind_cols(nuevo, as.data.frame(pred))

p7 <- ggplot(nuevo_pred, aes(x = Severity_index, y = fit, color = Treatment, fill = Treatment)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.16, color = NA) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = paleta_trat) +
  scale_fill_manual(values = paleta_trat) +
  tema_clase +
  labs(
    title = "Volumen predicho en función de la severidad sanitaria",
    subtitle = "Predicción del modelo manteniendo constante la edad media del rodal",
    x = "Índice de severidad sanitaria",
    y = expression("Volumen predicho"~(m^3~ha^{-1})),
    color = "Tratamiento",
    fill = "Tratamiento"
  )

p7

ggsave(
  "figuras_capitulo11/figura_07_prediccion_modelo.png",
  p7,
  width = 7.5,
  height = 5.2,
  dpi = 300
)


# 11. Figura multipanel con patchwork ----


# Las revistas científicas suelen organizar varias evidencias en una figura multipanel.
# Aquí se combinan cuatro gráficos en una sola imagen.

figura_multipanel <- (p1 + p2) / (p3 + p5) +
  plot_annotation(
    title = "Visualización integrada de productividad y condición sanitaria",
    subtitle = "Ejemplo de figura multipanel construida con ggplot2 y patchwork",
    tag_levels = "a"
  )

figura_multipanel

ggsave(
  "figuras_capitulo11/figura_08_multipanel_patchwork.png",
  figura_multipanel,
  width = 12,
  height = 9,
  dpi = 300
)

# 12. Exportación en formatos recomendados ----

# PNG: útil para presentaciones, documentos Word y clases.
ggsave(
  "figuras_capitulo11/figura_final_multipanel.png",
  figura_multipanel,
  width = 12,
  height = 9,
  dpi = 300
)

# PDF: útil para mantener calidad vectorial en manuscritos.
ggsave(
  "figuras_capitulo11/figura_final_multipanel.pdf",
  figura_multipanel,
  width = 12,
  height = 9
)

# TIFF: formato comúnmente solicitado por revistas científicas.
ggsave(
  "figuras_capitulo11/figura_final_multipanel.tiff",
  figura_multipanel,
  width = 12,
  height = 9,
  dpi = 300,
  compression = "lzw"
)

