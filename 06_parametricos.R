############################################################
# Bioestadística con R
# Script docente para análisis paramétricos clásicos
# Base asociada: Base_parametricos_bioestadistica.xlsx
############################################################


# 1. Paquetes ----

library(readxl)
library(dplyr)
library(ggplot2)
library(performance)

# 2. Data ----
datos <- read_excel("C:/Users/jcval/OneDrive - TEC/Courses/2026_1st_R/Material/cap_06_parametricas.xlsx", 
                    sheet = "data")
  
  str(datos)
  summary(datos)
  head(datos)
  
  datos <- datos %>%
    mutate(
      Tratamiento = factor(Tratamiento, levels = c("Control", "Fertilizado", "Riego")),
      Clon = factor(Clon, levels = c("A", "B")),
      Bloque = factor(Bloque)
    )
  str(datos)
  View(datos)  
  
# 3. Prueba t de Student para muestras independientes ----
  # ¿Difiere el DBH entre clones?
  t_ind <- t.test(DBH_cm ~ Clon, data = datos)
  t_ind
  
  # Si se desea asumir igualdad de varianzas
  t_ind_equal <- t.test(DBH_cm ~ Clon, data = datos, var.equal = TRUE)
  t_ind_equal

# 4. Prueba t pareada ----
  # ¿Cambió la altura entre inicio y fin del ensayo?
  t_par <- t.test(datos$Altura_inicial_m, datos$Altura_final_m, paired = TRUE)
  t_par

# 5. Correlación de Pearson ----
  cor_pearson <- cor.test(datos$Altura_m, datos$DBH_cm, method = "pearson")
  cor_pearson

# 6.  Regresión lineal simple ----
  modelo_lm_simple <- lm(DBH_cm ~ Altura_m, data = datos)
  summary(modelo_lm_simple)
  coef(modelo_lm_simple)
  confint(modelo_lm_simple)
    
    # Diagnóstico de supuestos con performance
    check_model(modelo_lm_simple)
    check_normality(modelo_lm_simple)
    check_heteroscedasticity(modelo_lm_simple)
    check_outliers(modelo_lm_simple)
    model_performance(modelo_lm_simple)
    
    # Gráfico
    p1 <- ggplot(datos, aes(x = Altura_m, y = DBH_cm)) +
      geom_point() +
      geom_smooth(method = "lm", se = TRUE) +
      theme_minimal() +
      labs(title = "Regresión lineal simple", x = "Altura (m)", y = "DBH (cm)")
    print(p1)

# 7. ANOVA de una vía ----
modelo_aov1 <- aov(DBH_cm ~ Tratamiento, data = datos)
summary(modelo_aov1)
TukeyHSD(modelo_aov1)
  
  check_normality(modelo_aov1)
  check_heteroscedasticity(modelo_aov1)

  # Medias por tratamiento
  datos %>%
    group_by(Tratamiento) %>%
    summarise(
      n = n(),
      media_DBH = mean(DBH_cm),
      sd_DBH = sd(DBH_cm)
    )

# 8. ANOVA de dos vías ----
modelo_aov2 <- aov(DBH_cm ~ Tratamiento * Clon, data = datos)
summary(modelo_aov2)

check_normality(modelo_aov2)
check_heteroscedasticity(modelo_aov2)

    # Medias por combinación de factores
    datos %>%
      group_by(Tratamiento, Clon) %>%
      summarise(
        n = n(),
        media_DBH = mean(DBH_cm),
        sd_DBH = sd(DBH_cm),
        .groups = "drop"
      )
    
    # Gráfico de interacción
    interaction.plot(
      x.factor = datos$Tratamiento,
      trace.factor = datos$Clon,
      response = datos$DBH_cm,
      fun = mean,
      type = "b",
      pch = 19,
      xlab = "Tratamiento",
      ylab = "Media de DBH",
      trace.label = "Clon"
    )

# 9. ANCOVA ----
modelo_ancova <- lm(DBH_cm ~ Tratamiento + Altura_m, data = datos)
summary(modelo_ancova)
anova(modelo_ancova)

  # Verificar homogeneidad de pendientes
  modelo_ancova_interaccion <- lm(DBH_cm ~ Tratamiento * Altura_m, data = datos)
  anova(modelo_ancova, modelo_ancova_interaccion)
  
  check_model(modelo_ancova)
  model_performance(modelo_ancova)

# 10. Visualizaciones útiles ----
p2 <- ggplot(datos, aes(x = Tratamiento, y = DBH_cm, fill = Tratamiento)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(title = "DBH por tratamiento", x = "Tratamiento", y = "DBH (cm)")
  
print(p2)

p3 <- ggplot(datos, aes(x = Clon, y = DBH_cm, fill = Clon)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(title = "DBH por clon", x = "Clon", y = "DBH (cm)")

print(p3)
