##############################################################
# Bioestadística con R (Forestal) 
# Objetivo: Familia de funciones apply
############################################################

rm(list = ls())

# 1. Datos de ejemplo ----

	# Matriz de datos numéricos
	mat <- matrix(
		c(12, 15, 18,
			10, 14, 16,
			20, 22, 25,
			13, 17, 19),
		nrow = 4,
		byrow = TRUE
	)
	
	colnames(mat) <- c("Sitio_1", "Sitio_2", "Sitio_3")
	rownames(mat) <- c("Arbol_1", "Arbol_2", "Arbol_3", "Arbol_4")
	
	mat
	
	# Data frame biológico
	datos <- data.frame(
		especie = c("A", "A", "A", "B", "B", "B", "C", "C", "C"),
		tratamiento = c("Control", "Riego", "Fertilizacion",
										"Control", "Riego", "Fertilizacion",
										"Control", "Riego", "Fertilizacion"),
		altura = c(15.2, 18.4, 19.1, 12.5, 14.8, 16.0, 20.1, 21.5, 23.0),
		biomasa = c(120, 140, 150, 100, 115, 130, 160, 172, 185)
	)
	
	datos

# 2. apply() ----
	# apply(X, MARGIN, FUN)
	# X = matriz o arreglo
	# MARGIN = 1 filas, 2 columnas
	# FUN = función a aplicar
	
	# Media por columnas
	apply(mat, 2, mean)
	
	# Media por filas
	apply(mat, 1, mean)
	
	# Desviación estándar por columnas
	apply(mat, 2, sd)
	
	# Rango por filas
	apply(mat, 1, range)
	
	# Ejemplo con función personalizada
	coef_var <- function(x) {
		sd(x) / mean(x) * 100
	}
	
	apply(mat, 2, coef_var)


# 3. lapply() ----

	# lapply(X, FUN)
	# Siempre devuelve una lista
	
	lista_vars <- list(
		altura = datos$altura,
		biomasa = datos$biomasa
	)
	
	# Media de cada variable
	lapply(lista_vars, mean)
	
	# Resumen estadístico de cada variable
	lapply(lista_vars, summary)
	
	# Aplicar una función personalizada
	lapply(lista_vars, function(x) round(sd(x), 2))

# 4. sapply() ----

	# sapply(X, FUN)
	# Intenta simplificar el resultado
	
	# Media de cada variable
	sapply(lista_vars, mean)
	
	# Desviación estándar
	sapply(lista_vars, sd)
	
	# Número de observaciones
	sapply(lista_vars, length)
	
	# Mínimo y máximo
	sapply(lista_vars, range)

# 5. vapply() ----
	
	# vapply(X, FUN, FUN.VALUE)
	# Similar a sapply(), pero más seguro
	# porque obliga a definir el tipo de salida esperado
	
	vapply(lista_vars, mean, numeric(1))
	
	vapply(lista_vars, sd, numeric(1))
	
	# Si la función devuelve más de un valor:
	vapply(lista_vars, range, numeric(2))


# 6. tapply() ----
	
	# tapply(X, INDEX, FUN)
	# Aplica una función a una variable agrupada por un factor
	
	# Media de altura por especie
	tapply(datos$altura, datos$especie, mean)
	
	# Biomasa media por tratamiento
	tapply(datos$biomasa, datos$tratamiento, mean)
	
	# Desviación estándar de altura por especie
	tapply(datos$altura, datos$especie, sd)
	
	# Altura media por combinación especie x tratamiento
	tapply(datos$altura, list(datos$especie, datos$tratamiento), mean)


# 7. mapply() ----

	# mapply(FUN, ...)
	# Aplica una función de manera paralela a varios objetos
	
	x <- c(1, 2, 3, 4)
	y <- c(10, 20, 30, 40)
	
	# Suma elemento a elemento
	mapply(function(a, b) a + b, x, y)
	
	# Potencia
	mapply(function(base, exponente) base^exponente, x, c(2, 2, 2, 2))
	
	# Ejemplo biológico:
	# estimar eficiencia = biomasa / altura
	mapply(function(b, h) b / h, datos$biomasa, datos$altura)


# 8. Comparación rápida entre funciones ----

	cat("\n--- apply() ---\n")
	print(apply(mat, 2, mean))
	
	cat("\n--- lapply() ---\n")
	print(lapply(lista_vars, mean))
	
	cat("\n--- sapply() ---\n")
	print(sapply(lista_vars, mean))
	
	cat("\n--- vapply() ---\n")
	print(vapply(lista_vars, mean, numeric(1)))
	
	cat("\n--- tapply() ---\n")
	print(tapply(datos$altura, datos$especie, mean))
	
	cat("\n--- mapply() ---\n")
	print(mapply(function(b, h) b / h, datos$biomasa, datos$altura))


# 9. Ejemplo práctico de bioestadística ----

	# Supongamos que queremos calcular estadísticas descriptivas
	# para variables numéricas del data frame
	
	vars_numericas <- datos[, c("altura", "biomasa")]
	
	# Media por variable
	apply(vars_numericas, 2, mean)
	
	# Desviación estándar por variable
	apply(vars_numericas, 2, sd)
	
	# Coeficiente de variación por variable
	apply(vars_numericas, 2, coef_var)
	
	# Resumen por especie usando tapply
	tapply(datos$altura, datos$especie, mean)
	tapply(datos$biomasa, datos$especie, mean)

# 10. Ejemplo con NA ----
	
	mat_na <- mat
	mat_na[2, 2] <- NA
	mat_na
	
	# Sin remover NA
	apply(mat_na, 2, mean)
	
	# Removiendo NA
	apply(mat_na, 2, mean, na.rm = TRUE)


# 11. Función auxiliar para análisis descriptivo ----

	
	resumen_var <- function(x) {
		c(
			n = length(x),
			media = mean(x, na.rm = TRUE),
			sd = sd(x, na.rm = TRUE),
			minimo = min(x, na.rm = TRUE),
			maximo = max(x, na.rm = TRUE)
		)
	}
	
	# Aplicar sobre variables numéricas del data frame
	apply(vars_numericas, 2, resumen_var)
