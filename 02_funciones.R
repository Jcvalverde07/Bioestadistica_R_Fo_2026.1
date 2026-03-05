############################################################
# Bioestadística con R (Forestal) — Funciones base 
# Objetivo: mostrar cómo crear, aplicar y reutilizar funciones
# usando ejemplos típicos de inventario y manejo forestal.
############################################################

# 0) Datos de ejemplo (inventario)

set.seed(123)

n <- 60

inv <- data.frame(
	Plot      = rep(paste0("P", 1:6), each = n/6),
	Species   = sample(c("Gmelina arborea", "Tectona grandis", "Eucalyptus spp."), n, replace = TRUE,
										 prob = c(0.40, 0.35, 0.25)),
	DBH_cm    = round(rnorm(n, mean = 24, sd = 6), 1),
	H_m       = round(rnorm(n, mean = 18, sd = 4), 1),
	Age_yr    = sample(4:12, n, replace = TRUE)
)

View(inv)
str(inv)
plot(inv$DBH_cm,inv$H_m)
boxplot(inv$DBH_cm)

boxplot(DBH_cm ~ Species, data = inv,				
				col = "white",
				border = "#8B3E2F",
				main = "DBH por especie",
				ylab = "DBH (cm)",
				las = 1)

stripchart(DBH_cm ~ Species, data = inv,
					 vertical = TRUE, method = "jitter",
					 pch = 16, cex = 0.6, add = TRUE)


# Evitar valores no realistas
inv$DBH_cm[inv$DBH_cm < 5] <- 10
inv$H_m[inv$H_m < 2] <- 5

boxplot(DBH_cm ~ Species, data = inv,				
				col = "white",
				border = "#8B3E2F",
				main = "DBH por especie",
				ylab = "DBH (cm)",
				las = 1)

stripchart(DBH_cm ~ Species, data = inv,
					 vertical = TRUE, method = "jitter",
					 pch = 16, cex = 0.6, add = TRUE)
# Simular mortalidad/ausencia (NA) en altura (p.ej., árbol quebrado)
inv$H_m[sample(1:n, size = 4)] <- NA

str(inv)
head(inv, 8)

# -----------------------------
# 1) Función: Validación de inventario (control de calidad)
# -----------------------------
qc_inventory <- function(df, dbh_col = "DBH_cm", h_col = "H_m") {
	if (!is.data.frame(df)) stop("df debe ser un data.frame.")
	if (!(dbh_col %in% names(df))) stop("No se encontró la columna de DAP (dbh_col).")
	if (!(h_col %in% names(df))) stop("No se encontró la columna de altura (h_col).")
	
	dbh <- df[[dbh_col]]
	h   <- df[[h_col]]
	
	issues <- list(
		n_rows          = nrow(df),
		missing_dbh     = sum(is.na(dbh)),
		missing_height  = sum(is.na(h)),
		nonpositive_dbh = sum(!is.na(dbh) & dbh <= 0),
		nonpositive_h   = sum(!is.na(h) & h <= 0),
		extreme_dbh     = sum(!is.na(dbh) & (dbh < 5 | dbh > 120)),
		extreme_h       = sum(!is.na(h) & (h < 2 | h > 70))
	)
	
	# Registros potencialmente problemáticos
	flag <- rep(FALSE, nrow(df))
	flag <- flag | is.na(dbh) | is.na(h)
	flag <- flag | (!is.na(dbh) & (dbh <= 0 | dbh < 5 | dbh > 120))
	flag <- flag | (!is.na(h)   & (h   <= 0 | h   < 2 | h   > 70))
	
	list(summary = issues, flagged_rows = df[flag, , drop = FALSE])
}

qc <- qc_inventory(inv)
qc$summary
qc$flagged_rows

# -----------------------------
# 2) Función: Área basal individual y total
#    g = pi * (dbh_m^2) / 4   [m^2 por árbol]
# -----------------------------
basal_area <- function(dbh_cm) {
	if (!is.numeric(dbh_cm)) stop("dbh_cm debe ser numérico.")
	dbh_m <- dbh_cm / 100
	g <- pi * (dbh_m^2) / 4
	return(g)
}

# Área basal por árbol
inv$G_m2 <- basal_area(inv$DBH_cm)

# Sumar área basal por parcela
G_by_plot <- tapply(inv$G_m2, inv$Plot, sum, na.rm = TRUE)
G_by_plot

G_by_plot <- round(G_by_plot, 3)
G_by_plot


# -----------------------------
# 3) Función: Volumen individual usando ecuación alométrica simple
#    V = a * (DBH^b) * (H^c)
#    Nota: aquí DBH en cm y H en m, con parámetros típicos de ejemplo.
# -----------------------------
tree_volume <- function(dbh_cm, h_m, a = 0.00008, b = 2.35, c = 1.0) {
	if (!is.numeric(dbh_cm) || !is.numeric(h_m)) stop("dbh_cm y h_m deben ser numéricos.")
	if (length(dbh_cm) != length(h_m)) stop("dbh_cm y h_m deben tener la misma longitud.")
	V <- a * ((dbh_cm/100)^b) * (h_m^c)
	return(V) # m^3 por árbol (aprox, depende de la ecuación)
}

inv$V_m3 <- tree_volume(inv$DBH_cm, inv$H_m)
summary(inv$V_m3)

# -----------------------------
# 4) Función: Agregación a hectárea (factor de expansión)
#    Si parcela_m2 es el área de parcela, el factor = 10000/parcela_m2
# -----------------------------
expand_to_ha <- function(x, plot_area_m2) {
	if (!is.numeric(x)) stop("x debe ser numérico.")
	if (!is.numeric(plot_area_m2) || length(plot_area_m2) != 1) stop("plot_area_m2 debe ser un número.")
	if (plot_area_m2 <= 0) stop("plot_area_m2 debe ser positivo.")
	factor <- 10000 / plot_area_m2
	return(x * factor)
}

plot_area_m2 <- 500  # ejemplo: parcela circular ~500 m2
V_by_plot_m3 <- tapply(inv$V_m3, inv$Plot, sum, na.rm = TRUE)
V_by_plot_m3_ha <- expand_to_ha(V_by_plot_m3, plot_area_m2)
V_by_plot_m3_ha

# -----------------------------
# 5) Función: Índice de sitio (ejemplo didáctico)
#    Modelo tipo Chapman-Richards simplificado:
#    Hdom = SI * (1 - exp(-k * Age))^p
#    Despeje de SI: SI = Hdom / (1 - exp(-k * Age))^p
# -----------------------------
site_index <- function(Hdom_m, Age_yr, k = 0.12, p = 1.4) {
	if (any(Age_yr <= 0, na.rm = TRUE)) stop("Age_yr debe ser > 0.")
	denom <- (1 - exp(-k * Age_yr))^p
	SI <- Hdom_m / denom
	return(SI)
}

# Supongamos Hdom ~ percentil 80 de alturas por parcela (dominantes)
Hdom_by_plot <- tapply(inv$H_m, inv$Plot, function(x) as.numeric(quantile(x, probs = 0.80, na.rm = TRUE)))
Age_by_plot  <- tapply(inv$Age_yr, inv$Plot, function(x) round(mean(x, na.rm = TRUE), 1))

SI_by_plot <- site_index(Hdom_by_plot, Age_by_plot)
data.frame(Plot = names(SI_by_plot), Hdom_m = Hdom_by_plot, Age_yr = Age_by_plot, SiteIndex = round(SI_by_plot, 2))

# -----------------------------
# 6) Función: Clasificación de árboles por clase diamétrica
# -----------------------------
dbh_class <- function(dbh_cm, breaks = seq(0, 60, by = 5), right = FALSE) {
	if (!is.numeric(dbh_cm)) stop("dbh_cm debe ser numérico.")
	cut(dbh_cm, breaks = breaks, right = right, include.lowest = TRUE)
}

inv$DBH_class <- dbh_class(inv$DBH_cm)
table(inv$DBH_class, useNA = "ifany")

# -----------------------------
# 7) Función: Resumen forestal por grupo (base R)
#    Devuelve N, DAP medio, H media, G total, V total (por grupo)
# -----------------------------
forest_summary <- function(df, group_col, dbh_col = "DBH_cm", h_col = "H_m",
													 g_col = "G_m2", v_col = "V_m3") {
	
	if (!is.data.frame(df)) stop("df debe ser un data.frame.")
	if (!(group_col %in% names(df))) stop("No se encontró group_col.")
	for (nm in c(dbh_col, h_col, g_col, v_col)) {
		if (!(nm %in% names(df))) stop(paste("No se encontró la columna:", nm))
	}
	
	grp <- df[[group_col]]
	
	N   <- tapply(df[[dbh_col]], grp, function(x) sum(!is.na(x)))
	Dm  <- tapply(df[[dbh_col]], grp, function(x) mean(x, na.rm = TRUE))
	Hm  <- tapply(df[[h_col]],   grp, function(x) mean(x, na.rm = TRUE))
	Gt  <- tapply(df[[g_col]],   grp, function(x) sum(x,  na.rm = TRUE))
	Vt  <- tapply(df[[v_col]],   grp, function(x) sum(x,  na.rm = TRUE))
	
	out <- data.frame(
		Group = names(N),
		N     = as.integer(N),
		DBH_mean_cm = round(as.numeric(Dm), 2),
		H_mean_m    = round(as.numeric(Hm), 2),
		G_total_m2  = round(as.numeric(Gt), 4),
		V_total_m3  = round(as.numeric(Vt), 4),
		row.names = NULL
	)
	
	# Ordenar por volumen descendente
	out <- out[order(out$V_total_m3, decreasing = TRUE), ]
	return(out)
}

# Resumen por especie
forest_summary(inv, group_col = "Species")

# Resumen por parcela
forest_summary(inv, group_col = "Plot")

# -----------------------------
# 8) Ejemplo clave: usar una función dentro de otra
#    Métrica simple de "stock" (proxy) = G_total * H_mean (solo demostración)
# -----------------------------
stand_stock_proxy <- function(df, group_col) {
	s <- forest_summary(df, group_col = group_col)
	s$StockProxy <- round(s$G_total_m2 * s$H_mean_m, 3)
	s <- s[order(s$StockProxy, decreasing = TRUE), ]
	s
}

stand_stock_proxy(inv, "Plot")

############################################################
# Fin del script
# Sugerencia didáctica
# 1) Modificar parámetros (a,b,c) de volumen
# 2) Cambiar área de parcela y comparar m3/ha
# 3) Agregar un filtro por especie antes del resumen
############################################################
