# Clase 1 Bioestadística R
# Version 1.0 Fecha 2/19/2026
# Juan Carlos Valverde

# Ejercios base
1+1
5-1
10*2

"Juan C Valverde"
print("Juan C Valverde")

c(1,2,3,4)

 lista_1 <- c(20,4,400,-2)
 print(lista_1)
 mean(lista_1)
 
 lista_2 <- list(2,3,4,6)
 print(lista_2)
str(lista_2) 

lista_3 <- list(2,40,44,"Juan")

is.numeric(lista_1)
is.numeric(lista_3)
is.na(lista_1)

lista_4 <- list(2,40,"",50)
is.na(lista_4)

lista_5 <- data.frame(c(lista_1,lista_2))
print(lista_5)

install.packages("readxl")
library(readxl)
data <- read_excel("data.xlsx", sheet = "Sheet1")
View(data)
is.numeric(data)
str(data)
mean(data$Y)
mean(data$X,data$Y)
c(data$X, mean(data$Y))

library(dplyr)


promedios <- data %>%
	group_by(X) %>%
	summarise(
		mean_Y = mean(Y)
	) %>%
	arrange(desc(mean_Y)) #con esta linea genero promedio de las categorias de la columna X sus valores de Y 

promedios
View(promedios)
