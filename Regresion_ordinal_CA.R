# Regresión ordinal con variable dependiente en escala Likert

# Librerías 
library(MASS)
library(readxl)

# Cargar datos de ¿Cómo Andamos Guanajuato?
datosCA <- read_excel("Base_datos_Como_Andamos_Gto_2022.xlsx", sheet = 2)

# Excluir las respuestas "No sabe/No responde"
datosCA <- datosCA[datosCA$BG != "No sabe / No respondió", ]

# Convertir la variable Likert (BG) en un factor ordenado con los niveles correctos
# Pregunta: ¿Qué tan satisfecho/a está con la forma en la que el municipio destina 
# los recursos públicos en su comunidad? Codificada como BG en la base de datos.

datosCA$BG <- factor(datosCA$BG,levels = c("Muy satisfecho/a", "Satisfecho/a", 
                                          "Insatisfecho/a", "Muy insatisfecho/a"),
                     ordered = TRUE)


# Ajustar el modelo de regresión ordinal utilizando polr()
modelo_ordinal <- polr(BG ~ BF + BL + BM + BN + B + C, 
                       data = datosCA, Hess = TRUE)

summary(modelo_ordinal)

# Descripción de las variables predictoras:
# BF = En una escala del 1 al 10, ¿Cuál es el nivel de confianza que tiene usted sobre el actual gobierno municipal? 
# BL = ¿Cómo evaluaría a su municipio en los siguientes atributos? Capacidad técnica y resolución de problemas
# BM = ¿Cómo evaluaría a su municipio en los siguientes atributos? Imparcialidad (trabaja para todos)
# BN = ¿Cómo evaluaría a su municipio en los siguientes atributos? Transparencia y difusión de información
# B: Sexo (Masculino/Femenino)
# C: Edad (Grupos de edad)


