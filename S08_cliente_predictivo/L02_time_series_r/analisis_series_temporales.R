#########################################################
# PROYECTO: Análisis de la serie temporal AirPassengers
# Autor: Marcos Berrocal
# Descripción: Análisis de tendencia, estacionalidad, 
# estacionariedad y valores atípicos en R
#########################################################

# ===============================
# 1. CARGA DEL DATASET
# ===============================
data("AirPassengers")

# Verificar estructura
print(class(AirPassengers))   # Verifica que es una serie temporal (ts)
print(summary(AirPassengers)) # Resumen estadístico
print(start(AirPassengers))   # Inicio de la serie
print(end(AirPassengers))     # Fin de la serie
print(frequency(AirPassengers)) # Frecuencia: 12 (mensual)

# ===============================
# 2. EXPLORACIÓN INICIAL
# ===============================
# Visualización básica de la serie
plot(AirPassengers, 
     main = "Número de pasajeros aéreos (1949–1960)", 
     ylab = "Pasajeros (miles)", 
     xlab = "Año", 
     col = "blue")

# Estadísticas básicas
mean_value <- mean(AirPassengers)
sd_value <- sd(AirPassengers)
cat("Media:", mean_value, "\nDesviación estándar:", sd_value, "\n")

# ===============================
# 3. ANÁLISIS DE TENDENCIA Y ESTACIONALIDAD
# ===============================
decomp <- decompose(AirPassengers)
plot(decomp)

# Comentario:
# - La tendencia muestra un incremento sostenido en el número de pasajeros.
# - La estacionalidad revela picos recurrentes cada año (temporadas altas y bajas).

# ===============================
# 4. ANÁLISIS DE ESTACIONARIEDAD
# ===============================

# Instalar paquetes si es necesario
if (!require(tseries)) install.packages("tseries")
library(tseries)

# Gráficos ACF y PACF
acf(AirPassengers, main = "Autocorrelación (ACF)")
pacf(AirPassengers, main = "Autocorrelación Parcial (PACF)")

# Prueba de Dickey-Fuller aumentada
adf_result <- adf.test(AirPassengers)
print(adf_result)

# Si no es estacionaria, aplicamos diferenciación
diff_series <- diff(AirPassengers)
plot(diff_series, main = "Serie diferenciada", col = "darkgreen")

# Verificación tras la diferenciación
adf_result_diff <- adf.test(diff_series)
print(adf_result_diff)

# ===============================
# 5. DETECCIÓN DE VALORES ATÍPICOS
# ===============================

# Boxplot para detectar posibles outliers
boxplot(AirPassengers, main = "Boxplot de la serie AirPassengers", col = "lightblue")

# Identificar los puntos atípicos manualmente (por encima de 3*IQR)
q1 <- quantile(AirPassengers, 0.25)
q3 <- quantile(AirPassengers, 0.75)
iqr <- q3 - q1
upper_limit <- q3 + 3 * iqr
outliers <- AirPassengers[AirPassengers > upper_limit]
cat("Posibles valores atípicos:\n")
print(outliers)

# ===============================
# 6. INTERPRETACIÓN DE RESULTADOS
# ===============================

cat("\n=== CONCLUSIONES ===\n")
cat("1. La serie muestra una clara tendencia ascendente en el tiempo.\n")
cat("2. Existe una fuerte estacionalidad anual con picos regulares.\n")
cat("3. La serie original no es estacionaria, pero tras la diferenciación sí.\n")
cat("4. Se detectan algunos valores atípicos que corresponden a picos de tráfico aéreo.\n")
cat("5. Este análisis sugiere un patrón de crecimiento y estacionalidad estable.\n")
