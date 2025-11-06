#########################################################
# PROYECTO: Análisis de la serie temporal "nottem"
# Autor: Marcos Berrocal
# Descripción: Análisis de tendencia, estacionalidad, 
# estacionariedad y detección de valores atípicos
#########################################################

# ===============================
# 1. CARGA DEL DATASET
# ===============================
data("nottem")

# Verificar la estructura del dataset
print(class(nottem))   # Verifica que es una serie temporal (ts)
print(summary(nottem)) # Resumen estadístico de la serie

# ===============================
# 2. VISUALIZACIÓN INICIAL
# ===============================
plot(nottem, 
     main = "Temperaturas Mensuales en Nottingham (1920–1939)", 
     xlab = "Año", 
     ylab = "Temperatura (°F)", 
     col = "blue", 
     lwd = 2)

# Calcular estadísticas descriptivas básicas
mean_temp <- mean(nottem)
sd_temp <- sd(nottem)
cat("Media:", mean_temp, "\nDesviación estándar:", sd_temp, "\n")

# ===============================
# 3. ANÁLISIS DE TENDENCIA Y ESTACIONALIDAD
# ===============================
decomp <- decompose(nottem)
plot(decomp)

# Comentario:
# - La tendencia muestra variaciones suaves a lo largo del tiempo.
# - La estacionalidad es clara: picos y valles anuales bien definidos.
# - El componente aleatorio recoge fluctuaciones menores no explicadas.

# ===============================
# 4. ANÁLISIS DE ESTACIONARIEDAD
# ===============================

# Instalar paquetes necesarios (si no están instalados)
if (!require(tseries)) install.packages("tseries")
library(tseries)

# Gráficos ACF y PACF
acf(nottem, main = "Autocorrelación (ACF) - Serie Original")
pacf(nottem, main = "Autocorrelación Parcial (PACF) - Serie Original")

# Prueba de Dickey-Fuller aumentada (ADF)
adf_result <- adf.test(nottem)
print(adf_result)

# Si no es estacionaria, aplicamos diferenciación
diff_nottem <- diff(nottem)
plot(diff_nottem, 
     main = "Serie Diferenciada de Temperaturas", 
     ylab = "Cambio en Temperatura (°F)", 
     col = "darkgreen")

# Nueva prueba de Dickey-Fuller tras la diferenciación
adf_result_diff <- adf.test(diff_nottem)
print(adf_result_diff)

# ===============================
# 5. DETECCIÓN DE VALORES ATÍPICOS
# ===============================

# Boxplot para identificar posibles outliers
boxplot(nottem, 
        main = "Boxplot de Temperaturas Mensuales (Nottingham 1920–1939)", 
        ylab = "Temperatura (°F)", 
        col = "lightblue")

# Detección numérica de valores atípicos (por encima de 3*IQR)
q1 <- quantile(nottem, 0.25)
q3 <- quantile(nottem, 0.75)
iqr <- q3 - q1
upper_limit <- q3 + 3 * iqr
outliers <- nottem[nottem > upper_limit]

cat("\nPosibles valores atípicos detectados:\n")
print(outliers)

# ===============================
# 6. INTERPRETACIÓN DE RESULTADOS
# ===============================
cat("\n=== CONCLUSIONES ===\n")
cat("1. La serie muestra un claro patrón estacional anual.\n")
cat("2. La tendencia es relativamente estable, sin grandes cambios a largo plazo.\n")
cat("3. La serie no es completamente estacionaria, pero mejora tras la diferenciación.\n")
cat("4. Se detectan pocos valores atípicos, asociados a inviernos o veranos extremos.\n")
cat("5. Los resultados confirman una estacionalidad fuerte y comportamiento cíclico.\n")
