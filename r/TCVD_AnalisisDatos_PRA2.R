# Se establece una semilla para hacer repetible el proceso
set.seed(123)
setwd("./")

# Carga del archivo
# Se carga el juego de datos, nótese "strip.white" = TRUE para eliminar los espacios en blanco al inicio y final.
df_train <- read.csv('/run/media/vv/DatosVv/Sync/Docs/U/UOC/2/TipologiaCicloVidaDatos/PRA2/dataset/train_summarized.csv', stringsAsFactors = FALSE, strip.white = TRUE, sep = ",")

# 4.1 Selección de los grupos de datos que se quieren analizar/comparar (planificación de los análisis a aplicar).
"
La siguiente tabla resume los test a aplicar y srespectivo grupo de datos
| Test | Conjunto de variables | Propósito/Preguntas |
| -- | -- | -- |
| Contraste de hipótesis | Neighborhood, LotArea, SalePrice | 1. Se puede asumir que las casas del barrio CollgCr tienen un mayor lote de área que las de BrkSide? 2. La varianza del precio de venta es diferente de 70442^2? |
| Correlación de variables | LotArea, OverallCond, OverallQual, YearBuilt, SalePrice, SalesCondition | Evaluación de la asociación de las variables mediante Pearson/Chi cuadrado |
| Regresión lineal múltiple | OverallQual, OverallCond, LotArea, Yearbuilt | Predicción del precio de una vivienda en función de un grupo de variables |
"

# 4.2 Comprobación de la normalidad y homogeneidad de la varianza
## Por inspección visual se examina la normalidad de las variables contempladas
if (!require('ggplot2')) install.packages('ggplot2', dependencies = TRUE); library('ggplot2')
qqnorm(df_train$LotArea, pch = 1); qqline(df_train$LotArea, lwd = 3)
qqnorm(df_train$SalePrice, pch = 1); qqline(df_train$SalePrice, lwd = 3)
qqnorm(df_train$TotRmsAbvGrd, pch = 1); qqline(df_train$TotRmsAbvGrd, lwd = 3)
qqnorm(df_train$OverallCond, pch = 1); qqline(df_train$OverallCond, lwd = 3)
qqnorm(df_train$OverallQual, pch = 1); qqline(df_train$OverallQual, lwd = 3)

## Se aplica test estadístico para verificar la normalidad de las variables contempladas
if (!require('nortest')) install.packages('nortest', dependencies = TRUE); library('nortest')
lillie.test(df_train$LotArea)
lillie.test(df_train$SalePrice)
lillie.test(df_train$TotRmsAbvGrd)
lillie.test(df_train$OverallCond)
lillie.test(df_train$OverallQual)

"
El p value es inferior a 0.05, por lo que se puede afirmar que todas las variables consideradas siguen una distribución normal
"
## Ahora se comprueba homocedasticidad de la varianza para los grupos de datos que se analizarán bajo contraste de hipótesis
## Se generan los dos grupos para el contrase de hipótesis 1 (posteriormente se indicarán las ecuaciones de la hipótesis respectiva)
df_train_Neighborhood_CollgCr <- subset(df_train, df_train$Neighborhood == "CollgCr")
df_train_Neighborhood_BrkSide <- subset(df_train, df_train$Neighborhood == "BrkSide")

## Una vez verificado la distribución normal de los datos de aplica el f-test para comprobar su homocedasticidad
var.test(df_train_Neighborhood_CollgCr$LotArea, df_train_Neighborhood_BrkSide$LotArea)

"
El p value es superior a 0.05, se concluye que la diferencia de las varianzas no es significativa. Se cumple el criterio de homocedasticidad
"

# 4.3 Aplicación de pruebas estadísticas para comparar los grupos de datos. En función de los datos y el objetivo del estudio, aplicar pruebas de contraste de  hipótesis, correlaciones, regresiones, etc. Aplicar al menos tres métodos de análisis diferentes.
## Contraste de hipótesis
## a) Se puede asumir que las casas del barrio CollgCr tienen un mayor lote de área que las de BrkSide?
"
Las hipótesis son:
* H0: U1 - U2 = 0
* H1: U1 - U2 > 0

Se aplicará contraste de dos muestras independientes con varianzas desconocidas:
* La hipótesis establece dos muestras: Barrio CollgCr y Barrio BrkSide
* Las muestras son independientes dado que no existe una relación explícita entre ellas
* No se conoce la varianza de las poblaciones. En el pasó anterior se aplicó el test de igualdad de varianzas y se concluyó que la diferencia de varianzas no es significativa
* Unilateral a la derecha
* Ambas muestras superan las 30 observaciones
"
### Se ejecuta el contraste unilateral por la derecha ("greater"), con varianzas iguales (var.equal = T) y se verifica
result <- t.test(df_train_Neighborhood_CollgCr$LotArea, df_train_Neighborhood_BrkSide$LotArea, alternative = "g", var.equal = T)
result; result$p.value

"
* El valor medio de LotArea de la población se encuentra en los rangos 1632.138 e infinito con un 95% de certeza
* Con un p value inferior a 0.05 se concluye que las casas del barrio CollgCr tienen una mayor área de lote que las casas del barrio BrkSide (H1)
"

## b) La varianza del precio de venta es diferente de 70442^2??
"
Las hipótesis son:
* H0: σ2 = 50000
* H1: σ2 != 50000

Se aplicará contraste sobre la varianza:
* Bilateral
* La variable SalePrice sigue una distribución normal (comprobada en pasos anteriores)
* Ambas muestras superan las 30 observaciones
"
### Se ejecuta el test de igualdad de varianzas con sigma elevado a 2
if (!require('EnvStats')) install.packages('EnvStats', dependencies = TRUE); library('EnvStats')
varTest(df_train$SalePrice, alternative = "two.sided", sigma.squared = 4962075364, conf.level = 0.95, data.name = NULL)

"
* La varianza para el atributo SalePrice se encuentra en los rangos 5877056096 y 6795343509 con un 95% de confianza
* Con un p-value menor a 0 se concluye que la varianza del precio de venta es diferente a 70442^2
"

## Correlación de variables
### Se aplicará Pearson para variables lineales y numéricas. Posteriormente se usará chi-cuadrado para las variables categóricas
### Dada la poca normalidad de los datos se convertirán a escala logarítmica con el propósito de mejorar la inspección visual. Se seleccionan solamente las variables numéricas de ínteres
if (!require('dplyr')) install.packages('dplyr', dependencies = TRUE); library('dplyr')
df_train.log <- log(select(df_train, c(LotArea, OverallCond, OverallQual, YearBuilt, SalePrice)))
### Se generan los gráficos de dispersión por cada variable de ínteres.
if (!require('dplyr')) install.packages('dplyr', dependencies = TRUE); library('dplyr')
pairs(df_train.log)
### Se calcula el grado de correlación lineal de las variables de ínteres con Pearson, dado que los datos siguen una distribución normal y cumplen con el criterio de homocedasticidad
cor(df_train.log, method = "pearson")
### Se visualiza gráficamente las relaciones entre las variables
if (!require('corrplot')) install.packages('corrplot', dependencies = TRUE); library('corrplot')
corrplot(cor(df_train.log, method = "pearson"), method = "number", type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

"
| Variable  | Grado de correlación con el precio de venta | Interpretacion |
| -- | -- | -- | -- |
| LotArea | 0.3999 | Grado de correlacion bajo, no parece existir una relación importante entre estas dos variables |
| OverralCond | 0.0008 | Grado de correlacion muy bajo, no parece existir una relación importante entre estas dos variables |
| OverrallQual | 0.79 | Grado de correlacion alto, parece existir una relación importante entre el precio de venta y la condición general (influencia positiva) |
| Yearbuilt | 0.5844 | Grado de correlación medio, parece existir una relación entre el precio de venta y el año de construcción (influencia positiva |
"

### Se aplica chi-cuadrado para las variables categóricas
### Se discretiza el precio de venta y condición general usando k-means (3 grupos)
if (!require('arules')) install.packages('arules', dependencies = TRUE); library('arules')

df_train$SalePrice_cat <- discretize(df_train$SalePrice, method = "cluster",
                                     breaks = 3,
                                     labels = c("LowPrice", "MidPrice", "HighPrice"))
df_train$OverallCond_cat <- discretize(df_train$OverallCond, method = "cluster",
                                       breaks = 3,
                                       labels = c("BadConditions", "AcceptableConditions", "VeryGoodConditions"))

contigency_OverallCond_catVsSalePrice_cat <- table(df_train$SalePrice_cat, df_train$OverallCond_cat); contigency_OverallCond_catVsSalePrice_cat

chisq <- chisq.test(contigency_OverallCond_catVsSalePrice_cat); chisq
round(chisq$p.value)

"
Con un p-value menor a 0.05, se puede establecer que existe una relación significativa entre el precio de venta categorizado y la condicion general de la casa categorizada
"

### A partir de la tabla de contingencia generada se obtendrán los porcentajes y su interpretación
contigency_OverallCond_catVsSalePrice_cat.perc <- prop.table(contigency_OverallCond_catVsSalePrice_cat, margin = 2) * 100; contigency_OverallCond_catVsSalePrice_cat.perc

"
* Cuando las condiciones son malas, el 93.18% de las casas en venta tienen un precio bajo
* Cuando las condiciones son malas, el 5.68% de las casas en venta tienen un precio medio
* Cuando las condiciones son malas, el 1.13% de las casas en venta tienen un precio alto
* Cuando las condiciones son aceptables, el 47.43% de las casas en venta tienen un precio bajo
* Cuando las condiciones son aceptables, el 42.68% de las casas en venta tienen un precio medio
* Cuando las condiciones son aceptables, el 9.87% de las casas en venta tienen un precio alto
* Cuando las condiciones son buenas, el 70.23% de las casas en venta tienen un precio bajo
* Cuando las condiciones son buenas, el 25.75% de las casas en venta tienen un precio medio
* Cuando las condiciones son buenas, el 4.01% de las casas en venta tienen un precio alto
"

## Regresión lineal múltiple
### Se debe examinar si existe o no colinealidad entre las variables regresoras del modelo. Para esto se calculará el factor de inflación de la varianza (FIV)

### Se genera el modelo de regresión lineal múltiple
salePrice_multiple_regression <- lm(SalePrice ~ LotArea + OverallCond + OverallQual + YearBuilt, data = df_train.log)
### Se imprimen los resultados para análisis
summary(salePrice_multiple_regression)
### Se calcula el factor de inflación de la varianza (FIV)
if (!require('faraway')) install.packages('faraway', dependencies = TRUE); library('faraway')
vif(salePrice_multiple_regression)

"
El FIV arroja un valor inferior a 5 para todas las variables regresoras. Se puede argumentar que no existe colinealidad, por tanto, se incluyen todas las variables regresoras en el modelo

Al examinar el modelo de regresión se obtiene
* El coeficiente R2 arroja un valor de 0.7499. La calidad de la recta de regresión y del modelo es buena
"

### Ahora se examinan los residuos como criterio de validación del modelo
### Se extraen y guardan los valores residuales y ajustados
df_train.log$predicted <- predict(salePrice_multiple_regression)
df_train.log$residuals <- residuals(salePrice_multiple_regression)

### Se pinta gráfica y se destacan los valores residuales
ggplot(df_train.log, aes(x = LotArea, y = SalePrice)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +
  geom_segment(aes(xend = LotArea, yend = predicted), alpha = .2) +
  geom_point(aes(color = abs(residuals), size = abs(residuals))) +
  scale_color_continuous(low = "green", high = "red") +
  geom_point(aes(y = predicted), shape = 1) +
  theme_bw()

plot(salePrice_multiple_regression, which = 2, col = c("red"))  # Q-Q Plot

"
Se evidencian varios datos atípicos que pueden ser tratados para mejorar la calidad del modelo. No obstante, el conjunto de puntos impresos en el gráfico no muestran ningún tipo de estructura, lo que confirmar, parcialmente, el correcto ajuste del modelo a los datos.
"

### Ahora se hará una predicción según el modelo generado con valores explícitos de las variables regresoras
lotArea <- log(8650)
overallCond <- log(6)
overallQual <- log(5)
yearBuilt <- log(2004)

## Se construye la recta de regresión y se aplican los valores para la predicción
salePrice_prediction <- -46.0660 +
  (lotArea * 0.2298) +
  (overallCond * 0.1557) +
  (overallQual * 0.9793) +
  (yearBuilt * 7.1163); exp(salePrice_prediction)

"
Dado un lote con área de 8650, condición general de 6, calidad general de 5 y año de construcción 2004, su precio es de 159027
"
