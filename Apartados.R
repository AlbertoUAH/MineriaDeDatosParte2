# Cargamos las librerias...
library(readxl)
library(knitr)
library(RColorBrewer)
library(pastecs)
library(ggplot2) 
library(corrplot)
library(factoextra) 
library(FactoMineR)
library(gridExtra)

# ...Ademas del conjunto de datos
provincias <- as.data.frame(read_excel("~/UCM/Mineria de Datos y Modelizacion Predictiva (II)/Practica 2/Provincias.xlsx"))

# En primer lugar, debemos analizar los tipos de datos
sapply(provincias, class) # Podemos comprobar como los campos son numericos

# Apartado 1. Calcular la matriz de correlaciones y su representacion grafica
# Antes de crear la matriz de correlaciones, pasamos el campo "Provincia" como nombre de fila (rowname)
rownames(provincias) <- provincias$Provincia; provincias <- provincias[, -1] # Eliminamos finalmente la columna "Provincia"
matriz.correlaciones <-cor(provincias)
knitr::kable(matriz.correlaciones, digits =2,caption = "Correlaciones")

# Tambien realizamos su representacion grafica
corrplot(matriz.correlaciones, type="upper", col=brewer.pal(n=8, name="RdYlBu"))

# Analizando la grafica de correlacion, de forma inversa nos encontramos con multiples variables aunque la mayoria de ellas con un valor muy cercano a 0.
# No obstante, si debemos destacar los siguientes casos:
# 1. Mortalidad - Natalidad: siguien una correlacion muy negativa (-0.74). Es decir, en aquellas provincias donde aumenta/disminuye la tasa de natalidad, la tasa de mortalidad disminuye/aumenta, respectivamente
# Hablamos de provincias como Madrid/Ceuta/Melilla/Almeria, con tasas de natalidad muy elevadas y tasas de mortalidad bajas.
# Por el contrario, regiones (en su mayoria rurales) como Asturias/Huesca/Leon/Lugo donde las tasas de mortalidad son muy elevadas en comparacion con las tasas de natalidad
# 2. Mortalidad - TasaActividad (-0.73): en provincias con elevadas tasas de mortalidad, el porcentaje de personas en "edad laboral" se reduce considerablemente.
# CONCLUSION: provincias con elevadas tasas de mortalidad presentan bajas tasas de natalidad y de edad laboral (y viceversa)
# No obstante, no existe una correlacion inversa entre natalidad y actividad (solo con mortalidad). De hecho, la correlacion entre ambos casos es positiva (0.47)
# 3. Mortalidad - Tasa Paro (-0.46): una correlacion algo menor en comparacion con las dos anteriores, aunque muy cercana a -0.5, por lo que debe tenerse en cuenta: a medida que
# aumenta la tasa de mortalidad, el numero de parados disminuye (y viceversa)
# 4. IPC - TasaParo (-0.58): una correlacion superior a -0.5, por lo que es significativa: aquellas provincias donde el indice de precios al consumidor es mayor, presentan bajas tasas de paro
# ¿El aumento mayoritario de precios se producen en provincias donde la tasa de paro es mas baja y por ello hay un mayor poder adquisitivo de los consumidores?
# O viceversa ¿Aquellas provincias con un menor IPC son aquellas donde la tasa de paro es mas alta y por ello presentan un menor poder adquisitivo?

# El resto de correlaciones negativas apenas alcanzan el -0.4, por lo que no se consideran relevantes.

# Apartado 2. Realizar un análisis de componentes principales sobre la matriz de correlaciones, calculando 7 componentes
analisis.comp <- PCA(provincias, scale.unit = TRUE, ncp = 7, graph = TRUE)
knitr::kable(analisis.comp$eig, digits =2,caption = "Autovalores")

# Analizando los resultados obtenidos, podemos observar como con 4 componentes explico aproximadamente el 92 % de la variabilidad de los datos. Pero antes, echemos
# un vistazo a las graficas obtenidas: en primer lugar, podemos observar que con tan solo dos dimensiones (con solo dos componentes principales), explicamos mas del 70 %
# de la variabilidad de los datos.

# Representamos graficamente la proporcion de varianza explicada por cada Componente Principal
fviz_eig(analisis.comp,addlabels=TRUE)

# Apartado 3. Hacer de nuevo el análisis sobre la matriz de correlaciones pero ahora indicando el número de componentes principales que hemos decidido retener
# Hemos decidido retener 4 componentes principales
analisis.comp<-PCA(provincias,scale.unit=TRUE, ncp=4, graph=TRUE)

# Apartado a)
analisis.comp$svd$V # Mostramos los coeficientes de la combinacion lineal

# ¿Cual es la expresion para calcular la primera Componente en funcion de las variables originales?
# Obtenemos la expresion de la primera componente a partir de la combinacion lineal de los autovectores (coeficientes)
analisis.comp$svd$V[,1]

# Apartado b)
var<-get_pca_var(analisis.comp)
knitr::kable(var$cor, digits =3,caption = "Correlaciones de la CP con las variables")

# Apartado c)
# Con la cuarta componente no existe ninguna variable que presente una elevada correlacion en comparacion con el resto de componentes.
fviz_pca_var(analisis.comp, axes = c(1, 2), col.var="cos2",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)
fviz_pca_var(analisis.comp, axes = c(1, 3), col.var="cos2",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)
analisis.comp<-PCA(provincias,scale.unit=TRUE, ncp=3, graph=TRUE)
var<-get_pca_var(analisis.comp)

# Apartado d) Mostrar la tabla y los gráficos que nos muestran la proporción de la varianza de cada variable que es explicado por cada componente
# Tabla cosenos al cuadrado
knitr::kable(var$cos2, digits =2,caption = "Cosenos al cuadrado")

# De forma grafica
corrplot(var$cos2, is.corr = FALSE)

# ¿Cual es la que esta peor explicada?
# La que esta peor explicada corresponde con el numero de viviendas secundarias
fviz_cos2(analisis.comp, choice = "var", axes = 1:3)

# Apartado e) Mostrar la tabla y los gráficos que nos muestran el porcentaje de la varianza de cada Componente que es debido a cada variable
# Tabla con el porcentaje de varianza de cada Componente
knitr::kable(var$contrib, digits = 2, caption = "Contribuciones")

# De forma grafica
corrplot(var$contrib, is.corr = FALSE)

# Contribucion de las variables a la componente 1
fviz_contrib(analisis.comp,choice="var",axes=1,top=11)

# Contribucion de las variables a la componente 2
fviz_contrib(analisis.comp,choice="var",axes=2,top=5)

# Contribucion de las variables a la componente 3
fviz_contrib(analisis.comp,choice="var",axes=3,top=4)

# Apartado f) Comentar las provincias que tienen una posición más destacada en cada componente, en positivo o negativo
# Grafico con las observaciones en los nuevos ejes
fviz_pca_ind(analisis.comp, axes = c(1, 2), col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)

# ¿Hay alguna provincia cuyos valores de las componentes esten dentro de la media?
observaciones.1.2 <- fviz_pca_ind(analisis.comp, axes = c(1, 2), col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)

observaciones.1.3 <- fviz_pca_ind(analisis.comp, axes = c(1, 3), col.ind = "cos2",
                                  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)

# ¿Que provincias no parecen destacar especialmente en ninguna de las componentes?
intersect(rownames(observaciones.1.2$data[(abs(observaciones.1.2$data$x) < 1 & abs(observaciones.1.2$data$y) < 1), ]),
          rownames(observaciones.1.3$data[(abs(observaciones.1.2$data$x) < 1 & abs(observaciones.1.3$data$y) < 1), ]))

# Grafico Biplot
# Componentes 1 y 2
fviz_pca_biplot(analisis.comp, repel = TRUE, col.var = "#2E9FDF", col.ind = "#696969")

# Componentes 1 y 3
fviz_pca_biplot(analisis.comp, repel = TRUE, axes = c(1,3), col.var = "#2E9FDF", col.ind = "#696969")


# CLUSTERING
library(cluster) 
library(heatmaply) 
library(NbClust)

# Apartado 1. Representar un mapa de calor de la matriz de datos, estandarizado y sin estandarizar para ver si se detectan inicialmente grupos de provincias.
# Sin estandarizar

distancias <- dist(provincias, method = "euclidean")
knitr::kable(as.matrix(distancias)[1:6,1:6], caption = "Distancias")
summary(unlist(distancias))
ggheatmap(provincias, seriate = "mean")
ggheatmap(as.matrix(distancias), seriate = "mean")

# Estandarizados

provincias_ST <- scale(provincias)
distancias_ST <- dist(provincias_ST, method = "euclidean")
ggheatmap(provincias_ST, seriate = "mean")
ggheatmap(as.matrix(distancias_ST), seriate = "mean", col = rainbow(256))

provincias_ST_m <- as.matrix(provincias_ST)
buscar_vecinos <- function(radio, fila, columna) {
  vector.vecinos <- c()
  for(j in seq(columna-1-radio:columna+radio)) {
    for(i in seq(fila-1-radio:fila+radio)){
      if(i >= 0 & i < nrow(provincias_ST_m) & j >= 0 & j < ncol(provincias_ST_m)) {
        vector.vecinos <- c(vector.vecinos, provincias_ST_m[i,j])
      }else {
        vector.vecinos <- c(vector.vecinos, 0)
      }
    }
  }
  return(vector.vecinos)
}
buscar_vecinos(3, 2, 2)


