# perceptronGraficar

Este paquete grafica la frontera de decision de un perceptron simple de dos variables sobre los datos. Marca con distinta forma las clasificaciones correctas e incorrectas.

Es el tercero de tres paquetes independientes: `perceptronEntrenar`, `perceptronPredecir` y `perceptronGraficar`.

## Instalacion

Puedes instalar la version de desarrollo desde GitHub con:

``` r
# install.packages("devtools")
devtools::install_github("estefanymarce-prog/perceptronGraficar", upgrade = "never")
```

## Ejemplo

``` r
library(perceptronGraficar)

set.seed(123)
X <- data.frame(horas = runif(60, 1, 10), asistencia = runif(60, 40, 100))
y <- as.integer(2 * X$horas + 0.08 * X$asistencia >= 10)

modelo <- list(
  pesos = c(0.07, 0.02),
  sesgo = 0.1,
  medias = c(5.5, 70),
  desviaciones = c(2.6, 17)
)

z <- scale(X, center = modelo$medias, scale = modelo$desviaciones) %*%
  modelo$pesos + modelo$sesgo
predicciones <- as.integer(z >= 0)

graficar_perceptron(modelo, X, y, predicciones)
```

## Modelo esperado

`graficar_perceptron()` recibe una lista con cuatro elementos:

- `pesos`: peso de cada variable.
- `sesgo`: termino de sesgo.
- `medias`: media de cada variable usada al entrenar.
- `desviaciones`: desviacion estandar de cada variable usada al entrenar.

Es la misma estructura que devuelve `entrenar_perceptron()` del paquete `perceptronEntrenar`. La funcion espera exactamente dos variables predictoras.
