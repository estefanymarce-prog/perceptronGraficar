#' Graficar la frontera de decision de un perceptron simple
#'
#' Dibuja los puntos de dos variables coloreados segun su clase real, marca
#' con distinta forma las clasificaciones correctas e incorrectas, y traza la
#' frontera de decision del perceptron.
#'
#' La funcion espera exactamente dos variables predictoras: la primera
#' columna de `X` se grafica en el eje x (como `horas`) y la segunda en el
#' eje y (como `asistencia`).
#'
#' @param modelo Lista con cuatro elementos: `pesos`, `sesgo`, `medias` y
#'   `desviaciones`, con la estructura que devuelve un entrenamiento de
#'   perceptron simple.
#' @param X Matriz o data frame con dos columnas numericas.
#' @param y Vector de clases reales con valores 0 y 1.
#' @param predicciones Vector de clases predichas (0 o 1), del mismo largo
#'   que `y`.
#'
#' @return De forma invisible, una lista con tres elementos: `grafica` (el
#'   objeto ggplot), `datos` (los datos usados para los puntos) y
#'   `frontera` (la cuadricula usada para trazar la frontera). La grafica
#'   tambien se imprime.
#'
#' @import ggplot2
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' set.seed(123)
#' X <- data.frame(horas = runif(60, 1, 10), asistencia = runif(60, 40, 100))
#' y <- as.integer(2 * X$horas + 0.08 * X$asistencia >= 10)
#' modelo <- list(
#'   pesos = c(0.07, 0.02),
#'   sesgo = 0.1,
#'   medias = c(5.5, 70),
#'   desviaciones = c(2.6, 17)
#' )
#' z <- scale(X, center = modelo$medias, scale = modelo$desviaciones) %*%
#'   modelo$pesos + modelo$sesgo
#' predicciones <- as.integer(z >= 0)
#' graficar_perceptron(modelo, X, y, predicciones)
graficar_perceptron <- function(modelo, X, y, predicciones) {
  X <- as.matrix(X)

  # Preparar los datos para la grafica
  datos_grafica <- data.frame(
    horas = X[, 1],
    asistencia = X[, 2],
    Real = factor(
      y,
      levels = c(0, 1),
      labels = c("No aprueba", "Aprueba")
    ),
    Prediccion = predicciones
  )

  # Identificar clasificaciones correctas e incorrectas
  datos_grafica$Resultado <- ifelse(
    y == predicciones,
    "Correcta",
    "Incorrecta"
  )

  # Crear una cuadricula para dibujar la frontera
  rejilla <- expand.grid(
    horas = seq(
      min(X[, 1]) - 0.5,
      max(X[, 1]) + 0.5,
      length.out = 150
    ),
    asistencia = seq(
      min(X[, 2]) - 2,
      max(X[, 2]) + 2,
      length.out = 150
    )
  )

  # Calcular la suma ponderada en cada punto
  rejilla_estandarizada <- scale(
    as.matrix(rejilla[, c("horas", "asistencia")]),
    center = modelo$medias,
    scale = modelo$desviaciones
  )

  rejilla$z <- as.vector(
    rejilla_estandarizada %*% modelo$pesos +
      modelo$sesgo
  )

  # Construir la grafica
  grafica <- ggplot() +
    # Frontera de decision del perceptron
    geom_contour(
      data = rejilla,
      aes(x = .data$horas, y = .data$asistencia, z = .data$z),
      breaks = 0,
      color = "black",
      linewidth = 1.1
    ) +
    # Puntos de los estudiantes
    geom_point(
      data = datos_grafica,
      aes(
        x = .data$horas,
        y = .data$asistencia,
        fill = .data$Real,
        shape = .data$Resultado
      ),
      size = 3.5,
      color = "black",
      stroke = 0.8
    ) +
    scale_fill_manual(
      values = c(
        "No aprueba" = "#3498DB",
        "Aprueba" = "#2ECC71"
      ),
      name = "Resultado real"
    ) +
    scale_shape_manual(
      values = c(
        "Correcta" = 21,
        "Incorrecta" = 24
      ),
      name = "Clasificacion"
    ) +
    guides(
      fill = guide_legend(override.aes = list(shape = 21))
    ) +
    labs(
      title = "Clasificacion con un perceptron simple",
      subtitle = "La linea negra representa la frontera de decision",
      x = "Horas de estudio",
      y = "Porcentaje de asistencia"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(
        face = "bold",
        size = 16
      ),
      legend.position = "bottom"
    )

  print(grafica)

  # Devolver tambien los datos y la grafica
  invisible(list(
    grafica = grafica,
    datos = datos_grafica,
    frontera = rejilla
  ))
}
