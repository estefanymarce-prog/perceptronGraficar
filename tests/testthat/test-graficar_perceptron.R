test_that("graficar_perceptron devuelve grafica, datos y frontera", {
  set.seed(123)
  X <- data.frame(horas = runif(30, 1, 10), asistencia = runif(30, 40, 100))
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

  resultado <- graficar_perceptron(modelo, X, y, predicciones)

  expect_named(resultado, c("grafica", "datos", "frontera"))
  expect_s3_class(resultado$grafica, "ggplot")
  expect_equal(nrow(resultado$datos), nrow(X))
  expect_true(all(resultado$datos$Resultado %in% c("Correcta", "Incorrecta")))
})
