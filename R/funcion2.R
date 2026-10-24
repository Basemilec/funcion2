# =====================================================
# 2. FUNCION PARA PREDECIR CON EL PERCEPTRON
# =====================================================

#' Predecir clases con un perceptron simple
#'
#' Realiza predicciones binarias utilizando un modelo
#' de perceptron previamente entrenado.
#'
#' Las variables predictoras se estandarizan utilizando
#' las mismas medias y desviaciones calculadas durante
#' el entrenamiento.
#'
#' @param modelo Lista que contiene los parametros del
#'   perceptron entrenado: pesos, sesgo, medias y
#'   desviaciones.
#' @param X Matriz o data frame con las variables
#'   predictoras de las nuevas observaciones.
#'
#' @return Vector de valores enteros:
#'   0 para la clase negativa y 1 para la positiva.
#'
#' @details
#' La prediccion se obtiene mediante la combinacion lineal:
#'
#' z = X_estandarizada * pesos + sesgo
#'
#' Si z es mayor o igual a cero, la prediccion es 1.
#' En caso contrario, la prediccion es 0.
#'
#' @examples
#' modelo <- list(
#'   pesos = c(0.5, 0.3),
#'   sesgo = -0.2,
#'   medias = c(5, 80),
#'   desviaciones = c(2, 10)
#' )
#'
#' X <- matrix(
#'   c(6, 90, 4, 70),
#'   ncol = 2,
#'   byrow = TRUE
#' )
#'
#' predecir_perceptron(modelo, X)
#'
#' @export

predecir_perceptron <- function(modelo, X) {

  # -------------------------------------------------
  # 1. VALIDAR EL MODELO
  # -------------------------------------------------

  parametros <- c(
    "pesos",
    "sesgo",
    "medias",
    "desviaciones"
  )

  if (!is.list(modelo) ||
      !all(parametros %in% names(modelo))) {
    stop("El modelo no contiene los parametros necesarios.")
  }

  # -------------------------------------------------
  # 2. CONVERTIR LOS DATOS A MATRIZ
  # -------------------------------------------------

  X <- as.matrix(X)

  if (!is.numeric(X) ||
      nrow(X) == 0 ||
      ncol(X) == 0 ||
      any(!is.finite(X))) {
    stop("X debe contener valores numericos finitos.")
  }

  # -------------------------------------------------
  # 3. COMPROBAR DIMENSIONES Y PARAMETROS
  # -------------------------------------------------

  if (!is.numeric(modelo$pesos) ||
      !is.numeric(modelo$medias) ||
      !is.numeric(modelo$desviaciones) ||
      !is.numeric(modelo$sesgo) ||
      length(modelo$pesos) != ncol(X) ||
      length(modelo$medias) != ncol(X) ||
      length(modelo$desviaciones) != ncol(X) ||
      length(modelo$sesgo) != 1 ||
      any(!is.finite(c(
        modelo$pesos,
        modelo$medias,
        modelo$desviaciones,
        modelo$sesgo
      ))) ||
      any(modelo$desviaciones <= 0)) {
    stop("Los parametros del modelo no son compatibles con X.")
  }

  # -------------------------------------------------
  # 4. ESTANDARIZAR VARIABLES
  # -------------------------------------------------

  Xs <- scale(
    X,
    center = modelo$medias,
    scale = modelo$desviaciones
  )

  # -------------------------------------------------
  # 5. CALCULAR COMBINACION LINEAL
  # -------------------------------------------------

  z <- as.vector(
    Xs %*% modelo$pesos + modelo$sesgo
  )

  # -------------------------------------------------
  # 6. GENERAR PREDICCIONES
  # -------------------------------------------------

  predicciones <- as.integer(z >= 0)

  return(predicciones)
}
