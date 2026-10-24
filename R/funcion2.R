# =====================================================
# 2. FUNCION PARA PREDECIR
# =====================================================

predecir_perceptron <- function(modelo, X) {
  X <- as.matrix(X)
  Xs <- scale(
    X,
    center = modelo$medias,
    scale = modelo$desviaciones
  )
  z <- as.vector(Xs %*% modelo$pesos + modelo$sesgo)
  as.integer(z >= 0)
}
