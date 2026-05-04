# =============================================================================
# ejemplo_bt_fit.R
# Script de desarrollo — NO forma parte del paquete.
# Carga las funciones directamente con source() y prueba bt_fit.
# =============================================================================

source("R/bt_win_matrix.R")
source("R/bt_fit.R")

# -----------------------------------------------------------------------------
# EJEMPLO 1: Caso base — 3 items, 2 periodos
# -----------------------------------------------------------------------------
datos_simple <- data.frame(
  item   = c("A", "B", "C", "A", "B", "C"),
  period = c(2022, 2022, 2022, 2023, 2023, 2023),
  score  = c(80,   65,   50,   75,   85,   60)
)

mat1 <- bt_win_matrix(datos_simple, score_col = "score")
fit1 <- bt_fit(mat1)

cat("=== Ejemplo 1: caso base ===\n")
print(fit1)
# Esperado:
#   C es el item de referencia (peor en ambas temporadas)
#   A y B tienen abilities > 0, similares entre sí


# -----------------------------------------------------------------------------
# EJEMPLO 2: Dominancia clara — un item gana siempre
# -----------------------------------------------------------------------------
datos_dom <- data.frame(
  item   = c("Top", "Mid", "Bot", "Top", "Mid", "Bot", "Top", "Mid", "Bot"),
  period = c(2021,  2021,  2021,  2022,  2022,  2022,  2023,  2023,  2023),
  score  = c(95,    60,    30,    92,    58,    28,    97,    62,    25)
)

mat2 <- bt_win_matrix(datos_dom, score_col = "score")
fit2 <- bt_fit(mat2)

cat("\n=== Ejemplo 2: dominancia clara ===\n")
print(fit2)
# Esperado:
#   Bot = referencia (ability 0)
#   Top tiene ability claramente mayor que Mid


# -----------------------------------------------------------------------------
# EJEMPLO 3: Comprobar el objeto btfit completo
# -----------------------------------------------------------------------------
cat("\n=== Ejemplo 3: estructura del objeto btfit ===\n")
cat("Clase:      ", class(fit1), "\n")
cat("Componentes:", paste(names(fit1), collapse = ", "), "\n")
cat("Reference:  ", fit1$reference, "\n")
cat("Items:      ", paste(fit1$items, collapse = ", "), "\n")
cat("Abilities:\n")
print(round(fit1$abilities, 4))


# -----------------------------------------------------------------------------
# EJEMPLO 4: Item sin comparaciones — debe lanzar warning y eliminarse
# -----------------------------------------------------------------------------
cat("\n=== Ejemplo 4: item sin comparaciones (warning esperado) ===\n")

mat_huerfano <- matrix(
  c(0, 2, 0,
    1, 0, 0,
    0, 0, 0),   # "Z" no tiene ninguna comparacion
  nrow = 3, ncol = 3,
  dimnames = list(c("X", "Y", "Z"), c("X", "Y", "Z"))
)

fit4 <- withCallingHandlers(
  bt_fit(mat_huerfano),
  warning = function(w) {
    cat("Warning capturado:", conditionMessage(w), "\n")
    invokeRestart("muffleWarning")
  }
)
cat("Items en el modelo tras eliminar Z:", paste(fit4$items, collapse = ", "), "\n")


# -----------------------------------------------------------------------------
# EJEMPLO 5: Validacion de errores
# -----------------------------------------------------------------------------
cat("\n=== Ejemplo 5: comprobacion de errores ===\n")

tryCatch(
  bt_fit("no soy una matriz"),
  error = function(e) cat("Error esperado (no matrix):", conditionMessage(e), "\n")
)

tryCatch(
  bt_fit(matrix(1:6, nrow = 2, ncol = 3)),
  error = function(e) cat("Error esperado (no cuadrada):", conditionMessage(e), "\n")
)

mat_sin_nombres <- matrix(c(0,1,1,0), nrow = 2)
tryCatch(
  bt_fit(mat_sin_nombres),
  error = function(e) cat("Error esperado (sin nombres):", conditionMessage(e), "\n")
)

