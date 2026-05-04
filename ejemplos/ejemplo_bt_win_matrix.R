# =============================================================================
# ejemplo_bt_win_matrix.R
# Script de desarrollo — NO forma parte del paquete.
# Carga la función directamente con source() y prueba casos representativos.
# =============================================================================

source("R/bt_win_matrix.R")  # ajusta la ruta si ejecutas desde fuera del proyecto

# -----------------------------------------------------------------------------
# EJEMPLO 1: Caso mínimo — 3 equipos, 2 temporadas, sin pesos
# -----------------------------------------------------------------------------
# Temporada 2022: A > B > C
# Temporada 2023: B > A > C
# Esperamos que A y B tengan winrates similares, C claramente por debajo.

datos_simple <- data.frame(
  item   = c("A", "B", "C",   "A", "B", "C"),
  period = c(2022, 2022, 2022, 2023, 2023, 2023),
  score  = c(80,   65,   50,   75,   85,   60)
)

mat1 <- bt_win_matrix(datos_simple, score_col = "score")

cat("=== Ejemplo 1: sin pesos ===\n")
print(mat1)
# Esperado:
#   A vs B: A gana en 2022, B gana en 2023 → 1-1
#   A vs C: A gana en ambas → 2-0
#   B vs C: B gana en ambas → 2-0


# -----------------------------------------------------------------------------
# EJEMPLO 2: Mismo caso con lower_is_better = FALSE
# Útil si la métrica es tiempo, errores, etc.
# -----------------------------------------------------------------------------
datos_tiempo <- data.frame(
  item   = c("Ana", "Ben", "Cara"),
  period = rep("ronda1", 3),
  score  = c(12.3, 14.1, 11.8)   # segundos — menor es mejor
)

mat2 <- bt_win_matrix(
  datos_tiempo,
  score_col        = "score",
  higher_is_better = FALSE
)

cat("\n=== Ejemplo 2: lower_is_better (tiempos) ===\n")
print(mat2)
# Esperado: Cara (11.8) > Ana (12.3) > Ben (14.1)


# -----------------------------------------------------------------------------
# EJEMPLO 3: Con empates — score idéntico en un periodo
# -----------------------------------------------------------------------------
datos_empate <- data.frame(
  item   = c("X", "Y", "Z"),
  period = rep("p1", 3),
  score  = c(100, 100, 80)   # X e Y empatan
)

mat3 <- bt_win_matrix(datos_empate, score_col = "score")

cat("\n=== Ejemplo 3: con empate ===\n")
print(mat3)
# Esperado:
#   X vs Y → 0.5 / 0.5 (empate)
#   X vs Z → 1 / 0
#   Y vs Z → 1 / 0


# -----------------------------------------------------------------------------
# EJEMPLO 4: Con pesos temporales manuales
# La temporada 2023 vale el doble que la 2022.
# -----------------------------------------------------------------------------
pesos <- c("2022" = 0.5, "2023" = 1.0)

mat4 <- bt_win_matrix(
  datos_simple,
  score_col = "score",
  weights   = pesos
)

cat("\n=== Ejemplo 4: con pesos temporales manuales ===\n")
print(mat4)
# A vs B: A gana 2022 (peso 0.5) + B gana 2023 (peso 1.0) → A:0.5, B:1.0
# A vs C: A gana ambas → 0.5 + 1.0 = 1.5
# B vs C: igual → 1.5


# -----------------------------------------------------------------------------
# EJEMPLO 5: Nombres de columna personalizados
# Útil cuando el usuario no quiere renombrar su data frame.
# -----------------------------------------------------------------------------
datos_vinos <- data.frame(
  vino        = c("Rioja", "Ribera", "Priorat"),
  convocatoria = rep("cata_2024", 3),
  puntuacion  = c(92, 88, 95)
)

mat5 <- bt_win_matrix(
  datos_vinos,
  item_col   = "vino",
  period_col = "convocatoria",
  score_col  = "puntuacion"
)

cat("\n=== Ejemplo 5: columnas con nombres personalizados (vinos) ===\n")
print(mat5)


# -----------------------------------------------------------------------------
# EJEMPLO 6: Validación de errores — comprueba que los checks funcionan
# -----------------------------------------------------------------------------
cat("\n=== Ejemplo 6: comprobación de errores ===\n")

tryCatch(
  bt_win_matrix("esto no es un data frame"),
  error = function(e) cat("Error esperado (no data frame):", conditionMessage(e), "\n")
)

tryCatch(
  bt_win_matrix(datos_simple, score_col = "columna_inexistente"),
  error = function(e) cat("Error esperado (columna ausente):", conditionMessage(e), "\n")
)

tryCatch(
  bt_win_matrix(data.frame(item = "solo_uno", period = 1, score = 99)),
  error = function(e) cat("Error esperado (un solo item):", conditionMessage(e), "\n")
)
