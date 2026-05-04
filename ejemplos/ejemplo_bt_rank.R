# =============================================================================
# ejemplo_bt_rank.R
# Script de desarrollo — NO forma parte del paquete.
# =============================================================================

source("R/bt_win_matrix.R")
source("R/bt_fit.R")
source("R/bt_rank.R")

# Datos compartidos para todos los ejemplos
datos <- data.frame(
  item   = c("A", "B", "C", "A", "B", "C"),
  period = c(2022, 2022, 2022, 2023, 2023, 2023),
  score  = c(80,   65,   50,   75,   85,   60)
)
mat <- bt_win_matrix(datos, score_col = "score")
fit <- bt_fit(mat)

# -----------------------------------------------------------------------------
# EJEMPLO 1: Ranking completo
# -----------------------------------------------------------------------------
cat("=== Ejemplo 1: ranking completo ===\n")
print(bt_rank(fit))
# Esperado: 3 filas, A y B empatados en ability, C con ability 0


# -----------------------------------------------------------------------------
# EJEMPLO 2: Top N
# -----------------------------------------------------------------------------
cat("\n=== Ejemplo 2: top_n = 2 ===\n")
print(bt_rank(fit, top_n = 2))
# Esperado: solo las 2 primeras filas


# -----------------------------------------------------------------------------
# EJEMPLO 3: top_n mayor que el numero de items — warning esperado
# -----------------------------------------------------------------------------
cat("\n=== Ejemplo 3: top_n > n items (warning esperado) ===\n")
print(bt_rank(fit, top_n = 10))


# -----------------------------------------------------------------------------
# EJEMPLO 4: ability_norm — verifica que el rango es [0, 1]
# -----------------------------------------------------------------------------
cat("\n=== Ejemplo 4: ability_norm en [0, 1] ===\n")
rk <- bt_rank(fit)
cat("Max ability_norm:", max(rk$ability_norm), "\n")   # debe ser 1
cat("Min ability_norm:", min(rk$ability_norm), "\n")   # debe ser 0


# -----------------------------------------------------------------------------
# EJEMPLO 5: Dominancia clara — abilities bien separadas
# -----------------------------------------------------------------------------
cat("\n=== Ejemplo 5: dominancia clara ===\n")
datos_dom <- data.frame(
  item   = c("Top", "Mid", "Bot", "Top", "Mid", "Bot", "Top", "Mid", "Bot"),
  period = c(2021,  2021,  2021,  2022,  2022,  2022,  2023,  2023,  2023),
  score  = c(95,    60,    30,    92,    58,    28,    97,    62,    25)
)
mat_dom <- bt_win_matrix(datos_dom, score_col = "score")
fit_dom <- bt_fit(mat_dom)
print(bt_rank(fit_dom))
# Esperado: Top > Mid > Bot con abilities claramente distintas


# -----------------------------------------------------------------------------
# EJEMPLO 6: Validacion de errores
# -----------------------------------------------------------------------------
cat("\n=== Ejemplo 6: comprobacion de errores ===\n")

tryCatch(
  bt_rank("no soy un btfit"),
  error = function(e) cat("Error esperado (no btfit):", conditionMessage(e), "\n")
)

tryCatch(
  bt_rank(fit, top_n = -1),
  error = function(e) cat("Error esperado (top_n negativo):", conditionMessage(e), "\n")
)

tryCatch(
  bt_rank(fit, top_n = 0),
  error = function(e) cat("Error esperado (top_n = 0):", conditionMessage(e), "\n")
)

