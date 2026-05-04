# =============================================================================
# ejemplo_bt_weights.R
# Script de desarrollo — NO forma parte del paquete.
# =============================================================================

source("R/bt_weights.R")

# -----------------------------------------------------------------------------
# EJEMPLO 1: Caso base — 5 temporadas, half_life = 2
# -----------------------------------------------------------------------------
cat("=== Ejemplo 1: half_life = 2 ===\n")
w1 <- bt_weights(periods = 2019:2023, half_life = 2)
print(round(w1, 4))
# Esperado: 2023 = 1.0, 2022 = 0.5, 2021 = 0.25, 2020 = 0.125, 2019 = 0.0625


# -----------------------------------------------------------------------------
# EJEMPLO 2: Decay agresivo — half_life = 1
# -----------------------------------------------------------------------------
cat("\n=== Ejemplo 2: half_life = 1 (agresivo) ===\n")
w2 <- bt_weights(periods = 2019:2023, half_life = 1)
print(round(w2, 4))
# Esperado: cada periodo vale la mitad del siguiente


# -----------------------------------------------------------------------------
# EJEMPLO 3: Decay suave — half_life = 10
# -----------------------------------------------------------------------------
cat("\n=== Ejemplo 3: half_life = 10 (suave) ===\n")
w3 <- bt_weights(periods = 2019:2023, half_life = 10)
print(round(w3, 4))
# Esperado: pesos cercanos entre si, todos proximos a 1


# -----------------------------------------------------------------------------
# EJEMPLO 4: Verificacion algebraica de la formula
# El periodo mas reciente siempre debe tener peso exactamente 1
# Cada periodo anterior debe tener peso = 0.5^(gap/half_life)
# -----------------------------------------------------------------------------
cat("\n=== Ejemplo 4: verificacion algebraica ===\n")
w4    <- bt_weights(periods = 2019:2023, half_life = 2)
t_max <- 2023
hl    <- 2

ok <- TRUE
for (t in 2019:2023) {
  esperado <- (0.5) ^ ((t_max - t) / hl)
  obtenido <- w4[as.character(t)]
  if (abs(esperado - obtenido) > 1e-12) {
    cat("FALLO en", t, ": esperado", esperado, "obtenido", obtenido, "\n")
    ok <- FALSE
  }
}
if (ok) cat("Formula correcta para todos los periodos.\n")


# -----------------------------------------------------------------------------
# EJEMPLO 5: Etiquetas de temporada como strings
# -----------------------------------------------------------------------------
cat("\n=== Ejemplo 5: periodos como strings ===\n")
w5 <- bt_weights(periods = c("2021-22", "2022-23", "2023-24"), half_life = 1)
print(round(w5, 4))
# Esperado: "2023-24" = 1.0, "2022-23" = 0.5, "2021-22" = 0.25


# -----------------------------------------------------------------------------
# EJEMPLO 6: Integracion con bt_win_matrix
# Los nombres del vector deben coincidir con los valores de period en el df
# -----------------------------------------------------------------------------
cat("\n=== Ejemplo 6: integracion con bt_win_matrix ===\n")
datos <- data.frame(
  item   = c("A", "B", "C", "A", "B", "C"),
  period = c(2022, 2022, 2022, 2023, 2023, 2023),
  score  = c(80,   65,   50,   75,   85,   60)
)
w6  <- bt_weights(periods = c(2022, 2023), half_life = 1)
cat("Pesos generados:\n")
print(w6)
mat <- bt_win_matrix(datos, score_col = "score", weights = w6)
cat("Win matrix con pesos temporales:\n")
print(mat)
# Esperado: 2023 (peso 1.0) contribuye mas que 2022 (peso 0.5)


# -----------------------------------------------------------------------------
# EJEMPLO 7: Validacion de errores
# -----------------------------------------------------------------------------
cat("\n=== Ejemplo 7: comprobacion de errores ===\n")

tryCatch(
  bt_weights(periods = c(), half_life = 2),
  error = function(e) cat("Error esperado (periods vacio):", conditionMessage(e), "\n")
)

tryCatch(
  bt_weights(periods = 2019:2023, half_life = -1),
  error = function(e) cat("Error esperado (half_life negativo):", conditionMessage(e), "\n")
)

tryCatch(
  bt_weights(periods = 2019:2023, half_life = 0),
  error = function(e) cat("Error esperado (half_life = 0):", conditionMessage(e), "\n")
)

tryCatch(
  bt_weights(periods = 2019:2023, half_life = c(1, 2)),
  error = function(e) cat("Error esperado (half_life no escalar):", conditionMessage(e), "\n")
)
