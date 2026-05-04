# =============================================================================
# ejemplo_bt_rank_all.R
# Script de desarrollo — NO forma parte del paquete.
# =============================================================================

source("R/bt_win_matrix.R")
source("R/bt_fit.R")
source("R/bt_rank.R")
source("R/bt_prob_matrix.R")
source("R/bt_weights.R")
source("R/bt_rank_all.R")

# Datos compartidos
datos <- data.frame(
  item   = c("A", "B", "C", "A", "B", "C"),
  period = c(2022, 2022, 2022, 2023, 2023, 2023),
  score  = c(80,   65,   50,   75,   85,   60)
)

# -----------------------------------------------------------------------------
# EJEMPLO 1: Pipeline completo en una sola llamada
# -----------------------------------------------------------------------------
cat("=== Ejemplo 1: pipeline completo ===\n")
print(bt_rank_all(datos, score_col = "score"))


# -----------------------------------------------------------------------------
# EJEMPLO 2: Con ponderacion temporal
# -----------------------------------------------------------------------------
cat("\n=== Ejemplo 2: con half_life = 1 ===\n")
print(bt_rank_all(datos, score_col = "score", half_life = 1))
# Con half_life = 1, 2023 pesa el doble que 2022
# B gano en 2023 (peso alto) -> B deberia superar a A


# -----------------------------------------------------------------------------
# EJEMPLO 3: Top N
# -----------------------------------------------------------------------------
cat("\n=== Ejemplo 3: top_n = 2 ===\n")
print(bt_rank_all(datos, score_col = "score", top_n = 2))


# -----------------------------------------------------------------------------
# EJEMPLO 4: Columnas con nombres personalizados
# -----------------------------------------------------------------------------
cat("\n=== Ejemplo 4: nombres de columna personalizados ===\n")
datos_vinos <- data.frame(
  vino   = c("Rioja", "Ribera", "Priorat", "Rioja", "Ribera", "Priorat"),
  cata   = c(2022,    2022,     2022,      2023,    2023,     2023),
  nota   = c(92,      88,       95,        89,      91,       96)
)
print(bt_rank_all(
  datos_vinos,
  item_col   = "vino",
  period_col = "cata",
  score_col  = "nota"
))


# -----------------------------------------------------------------------------
# EJEMPLO 5: Metrica donde menor es mejor
# -----------------------------------------------------------------------------
cat("\n=== Ejemplo 5: lower_is_better (tiempos) ===\n")
datos_tiempo <- data.frame(
  item   = c("Ana", "Ben", "Cara", "Ana", "Ben", "Cara"),
  period = c(2022,  2022,  2022,   2023,  2023,  2023),
  score  = c(12.3,  14.1,  11.8,   12.0,  13.5,  11.5)
)
print(bt_rank_all(datos_tiempo, score_col = "score", higher_is_better = FALSE))
# Esperado: Cara > Ana > Ben (tiempos mas bajos = mejor)


# -----------------------------------------------------------------------------
# EJEMPLO 6: Equivalencia con pipeline manual
# Verifica que bt_rank_all produce exactamente el mismo resultado
# que llamar a las funciones por separado
# -----------------------------------------------------------------------------
cat("\n=== Ejemplo 6: equivalencia con pipeline manual ===\n")

# Pipeline manual
w   <- bt_weights(periods = c(2022, 2023), half_life = 1)
mat <- bt_win_matrix(datos, score_col = "score", weights = w)
fit <- bt_fit(mat)
rk_manual <- bt_rank(fit)

# Wrapper
rk_wrapper <- bt_rank_all(datos, score_col = "score", half_life = 1)

cat("Resultados identicos:", identical(rk_manual, rk_wrapper), "\n")


# -----------------------------------------------------------------------------
# EJEMPLO 7: Validacion de errores
# -----------------------------------------------------------------------------
cat("\n=== Ejemplo 7: comprobacion de errores ===\n")

tryCatch(
  bt_rank_all("no soy un data frame"),
  error = function(e) cat("Error esperado (no data frame):", conditionMessage(e), "\n")
)

tryCatch(
  bt_rank_all(datos, score_col = "score", half_life = -2),
  error = function(e) cat("Error esperado (half_life negativo):", conditionMessage(e), "\n")
)

tryCatch(
  bt_rank_all(datos, score_col = "score", half_life = 0),
  error = function(e) cat("Error esperado (half_life = 0):", conditionMessage(e), "\n")
)
