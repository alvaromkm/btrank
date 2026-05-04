# =============================================================================
# ejemplo_bt_prob_matrix.R
# Script de desarrollo — NO forma parte del paquete.
# =============================================================================

source("R/bt_win_matrix.R")
source("R/bt_fit.R")
source("R/bt_rank.R")
source("R/bt_prob_matrix.R")

# Datos compartidos
datos <- data.frame(
  item   = c("A", "B", "C", "A", "B", "C"),
  period = c(2022, 2022, 2022, 2023, 2023, 2023),
  score  = c(80,   65,   50,   75,   85,   60)
)
mat <- bt_win_matrix(datos, score_col = "score")
fit <- bt_fit(mat)

# -----------------------------------------------------------------------------
# EJEMPLO 1: Matriz completa ordenada por ability
# -----------------------------------------------------------------------------
cat("=== Ejemplo 1: orden por ability ===\n")
pm <- bt_prob_matrix(fit)
print(round(pm, 3))
# Esperado:
#   Diagonal = NA
#   P(A beats C) y P(B beats C) cercanas a 1
#   P(A beats B) y P(B beats A) cercanas a 0.5 (abilities identicas)


# -----------------------------------------------------------------------------
# EJEMPLO 2: Orden alfabetico
# -----------------------------------------------------------------------------
cat("\n=== Ejemplo 2: orden alfabetico ===\n")
pm_alpha <- bt_prob_matrix(fit, order_by = "name")
print(round(pm_alpha, 3))


# -----------------------------------------------------------------------------
# EJEMPLO 3: Acceso a probabilidades individuales
# -----------------------------------------------------------------------------
cat("\n=== Ejemplo 3: probabilidades individuales ===\n")
cat("P(A beats C):", round(pm["A", "C"], 4), "\n")   # debe ser ~1
cat("P(C beats A):", round(pm["C", "A"], 4), "\n")   # debe ser ~0
cat("P(A beats B):", round(pm["A", "B"], 4), "\n")   # debe ser 0.5 (empate perfecto)
cat("P(A beats B) + P(B beats A) =",
    round(pm["A", "B"] + pm["B", "A"], 6), "\n")     # debe ser exactamente 1


# -----------------------------------------------------------------------------
# EJEMPLO 4: Dominancia clara — probabilidades extremas
# -----------------------------------------------------------------------------
cat("\n=== Ejemplo 4: dominancia clara ===\n")
datos_dom <- data.frame(
  item   = c("Top", "Mid", "Bot", "Top", "Mid", "Bot", "Top", "Mid", "Bot"),
  period = c(2021,  2021,  2021,  2022,  2022,  2022,  2023,  2023,  2023),
  score  = c(95,    60,    30,    92,    58,    28,    97,    62,    25)
)
fit_dom <- bt_fit(bt_win_matrix(datos_dom, score_col = "score"))
pm_dom  <- bt_prob_matrix(fit_dom)
print(round(pm_dom, 4))
# Esperado: P(Top beats Bot) muy cercano a 1


# -----------------------------------------------------------------------------
# EJEMPLO 5: Simetria — P(i,j) + P(j,i) = 1 para todo par
# -----------------------------------------------------------------------------
cat("\n=== Ejemplo 5: verificacion de simetria ===\n")
items <- rownames(pm_dom)
ok <- TRUE
for (i in seq_along(items)) {
  for (j in seq_along(items)) {
    if (i == j) next
    suma <- pm_dom[i, j] + pm_dom[j, i]
    if (abs(suma - 1) > 1e-10) {
      cat("FALLO en par", items[i], "-", items[j], ": suma =", suma, "\n")
      ok <- FALSE
    }
  }
}
if (ok) cat("Simetria correcta: P(i,j) + P(j,i) = 1 para todos los pares.\n")


# -----------------------------------------------------------------------------
# EJEMPLO 6: Validacion de errores
# -----------------------------------------------------------------------------
cat("\n=== Ejemplo 6: comprobacion de errores ===\n")

tryCatch(
  bt_prob_matrix("no soy un btfit"),
  error = function(e) cat("Error esperado (no btfit):", conditionMessage(e), "\n")
)

tryCatch(
  bt_prob_matrix(fit, order_by = "invalido"),
  error = function(e) cat("Error esperado (order_by invalido):", conditionMessage(e), "\n")
)

