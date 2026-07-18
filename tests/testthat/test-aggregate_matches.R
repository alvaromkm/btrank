test_that(".matches_to_scores computes points and goal difference correctly", {
  m <- data.frame(
    period     = c(2020, 2020, 2020),
    home       = c("A", "B", "A"),
    away       = c("B", "A", "C"),
    home_goals = c(2, 1, 0),
    away_goals = c(1, 1, 0)
  )
  out <- btrank:::.matches_to_scores(
    m, period_col = "period", home_col = "home", away_col = "away",
    home_goals_col = "home_goals", away_goals_col = "away_goals"
  )
  
  a <- out[out$item == "A", ]
  b <- out[out$item == "B", ]
  c <- out[out$item == "C", ]
  
  expect_equal(a$points, 5); expect_equal(a$goal_diff, 1)
  expect_equal(b$points, 1); expect_equal(b$goal_diff, -1)
  expect_equal(c$points, 1); expect_equal(c$goal_diff, 0)
})

test_that(".matches_to_scores drops rows with missing data and warns", {
  m <- data.frame(
    period = c(2020, 2020), home = c("A", NA), away = c("B", "C"),
    home_goals = c(1, 2), away_goals = c(0, 1)
  )
  expect_warning(
    out <- btrank:::.matches_to_scores(m),
    "1 row"
  )
  expect_equal(nrow(out), 2)  # only A and B from the one valid match
})

test_that(".matches_to_scores errors on missing required columns", {
  m <- data.frame(period = 2020, home = "A")
  expect_error(btrank:::.matches_to_scores(m), "not found")
})