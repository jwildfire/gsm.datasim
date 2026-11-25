snapshot_for_results <- function() {
  list(
    Raw_SUBJ = data.frame(
      subjid = sprintf("S%03d", 1:60), enrollyn = c(rep("Y", 50), rep("N", 10)),
      firstdosedate = as.Date("2025-01-10"), stringsAsFactors = FALSE
    ),
    Raw_VISIT = data.frame(
      studyid = "S", subjid = rep(sprintf("S%03d", 1:60), each = 3),
      instancename = rep(c("Screening", "Week 4", "Week 8"), 60),
      visit_dt = rep(as.Date("2025-01-10") + c(-7, 28, 56), 60), stringsAsFactors = FALSE
    ),
    Raw_LB = data.frame(
      subjid = rep(sprintf("S%03d", 1:60), each = 6),
      visnam = rep(rep(c("Screening", "Week 4", "Week 8"), each = 2), 60),
      lbtstnam = rep(c("ALT (SGPT)", "Total Bilirubin"), 180), toxgrg_nsv = "0",
      stringsAsFactors = FALSE
    )
  )
}

test_that("results add an arm to enrolled participants only", {
  out <- apply_study_results(snapshot_for_results(), TRUE)
  expect_setequal(unique(out$Raw_SUBJ$arm[1:50]), c("Placebo", "Active"))
  expect_true(all(is.na(out$Raw_SUBJ$arm[51:60])))
})

test_that("every lab has a result, a unit and a range, and most are in range", {
  out <- apply_study_results(snapshot_for_results(), TRUE)$Raw_LB
  expect_false(anyNA(out$lbresn))
  expect_true(all(out$lbnrlo < out$lbnrhi))
  expect_setequal(unique(out$lbresu), c("U/L", "mg/dL"))
  expect_gt(mean(out$lbresn >= out$lbnrlo & out$lbresn <= out$lbnrhi), 0.7)
})

test_that("there is an ECG row for each visit and measure", {
  out <- apply_study_results(snapshot_for_results(), TRUE)$Raw_EG
  expect_equal(nrow(out), 180 * 3)
  expect_setequal(unique(out$egtstnam), c("QTcF Interval, Aggregate", "QTcB Interval, Aggregate", "ECG Mean Heart Rate"))
})

test_that("applying results twice gives what applying them once gives, and FALSE gives nothing", {
  once <- apply_study_results(snapshot_for_results(), list(arms = c("Placebo", "Low", "High")))
  expect_identical(apply_study_results(once, list(arms = c("Placebo", "Low", "High"))), once)
  expect_identical(apply_study_results(snapshot_for_results(), FALSE), snapshot_for_results())
})
