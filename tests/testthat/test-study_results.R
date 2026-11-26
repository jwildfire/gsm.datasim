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

score_for_results <- function() {
  list(
    name = "ADAS-Cog (11)", visits = c("Screening", "Week 8"), baseline = 24, sd = 6.5,
    change = 2.5, effects = c(Active = -3), day = 57, min = 0, max = 70
  )
}

test_that("a score is a row per enrolled participant and visit it is measured at", {
  out <- apply_study_results(snapshot_for_results(), list(scores = list(score_for_results())))
  qs <- out$Raw_QS
  expect_equal(nrow(qs), 50 * 2)
  expect_setequal(unique(qs$visnam), c("Screening", "Week 8"))
  expect_setequal(unique(qs$subjid), sprintf("S%03d", 1:50))
  expect_true(all(qs$qsresn >= 0 & qs$qsresn <= 70))
  expect_equal(unique(qs$qststnam), "ADAS-Cog (11)")
})

test_that("a score moves by the arm's mean change, and by nothing before the first dose", {
  big <- snapshot_for_results()
  n <- 2000
  ids <- sprintf("S%04d", seq_len(n))
  big$Raw_SUBJ <- data.frame(
    subjid = ids, enrollyn = "Y", firstdosedate = as.Date("2025-01-10"), stringsAsFactors = FALSE
  )
  big$Raw_VISIT <- data.frame(
    studyid = "S", subjid = rep(ids, each = 2), instancename = rep(c("Screening", "Week 8"), n),
    visit_dt = rep(as.Date("2025-01-10") + c(-7, 56), n), stringsAsFactors = FALSE
  )
  big$Raw_LB <- NULL
  out <- apply_study_results(big, list(scores = list(score_for_results())))
  qs <- merge(out$Raw_QS, out$Raw_SUBJ[, c("subjid", "arm")], by = "subjid")
  wide <- merge(
    qs[qs$visnam == "Screening", c("subjid", "arm", "qsresn")],
    qs[qs$visnam == "Week 8", c("subjid", "qsresn")],
    by = "subjid", suffixes = c("_base", "_end")
  )
  change <- tapply(wide$qsresn_end - wide$qsresn_base, wide$arm, mean)
  expect_equal(unname(change["Placebo"]), 2.5, tolerance = 0.25)
  expect_equal(unname(change["Active"] - change["Placebo"]), -3, tolerance = 0.25)
  expect_equal(stats::sd(wide$qsresn_end - wide$qsresn_base), 6.5, tolerance = 0.15)
})

test_that("a score is the same in every snapshot that holds the visit, and applying twice changes nothing", {
  whole <- snapshot_for_results()
  early <- whole
  early$Raw_VISIT <- whole$Raw_VISIT[whole$Raw_VISIT$instancename != "Week 8", ]
  results <- list(scores = list(score_for_results()))
  first <- apply_study_results(early, results)$Raw_QS
  later <- apply_study_results(whole, results)$Raw_QS
  expect_equal(first$qsresn, later$qsresn[later$visnam == "Screening"])
  expect_identical(apply_study_results(apply_study_results(whole, results), results)$Raw_QS, later)
})

test_that("scores that cannot be used are refused, and none asked for makes no table", {
  expect_null(apply_study_results(snapshot_for_results(), TRUE)$Raw_QS)
  expect_error(validate_study_results(list(scores = list(list(sd = 5)))), "name")
  expect_error(validate_study_results(list(scores = list(list(name = "A", sd = 0)))), "sd")
  expect_error(
    validate_study_results(list(scores = list(list(name = "A", effects = c(1, 2))))), "named by arm"
  )
})
