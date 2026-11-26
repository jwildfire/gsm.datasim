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

snapshot_for_events <- function(n = 600) {
  ids <- sprintf("S%04d", seq_len(n))
  day_one <- as.Date("2025-01-10")
  list(
    Raw_SUBJ = data.frame(
      studyid = "S", subjid = ids, invid = "SITE1", enrollyn = "Y", firstdosedate = day_one,
      stringsAsFactors = FALSE
    ),
    Raw_VISIT = data.frame(
      studyid = "S", subjid = rep(ids, each = 3), instancename = rep(c("Baseline", "Week 12", "Week 24"), n),
      visit_dt = rep(day_one + c(0, 84, 168), n), stringsAsFactors = FALSE
    ),
    Raw_AE = data.frame(
      studyid = "S", subjid = rep(ids, each = 3), aeser = rep(c("Y", "N", "N"), n),
      mdrpt_nsv = "term1", mdrsoc_nsv = "soc1", aetoxgr = rep(c(1L, 5L, 2L), n), aeongo = "N", aerel = "N",
      mincreated_dts = day_one, aest_dt = rep(day_one + c(10, 60, 120), n),
      aeen_dt = rep(day_one + c(12, 62, 122), n), stringsAsFactors = FALSE
    ),
    Raw_STUDCOMP = data.frame(
      studyid = "S", compyn = NA_character_, compreas = "", mincreated_dts = day_one,
      subjid = "NEVER-ENROLLED", invid = "SITE1", stringsAsFactors = FALSE
    )
  )
}

test_that("adverse events are given terms from the list, and grades agree with seriousness", {
  out <- apply_study_results(snapshot_for_events(), TRUE)$Raw_AE
  terms <- .results_ae_terms()
  expect_true(all(out$mdrpt_nsv %in% terms$term))
  expect_equal(out$mdrsoc_nsv, terms$soc[match(out$mdrpt_nsv, terms$term)])
  expect_true(all(out$aetoxgr[out$aeser == "Y"] >= 3))
  expect_true(all(out$aetoxgr[out$aeser == "N"] <= 3))
  # Nothing but terms, grades and relatedness changes: no event is added or moved.
  before <- snapshot_for_events()$Raw_AE
  expect_equal(out[, c("subjid", "aeser", "aest_dt", "aeen_dt")], before[, c("subjid", "aeser", "aest_dt", "aeen_dt")])
})

test_that("drug-type events are more common on an active arm by the ratio asked for", {
  out <- apply_study_results(snapshot_for_events(3000), list(events = list(ratio = 2, signals = FALSE)))
  arm <- out$Raw_SUBJ$arm[match(out$Raw_AE$subjid, out$Raw_SUBJ$subjid)]
  drug <- out$Raw_AE$mdrpt_nsv %in% .results_ae_terms()$term[.results_ae_terms()$drug]
  expect_equal(mean(drug[arm == "Active"]) / mean(drug[arm == "Placebo"]), 2, tolerance = 0.1)
  untouched <- apply_study_results(snapshot_for_events(), list(events = FALSE))$Raw_AE
  expect_true(all(untouched$mdrpt_nsv == "term1"))
})

test_that("a simulated rise names an event on an active arm only", {
  out <- apply_study_results(snapshot_for_events(3000), TRUE)
  arm <- out$Raw_SUBJ$arm[match(out$Raw_AE$subjid, out$Raw_SUBJ$subjid)]
  named <- out$Raw_AE$mdrpt_nsv %in% c(
    "Alanine aminotransferase increased", "Blood creatinine increased", "Electrocardiogram QT prolonged"
  )
  expect_gt(sum(named), 0)
  expect_true(all(arm[named] == "Active"))
  expect_true(all(out$Raw_AE$aerel[named] == "Y"))
})

test_that("dropout leaves nothing dated after a participant's last day, and the completion table agrees", {
  day_one <- as.Date("2025-01-10")
  results <- list(dropout = list(rate = 0.3, arms = c(Active = 0.5), day = 169))
  out <- apply_study_results(snapshot_for_events(3000), results, snapshot_date = day_one + 400)
  gone <- out$Raw_STUDCOMP[out$Raw_STUDCOMP$compyn %in% "N", ]
  arm <- stats::setNames(out$Raw_SUBJ$arm, out$Raw_SUBJ$subjid)
  expect_equal(mean(names(arm)[arm == "Placebo"] %in% gone$subjid), 0.3, tolerance = 0.1)
  expect_equal(mean(names(arm)[arm == "Active"] %in% gone$subjid), 0.5, tolerance = 0.1)
  last <- stats::setNames(as.Date(gone$mincreated_dts), gone$subjid)
  for (table in list(c("Raw_VISIT", "visit_dt"), c("Raw_AE", "aest_dt"))) {
    until <- last[out[[table[1]]]$subjid]
    expect_false(any(!is.na(until) & as.Date(out[[table[1]]][[table[2]]]) > until))
  }
  expect_true(all(gone$compreas %in% c("Withdrew Consent", "Lost to Follow-Up", "Adverse Event")))
  # Everyone enrolled who did not leave has finished by day 400; the row of someone never enrolled is kept.
  expect_equal(sum(out$Raw_STUDCOMP$compyn %in% "Y") + nrow(gone), 3000)
  expect_true("NEVER-ENROLLED" %in% out$Raw_STUDCOMP$subjid)
})

test_that("dropout is the same applied twice, and an earlier snapshot's leavers have still left later", {
  day_one <- as.Date("2025-01-10")
  results <- list(dropout = 0.4)
  late <- apply_study_results(snapshot_for_events(), results, snapshot_date = day_one + 400)
  expect_identical(apply_study_results(late, results, snapshot_date = day_one + 400), late)
  early <- apply_study_results(snapshot_for_events(), results, snapshot_date = day_one + 60)
  left_early <- early$Raw_STUDCOMP$subjid[early$Raw_STUDCOMP$compyn %in% "N"]
  expect_true(all(left_early %in% late$Raw_STUDCOMP$subjid[late$Raw_STUDCOMP$compyn %in% "N"]))
  expect_identical(early$Raw_AE, late$Raw_AE)
  # With none asked for, the completion table is left alone.
  expect_identical(apply_study_results(snapshot_for_events(), TRUE)$Raw_STUDCOMP, snapshot_for_events()$Raw_STUDCOMP)
})

test_that("events and dropout that cannot be used are refused", {
  expect_null(validate_study_results(list(events = FALSE))$events)
  expect_equal(validate_study_results(TRUE)$events$ratio, 1.5)
  expect_null(validate_study_results(TRUE)$dropout)
  expect_null(validate_study_results(list(dropout = 0))$dropout)
  expect_error(validate_study_results(list(events = list(ratio = 0))), "ratio")
  expect_error(validate_study_results(list(dropout = 1.5)), "0 to 1")
  expect_error(validate_study_results(list(dropout = list(rate = 0.2, arms = c(0.1, 0.2)))), "named by arm")
  expect_error(validate_study_results(list(dropout = list(rate = 0.2, day = 1))), "2 or more")
})
