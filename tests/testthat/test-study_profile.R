profile_snapshot <- function(n = 3000) {
  ids <- sprintf("S%05d", seq_len(n))
  set.seed(1)
  list(
    Raw_SUBJ = data.frame(
      subjid = ids, enrollyn = "Y", firstdosedate = as.Date("2025-01-10"), stringsAsFactors = FALSE
    ),
    Raw_VISIT = data.frame(
      studyid = "S", subjid = rep(ids, each = 2), instancename = rep(c("Screening", "Week 8"), n),
      visit_dt = rep(as.Date("2025-01-10") + c(-7, 56), n), stringsAsFactors = FALSE
    ),
    Raw_LB = data.frame(
      subjid = rep(ids, each = 4), visnam = rep(rep(c("Screening", "Week 8"), each = 2), n),
      lbtstnam = rep(c("ALT (SGPT)", "Sodium"), 2 * n), toxgrg_nsv = "0", stringsAsFactors = FALSE
    ),
    Raw_AE = data.frame(
      subjid = rep(ids, each = 2), mdrpt_nsv = "term1", mdrsoc_nsv = "soc1",
      aeser = rep(c("N", "N", "N", "Y"), length.out = 2 * n),
      aetoxgr = rep(c(1L, 2L, 3L, 4L), length.out = 2 * n),
      aest_dt = as.Date("2025-02-01"), stringsAsFactors = FALSE
    )
  )
}

test_that("Phase 2 with no area is neutral, and so is a profile of FALSE", {
  neutral <- study_profile()
  expect_equal(neutral$spread, 1)
  expect_equal(c(neutral$chronic, neutral$liver, neutral$kidney), c(1, 1, 1))
  expect_equal(c(neutral$serious, neutral$severe), c(0, 0))
  expect_length(neutral$classes, 0)
  expect_equal(study_profile("P2")$spread, 1)
  expect_equal(study_profile("Phase II", "nowhere")$spread, 1)
  raw <- profile_snapshot(200)
  expect_identical(
    apply_study_results(raw, list(profile = FALSE)),
    apply_study_results(raw, TRUE)
  )
  expect_identical(
    apply_study_results(raw, list(profile = study_profile("P2"))),
    apply_study_results(raw, TRUE)
  )
})

test_that("a phase is read however it is written, and each area has its numbers", {
  expect_equal(study_profile("P1")$phase, 1L)
  expect_equal(study_profile("Phase 3")$phase, 3L)
  expect_equal(study_profile("IV")$phase, 4L)
  expect_equal(study_profile(1)$spread, 0.85)
  expect_equal(study_profile("P3", "Oncology")$spread, 1.10 * 1.15)
  expect_equal(study_profile("P1", "oncology")$serious, -0.30 + 0.08)
  expect_equal(study_profile(NULL, "Virology")$chronic, 2)
  expect_equal(study_profile(NULL, "Oncology")$indication, "Advanced solid tumours")
  expect_true(nzchar(study_profile("P4", "Inflammation")$said))
})

test_that("a phase changes how often a lab is out of range, in the order the profile says", {
  raw <- profile_snapshot()
  out_of_range <- function(phase) {
    lb <- apply_study_results(raw, list(profile = study_profile(phase)))$Raw_LB
    sodium <- lb[lb$lbtstnam == "Sodium", ]
    mean(sodium$lbresn < sodium$lbnrlo | sodium$lbresn > sodium$lbnrhi)
  }
  # Results are rounded to three figures, so sodium shows fewer out of range
  # than the spread alone would give; the order and rough sizes hold.
  expect_lt(out_of_range("P1"), 0.6 * out_of_range("P2"))
  expect_gt(out_of_range("P3"), 1.3 * out_of_range("P2"))
  expect_gt(out_of_range("P2"), 0.015)
  expect_lt(out_of_range("P2"), 0.05)
})

test_that("Phase 1 has fewer serious events and milder ones; Oncology more of both", {
  raw <- profile_snapshot()
  events <- function(profile) apply_study_results(raw, list(profile = profile))$Raw_AE
  base <- events(study_profile("P2"))
  early <- events(study_profile("P1"))
  onc <- events(study_profile("P2", "Oncology"))
  expect_equal(mean(early$aeser == "Y") / mean(base$aeser == "Y"), 0.7, tolerance = 0.08)
  expect_lt(mean(early$aetoxgr), mean(base$aetoxgr))
  expect_gt(mean(onc$aeser == "Y"), mean(base$aeser == "Y"))
  expect_gt(mean(onc$aetoxgr), mean(base$aetoxgr))
  # A grade still agrees with seriousness.
  for (ae in list(early, onc)) {
    expect_true(all(ae$aetoxgr[ae$aeser == "Y"] >= 3))
    expect_true(all(ae$aetoxgr[ae$aeser == "N"] <= 3))
  }
  # No event is added or removed, and whose it is does not change.
  expect_identical(onc$subjid, base$subjid)
})

test_that("an area makes its classes of event commoner and its lab patterns likelier", {
  raw <- profile_snapshot()
  share <- function(area, class) {
    ae <- apply_study_results(raw, list(profile = study_profile("P2", area)))$Raw_AE
    mean(ae$mdrsoc_nsv == class)
  }
  expect_gt(
    share("Inflammation", "Musculoskeletal and connective tissue disorders"),
    1.2 * share(NULL, "Musculoskeletal and connective tissue disorders")
  )
  expect_gt(share("Virology", "Infections and infestations"), 1.2 * share(NULL, "Infections and infestations"))
  raised_at_start <- function(area) {
    lb <- apply_study_results(raw, list(profile = study_profile("P2", area)))$Raw_LB
    alt <- lb[lb$lbtstnam == "ALT (SGPT)" & lb$visnam == "Screening", ]
    mean(alt$lbresn > 2 * alt$lbnrhi)
  }
  expect_gt(raised_at_start("Virology"), 1.5 * raised_at_start(NULL))
})

test_that("a profile that cannot be used is refused", {
  expect_error(validate_study_results(list(profile = "Oncology")), "study_profile")
  expect_error(validate_study_results(list(profile = list(spread = 0))), "spread")
  expect_error(validate_study_results(list(profile = list(classes = c(1.2)))), "named by system organ class")
})
