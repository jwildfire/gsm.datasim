test_that("a design is validated: names, days and windows", {
  design <- list(visits = list(
    list(name = "Week 4", day = 29, before = 3, after = 3, vs = TRUE),
    list(name = "Screening", day = -14)
  ))
  valid <- validate_study_design(design)
  expect_equal(vapply(valid$visits, function(v) v$name, character(1)), c("Screening", "Week 4"))
  expect_equal(valid$visits[[1]]$before, 0L)
  expect_false(valid$visits[[1]]$vs)
  expect_null(validate_study_design(NULL))
  expect_error(validate_study_design(list(visits = list())), "non-empty")
  expect_error(validate_study_design(list(visits = list(list(name = "A", day = 0)))), "not 0")
  expect_error(
    validate_study_design(list(visits = list(list(name = "A", day = 1), list(name = "A", day = 8)))),
    "name of its own"
  )
})

test_that("a study follows its design: visits on their days, vitals and labs where scheduled", {
  design <- list(visits = list(
    list(name = "Screening", day = -14, lb = list(list(panel = "Chemistry", tests = c("ALT", "AST")))),
    list(name = "Baseline", day = 1, vs = TRUE),
    list(name = "Week 2", day = 15, before = 3, after = 3, vs = TRUE,
      lb = list(list(panel = "HEMATOLOGY", tests = character(0))))
  ))
  set.seed(1)
  study <- create_longitudinal_study(
    participants = 30, sites = 4, snapshots = 3,
    domains = c("AE", "LB", "VISIT"), design = design
  )
  last <- study$raw_data[[length(study$raw_data)]]
  expect_setequal(unique(last$Raw_VISIT$instancename), c("Screening", "Baseline", "Week 2"))
  screening <- last$Raw_LB[last$Raw_LB$visnam == "Screening", ]
  expect_setequal(unique(screening$lbtstnam), c("ALT", "AST"))
  expect_false(any(last$Raw_LB$visnam == "Baseline"))
  expect_true(all(grepl("HEMATOLOGY", last$Raw_LB$battrnam[last$Raw_LB$visnam == "Week 2"])))
})

test_that("applying a design twice gives what applying it once gives", {
  design <- list(visits = list(
    list(name = "Screening", day = -7),
    list(name = "Baseline", day = 1, before = 1, after = 1)
  ))
  set.seed(2)
  study <- create_longitudinal_study(participants = 20, sites = 3, snapshots = 2, domains = c("LB", "VISIT"))
  once <- apply_study_design(study$raw_data[[2]], design)
  twice <- apply_study_design(once, design)
  expect_equal(once$Raw_VISIT, twice$Raw_VISIT)
})
