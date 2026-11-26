# The options of create_longitudinal_study() that let a caller ask the
# simulator for what it would otherwise have to change afterwards: vital signs
# by a spec handed in, gsm's column names, a site id prefix, and the phase and
# the area the study table says.
#
# Written on a machine with no desktop R, so these were not run there. The
# same code is run in R in the browser by study.builder's `npm run test:r`.

study_with <- function(...) {
  set.seed(7)
  create_longitudinal_study(
    "S-OPT",
    participants = 30, sites = 4, snapshots = 3,
    domains = c("AE", "VISIT"), ...
  )
}

test_that("the site id prefix is the caller's, and changes nothing else", {
  test_at_log_threshold()
  usual <- study_with()
  safe <- study_with(site_id_prefix = "SITE")
  last <- length(usual$raw_data)
  expect_true(all(grepl("^0X[0-9]+$", usual$raw_data[[last]]$Raw_SITE$invid)))
  expect_true(all(grepl("^SITE[0-9]+$", safe$raw_data[[last]]$Raw_SITE$invid)))
  # The same ids under another prefix, in every table that carries one.
  for (table in names(usual$raw_data[[last]])) {
    before <- usual$raw_data[[last]][[table]]
    after <- safe$raw_data[[last]][[table]]
    for (column in names(before)) {
      if (is.character(before[[column]])) {
        before[[column]] <- sub("^0X([0-9]+)$", "SITE\\1", before[[column]])
      }
    }
    expect_equal(after, before, info = table)
  }
  expect_error(study_with(site_id_prefix = "0 X"), "letters and digits")
  # The option is put back.
  expect_null(getOption("gsm.datasim.site_id_prefix"))
})

test_that("the study table says the phase and the area asked for, and nothing else moves", {
  test_at_log_threshold()
  usual <- study_with()
  named <- study_with(phase = "P3", therapeutic_area = "Virology")
  for (snapshot in seq_along(named$raw_data)) {
    expect_equal(unique(named$raw_data[[snapshot]]$Raw_STUDY$phase), "P3")
    expect_equal(unique(named$raw_data[[snapshot]]$Raw_STUDY$therapeutic_area), "Virology")
    for (table in setdiff(names(usual$raw_data[[snapshot]]), "Raw_STUDY")) {
      expect_equal(
        named$raw_data[[snapshot]][[table]], usual$raw_data[[snapshot]][[table]],
        info = table
      )
    }
  }
  kept <- setdiff(names(usual$raw_data[[1]]$Raw_STUDY), c("phase", "therapeutic_area"))
  expect_equal(named$raw_data[[1]]$Raw_STUDY[kept], usual$raw_data[[1]]$Raw_STUDY[kept])
  expect_equal(named$config$phase, "P3")
})

test_that("columns are named as gsm names them when asked, and say what they were called", {
  test_at_log_threshold()
  usual <- study_with()
  named <- study_with(column_names = "gsm")
  last <- length(usual$raw_data)
  again <- apply_gsm_names(usual$raw_data[[last]])
  renamed <- FALSE
  for (table in names(again)) {
    expect_equal(named$raw_data[[last]][[table]], again[[table]], info = table)
    was <- attr(again[[table]], "renamed")
    if (length(was)) {
      renamed <- TRUE
      expect_true(all(names(was) %in% names(again[[table]])))
      expect_false(any(unname(was) %in% names(again[[table]])))
      expect_true(all(unname(was) %in% names(usual$raw_data[[last]][[table]])))
    }
  }
  expect_true(renamed)
  # Applying it twice gives what applying it once gives.
  expect_equal(apply_gsm_names(again), again)
  expect_error(study_with(column_names = "sdtm"), "should be one of")
})

test_that("a domain whose spec is handed in is made in every snapshot, and moves no other table", {
  test_at_log_threshold()
  spec <- list(Raw_VS = make_vs_full_spec())
  usual <- study_with()
  with_vitals <- study_with(domains = c("AE", "VISIT", "VS"), specs = spec)
  for (snapshot in seq_along(usual$raw_data)) {
    expect_true("Raw_VS" %in% names(with_vitals$raw_data[[snapshot]]))
    expect_gt(nrow(with_vitals$raw_data[[snapshot]]$Raw_VS), 0)
    for (table in names(usual$raw_data[[snapshot]])) {
      expect_equal(
        with_vitals$raw_data[[snapshot]][[table]], usual$raw_data[[snapshot]][[table]],
        info = paste(snapshot, table)
      )
    }
  }
  # What a snapshot holds of vital signs is in the next one, unchanged.
  for (snapshot in seq_len(length(with_vitals$raw_data) - 1)) {
    now <- with_vitals$raw_data[[snapshot]]$Raw_VS
    then <- with_vitals$raw_data[[snapshot + 1]]$Raw_VS
    expect_equal(then[seq_len(nrow(now)), , drop = FALSE], now, ignore_attr = TRUE)
  }
  # The same seed gives the same vital signs.
  expect_equal(
    study_with(domains = c("AE", "VISIT", "VS"), specs = spec)$raw_data,
    with_vitals$raw_data
  )
})

test_that("specs that cannot be used are refused, and a mapping spec's other tables are left out", {
  expect_null(validate_study_specs(NULL))
  expect_error(validate_study_specs(list(list(a = 1))), "named by table")
  expect_error(validate_study_specs(list(Mapped_SUBJ = list(subjid = list()))), "names no raw table")
  expect_error(validate_study_specs(list(Raw_VS = list())), "names no column")
  kept <- validate_study_specs(list(
    Raw_VS = list(subjid = list(type = "character")),
    Mapped_SUBJ = list(subjid = list(type = "character"))
  ))
  expect_named(kept, "Raw_VS")
})

test_that("code on a stream of its own leaves the main stream where it was", {
  set.seed(3)
  expected <- stats::runif(3)
  set.seed(3)
  own <- gsm.datasim:::.with_own_stream(1, stats::runif(5))
  expect_equal(stats::runif(3), expected)
  set.seed(3)
  expect_equal(gsm.datasim:::.with_own_stream(1, stats::runif(5)), own)
  expect_false(isTRUE(all.equal(gsm.datasim:::.with_own_stream(2, stats::runif(5)), own)))
})
