#' Test assorted functions in the Elo Ratings script for performance
context("Testing Elo Code")

test_that("Test Predicting Elo Result", {
    # Test the Elo Result Predicting Engine, within tolerance of 1e-06
    expect_equal(predictEloResult(1500, 1500), 0.5)
    expect_equal(predictEloResult(2000, 1200), 0.9901, tolerance = 1e-06)
    expect_equal(predictEloResult(1200, 2000), 0.0099, tolerance = 1e-06)

})

test_that("Test New Elo Ratings", {
    # Test that rankings are adjusted properly. Equal, mod k, extreme
    expect_equal(newRankings(1500, 1500, 0.5), c(1500, 1500))
    expect_equal(newRankings(1500, 1500, 1), c(1504, 1496))
    expect_equal(newRankings(1500, 1500, 0), c(1496, 1504))
    expect_equal(newRankings(2400, 2000, 1, k = 32), c(2402.9091, 1997.0909))
    expect_equal(newRankings(2400, 2000, 0, k = 32), c(2370.9091, 2029.0909))
    expect_equal(newRankings(2000, 1200, 1), c(2000.0792, 1199.9208))
    expect_equal(newRankings(2000, 1200, 0), c(1992.0792, 1207.9208))

})

test_that("Test Date Separator", {
    # test that the dates are properly separated by season.
    datelist1 <- c(as.Date("2012-01-05"), as.Date("2012-01-05"), as.Date("2012-09-05"), as.Date("2011-11-20"), as.Date("2013-01-05"), as.Date("2015-01-05"))
    datelist1 <- sort(datelist1)
    expect_equal(splitDates(datelist1), list(c(as.Date("2011-11-20"), as.Date("2012-01-05"), as.Date("2012-01-05")), c(as.Date("2012-09-05"), as.Date("2013-01-05")),
        c(as.Date("2015-01-05"))))
})

test_that("Test Load and Preparation of Elo Data", {
    # Test that loadEloData properly munches on crispy data files Test that the prepareEloData correctly munches data, splits past and present

})

test_that("Test meta Statistics", {
    # Test that the meta statistics are correct
    testdf <- data.frame("Date" = c(as.Date("2012-01-05"), as.Date("2012-01-06"), as.Date("2012-01-07"), as.Date("2012-01-08"), as.Date("2012-01-09"),
        as.Date("2012-01-15")), "TeamA" = c(10, 9, 10, 11, 10, 10), "TeamB" = c(10, 6, 6, 5, 6, 9), "TeamC" = c(8, 7, 12, 7, 8, 6))

    # Hand-calculated Answers
    answer <- data.frame("season.end" = as.Date("2012-01-15"), "mean" = 8.33333333, "max.team" = "TeamC", "max.date" = as.Date("2012-01-07"), "max.val" = 12, "min.team" = "TeamB", "min.date" = as.Date("2012-01-08"),
        "min.val" = 5, "best.avg.team" = "TeamA", "best.avg.team.avg" = 10, "worst.avg.team" = "TeamB", "worst.avg.team.avg" = 7, stringsAsFactors = FALSE)

    answer2 <- data.frame("season.end" = as.Date("2012-01-15"), "mean" = 8.33333333, "max.team" = "TeamC", "max.date" = as.Date("2012-01-07"), "max.val" = 12, "min.team" = "TeamB", "min.date" = as.Date("2012-01-08"),
        "min.val" = 5, "best.avg.team" = "TeamA", "best.avg.team.avg" = 10, "worst.avg.team" = "TeamB", "worst.avg.team.avg" = 7.2, stringsAsFactors = FALSE)

    answer3 <- data.frame("season.end" = as.Date("2012-01-15"), "mean" = 8, "max.team" = "TeamC", "max.date" = as.Date("2012-01-07"), "max.val" = 12, "min.team" = "TeamC", "min.date" = as.Date("2012-01-15"),
                          "min.val" = 6, "best.avg.team" = "TeamA", "best.avg.team.avg" = 10, "worst.avg.team" = "TeamC", "worst.avg.team.avg" = 8, stringsAsFactors = FALSE)

    expect_equal(metaElo(testdf), answer)
    expect_equal(metaElo(testdf, c(as.Date("2012-01-05"), as.Date("2012-01-06"), as.Date("2012-01-07"), as.Date("2012-01-08"), as.Date("2012-01-15"))),
        answer2)
    expect_equal(metaElo(testdf,teams = c("TeamA","TeamC")), answer3)

})

