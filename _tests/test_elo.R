#' Test assorted functions in the Elo Ratings script for performance
context("Testing Elo Code")

test_that("Test Predicting Elo Result", {
    # Test the Elo Result Predicting Engine
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

test_that("Test Regression To Mean", {
    #Test that the regression to the mean is proper. That dates are properly accepted or applied
    testdf<-data.frame("Date"=c(as.Date("2012-01-05"), as.Date("2012-09-05")), "TeamA"=c(1450, 1500), "TeamB"=c(1500, 1450), "TeamC"=c(1650, 1700))
    answerdf1<-data.frame("Date"=c(as.Date("2012-01-05"), as.Date("2012-09-05"), as.Date("2012-09-06")), "TeamA"=c(1450, 1500, 1500), "TeamB"=c(1500, 1450, 1462.5), "TeamC"=c(1650, 1700, 1650))
    answerdf2<-data.frame("Date"=c(as.Date("2012-01-05"), as.Date("2012-09-05"), as.Date("2012-09-12")), "TeamA"=c(1450, 1500, 1500), "TeamB"=c(1500, 1450, 1475), "TeamC"=c(1650, 1700, 1600))
    answerdf3<-data.frame("Date"=c(as.Date("2012-01-05"), as.Date("2012-09-05"), as.Date("2012-09-06")), "TeamA"=c(1450, 1500, 1487.5), "TeamB"=c(1500, 1450, 1450), "TeamC"=c(1650, 1700, 1637.5))

    expect_equal(.regressToMean(testdf), answerdf1)
    expect_equal(.regressToMean(testdf, rstrength=1, rdate=as.Date("2012-09-12")), answerdf2)
    expect_equal(.regressToMean(testdf, rmean=1450), answerdf3)
})
