#' Test assorted functions in the Elo Ratings script for performance

test_that("Test Predicting Elo Result",{
    #
    #expect_equal(a,b)
    #expect_match(a,b)
    #expect_is(a,b)
    #expect_true()
    #expect_false()
})

test_that("Test New Elo Ratings", {
    #

})

test_that("Test Date Separator", {
    #
    datelist1<-c(as.Date("2012-01-05"),as.Date("2012-01-05"),as.Date("2012-09-05"),as.Date("2011-11-20"),as.Date("2013-01-05"))
    datelist1<-sort(datelist1)
    expect_equal(splitDates(datelist1), list(c(as.Date("2011-11-20"), as.Date("2012-01-05"), as.Date("2012-01-05")), c(as.Date("2012-09-05"), as.Date("2013-01-05"))))
})
