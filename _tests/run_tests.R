require(testthat)

source("./_rscripts/calculateEloRatings.R")

test_results <- test_dir("./_tests/", reporter="summary")

