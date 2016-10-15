getAndSaveWHAGames <- function(start = 1973, end = 1979, wait = 30) {
    if (!require(XML)) {
        return(FALSE)
    }
    if (start > end) {
        message("Start must be less than end. Reversing values.")
        tmp <- start
        start <- end
        end <- tmp
        rm(tmp)
    }
    if (start < 1973) {
        message("WHA started in 1972-1973. Collecting data from there forwards.")
        start <- 1973
    }
    if (start > 1979) {
        message("WHA ended in 1978-1979. Collecting final season.")
        start <- 1979
        end <- 1979
    }
    if (end > 1979) {
        message("WHA ended in 1978-1979. Collecting up to there.")
        end <- 1979
    }
    for (i in c(start:end)) {
        message(paste0("Working on WHA season: ", i - 1, "-", i), "\r", appendLF = FALSE)
        message(" Downloading...", "\r", appendLF = FALSE)
        tables <- tryCatch({
            readHTMLTable(paste0("http://www.hockey-reference.com/leagues/WHA_", i, "_games.html"))
        }, warning = function(w) {
            message(w)
        }, error = function(e) {
            message(e)
            ## In case of download error, don't pass nothing.
            tables <- NULL
        })
        if (!is.null(tables)) {
            ## In case of download error, don't process
            message(" Processing...", "\r", appendLF = FALSE)
            regular <- tables[["games"]]
            playoff <- tables[["games_playoffs"]]
            message(" Saving...", "\r", appendLF = FALSE)
            write.csv(regular, file = paste0("./wha", i - 1, i, ".csv"))
            write.csv(playoff, file = paste0("./wha", i - 1, i, "Playoffs.csv"))
        }
        message(paste0(" Waiting ", wait, " seconds."))
        Sys.sleep(wait)
    }
}

getAndSaveNHLGames <- function(start = 1918, end = 2017, wait = 30) {
    if (!require(XML)) {
        return(FALSE)
    }
    if (start > end) {
        message("Start must be less than end. Reversing values.")
        tmp <- start
        start <- end
        end <- tmp
        rm(tmp)
    }
    if (start < 1918) {
        message("NHL started in 1917-1918. Collecting data from there forwards.")
        start <- 1918
    }
    if (start > 2017) {
        message("Can's start collecting data past next season. Collecting final season.")
        start <- 2017
        end <- 2017
    }
    if (end > 2017) {
        message("Can't collect past this season. Collecting up to there.")
        end <- 2017
    }
    if (start == 2005 && end == 2005) {
        message("Can't collect 2004-2005. No season due to lockout. Not collecting any data.")
        return(FALSE)
    }
    if (start == 2005) {
        message("Can't collect 2004-2005. No season due to lockout. Collecting from 2005-2006 to there.")
        start <- 2006
    }
    if (end == 2005) {
        message("Can't collect 2004-2005. No season due to lockout. Collecting to 2003-2004 to there.")
        end <- 2004
    }
    
    for (i in c(start:end)) {
        # No season in 2004-2005. Don't try process that year.
        if (i == 2005) {
            message("Skipping 2004-2005 season (lockout)")
            next
        }
        message(paste0("Working on NHL season: ", i - 1, "-", i), "\r", appendLF = FALSE)
        message(" Downloading...", "\r", appendLF = FALSE)
        tables <- tryCatch({
            readHTMLTable(paste0("http://www.hockey-reference.com/leagues/NHL_", i, "_games.html"))
        }, warning = function(w) {
            message(w)
        }, error = function(e) {
            message(e)
            ## In case of download error, don't pass nothing.
            tables <- NULL
        })
        if (!is.null(tables)) {
            ## In case of download error, don't process
            message(" Processing...", "\r", appendLF = FALSE)
            regular <- tables[["games"]]
            playoff <- tables[["games_playoffs"]]
            message(" Saving...", "\r", appendLF = FALSE)
            write.csv(regular, file = paste0("./", i - 1, i, ".csv"))
            if (!is.null(playoff)) {
                write.csv(playoff, file = paste0("./", i - 1, i, "Playoffs.csv"))
            }
        }
        message(paste0("Waiting ", wait, " seconds."))
        Sys.sleep(wait)
    }
    return(TRUE)
}
