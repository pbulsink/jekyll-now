require(RCurl)
require(XML)
require(stringr)
require(plyr)

getPlayerList<-function(sleep=30){
    pattern<-'<p class="([a-z\\_]+)">(?:<strong>)*<a href="(\\/players\\/[a-z]+\\/[a-zA-Z0-9]+\\.html)">([a-zA-Z ]+)<\\/a>(?:<\\/strong>)*\\s*\\(([0-9-]+)*'
    player_list<-data.frame(Complete=character(), BlnNHL=character(), URL=character(), Name=character())
    for(letter in letters){
        message(letter)
        url<-paste0('http://www.hockey-reference.com/players/', letter, '/')
        raw_player_list<-getURL(url)
        pl<-str_match_all(raw_player_list, pattern)
        pl<-as.data.frame(pl[1], stringsAsFactors = FALSE)
        colnames(pl)<-c('Complete', 'BlnNHL', 'URL', 'Name', 'Active')
        player_list<-rbind(player_list, pl)
        Sys.sleep(sleep)
    }
    player_list[player_list$BlnNHL == 'nhl', 'BlnNHL']<-TRUE
    player_list[player_list$BlnNHL == 'non_nhl', 'BlnNHL']<-FALSE
    player_list$BlnNHL <- as.factor(player_list$BlnNHL)
    return(player_list)
}

getPlayerTables<-function(url){
    agents <- c("Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.99 Safari/537.36",
                "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.99 Safari/537.36",
                "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.99 Safari/537.36",
                "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.98 Safari/537.36",
                "Mozilla/5.0 (Windows NT 10.0; WOW64; rv:50.0) Gecko/20100101 Firefox/50.0",
                "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_1) AppleWebKit/602.2.14 (KHTML, like Gecko) Version/10.0.1 Safari/602.2.14",
                "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:50.0) Gecko/20100101 Firefox/50.0",
                "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.98 Safari/537.36")
    htmlpage <- try(getURL(url, header = FALSE,
                           .opts = curlOptions(
                               referer = 'hockey-reference.com',
                               header = TRUE,
                               followLocation = TRUE,
                               useragent = agents[sample(1:8, 1)]))
    )

    if(class(htmlpage) == "try-error") {
        message(paste0('HTML Try Error on: ', url))
        htmlpage <- getURL(url, header = FALSE,
                           .opts = curlOptions(
                               referer = 'hockey-reference.com',
                               header = TRUE,
                               followLocation = TRUE,
                               useragent = agents[sample(1:8, 1)]))
    }
    htmlpage<-gsub(htmlpage, pattern = '<!--', replacement = '')
    htmlpage<-gsub(htmlpage, pattern = '-->', replacement = '')

    #Read in Tables
    tables<-readHTMLTable(htmlpage)

    m1<-'<p><strong>Position<\\/strong>:\\s*([A-Z\\/]+)\\&'
    meta_pos<-str_match(htmlpage, m1)[,2]
    names(meta_pos)<-"Position"

    m1b<-'<strong>(?:Shoots|Catches)<\\/strong>:\\s*([A-Za-z\\/]+)\\s*'
    meta_hand<-str_match(htmlpage, m1b)[,2]
    names(meta_hand)<-'Handed'

    m2<-'<p><span itemprop="height">([0-9-]+)<\\/span>.+itemprop="weight">([0-9]+)lb.+\\(([0-9]+)cm,.+;([0-9]+)kg\\).<\\/p>'
    meta_h_w<-str_match(htmlpage, m2)[,c(2:5)]
    names(meta_h_w)<-c("HeightImp", "WeightImp", "HeightMetric", "WeightMetric")

    m3<-'data-birth="([0-9-]*)"+>.+"birthPlace">\\s*in\\&nbsp;([A-Za-z]*),.+country=([A-Za-z]*).+province=([A-Za-z]*).+state=([A-Za-z]*)"'
    meta_birth<-str_match(htmlpage, m3)[2:6]
    names(meta_birth)<-c("Birthdate", "BirthPlace", "Country", "Province", "State")

    m4<-'data-death="([0-9-]*)"'
    meta_death<-str_match(htmlpage, m4)[[1]][2]
    names(meta_death)<-c("Deathdate")

    m5<-'draft.html">([A-Za-z]+)<\\/a>,\\s*([0-9A-Za-z]+)\\s*round\\s*\\(([0-9]+)[a-z]{2}\\&nbsp;overall\\), <[a-zA-Z\\s\\/="+_0-9]+\\.html">([0-9]{4})'
    meta_draft<-str_match_all(htmlpage, m5)[[1]][,c(2:5)]
    meta_draft<-unlist(meta_draft)
    if (length(meta_draft) == 0)
        meta_draft<-c(rep(NA, 4))
    meta_draft<-tail(meta_draft, 4)
    names(meta_draft)<-c("DraftTeam", "DraftRound", "DraftOverall", "DraftYear")

    metas<-unlist(list(meta_pos, meta_hand, meta_h_w, meta_birth, meta_death, meta_draft))

    return(list(tables, metas))
}


flattenTables<-function(tables){
    stats_nhl<-data.frame()
    if('stats_basic_plus_nhl' %in% names(tables)){
        stats_nhl<-tables$stats_basic_plus_nhl
        stats_nhl$Playoffs=FALSE
        #Sometimes there's "" named columns. Messes up rbind.fill
        if("" %in% colnames(stats_nhl))
            stats_nhl<-stats_nhl[,-which(names(stats_nhl) %in% "")]
        colnames(stats_nhl)[colnames(stats_nhl)=="Tm"] <- "Team"
    }
    else if('stats_basic_nhl' %in% names(tables)){
        stats_nhl<-tables$stats_basic_nhl
        stats_nhl$Playoffs=FALSE
        #Sometimes there's "" named columns. Messes up rbind.fill
        if("" %in% colnames(stats_nhl))
            stats_nhl<-stats_nhl[,-which(names(stats_nhl) %in% "")]
        colnames(stats_nhl)[colnames(stats_nhl)=="Tm"] <- "Team"
    }

    if('skaters_advanced' %in% names(tables)){
        merge(stats_nhl, tables$skaters_advanced, by=c("Season", "Team"), all=TRUE)
        #stats_nhl<-cbind(stats_nhl, tables$skaters_advanced)
        if("Age.y" %in% colnames(stats_nhl))
            stats_nhl<-stats_nhl[,-which(names(stats_nhl) %in% "")]
        if("Lg.y" %in% colnames(stats_nhl))
            stats_nhl<-stats_nhl[,-which(names(stats_nhl) %in% "")]
        # stats_nhl<-stats_nhl[,-which(names(stats_nhl) %in% "Age.y", "Lg.y")]

        if("" %in% colnames(stats_nhl))
            stats_nhl<-stats_nhl[,-which(names(stats_nhl) %in% "")]
        colnames(stats_nhl)[colnames(stats_nhl)=="Age.x"] <- "Age"
        colnames(stats_nhl)[colnames(stats_nhl)=="Lg.x"] <- "Lg"
        colnames(stats_nhl)[colnames(stats_nhl)=="GP.x"] <- "GP"
        colnames(stats_nhl)[colnames(stats_nhl)=="TOI.x"] <- "TOI"
    }

    playoffs_nhl<-data.frame()
    if('stats_playoffs_nhl' %in% names(tables)){
        playoffs_nhl<-tables$stats_playoffs_nhl
        playoffs_nhl$Playoffs=TRUE
        #Sometimes there's "" named columns. Messes up rbind.fill
        if("" %in% colnames(playoffs_nhl))
            playoffs_nhl<-playoffs_nhl[,-which(names(playoffs_nhl) %in% "")]
    }

    stats_other<-data.frame()
    if('stats_basic_other' %in% names(tables)){
        stats_other<-tables$stats_basic_other
        stats_other$Playoffs=FALSE
        #Sometimes there's "" named columns. Messes up rbind.fill
        if("" %in% colnames(stats_other))
            stats_other<-stats_other[,-which(names(stats_other) %in% "")]
    }

    playoffs_other<-data.frame()
    if('stats_playoffs_other' %in% names(tables)){
        playoffs_other<-tables$stats_playoffs_other
        playoffs_other$Playoffs=TRUE
        #Sometimes there's "" named columns. Messes up rbind.fill
        if("" %in% colnames(playoffs_other))
            playoffs_other<-playoffs_other[,-which(names(playoffs_other) %in% c(""))]
    }

    stats<-rbind.fill(stats_nhl, playoffs_nhl, stats_other, playoffs_other)

    return(stats)
}

getPlayerStats<-function(player_list, sleep=30){
    player_stats_tables<-data.frame()
    goalie_stats_tables<-data.frame()
    player_meta_tables<-data.frame()
    plist<-player_list[player_list$BlnNHL == TRUE, ]
    pb<-txtProgressBar(min = 0, max = nrow(plist), initial = 0)
    for(player in c(1:nrow(plist))){
        #prep HTML
        url<-paste0('http://www.hockey-reference.com', plist[player, 'URL'])

        pname<-plist[player, 'Name']
        if('02.html' %in% url){
            pname<-paste(pname, "02")
        }
        else if('03.html' %in% url){
            pname<-paste(pname, "03")
        }

        scrape<-getPlayerTables(url)
        #Add to record

        tables<-flattenTables(scrape[[1]])
        tables$Name<-pname

        if("G" %in% scrape[[2]]['Position']){
            goalie_stats_tables<-rbind.fill(goalie_stats_tables, tables)
        }
        else{
            player_stats_tables<-rbind.fill(player_stats_tables, tables)
        }
        player_meta_tables<-rbind.fill(player_meta_tables, data.frame("Name"=pname, "Active"=plist[player, 'Active'], t(unlist(scrape[[2]]))))

        setTxtProgressBar(pb,player)
        Sys.sleep(sleep)
    }
    return(list("PlayerStats"=player_stats_tables, "GoalieStats"=goalie_stats_tables, "PlayerMeta"=player_meta_tables))
}

#'scrapeByAlphabet
#'A function to scrape and save player tables by last name, breaking up the scraping
#'into each chunk to prevent progress loss by scraping error (HTML error)
#'@param player_list a player list of the type created by getPlayerNames
#'@param letters_to_scrape the letters of last names to scrape (default all letters)
#'@param letter_sleep The length of time to sleep between letters
#'@param ... Additional params for getPlayerStats
#'
#'@return True, if successful
scrapeByAlphabet<-function(player_list, letters_to_scrape=letters, letter_sleep=120, ...){
    for(letter in letters_to_scrape){
        message(paste0("Getting Players with last name of ", toupper(letter), "."))
        ps<-getPlayerStats(player_list[startsWith(player_list$URL, paste0('/players/', letter)),], ...)
        saveRDS(ps, paste0("./_data/players/players_",letter,".RDS"))
        Sys.sleep(letter_sleep)
    }
    return(TRUE)
}
