otTagger<-function(row) {
    if (row['OT.SO'] != '') {
        if(row['AG']>row['HG']) {
            return("A")
        }
        else{
            return("H")
        }
    }
    else {
        return("")
    }
}

otFixer<-function(row) {
    H<-as.integer(row['HG'])
    A<-as.integer(row['AG'])
    if(row['OT.Win']=='H') {
        H<-H-1
    }
    else if (row['OT.Win']=='A') {
        A<-A-1
    }
    return(data.frame('HG'=H,'AG'=A))
}

nhlDataPrep<-function(df) {
    colnames(df)<-c("Date", "AwayTeam","AG","HomeTeam","HG","OT.SO")
    df<-df[!is.na(df$AG),]
    df$OT.Win<-apply(df,1,otTagger)
    scores<-do.call("rbind",apply(df,1,otFixer))
    df$AG<-scores$AG
    df$HG<-scores$HG
    df$Date<-as.Date(df$Date)
    return(df)
}

clean_teams<-function(df){
    df[df$Home == "Phoenix Coyotes",]$Home <- "Arizona Coyotes"
    df[df$Visitor == "Phoenix Coyotes",]$Visitor <- "Arizona Coyotes"
    df[df$Home == "Mighty Ducks of Anaheim",]$Home <- "Anaheim Ducks"
    df[df$Visitor == "Mighty Ducks of Anaheim",]$Visitor <- "Anaheim Ducks"
    df[df$Home == "Atlanta Thrashers",]$Home <- "Winnipeg Jets"
    df[df$Visitor == "Atlanta Thrashers",]$Visitor <- "Winnipeg Jets"
}


getAndPrepAllData<-function(year_list=c(2006, 2007, 2008, 2009,2010,2011,2012,2013,2014,2015,2016)){
    df<-data.frame(Date=NULL, Visitor=NULL, G=NULL, Home=NULL, G.1=NULL, X.1=NULL)
    for (year in 1:length(year_list)){
        df<-rbind(df, read.csv(paste('./_data/',year_list[year]-1,year_list[year],".csv", sep=''))[2:7])
    }
    try(clean_teams(df), silent=TRUE)
    df<-droplevels(df)
    df<-nhlDataPrep(df)
    return(df)
}
