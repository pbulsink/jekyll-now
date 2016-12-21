library(RCurl)
library(XML)
library(stringr)

#Remove comments hiding tables:
#url<-'http://www.hockey-reference.com/players/a/abdelju01.html'
#htmlpage<-getURL(url)
#htmlpage<-gsub(htmlpage, pattern = '<!--', replacement = '')
#htmlpage<-gsub(htmlpage, pattern = '-->', replacement = '')

#Read in Tables
#tables<-readHTMLTable(htmlpage)

#Get List of all Players
#url<-'http://www.hockey-reference.com/players/a/'
#raw_player_list<-getURL(url)

pattern<-'<p class="([a-z\\_]+)"><a href="(\\/players\\/[a-z]+\\/[a-zA-Z0-9]+\\.html)">([a-zA-Z ]+)<\\/a>'
#player_list<-str_match_all(raw_player_list, pattern)

get_player_list<-function(){
    player_list<-data.frame(Complete=character(), URL=character(), Name=character())
    for(letter in letters){
        message(letter)
        url<-paste0('http://www.hockey-reference.com/players/', letter, '/')
        raw_player_list<-getURL(url)
        pl<-str_match_all(raw_player_list, pattern)
        pl<-as.data.frame(pl[1])
        colnames(pl)<-c('Complete', 'URL', 'Name')
        player_list<-rbind(player_list, pl)
        Sys.sleep(5)
    }
    player_list
}