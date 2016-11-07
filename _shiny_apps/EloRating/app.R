library(shiny)

library(ggplot2)
library(reshape2)
require(shinyjs)
library(plotly)

elo_all <- readRDS("./elo_all.RDS")
elo_all_long <- melt(elo_all$Ratings, id = "Date", value.name = "Rating", variable.name = "Team", na.rm = TRUE)
elo_all_long$Rating<-round(elo_all_long$Rating)
elo_long<-elo_long<-elo_all_long[head(elo_all_long$Rating, -1) == tail(elo_all_long$Rating, -1),]
elo_thin<-elo_long[seq(1, nrow(elo_long), 2), ]
elo_thin2<-elo_long[seq(1, nrow(elo_long), 10), ]


plotlist<-list()
ranges <- reactiveValues(x = NULL, y = NULL)

teams <- sort(c("Montreal Wanderers", "St. Louis Eagles", "Toronto Maple Leafs", "Montreal Canadiens", "Brooklyn Americans", "Boston Bruins", "Montreal Maroons",
    "Philadelphia Quakers", "New York Rangers", "Chicago Blackhawks", "Detroit Red Wings", "Cleveland Barons", "Pittsburgh Penguins", "St. Louis Blues",
    "Los Angeles Kings", "Philadelphia Flyers", "Dallas Stars", "Vancouver Canucks", "Buffalo Sabres", "New York Islanders", "Calgary Flames", "Washington Capitals",
    "New Jersey Devils", "Colorado Avalanche", "Edmonton Oilers", "Arizona Coyotes", "Carolina Hurricanes", "San Jose Sharks", "Tampa Bay Lightning",
    "Ottawa Senators", "Anaheim Ducks", "Florida Panthers", "Nashville Predators", "Winnipeg Jets", "Columbus Blue Jackets", "Minnesota Wild", "Cleveland Crusaders",
    "Birmingham Bulls", "Houston Aeros", "San Diego Mariners", "Michigan Stags/Baltimore Blades", "Minnesota Fighting Saints", "Calgary Cowboys", "Chicago Cougars",
    "Phoenix Roadrunners", "Indianapolis Racers", "Denver Spurs/Ottawa Civics", "Cincinnati Stingers","All Teams"))

teamColour <- list(
    "Anaheim.Ducks" = "#EF5225",
    "Arizona.Coyotes" = "#841F27",
    "Birmingham.Bulls" = "#005FA9",
    "Boston.Bruins" = "#FFC422",
    "Brooklyn.Americans" = "#101077",
    "Buffalo.Sabres" = "#002E62",
    "Calgary.Cowboys" = "#EC1C24",
    "Calgary.Flames" = "#E03A3E",
    "Carolina.Hurricanes" = "#E03A3E",
    "Chicago.Blackhawks" = "#E3263A",
    "Chicago.Cougars" = "#335C3B",
    "Cincinnati.Stingers" = "#FFD700",
    "Cleveland.Barons" = "#9E2532",
    "Cleveland.Crusaders" = "#34007C",
    "Colorado.Avalanche" = "#8B2942",
    "Columbus.Blue.Jackets" = "#00285C",
    "Dallas.Stars" = "#006A4E",
    "Denver.Spurs.Ottawa.Civics" = "#EF6310",
    "Detroit.Red.Wings" = "#EC1F26",
    "Edmonton.Oilers" = "#003777",
    "Florida.Panthers" = "#C8213F",
    "Houston.Aeros" = "#2FABC9",
    "Indianapolis.Racers" = "#101077",
    "Los.Angeles.Kings" = "#000000",
    "Michigan.Stags.Baltimore.Blades" = "#FA343D",
    "Minnesota.Fighting.Saints" = "#FFCE00",
    "Minnesota.Wild" = "#025736",
    "Montreal.Canadiens" = "#BF2F38",
    "Montreal.Maroons" = "#8B2942",
    "Montreal.Wanderers" = "#E81B31",
    "Nashville.Predators" = "#FDBB2F",
    "New.Jersey.Devils" = "#E03A3E",
    "New.York.Islanders" = "#00529B",
    "New.York.Rangers" = "#0161AB",
    "Ottawa.Senators" = "#E4173E",
    "Philadelphia.Flyers" = "#F47940",
    "Philadelphia.Quakers" = "#FFA500",
    "Phoenix.Roadrunners" = "#6495ED",
    "Pittsburgh.Penguins" = "#FFCC33",
    "San.Diego.Mariners" = "#FF0000",
    "San.Jose.Sharks" = "#05535D",
    "St..Louis.Blues" = "#0546A0",
    "St..Louis.Eagles" = "#B20907",
    "Tampa.Bay.Lightning" = "#013E7D",
    "Toronto.Maple.Leafs" = "#003777",
    "Vancouver.Canucks" = "#07346F",
    "Washington.Capitals" = "#CF132B",
    "Winnipeg.Jets" = "#002E62"
    )

# Define UI for application
ui <- shinyUI(
    fluidPage(
        shinyjs::useShinyjs(),
        titlePanel("Elo Ratings"),
        sidebarLayout(
            sidebarPanel(
                selectizeInput("team", "Teams:",
                               teams),
                actionButton("submit", "Submit"),
                actionButton("zIn", "Zoom In"),
                actionButton("zOut", "Zoom Out")
            ),
            mainPanel(
                plotlyOutput('eloPlot', height = "900px")
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {

    output$eloPlot <- renderPlotly({

        if (input$team != "All Teams"){
            teamv<-make.names(input$team)
        }
        else{
            teamv<-"All"
            shinyjs::disable('zIn')
            shinyjs::disable('zOut')
            ranges$x <- NULL
            ranges$y <- NULL
        }

        # generate plot
        p<-ggplot()
        if (teamv != "All"){
            p<-p + geom_line(data=elo_thin2, aes(x=Date, y=Rating), colour="lightgrey", size=0.1)
            p<-p + geom_line(data=elo_long[elo_long$Team == teamv,], aes(x=Date, y=Rating), colour=teamColour[teamv])
        }
        else{
            p<-p + geom_line(data=elo_thin, aes(x=Date, y=Rating, colour=Team), size=0.1)
        }
        p<-p + ggtitle(paste0("Elo Rating of ", input$team, " Through History"))
        p<-p + xlab("Date")
        p<-p + ylab("Elo Rating")
        p<-p + theme_bw() + theme(legend.position="none")
        p<-p + coord_cartesian(xlim = ranges$x, ylim = ranges$y)

        ggplotly(p)

    })

    observeEvent(input$zIn, {
        if (!(input$team == "All Teams")){
            teamv<-make.names(input$team)
            ranges$x<-c((elo_all_long[elo_all_long$Team == teamv, "Date"][1]-365), (elo_all_long[elo_all_long$Team == teamv, "Date"][nrow(elo_all_long[elo_all_long$Team == teamv,])]+365))
        }
        else{
            ranges$x <- NULL
            ranges$y <- NULL
        }
        shinyjs::enable("zOut")
        shinyjs::disable("zIn")
    })

    observeEvent(input$zOut, {
        ranges$x <- NULL
        ranges$y <- NULL
        shinyjs::enable("zIn")
        shinyjs::disable("zOut")
    })

    observeEvent(input$team, {
        ranges$x <- NULL
        ranges$y <- NULL
        shinyjs::enable("zIn")
        shinyjs::disable("zOut")
        shinyjs::enable("teamColour")
    })
})

# Run the application
shinyApp(ui = ui, server = server)

