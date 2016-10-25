library(shiny)

library(ggplot2)
library(reshape2)
require(shinyjs)

elo_all <- readRDS("./elo_all.RDS")
elo_all_long <- melt(elo_all$Ratings, id = "Date", value.name = "Rating", variable.name = "Team", na.rm = TRUE)

plotlist<-list()
ranges <- reactiveValues(x = NULL, y = NULL)

generate_plots<-function(input, dates){
    if (!(input$team == "All Teams")){
        teamv<-make.names(input$team)
        shinyjs::enable('zIn')
        shinyjs::disable('zOut')
    }
    else{
        teamv<-"All"
        shinyjs::disable('zIn')
        shinyjs::disable('zOut')
        ranges$x <- NULL
        ranges$y <- NULL
    }

    hash<-paste0(teamv,ranges$x[1])

    if (!(hash %in% names(plotlist))){
        p<-ggplot()
        if (teamv != "All"){
            p<-p + geom_line(data=elo_all_long, aes(x=Date, y=Rating), colour="lightgrey", size=0.1)
            p<-p + geom_line(data=elo_all_long[elo_all_long$Team == teamv,], aes(x=Date, y=Rating), colour="red")
        }
        else{
            p<-p + geom_line(data=elo_all_long, aes(x=Date, y=Rating, colour=Team), size=0.1)
        }
        p<-p + ggtitle(paste0("Elo Rating of ", input$team, " Through History"))
        p<-p + xlab("Date")
        p<-p + ylab("Elo Rating")
        p<-p + theme_bw() + theme(legend.position="none")
        p<-p + coord_cartesian(xlim = ranges$x, ylim = ranges$y)
        plotlist[[hash]]<-p

    }
    plotlist[[hash]]
}

teams <- sort(c("Montreal Wanderers", "St. Louis Eagles", "Toronto Maple Leafs", "Montreal Canadiens", "Brooklyn Americans", "Boston Bruins", "Montreal Maroons",
    "Philadelphia Quakers", "New York Rangers", "Chicago Blackhawks", "Detroit Red Wings", "Cleveland Barons", "Pittsburgh Penguins", "St. Louis Blues",
    "Los Angeles Kings", "Philadelphia Flyers", "Dallas Stars", "Vancouver Canucks", "Buffalo Sabres", "New York Islanders", "Calgary Flames", "Washington Capitals",
    "New Jersey Devils", "Colorado Avalanche", "Edmonton Oilers", "Arizona Coyotes", "Carolina Hurricanes", "San Jose Sharks", "Tampa Bay Lightning",
    "Ottawa Senators", "Anaheim Ducks", "Florida Panthers", "Nashville Predators", "Winnipeg Jets", "Columbus Blue Jackets", "Minnesota Wild", "Cleveland Crusaders",
    "Birmingham Bulls", "Houston Aeros", "San Diego Mariners", "Michigan Stags/Baltimore Blades", "Minnesota Fighting Saints", "Calgary Cowboys", "Chicago Cougars",
    "Phoenix Roadrunners", "Indianapolis Racers", "Denver Spurs/Ottawa Civics", "Cincinnati Stingers","All Teams"))

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
                plotOutput("eloPlot")
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {

    output$eloPlot <- renderPlot({
        # generate plot, if needed
        generate_plots(input, ranges)
    })

    observeEvent(input$zIn, {
        shinyjs::disable("zIn")
        shinyjs::enable("zOut")
        if (!(input$team == "All Teams")){
            teamv<-make.names(input$team)
            ranges$x<-c((elo_all_long[elo_all_long$Team == teamv, "Date"][1]-365), (elo_all_long[elo_all_long$Team == teamv, "Date"][nrow(elo_all_long[elo_all_long$Team == teamv,])]+365))
        }
        else{
            ranges$x <- NULL
            ranges$y <- NULL
        }
        #generate_plots(input, ranges)
    })

    observeEvent(input$zOut, {
        shinyjs::enable("zIn")
        shinyjs::disable("zOut")
        ranges$x <- NULL
        ranges$y <- NULL
        generate_plots(input, ranges)
    })

    observeEvent(input$submit, {})

    observeEvent(input$team, {
        ranges$x <- NULL
        ranges$y <- NULL
    })
})

# Run the application
shinyApp(ui = ui, server = server)

