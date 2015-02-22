# server.R
# author: Eugene Jarder
#
# Server logic for the WoW Guild Distributions App. The charts use Highcharts
# through rCharts
#

library('RCurl')    # To grab data from the WoW API
library('rjson')    # To convert the WoW API json data
library('plyr')     # Summarize the data for the histogram

# Template for the API URL for the list of members in the guild. Parameters are
# the hostname, realm, and guild name, in that order.
#
guild.url.template <- 'http://%s/api/wow/guild/%s/%s?fields=members'

# URL for the different races available in game
#
races.url <- 'http://us.battle.net/api/wow/data/character/races'

# The point format to use for the charts
#
point.format <- '<span style="color:{point.color}">\u25CF</span> {point.y}'

# The races are unlikely to change, so we grab them upon launching the app.
#
races.json <- fromJSON(getURLContent(races.url))
races <- character()

for (race in races.json$races) { 
    races[as.numeric(race$id)] <- race$name
}

# The relevant data to display from the member list
#
relevant.data <- c('name', 'class', 'level', 'race', 'gender')

# The different game classes and their corresponding colors
#
class.info <- data.frame(
    class.name = c('Warrior', 'Paladin', 'Hunter', 'Rogue', 'Priest',
                   'Death Knight', 'Shaman', 'Mage', 'Warlock', 'Monk',
                   'Druid'),
    color = c('#C79C6E', '#F58CBA', '#ABD473', '#FFF569', '#FFFFFF', '#C41F3B',
              '#0070DE', '#69CCF0', '#9482C9', '#00FF96', '#FF7D0A'),
    stringsAsFactors = F
)

# convert.data()
# Converts the given relevant data data based on the data type
#
# parameters:
#   data.type - The data type
#   data - the actual data
#
convert.data <- function(data.type, data) {
    switch(data.type,
           'class' = class.info$class.name[as.numeric(data)],
           'race' = races[as.numeric(data)],
           'gender' = if (data == 0) 'Male' else 'Female',
           data)
}

# get.character.details()
# Get the details of one character in the member list.
#
# parameters:
#   member.data - list object based on the member data json
#
get.character.details <- function(member.data) {
    character.data <- member.data$character
    rank <- if (as.numeric(member.data$rank) == 0) {
        'Guild Master'
    } else {
        paste('Rank', member.data$rank)
    }
    c(sapply(relevant.data, function(x) { 
        convert.data(x, character.data[[x]]) }), rank)
}

# get.member.data()
# Convert the member list taken from json to a data frame.
#
# parameters:
#   member.list - list object based on the member list json
#
get.member.data <- function(member.list) {
    member.data <- sapply(member.list, get.character.details)
    member.data <- data.frame(matrix(member.data, ncol = 6, byrow = T),
                              stringsAsFactors = F)
    names(member.data) <- c(relevant.data, 'guild rank')
    member.data
}

# convert.with.color()
# Convert a row in the class.counts data frame to a list obect for the chart
# series. To be used with sapply.
#
# parameters:
#   class.count - one row in the class.counts data frame.
#
convert.with.color <- function(class.count) {
    list(y = as.numeric(class.count['count']),
         color = class.info$color[class.info$class == class.count['class']]) 
}

# get.histogram()
# Generate a histogram with common settings
#
get.histogram <- function() {
    histogram <- rCharts::Highcharts$new()
    histogram$chart(type = 'column', backgroundColor = '#F9F9F9')
    histogram$plotOptions(
        column = list(groupPadding = 0, pointPadding = 0,
                      borderColor = '#C9C9C9')
    )
    histogram$legend(enabled = F)
    histogram$tooltip(pointFormat = point.format)
    histogram$params$width = 'auto-scale'
    histogram
}

# clean.url()
# Clean up the spaces in the URL and replace them with %20
#
# parameters:
#   url - The url to clean up.
#
clean.url <- function(url) {
    gsub(' ', '%20', url)
}

shinyServer(function(input, output) {
    # the host based on the selected region
    #
    host <- reactive({
        input$submitButton
        isolate(region.data$host[region.data$region == input$region])
    })
    
    # the guild list object based from the json
    #
    guild.data <- reactive({
        input$submitButton
        
        guild.url <- isolate(
            sprintf(guild.url.template, host(),
                    input$realmInput, input$guildInput))
        
        guild.json <- getURLContent(clean.url(guild.url))
        
        fromJSON(guild.json)
    })
    
    # the level range of players to display
    #
    levels <- reactive({
        input$submitButton
        
        isolate(input$levels)
    })

    # the members data frame based on the members list object
    #
    member.data <- reactive({
        member.df <- get.member.data(guild.data()$members)
        member.levels <- as.numeric(member.df$level)
        member.df[member.levels >= levels()[1]
                  & member.levels <= levels()[2],]
    })

    # Display the guild name
    #
    output$guildName <- renderText({
        guild.data()$name
    })
    
    # Display debug data. Should not contain anything right now.
    #
    output$debug <- renderText({
    })
    
    # Render the value box with the guild level
    #
    output$guildLevels <- renderValueBox({
        color <- if(guild.data()$side == 0) 'blue' else 'red'
        valueBox(guild.data()$level, 'Guild Level', icon = icon('users'),
                 color = color)
    })
    
    # Render the value box with the achievement points
    #
    output$achievementPoints <- renderValueBox({
        valueBox(guild.data()$achievementPoints, 'Achievement Points',
                 icon = icon('shield'), color = 'yellow')
    })

    # Show the member list data table
    #
    output$memberList <- renderDataTable({
        member.data()
    }, options = list(pageLength = 5, lengthChange = F, scrollY = 200))
    
    # Class distribution histogram
    #
    output$classDistribution <- renderChart2({
        class.counts <-
            ddply(member.data(), 'class', summarise, count = length(class))
        counts.with.color <- apply(class.counts, 1, convert.with.color)
        class.distribution <- get.histogram()
        class.distribution$series(data = counts.with.color)
        class.distribution$xAxis(categories = class.counts$class)
        class.distribution
    })
    
    # Race distribution histogram
    #
    output$raceDistribution <- renderChart2({
        race.counts <-
            ddply(member.data(), 'race', summarise, count = length(race))
        race.distribution <- get.histogram()
        race.distribution$series(data = race.counts$count)
        race.distribution$xAxis(categories = race.counts$race)
        race.distribution
    })
    
    # Gender distribution pie chart
    #
    output$genderDistribution <- renderChart2({
        gender.counts <-
            ddply(member.data(), 'gender', summarise, count = length(gender))
        gender.distribution <- hPlot(x = 'gender', y = 'count',
                                     data = gender.counts, type = 'pie')
        gender.distribution$legend(enabled = F)
        gender.distribution$tooltip(pointFormat = point.format)
        gender.distribution$params$width = 'auto-scale'
        gender.distribution
    })
})
