library('RCurl')
library('rjson')
library('devtools')
library('plyr')

guild.url.template <-
    'http://%s/api/wow/guild/%s/%s?fields=members'
races.url <- 'http://us.battle.net/api/wow/data/character/races'

point.format <- '<span style="color:{point.color}">\u25CF</span> {point.y}'

races.json <- fromJSON(getURLContent(races.url))
races <- character()

for (race in races.json$races) { 
    races[as.numeric(race$id)] <- race$name
}

relevant.data <- c('name', 'class', 'level', 'race', 'gender')

class.info <- data.frame(
    class.name = c('Warrior', 'Paladin', 'Hunter', 'Rogue', 'Priest',
                   'Death Knight', 'Shaman', 'Mage', 'Warlock', 'Monk',
                   'Druid'),
    color = c('#C79C6E', '#F58CBA', '#ABD473', '#FFF569', '#FFFFFF', '#C41F3B',
              '#0070DE', '#69CCF0', '#9482C9', '#00FF96', '#FF7D0A'),
    stringsAsFactors = F
)

convert.data <- function(data.type, data) {
    switch(data.type,
           'class' = class.info$class.name[as.numeric(data)],
           'race' = races[as.numeric(data)],
           'gender' = if (data == 0) 'Male' else 'Female',
           data)
}

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

get.member.data <- function(member.list) {
    member.data <- sapply(member.list, get.character.details)
    member.data <- data.frame(matrix(member.data, ncol = 6, byrow = T),
                              stringsAsFactors = F)
    names(member.data) <- c(relevant.data, 'guild rank')
    member.data
}

convert.with.color <- function(class.count, member.list) {
    list(y = as.numeric(class.count['count']),
         color = class.info$color[class.info$class == class.count['class']],
         characters = member.list[member.list$class == class.count['class'],]) 
}

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

clean.url <- function(url) {
    gsub(' ', '%20', url)
}

shinyServer(function(input, output) {
    host <- reactive({
        region.data$host[region.data$region == input$region]
    })
    
    guild.data <- reactive({
        input$submitButton
        
        guild.url <- isolate(
            sprintf(guild.url.template, host(),
                    input$realmInput, input$guildInput))
        
        guild.json <- getURLContent(clean.url(guild.url))
        
        fromJSON(guild.json)
    })
    
    levels <- reactive({
        input$submitButton
        
        isolate(input$levels)
    })

    member.data <- reactive({
        member.df <- get.member.data(guild.data()$members)
        member.levels <- as.numeric(member.df$level)
        member.df[member.levels >= levels()[1]
                  & member.levels <= levels()[2],]
    })

    output$guildName <- renderText({
        guild.data()$name
    })
    
    output$debug <- renderText({
    })
    
    output$guildLevels <- renderValueBox({
        color <- if(guild.data()$side == 0) 'blue' else 'red'
        valueBox(guild.data()$level, 'Guild Level', icon = icon('users'),
                 color = color)
    })
    
    output$achievementPoints <- renderValueBox({
        valueBox(guild.data()$achievementPoints, 'Achievement Points',
                 icon = icon('shield'), color = 'yellow')
    })

    output$memberList <- renderDataTable({
        member.data()
    }, options = list(pageLength = 5, lengthChange = F, scrollY = 200))
    
    output$classDistribution <- renderChart2({
        class.counts <-
            ddply(member.data(), 'class', summarise, count = length(class))
        counts.with.color <- apply(class.counts, 1, convert.with.color,
                                   member.data())
        class.distribution <- get.histogram()
        class.distribution$series(data = counts.with.color)
        class.distribution$xAxis(categories = class.counts$class)
        class.distribution
    })
    
    output$raceDistribution <- renderChart2({
        race.counts <-
            ddply(member.data(), 'race', summarise, count = length(race))
        race.distribution <- get.histogram()
        race.distribution$series(data = race.counts$count)
        race.distribution$xAxis(categories = race.counts$race)
        race.distribution
    })
    
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
