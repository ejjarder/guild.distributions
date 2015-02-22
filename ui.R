# ui.R
# author: Eugene Jarder
#
# UI logic for the WoW Guild Distributions App. Uses the shinyDashboard package
# for the layout.
#

# chart.box()
# Displays a box that will be used for charts.
#
# parameters:
#   title - The text to display on the box header
#   width - the number of columns to occupy on the dashboard
#
chart.box <- function(title, chart.id) {
    box(
        title = title, solidHeader = T,
        showOutput(chart.id, 'highcharts'),
        collapsible = T, status = 'info', width = 4
    )
}

dashboardPage(
    dashboardHeader(title = 'Guild Distributions'),
    dashboardSidebar(
        selectInput('region', label = 'Region', region.data$region),
        textInput('realmInput', label = 'Realm', value = 'Bonechewer'),
        textInput('guildInput', label = 'Guild Name', value = 'Zichar'),
        sliderInput('levels', label = 'Level range',
                    min = 1, max = 100, value = c(1, 100)),
        actionButton('submitButton', 'Submit')
    ),
    dashboardBody(
        helpText('TLDR: Select region, input realm, input guild,',
                 'select level range, Submit!'),
        h1(textOutput('guildName')),
        fluidRow(
            valueBoxOutput('guildLevels'),
            valueBoxOutput('achievementPoints')
        ),
        fluidRow(
            box(
                title = 'Member List', width = 12, solidHeader = T,
                dataTableOutput('memberList'), collapsible = T, status = 'info'
            )            
        ),
        fluidRow(
            chart.box('Class Distribution', 'classDistribution'),
            chart.box('Race Distribution', 'raceDistribution'),
            chart.box('Gender Distribution', 'genderDistribution')
        ),
        textOutput('debug')
    )
)
