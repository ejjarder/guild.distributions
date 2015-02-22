# WoW Guild Distributions
Code for the [WoW Guild Distributions Shiny App]
(https://ejjarder.shinyapps.io/GuildDistributions/).

The usage guide can be found [here](http://ejjarder.github.io/guild.distributions).

## Dependencies

The WoW Guild Distributions App uses the following packages:

* RCurl - To get data from the
[WoW API](http://blizzard.github.io/api-wow-docs/)
* rjson - To convert the json data from the API to an R list object
* plyr - To summarize the data so they can be shown in the histograms
* shinydashboard - The layout of the shiny app
* Highcharts from rCharts - The chart framework used