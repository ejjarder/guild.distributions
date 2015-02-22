# global.R
# author: Eugene Jarder
#
# Dependencies and variables used for both server and UI
#

library('shinydashboard') # The application layout
library('rCharts')        # Using Highcharts to visualize data

# The different WoW regions and their corresponding hosts
#
region.data <- data.frame(region = c('US', 'EU', 'KR', 'TW', 'CN'),
                          host = c('us.battle.net', 'eu.battle.net',
                                   'kr.battle.net', 'tw.battle.net',
                                   'www.battlenet.com.cn'),
                          stringsAsFactors = F)
