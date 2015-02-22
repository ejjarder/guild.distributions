# install dependencies
#
library('shinydashboard')
library('rCharts')

region.data <- data.frame(region = c('US', 'EU', 'KR', 'TW', 'CN'),
                          host = c('us.battle.net', 'eu.battle.net',
                                   'kr.battle.net', 'tw.battle.net',
                                   'www.battlenet.com.cn'),
                          stringsAsFactors = F)
