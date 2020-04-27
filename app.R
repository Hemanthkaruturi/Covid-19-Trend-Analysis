library(shiny)
library(shinydashboard)
library(plotly)
library(tseries)
library(forecast)
library(DT)
library(reshape2)
library(data.table)
library(dplyr)

#################################### Preparing Data ###################################
library(rjson)
result <- fromJSON(file = "https://api.covid19india.org/data.json")
district_result <- fromJSON(file = "https://api.covid19india.org/state_district_wise.json")
d_state <- fromJSON(file = "https://api.covid19india.org/states_daily.json")

## Prepare district level data
state <- c()
district <- c()
dis_confirmed <- c()
dis_active <- c()
dis_deceased <- c()
dis_recovered <- c()

a <- 0
for (i in seq(length(district_result))){
  for (j in seq(length(district_result[[i]]$districtData))){
    for (k in seq(length(district_result[[i]]$districtData))) {
      a = a+1
      state[a] <- names(district_result)[i]
      district[a] <- names(district_result[[i]]$districtData)[k]
      dis_confirmed[a] <- district_result[[i]]$districtData[[k]]$confirmed
      dis_active[a] <- district_result[[i]]$districtData[[k]]$active
      dis_deceased[a] <- district_result[[i]]$districtData[[k]]$deceased
      dis_recovered[a] <- district_result[[i]]$districtData[[k]]$recovered
      
    }
    
  }
  
}

district_data <- data.frame(state,district,dis_confirmed,dis_active,dis_deceased,dis_recovered)

# convert data
district_data$state <- as.character(district_data$state)
district_data$district <- as.character(district_data$district)
district_data$dis_confirmed <- as.numeric(district_data$dis_confirmed)
district_data$dis_active <- as.numeric(district_data$dis_active)
district_data$dis_deceased <- as.numeric(district_data$dis_deceased)
district_data$dis_recovered <- as.numeric(district_data$dis_recovered)

district_data <- district_data[!duplicated(district_data), ]

row.names(district_data) <- seq(1,length(district_data$state))

district_data[length(district_data$district)+1,"state"] <- "Total"
district_data[length(district_data$district),"district"] <- "Total"
district_data[length(district_data$district),"dis_confirmed"] <- sum(district_data$dis_confirmed,na.rm = T)
district_data[length(district_data$district),"dis_active"] <- sum(district_data$dis_active,na.rm = T)
district_data[length(district_data$district),"dis_deceased"] <- sum(district_data$dis_deceased,na.rm = T)
district_data[length(district_data$district),"dis_recovered"] <- sum(district_data$dis_recovered,na.rm = T)

names(district_data) <- c("STATE","DISTRICT","CONFIRMED","ACTIVE","DECEASED","RECOVERED")


# Get total samples tested
total_samples_tested <- c()

for (i in seq(1,length(result$tested))){
  total_samples_tested[i] <-  result$tested[[i]]$totalsamplestested
}

# Remove null values from the tests data
total_tested <- c()
j = 0
for (i in seq(length(total_samples_tested))){
  if (nchar(total_samples_tested[i]) > 1){
    j = j+1
    total_tested[j] = total_samples_tested[i]
  }
}

# prepare overall data
confirmed <- c()
deaths <- c()
recovered <- c()
date <- c()
total_confirmed <- c()
total_deaths <- c()
total_recovered <- c()

# get the cases detais
for (i in seq(1,length(result$cases_time_series))){
  confirmed[i] <- result$cases_time_series[[i]]$dailyconfirmed
  deaths[i] <- result$cases_time_series[[i]]$dailydeceased
  recovered[i] <- result$cases_time_series[[i]]$dailyrecovered
  date[i] <- result$cases_time_series[[i]]$date
  total_confirmed[i] <- result$cases_time_series[[i]]$totalconfirmed
  total_deaths[i] <- result$cases_time_series[[i]]$totaldeceased
  total_recovered[i] <- result$cases_time_series[[i]]$totalrecovered
  
  
}

covid_data <- data.frame(date,confirmed,deaths,recovered,total_confirmed,total_deaths,total_recovered)

covid_data['year'] <- "2020"

covid_data['date'] <- paste(covid_data$date,covid_data$year)

# convert date format
covid_data['date'] <- as.Date(covid_data$date, format = "%d %B %y")

# convert other variables to numeric
covid_data$confirmed <- as.character(covid_data$confirmed)
covid_data$confirmed <- as.numeric(covid_data$confirmed)
covid_data$deaths <- as.numeric(as.character(covid_data$deaths))
covid_data$recovered <- as.numeric(as.character(covid_data$recovered))
covid_data$total_confirmed <- as.numeric(as.character(covid_data$total_confirmed))
covid_data$total_deaths <- as.numeric(as.character(covid_data$total_deaths))
covid_data$total_recovered <- as.numeric(as.character(covid_data$total_recovered))

###################### Preparing state data ##########################################
state_date <- c()
state_active <- c()
state_confirmed <- c()
state_deaths <- c()
state_recovered <- c()
state <- c()
state_code <- c()

for (i in seq(1,length(result$statewise))){
  state_date[i] <- result$statewise[[i]]$lastupdatedtime
  state_active[i] <- result$statewise[[i]]$active
  state_confirmed[i] <- result$statewise[[i]]$confirmed
  state_deaths[i] <- result$statewise[[i]]$deaths
  state_recovered[i] <- result$statewise[[i]]$recovered
  state[i] <- result$statewise[[i]]$state
  state_code[i] <- result$statewise[[i]]$statecode
}

state_date <- substr(state_date,1,10)

state_date <- as.Date(state_date, format = "%d/%m/%y")

state_active <- as.numeric(state_active)
state_confirmed <- as.numeric(state_confirmed)
state_deaths <- as.numeric(state_deaths)
state_recovered <- as.numeric(state_recovered)

covid_state <- data.frame(state_date,state_active,state_confirmed,state_deaths,state_recovered,state,state_code)

# change column names
names(covid_state) <- c("DATE","ACTIVE","CONFIRMED","DEATHS","RECOVERED","STATE","STATE CODE")

###### Daily state data ####
test <- as.data.frame(d_state$states_daily)

i = 0
d_state_data <- data.frame(test[,seq(1,40)])
for (a in seq(0,round(length(names(test))/40))){
  i = i+1
  j = i+39
  if (j <= length(names(test))){
    d_state_data <- rbindlist(list(d_state_data,test[,seq(i,j)]), use.names = F)
  }
  
  i = j
}

state_hover <- d_state_data
state_hover$date <- as.Date(state_hover$date,format = "%d-%b-%y")
state_hover$mh <- as.numeric(state_hover$mh)
state_hover$gj <- as.numeric(state_hover$gj)
state_hover$dl <- as.numeric(state_hover$dl)
state_hover$rj <- as.numeric(state_hover$rj)
state_hover$mp <- as.numeric(state_hover$mp)
state_hover$tn <- as.numeric(state_hover$tn)
state_hover$up <- as.numeric(state_hover$up)
state_hover$ap <- as.numeric(state_hover$ap)
state_hover$tg <- as.numeric(state_hover$tg)
state_hover$wb <- as.numeric(state_hover$wb)

state_over <- aggregate(state_hover[,c("mh","gj","dl","rj","mp","tn","up","ap","tg","wb")], by = list(date = state_hover$date), FUN=sum)

# calculate cummulative of states
state_over$cum_mh <- cumsum(state_over$mh)
state_over$cum_gj <- cumsum(state_over$gj)
state_over$cum_dl <- cumsum(state_over$dl)
state_over$cum_rj <- cumsum(state_over$rj)
state_over$cum_mp <- cumsum(state_over$mp)
state_over$cum_tn <- cumsum(state_over$tn)
state_over$cum_up <- cumsum(state_over$up)
state_over$cum_ap <- cumsum(state_over$ap)
state_over$cum_tg <- cumsum(state_over$tg)
state_over$cum_wb <- cumsum(state_over$wb)

m_data <- melt(d_state_data, id.vars = c("date","status"))

state_d_confirmed <- m_data[m_data$status == "Confirmed",] 
state_d_deaths <- m_data[m_data$status == "Deceased",]
state_d_recovered <- m_data[m_data$status == "Recovered",]

s_d_conf <- state_d_confirmed[,c("date","variable","value")]
names(s_d_conf) <- c("Date","state_code","Confirmed")

s_d_deaths <- state_d_deaths[,c("date","variable","value")]
names(s_d_deaths) <- c("Date","state_code","Deaths")

s_d_recovered <- state_d_recovered[,c("date","variable","value")]
names(s_d_recovered) <- c("Date","state_code","Recovered")


daily_state_d <- merge(s_d_conf,s_d_deaths) 
daily_state_data <- merge(daily_state_d,s_d_recovered)

daily_state_data$state_code <- toupper(daily_state_data$state_code)

names(daily_state_data)[names(daily_state_data) == "state_code"] <- "STATE CODE"

daily_state_data <- merge(covid_state[,c("STATE","STATE CODE")],daily_state_data)

daily_state_data$Date <- as.Date(daily_state_data$Date, format = "%d-%b-%y")
daily_state_data$Confirmed <- as.numeric(daily_state_data$Confirmed)
daily_state_data$Deaths <- as.numeric(daily_state_data$Deaths)
daily_state_data$Recovered <- as.numeric(daily_state_data$Recovered)

################################# Predictions on India data #######################################
#covid_confirm <- covid_state[covid_state$STATE == "Total",c("DATE","CONFIRMED")]
#row.names(covid_confirm) <- seq(1,length(covid_confirm$Confirmed))
#covid_confirm <- covid_confirm[order(covid_confirm$Date),]
#covid_confirm["Confirmed"] <- cumsum(covid_confirm$Confirmed)
#names(covid_confirm) <- c("date","total_confirmed")
covid_confirm <- covid_data[,c("date","total_confirmed")]

#Fitting auto.arima ()
fit2 <- auto.arima(covid_confirm$total_confirmed, seasonal = FALSE)
#fit2

#Forecasting
forecast2 <- forecast(fit2, h = 10)

forecasts <- as.numeric(forecast2$mean)

covid_pred <- covid_confirm

start <- length(covid_pred$date)+1
end <- start+9

covid_pred[start:end,'predictions'] <- forecasts

today <- as.Date(Sys.Date())+1

end_day <- today + 9

covid_pred[start:end,'date'] <- seq(as.Date(today),as.Date(end_day),by = 1)

# Cacuations for vaue box
#Confirmed <- cumsum(daily_state_data[daily_state_data$STATE == "Total","Confirmed"])[length(daily_state_data[daily_state_data$STATE == "Total","Confirmed"])]
#Deaths <- cumsum(daily_state_data[daily_state_data$STATE == "Total","Deaths"])[length(daily_state_data[daily_state_data$STATE == "Total","Deaths"])]
#Recovered <- cumsum(daily_state_data[daily_state_data$STATE == "Total","Recovered"])[length(daily_state_data[daily_state_data$STATE == "Total","Recovered"])]
Confirmed <- covid_state[covid_state$STATE == "Total","CONFIRMED"]
Deaths <- covid_state[covid_state$STATE == "Total","DEATHS"]
Recovered <- covid_state[covid_state$STATE == "Total","RECOVERED"]
#Confirmed <- sum(covid_data$confirmed)
#Deaths <- sum(covid_data$deaths)
#Recovered <- sum(covid_data$recovered)
Active <- Confirmed-Deaths-Recovered
Tested <-  total_tested[length(total_tested)]

# Plotting theme
color1 <- 'rgb(60,60,65)'
color2 <- 'rgb(150,150,150)'
color3 <- 'rgb(0,228,135)'
color4 <- 'rgb(255,198,20)'
color5 <- 'rgb(80,199,215)'
color6 <- 'rgb(238,110,1)'
color7 <- 'rgb(116,57,133)'
color8 <- 'rgb(20,50,255)'
color9 <- 'rgb(227,29,48)'

marker.style <- function(size.size=8,color.style,line.color=marker.line.color,width.length=2){
  line.style <- list(color=line.color)
  return (list(size=size.size,color=color.style,line=line.style,width=width.length))
}

marker.line.color <-  'rgba(152, 0, 0, .8)'


l <- list(
  font = list(
    family = "sans-serif",
    size = 12,
    color = "#000"),
  bgcolor = "#FFFFFF",
  bordercolor = "#FFFFFF",
  borderwidth = 2)

hover.text <- function(t1,t2,d1,d2){
  return (paste('</br>',t1,':',d1,'</br>',t2,':',d2))
}


#################################### Buiding  Dashboard ###################################

ui <- dashboardPage(skin = "black",
  dashboardHeader(title = "Covid-19 India Analytics"),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
      menuItem("Nation Level", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("State Level", tabName = "statewise", icon = icon("th")),
      menuItem("Source Data", icon = icon("database"), href = "https://api.covid19india.org/"),
      menuItem(" Source code", icon = icon("github"), href = "https://github.com/Hemanthkaruturi/Covid-19-Trend-Analysis/")
    )
  ),
  dashboardBody(
    tags$script(HTML("
        var openTab = function(tabName){
          $('a', $('.sidebar')).each(function() {
            if(this.getAttribute('data-value') == tabName) {
              this.click()
            };
          });
        }
      ")),
    tabItems(
      # First tab content
      tabItem(
        tabName = "dashboard",
        fluidRow(
          box(width = 12,
              fluidRow(
                column(width = 2,valueBoxOutput(outputId = "Total_Cases",width = NULL)),
                column(width = 2,valueBoxOutput(outputId = "Total_Active",width = NULL)),
                column(width = 2,valueBoxOutput(outputId = "Total_Deaths",width = NULL)),
                column(width = 2,valueBoxOutput(outputId = "Total_Recovered",width = NULL)),
                column(width = 2,valueBoxOutput(outputId = "Total_Tested",width = NULL))
              )
          )

        ),
        
        fluidRow(
          column(12,
            box(
              plotlyOutput('main_plot'),
              title = 'Covid-19 confirmed cases with predictions for next 10 days',
              status = 'primary',
              collapsible = F,
              solidHeader = F,
              width = '450px',
              height = '500px'
            )
          )
        ),
        
        fluidRow(
          column(12,
                 box(
                   plotlyOutput('over_plot'),
                   title = 'Covid-19 confirmed cases top 10 States',
                   status = 'primary',
                   collapsible = F,
                   solidHeader = F,
                   width = '450px',
                   height = '500px'
                 )
          )
        ),
        
        
        fluidRow(
          column(4,
            box(
              plotlyOutput('daily_confirm'),
              title = 'Covid-19 daily confirmed cases',
              status = 'primary',
              collapsible = F,
              solidHeader = F,
              width = '87px',
              height = '500px'
            )
          ),
          
          column(4,
            box(
              plotlyOutput('daily_deaths'),
              title = 'Covid-19 daily deaths',
              status = 'primary',
              collapsible = F,
              solidHeader = F,
              width = '87px',
              height = '500px'
            )
          ),
          
          column(4,
            box(
              plotlyOutput('daily_recovered'),
              title = 'Covid-19 daily recovered',
              status = 'primary',
              collapsible = F,
              solidHeader = F,
              width = '87px',
              height = '500px'
            )
          )
        ),
        
        fluidRow(
          column(12,
                   infoBoxOutput("out1")
          )
        ),
        
        fluidRow(
          box(
            column(12,DTOutput("state_table")),
            width = '350px',
            height = '630px',
            extensions=c('ColReorder','Responsive')
          )
          
          
        )
      ),
      
      # Second tab content
      tabItem(
        tabName = "statewise",
        fluidRow(
          column(4,
                 box(
                   selectInput("state", "State", 
                               choices=covid_state$STATE),
                   width = '87px',
                   height = '100px'
                 )
          )
        ),
        
        fluidRow(
          box(width = 12,
              fluidRow(
                column(width = 3,valueBoxOutput(outputId = "State_Cases",width = NULL)),
                column(width = 3,valueBoxOutput(outputId = "State_Active",width = NULL)),
                column(width = 3,valueBoxOutput(outputId = "State_Deaths",width = NULL)),
                column(width = 3,valueBoxOutput(outputId = "State_Recovered",width = NULL))
              )
            
          )
          
        ),
        
        fluidRow(
          column(12,
                 box(
                   plotlyOutput('state_pred'),
                   title = 'Covid-19 confirmed cases with predictions',
                   status = 'primary',
                   collapsible = F,
                   solidHeader = F,
                   width = '450px',
                   height = '500px'
                 )
          )
        ),
        
        fluidRow(
          column(12,
                 box(
                   plotlyOutput('top_plot'),
                   title = 'Covid-19 confirmed cases top 10 Districts',
                   status = 'primary',
                   collapsible = F,
                   solidHeader = F,
                   width = '450px',
                   height = '500px'
                 )
          )
        ),
        
        fluidRow(
          column(4,
                 box(
                   plotlyOutput('daily_state_confirmed'),
                   title = 'Covid-19 daily confirmed',
                   status = 'primary',
                   collapsible = F,
                   solidHeader = F,
                   width = '87px',
                   height = '500px'
                 )
          ),
          column(4,
                 box(
                   plotlyOutput('daily_state_deaths'),
                   title = 'Covid-19 daily deaths',
                   status = 'primary',
                   collapsible = F,
                   solidHeader = F,
                   width = '87px',
                   height = '500px'
                 )
          ),
          column(4,
                 box(
                   plotlyOutput('daily_state_recovered'),
                   title = 'Covid-19 daily recovered',
                   status = 'primary',
                   collapsible = F,
                   solidHeader = F,
                   width = '87px',
                   height = '500px'
                 )
          )
        ),
        
        fluidRow(
          box(
            column(12,dataTableOutput("district_table")),
            width = '350px',
            height = '630px'
          )
        )
        
      )
    )
  )
)

server <- function(input, output, session) {
  
  output$out1 <- renderInfoBox({
    infoBox("Click here for State Level Analysis",  
            a("State Level Analysis", onclick = "openTab('statewise')", href="#"),
            icon = icon("map-marker"), color = "green"
    )
  })
  
  ## Boxes
  output$Total_Cases <- renderValueBox({
    valueBox(
      formatC(Confirmed,format="d", big.mark=','),
      paste('Total Confirmed Cases'),
      #icon = icon('stats',lib='glyphicon'),
      color = 'orange'
    )
  })
  
  output$Total_Active <- renderValueBox({
    valueBox(
      formatC(Active,format="d", big.mark=','),
      paste('Total Active Cases'),
      #icon = icon('stats',lib='glyphicon'),
      color = 'aqua'
    )
  })
  
  output$Total_Deaths <- renderValueBox({
    valueBox(
      formatC(Deaths,format="d", big.mark=','),
      paste('Total Deceased'),
      #icon = icon('stats',lib='glyphicon'),
      color = 'red'
    )
  })
  
  output$Total_Recovered <- renderValueBox({
    valueBox(
      formatC(Recovered,format="d", big.mark=','),
      paste('Total Recovered'),
      #icon = icon('stats',lib='glyphicon'),
      color = 'green'
    )
  })
  
  output$Total_Tested <- renderValueBox({
    valueBox(
      formatC(Tested,format="d", big.mark=','),
      paste('Total Tested'),
      #icon = icon('stats',lib='glyphicon'),
      color = 'blue'
    )
  })
  
  ## Main Pot
  output$main_plot <- renderPlotly(
    plot_ly(data = covid_pred, x = ~date,y=~total_confirmed, type="scatter",mode = 'lines+markers',hoverinfo = 'text',text=~hover.text('Date','Cases',date,paste0(round(covid_pred$total_confirmed,0))) ,line = list(color = color1),marker =  marker.style(color.style = color1) ,name="cases till date") %>%
      add_trace(y =~predictions, type="scatter",mode = 'lines+markers',hoverinfo = 'text',text=~hover.text('Date','Cases',date,paste0(round(covid_pred$predictions,0))) ,line = list(color = 'rgb(150,150,150)',dash='dash'),marker =  marker.style(color.style = color2) ,name="10 days forecasts") %>%
      
      layout(xaxis=list(title="Date"),
             yaxis=list(title="Confirmed cases"),
             legend = l)%>%
      config(displaylogo = FALSE)
  )
  
  output$over_plot <- renderPlotly(
    plot_ly(state_over,x = ~date, y = ~cum_mh, type = 'scatter', mode = "lines+markers",
            hoverinfo = 'text', text=~hover.text('Date','Cases',date,paste0(round(cum_mh,0))),
            marker = list(color = color1,
                          line = list(color = color2, width = 1.5)), name = "Maharashtra") %>%
      add_trace(y = ~cum_gj, type = 'scatter', mode = "lines+markers",
                hoverinfo = 'text', text=~hover.text('Date','Cases',date,paste0(round(cum_gj,0))),
                marker = list(color = color2,
                              line = list(color = color3, width = 1.5)), name = "Gujarat") %>%
      add_trace(y = ~cum_dl, type = 'scatter', mode = "lines+markers",
                hoverinfo = 'text', text=~hover.text('Date','Cases',date,paste0(round(cum_dl,0))),
                marker = list(color = color3,
                              line = list(color = color2, width = 1.5)), name = "Delhi") %>%
      add_trace(y = ~cum_rj, type = 'scatter', mode = "lines+markers",
                hoverinfo = 'text', text=~hover.text('Date','Cases',date,paste0(round(cum_rj,0))),
                marker = list(color = color3,
                              line = list(color = color2, width = 1.5)), name = "Rajasthan") %>%
      add_trace(y = ~cum_mp, type = 'scatter', mode = "lines+markers",
                hoverinfo = 'text', text=~hover.text('Date','Cases',date,paste0(round(cum_mp,0))),
                marker = list(color = color4,
                              line = list(color = color2, width = 1.5)), name = "Madhya Pradesh") %>%
      add_trace(y = ~cum_tn, type = 'scatter', mode = "lines+markers",
                hoverinfo = 'text', text=~hover.text('Date','Cases',date,paste0(round(cum_tn,0))),
                marker = list(color = color5,
                              line = list(color = color2, width = 1.5)), name = "Tamil Nadu") %>%
      add_trace(y = ~cum_up, type = 'scatter', mode = "lines+markers",
                hoverinfo = 'text', text=~hover.text('Date','Cases',date,paste0(round(cum_up,0))),
                marker = list(color = color6,
                              line = list(color = color2, width = 1.5)), name = "Uttar Pradesh") %>%
      add_trace(y = ~cum_ap, type = 'scatter', mode = "lines+markers",
                hoverinfo = 'text', text=~hover.text('Date','Cases',date,paste0(round(cum_ap,0))),
                marker = list(color = color7,
                              line = list(color = color2, width = 1.5)), name = "Andhra Pradesh") %>%
      add_trace(y = ~cum_tg, type = 'scatter', mode = "lines+markers",
                hoverinfo = 'text', text=~hover.text('Date','Cases',date,paste0(round(cum_tg,0))),
                marker = list(color = color8,
                              line = list(color = color2, width = 1.5)), name = "Telangana") %>%
      add_trace(y = ~cum_wb, type = 'scatter', mode = "lines+markers",
                hoverinfo = 'text', text=~hover.text('Date','Cases',date,paste0(round(cum_wb,0))),
                marker = list(color = "Green",
                              line = list(color = color2, width = 1.5)), name = "West Bengal") %>%
      layout(xaxis = list(title = "Date"),
             yaxis = list(title = "Confirmed")) %>% config(displaylogo = FALSE)
    
  )
  
  ## daily graphs
  output$daily_confirm <- renderPlotly(
    plot_ly(covid_data, x = ~date, y = ~confirmed, type = 'bar',
            hoverinfo = 'text', text=~hover.text('Date','Cases',date,paste0(round(covid_data$confirmed,0))),
            marker = list(color = color1,
                          line = list(color = color2, width = 1.5))) %>%
      layout(xaxis = list(title = "Date"),
              yaxis = list(title = "Confirmed")) %>% config(displaylogo = FALSE)
  )
  
  output$daily_deaths <- renderPlotly(
    plot_ly(covid_data, x = ~date, y = ~deaths, type = 'bar',
            hoverinfo = 'text', text=~hover.text('Date','Deaths',date,paste0(round(covid_data$deaths,0))),
            marker = list(color = color2,
                          line = list(color = color3, width = 1.5))) %>%
      layout(xaxis = list(title = "Date"),
             yaxis = list(title = "Deaths")) %>% config(displaylogo = FALSE)
  )
  
  output$daily_recovered <- renderPlotly(
    plot_ly(covid_data, x = ~date, y = ~recovered, type = 'bar',
            hoverinfo = 'text', text=~hover.text('Date','Recovered',date,paste0(round(covid_data$recovered,0))),
            marker = list(color = color3,
                          line = list(color = color2, width = 1.5))) %>%
      layout(xaxis = list(title = "Date"),
             yaxis = list(title = "Recovered")) %>% config(displaylogo = FALSE)
  )
  
  ## state table
  output$state_table <- renderDT(covid_state[,c("STATE","CONFIRMED","ACTIVE","DEATHS","RECOVERED")], selection = list(mode = 'multiple', selected = c(1)))
  
  ## statewise value boxes
  output$State_Cases <- renderValueBox({
    valueBox(
      formatC(as.numeric(covid_state[covid_state$STATE == input$state,"CONFIRMED"]),format="d", big.mark=','),
      paste('Total Cases'),
      #icon = icon('stats',lib='glyphicon'),
      color = 'blue',
    )
  })
  
  output$State_Active <- renderValueBox({
    valueBox(
      formatC(as.numeric(covid_state[covid_state$STATE == input$state,"ACTIVE"]),format="d", big.mark=','),
      paste('Total Active'),
      #icon = icon('stats',lib='glyphicon'),
      color = 'aqua'
    )
  })
  
  output$State_Deaths <- renderValueBox({
    valueBox(
      formatC(as.numeric(covid_state[covid_state$STATE == input$state,"DEATHS"]),format="d", big.mark=','),
      paste('Total Deaths'),
      #icon = icon('stats',lib='glyphicon'),
      color = 'red'
    )
  })
  
  output$State_Recovered <- renderValueBox({
    valueBox(
      formatC(as.numeric(covid_state[covid_state$STATE == input$state,"RECOVERED"]),format="d", big.mark=','),
      paste('Total Recovered'),
      #icon = icon('stats',lib='glyphicon'),
      color = 'green'
    )
  })
  
  # state pred
  output$state_pred <- renderPlotly({
    # do statewise predictions
    pred_data <- daily_state_data[daily_state_data$STATE == input$state,c("Date","Confirmed")]
    
    pred_data <- pred_data[order(pred_data$Date),]
    
    pred_data$Confirmed <- cumsum(pred_data$Confirmed)
    
    # reset index
    row.names(pred_data) <- seq(1,length(pred_data$Date))
    
    #Fitting auto.arima ()
    fit2 <- auto.arima(pred_data$Confirmed, seasonal = FALSE)
    
    #Forecasting
    forecast2 <- forecast(fit2, h = 10)
    
    forecasts <- as.numeric(forecast2$mean)
    
    start <- length(pred_data$Date)+1
    end <- start+9
    
    pred_data[start:end,'predictions'] <- round(forecasts,0)
    
    today <- as.Date(Sys.Date())+1
    
    end_day <- today + 9
    
    pred_data[start:end,'Date'] <- seq(as.Date(today),as.Date(end_day),by = 1)
    
    plot_ly(pred_data,x = ~Date, y = ~Confirmed, type = 'scatter', mode = "lines+markers",
            hoverinfo = 'text', text=~hover.text('Date','Cases',Date,paste0(round(Confirmed,0))),
            marker = list(color = color1,
                          line = list(color = color2, width = 1.5)), name = input$state) %>%
      add_trace(y = ~predictions, type = 'scatter', mode = "lines+markers",
                hoverinfo = 'text', text=~hover.text('Date','Cases',Date,paste0(round(predictions,0))),
                marker = list(color = color2,
                              line = list(color = color3, width = 1.5)), name = "predictions")%>% config(displaylogo = FALSE)
  })
  
  # top pots
  output$top_plot <- renderPlotly(
    district_data %>% filter(STATE == input$state) %>% arrange(desc(CONFIRMED)) %>% head(10) %>% plot_ly(x = ~DISTRICT, y = ~CONFIRMED, type = 'bar',
                                                                                                    hoverinfo = 'text', text=~hover.text('Date','Cases',DISTRICT,paste0(round(CONFIRMED,0))),
                                                                                                    marker = list(color = color1,
                                                                                                                  line = list(color = color2, width = 1.5))) %>%
      layout(xaxis = list(title = "Date"),
             yaxis = list(title = "Confirmed")) %>% config(displaylogo = FALSE)
  )
    
  ## Top districts
  output$daily_state_confirmed <- renderPlotly(
    daily_state_data %>% filter(STATE == input$state) %>% plot_ly(x = ~Date, y = ~Confirmed, type = 'bar',
                                                              hoverinfo = 'text', text=~hover.text('Date','Cases',Date,paste0(round(Confirmed,0))),
                                                              marker = list(color = color1,
                                                                            line = list(color = color2, width = 1.5))) %>%
      layout(xaxis = list(title = "Date"),
             yaxis = list(title = "Confirmed")) %>% config(displaylogo = FALSE)
  )
  
  output$daily_state_deaths <- renderPlotly(
    daily_state_data %>% filter(STATE == input$state) %>% plot_ly(x = ~Date, y = ~Deaths, type = 'bar',
                                                                  hoverinfo = 'text', text=~hover.text('Date','Cases',Date,paste0(round(Deaths,0))),
                                                                  marker = list(color = color2,
                                                                                line = list(color = color3, width = 1.5))) %>%
      layout(xaxis = list(title = "Date"),
             yaxis = list(title = "Confirmed")) %>% config(displaylogo = FALSE)
  )
  
  output$daily_state_recovered <- renderPlotly(
    daily_state_data %>% filter(STATE == input$state) %>% plot_ly(x = ~Date, y = ~Recovered, type = 'bar',
                                                                  hoverinfo = 'text', text=~hover.text('Date','Cases',Date,paste0(round(Recovered,0))),
                                                                  marker = list(color = color3,
                                                                                line = list(color = color2, width = 1.5))) %>%
      layout(xaxis = list(title = "Date"),
             yaxis = list(title = "Confirmed")) %>% config(displaylogo = FALSE)
  )
  
  ## district tabe
  output$district_table <- renderDataTable({
    adj_data <- district_data[district_data$STATE ==input$state, c("DISTRICT","CONFIRMED","ACTIVE","DECEASED","RECOVERED")]
    adj_data[order(adj_data$CONFIRMED, decreasing = T),]
  })
  
  
}

shinyApp(ui, server)
