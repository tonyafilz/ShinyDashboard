library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)

ui <- dashboardPage(
  skin = "yellow",
  dashboardHeader(title = "A Wooly Dashboard",
                  
                  #Add in message menu
                  dropdownMenu(type = "messages",
                               messageItem(
                                 from = "Data Science Lead",
                                 message = "Hope you enjoy the new dashboard!"
                               ),
                               
                               messageItem(
                                 from = "FAQ",
                                 message = "For a list of frequently asked questions, please see the Wooly Dashboard User Guide",
                                 icon = icon("question"),
                                 time = Sys.time()
                               ),
                               
                               messageItem(
                                 from = "Support",
                                 message = "Update complete",
                                 icon = icon("life-ring"),
                                 time = "2017-11-13"
                               )),
                  
                  #Add in Notification menu
                  dropdownMenu(type = "notifications",
                               
                               notificationItem(
                                 text = "7 new users today",
                                 icon("users")
                               ),
                               
                               notificationItem(
                                 text = "Migration to ODBC drivers complete",
                                 icon ("truck"),
                                 status = "success"
                               ),
                               
                               notificationItem(
                                 text = "Planned downtime this Saturday",
                                 icon("exclamation-triangle"),
                                 status = "warning"
                               )
                  ),
                  
                  #Add task menu
                  dropdownMenu(type = "tasks", badgeStatus = "success",
                               taskItem(value = 100, color = "green",
                                        "Documentation"
                               ),

                               taskItem(value = 75, color = "yellow",
                                        "Onboarding"
                               ),
                               taskItem(value = 10, color = "red",
                                        "Add custom UI"
                               )
                  )
  ),
  
  dashboardSidebar(
    sidebarMenu(
      
      #Add in search bar
      sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                        label = "Search..."),
      
      #Add in 'Dashboard' tab
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      
      #Add in 'Table' tab
      menuItem("Table", icon = icon("th"), tabName = "table",
               badgeLabel = "new", badgeColor = "green"),
      
      #Add in 'Documentation tab' with link to Shiny Dashboard site
      menuItem("Documentation", icon = icon("file", lib= "glyphicon"), 
               href = "https://rstudio.github.io/shinydashboard/index.html")
    )
  ),
  
  dashboardBody(
    tabItems(
      
      #Create body for 'Dashboard' tab
      tabItem(tabName = "dashboard",
              fluidRow(
                box(
                  tags$div(class = "header", checked = NA,
                           tags$p("Welcome to the Wool Dashboard!
                                    This dashboard uses the warpbreaks data set in R and will show you some of the basic componenets of a Shiny Dashboard.",
                                  tags$hr(),
                                  "To begin, simply start changing the inputs and watching the reactions in the plotly plot, as well as the value boxes.")), width = 12
                ),
                
                valueBox(27, "Number of Observations", icon = icon("ok", lib = "glyphicon"), color = "yellow"),
                
                valueBoxOutput("breakbox"),
                
                valueBoxOutput("meanbreakbox"),
                
                box(plotlyOutput("plot1")),
                
                box(
                  title = "Wool Type",
                  selectInput("wool",
                              "Wool",
                              choices = levels(warpbreaks$wool))
                )
              )
              ),
    
    #Create body for 'Table' tab 
    tabItem(tabName = "table",
            fluidRow(
              box(
                tags$div(class = "header", checked = NA,
                         tags$p("This is an interactive table based on the warpbreaks dataset in R. Note the interactivity and reactiveness of the data.")
              ),
              
              box(
                dataTableOutput("table1"), width = 16
              )
              )
            )
          )
  )
))

server <- function(input, output) { 
  
  #Reactive output for 'Dashboard'
  output$plot1 <- renderPlotly(
    plot_ly(subset(warpbreaks, wool==input$wool), x= ~tension, y= ~breaks)
  )
  
  woolsum <- reactive(subset(warpbreaks, wool == input$wool))
  woolsubsum <- reactive(sum(woolsum()$breaks))
  
  output$breakbox <- renderValueBox(
    valueBox(
      woolsubsum(), "Number of Breaks", icon = icon("remove", lib = "glyphicon"),
      color = "yellow"
      )
    )
  
  output$meanbreakbox <- renderValueBox(
    valueBox(
      round((woolsubsum()/27), digits = 2), "Mean Number of Breaks", icon = icon("stats", lib= "glyphicon"),
      color = "yellow"
    )
  )

  
  #Interactive table for 'Table'
  output$table1 <- renderDataTable(warpbreaks)
  
  }

shinyApp(ui, server)