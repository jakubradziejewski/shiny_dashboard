library(shiny)
library(shinydashboard)
library(plotly)
library(DT) # Make sure DT is loaded in ui.R too if you define ui and server separately

ui <- dashboardPage(
  dashboardHeader(title = "Boredom Level"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th")),
      menuItem("Data", tabName = "data", icon = icon("database"))
    ),
    collapsed = TRUE
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              h2("Boredom Level"),
              fluidRow(box(title = "Boredom Level", status = "primary", solidHeader = TRUE,
                           sliderInput("testRange", "Boredom level:", min = 0, max = 100, value = c(20, 80)),
                           textOutput("testText"),
                           width = 12)
              ),
              box(title = "Boredom Level", status = "primary", solidHeader = TRUE,
                  plotlyOutput("plot3"),
                  width = 12
              ),
              fluidRow(
                column(6, plotlyOutput("plot4")),
                column(6, plotlyOutput("plot5"))
              )
      ),
      tabItem(tabName = "widgets",
              h2("Widgets tab content")
      ),
      tabItem(tabName = "data",
              h2("Data tab content"),
              p("This is a placeholder for the data tab content. You can add your own content here."),
              dataTableOutput("data") # Corrected function name
      )
    )
  )
)