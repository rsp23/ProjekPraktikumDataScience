library(shiny)
library(shinycssloaders)
library(shinydashboard)


# User Interface

source("dataprep.R")

ui <- shinyUI(
  dashboardPage(
    # application title
    dashboardHeader(title ="Customer Segmen"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Raw Data",tabName = "raw",icon=icon("table")),
        menuItem("Wordcloud",tabName = "wdc",icon=icon("cloudversify")),
        menuItem("K-means clustering",tabName= "kmeans",icon=icon("users",lib="font-awesome")),
        menuItem("K-means Product clustering",tabName= "kmeansproduct",icon=icon("boxes",lib="font-awesome")),
        menuItem("Sales amount",tabName = "countrygroup",icon=icon("globe-americas"))
      )),
    dashboardBody(
      tabItems(
        tabItem(tabName = "raw",h1("Customer Data"),fluidRow(column(5,tableOutput("rawdata")))),
        tabItem(
          tabName = "wdc",
          #hasil cloud
          fluidRow(
            box(plotOutput("wordcloud", height = 750), width = 750)
          )
        ),
        tabItem(tabName = "kmeans",h1("K-means clustering customor spend money"),
                fluidRow(
                  box(plotOutput("kmeans")),
                ),
                fluidRow(
                  box(
                    #input data yang ingin ditampilkan
                    sliderInput(
                      "sizeKmeans",
                      "Total Kluster(Recomended 4)",
                      min = 1,
                      max = 10,
                      value = 4
                    ),
                    width=12,
                    solidHeader = T
                  )
                )
          ),
          tabItem(
            tabName = "countrygroup",h1("Sales amount based on country"),
            #hasil cloud
            fluidRow(
              box(plotOutput("countrygroup"))
            )
          ),
        tabItem(tabName = "kmeansproduct",h1("K-means clustering sold product"),
                fluidRow(
                  box(plotOutput("kmeansproduct")),
                ),
                fluidRow(
                  box(
                    #input data yang ingin ditampilkan
                    sliderInput(
                      "sizeKmeans",
                      "Total Kluster(Recomended 4)",
                      min = 1,
                      max = 10,
                      value = 4
                    ),
                    width=12,
                    solidHeader = T
                  )
                )
          )
         )
      )
    ))    