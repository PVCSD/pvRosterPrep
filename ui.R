library(shiny)
library(rhandsontable)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(stringr)
library(shinythemes)


###### UI Layout ######
ui <- navbarPage( theme = shinytheme("slate"),
  "Rostering Preperation",
  id = "tabs",

  # #### HOME PAGE #####
  # tabPanel(
  #   "Home",
  #   h1("About this App"),
  #   fluidRow(
  #     column(width = 3, img(src = "spartanLogo.png", align = "center")),
  #     column(
  #       width = 9,
  #       p("This App is designed to prep ad-hoc exports from Infinite Campus to various other programs that require student Rosters."),
  #       p("Simply upload the ad hoc from campus on the upload tab. On the Prep tab select the options for the roster you need."),
  #       p(tags$b("It is reccomended that this App be run in a full sized window"))
  #     )
  #   )
  # ),

  #### UPLOAD PAGE #####
  ## right now this has file options, I may move those to the prep page.
  tabPanel(
    "Upload",
    titlePanel("Uploading Files"),
    sidebarLayout(
      sidebarPanel(
        fileInput("file1", "Choose CSV File",
          multiple = FALSE,
          accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv"
          )
        ),
        shinyjs::useShinyjs(),
        actionButton("append", "Show Preperations"),
        
        p("This App is designed to prep ad-hoc exports from Infinite Campus to various other programs that require student Rosters."),
        p("Upload a file with the approriate column names from an ad-hoc from Campus and press the button 'Show Preperations'"),


        # Input: Select number of rows to display
        radioButtons("disp", "Display",
          choices = c(
            Head = "head",
            All = "all"
          ),
          selected = "head"
        )
      ),

      ## this shows the uploaded data set
      mainPanel(
        tableOutput("contents")
      )
    )
  )
)
