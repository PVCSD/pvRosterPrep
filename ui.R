library(shiny)
library(rhandsontable)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(stringr)
library(shinythemes)


###### UI Layout ######
ui <- navbarPage(
  "Rostering Preperation",
  id = "tabs",
  
  #### HOME PAGE #####
  tabPanel(
    "Home",
    h1("About this App"),
    fluidRow(
      column(width = 3, img(src = "spartanLogo.png", align = "center")),
      column(
        width = 9,
        p("This App is designed to prep ad-hoc exports from Infinite Campus to various other programs that require student Rosters."),
        p("Simply upload the ad hoc from campus on the upload tab. On the Prep tab select the options for the roster you need."),
        p(tags$b("It is reccomended that this App be run in a full sized window"))
      )
    )
  ),
  
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
        selectInput("rosterType", "Choose a roster to prep",
                    choices = c("None", "HMH (Gov/Hist)", "IHT", "PLTW (Elementary)", "Waterford")
        ),
       
        
        
        
        h6("Options should remain in the default position unless you know what you're doing"),
        # Horizontal line
        tags$hr(),
        
        # Input: Checkbox if file has header
        checkboxInput("header", "Header", TRUE),
        
        # Horizontal line
        tags$hr(),
        
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
  ),
  
  #### PREPARED FILE #####
  tabPanel(
    "Prep",
    mainPanel(
      uiOutput("exportTitle"),
      downloadButton("downloadData", "Download"),
      tableOutput("prepped")
    )
  )
)

