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
        a(href="https://docs.google.com/spreadsheets/d/1_Rm5L1kfQbUfEa2TgqVoGyS6LPjN_GjARe8amcx9Csg/edit?usp=sharing", "You can see templates for the upload here"),
        br(),


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
