library(shiny)
library(rhandsontable)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(stringr)
library(shinythemes)



###### SERVER FUNCTIONS ######
server <- function(input, output, session) {
  
  
  
  #### Data ####
  
  ## Extract the CSV
  FileData <- reactive({
    infile <- input$file1
    if (FileReady() == F) {
      return(NULL)
    }
    
    read.csv(infile$datapath,
             header = input$header,
             sep = ",",
             quote = '"',
             stringsAsFactors = FALSE
    )
  })
  
  ## Show Uploaded Data
  output$contents <- renderTable({
    df <- FileData()
    
    if (input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
  })
  
  ## What Kind of file is being Prepped?
  FilePrep <- reactive({
    if (FileReady() == F) {
      return(NULL)
    }
    
    ## if it's IHT Do the IHT PREP
    if (input$rosterType == "IHT") {
    
    }
    
    else if (input$rosterType == "HMH (Gov/Hist)") {
      if (input$hmhFileDropdown == "Class") {
        return(HMHClass())
      }
      else if (input$hmhFileDropdown == "Users") {
        return(HMHUsers())
      }
      else if (input$hmhFileDropdown == "Class Assignments") {
        return(HMHClassAssignments())
      }
      else {
        return(NULL)
      }
    }
    else if (input$rosterType == "Waterford") {
      return(WaterfordPrep())
    }
    
    else {
      return(NULL)
    }
  })
  
  
  ###### PREPPING THE FILES ######
  
  
  ##### IHT  #####
  
  #### IHT PREP ELEMENTARY LEVEL ####
  ElementaryIHT <- reactive({
    FileData() %>%
      as_tibble() %>%
      ## Remove duplicates
      distinct(student_studentNumber, .keep_all = T) %>%
      # fix the kindergarten output from campus to match what IHT needs
      mutate(student_grade = replace(student_grade, student_grade == "KF", "K")) %>%
      # replace commas in classroom teacher display name to make splitting easier
      mutate(courseSection_teacherDisplay = str_replace_all(courseSection_teacherDisplay, ",", "-")) %>%
      # split the classroom teacher from the PE teacher
      separate(courseSection_teacherDisplay, into = c("peTeach", "elemteach"), sep = "-", extra = "drop") %>%
      # filter out the teacher
      filter(peTeach == input$teacherDropdown) %>%
      # Select and rename variables
      select(c(
        "grade level" = student_grade,
        "section" = function_IHTClassName,
        "student id" = student_studentNumber,
        "last name*" = student_lastName,
        "first name" = student_firstName,
        "secondary email" = contacts_email,
        "gender*" = student_gender,
        "birthdate" = student_birthdate
      )) %>%
      # add the blank columns
      add_column("email" = NA, .before = "secondary email") %>%
      add_column(height = NA, weight = NA, .before = "birthdate") %>%
      add_column(rhr = NA, max = NA, .after = "birthdate") -> ihtELM
    
    
    return(ihtELM)
  })
  
  #### IHT PREP JR. HIGH SCHOOL LEVEL ####
  JrHighSchoolIHT <- reactive({
    FileData() %>%
      as_tibble() %>%
      ## Remove duplicates
      distinct(student_studentNumber, .keep_all = T) %>%
      # fix the kindergarten output from campus to match what IHT needs
      mutate(student_grade = replace(student_grade, student_grade == "KF", "K")) %>%
      unite("IHTClassName", function_IHTClassName, function_Schoolyear) %>%
      # Select and rename variables
      select(c(
        "grade level" = student_grade,
        "section" = IHTClassName,
        "student id" = student_studentNumber,
        "last name*" = student_lastName,
        "first name" = student_firstName,
        "secondary email" = contacts_email,
        "gender*" = student_gender,
        "birthdate" = student_birthdate
      )) %>%
      # add the blank columns
      add_column("email" = NA, .before = "secondary email") %>%
      add_column(height = NA, weight = NA, .before = "birthdate") %>%
      add_column(rhr = NA, max = NA, .after = "birthdate") -> ihtJH
    return(ihtJH)
  })
  
  #### IHT PREP HIGH SCHOOL LEVEL ####
  HighSchoolIHT <- reactive({
    FileData() %>%
      as_tibble() %>%
      ## Remove duplicates
      distinct(student_studentNumber, .keep_all = T) %>%
      # fix the kindergarten output from campus to match what IHT needs
      mutate(student_grade = replace(student_grade, student_grade == "KF", "K")) %>%
      # filter out the teacher
      filter(str_detect(function_IHTClassName, pattern = input$periodDropdown)) %>%
      # Select and rename variables
      select(c(
        "grade level" = student_grade,
        "section" = function_IHTClassName,
        "student id" = student_studentNumber,
        "last name*" = student_lastName,
        "first name" = student_firstName,
        "secondary email" = contacts_email,
        "gender*" = student_gender,
        "birthdate" = student_birthdate
      )) %>%
      # add the blank columns
      add_column("email" = NA, .before = "secondary email") %>%
      add_column(height = NA, weight = NA, .before = "birthdate") %>%
      add_column(rhr = NA, max = NA, .after = "birthdate") -> ihtHS
    return(ihtHS)
  })
  
  
  
  
  ##### HMH ####
  
  #### HMH Class file ####
  HMHClass <- reactive({
    FileData() %>%
      distinct(courseSection_courseID, courseSection_sectionNumber, .keep_all = T) %>%
      mutate(courseSection_courseName, COURSESUBJECT = ifelse(courseSection_courseName == "American Government", "Government", "History")) %>%
      mutate(courseSection_courseName, GRADE = ifelse(courseSection_courseName == "American Government", "11", "9")) %>%
      mutate("CLASSLOCALID" = paste(cal_endYear, courseSection_courseID, sectionSchedule_periodStart, sep = "_")) %>%
      mutate("CLASSNAME" = paste(cal_endYear, courseSection_courseName, "Sec", courseSection_sectionNumber, sep = " ")) %>%
      add_column("TERMID" = NA, "ORGANIZATIONID" = 250932, "CLASSDESCRIPTION" = NA, "ORGANIZATIONTYPEID" = NA, HMHAPPLICATIONS = "ED") %>%
      select(c(
        "SCHOOLYEAR" = cal_endYear,
        CLASSLOCALID,
        "COURSEID" = courseSection_courseID,
        "COURSENAME" = courseSection_courseName,
        COURSESUBJECT,
        CLASSNAME,
        CLASSDESCRIPTION,
        "CLASSPERIOD" = sectionSchedule_periodStart,
        ORGANIZATIONTYPEID,
        ORGANIZATIONID,
        GRADE,
        TERMID,
        HMHAPPLICATIONS
      )) -> CLASS
    return(CLASS)
  })
  
  #### HMH Users file ####
  HMHUsers <- reactive({
    FileData() %>%
      distinct(student_studentNumber, .keep_all = T) %>%
      mutate(student_studentNumber, "ROLE" = ifelse(student_studentNumber >= 500000, "T", "S")) %>%
      mutate("USERNAME" = gsub(" ", "", str_remove_all(tolower(paste0(student_firstName, student_lastName)), "[~!@#$%^&*(){}_+:<>?,./;'-]"))) %>%
      add_column("MIDDLENAME" = NA, "ORGANIZATIONTYPEID" = "MDR", "ORGANIZATIONID" = 250932, "PRIMARYEMAIL" = NA, HMHAPPLICATIONS = "ED") %>%
      select(
        "SCHOOLYEAR" = cal_endYear,
        ROLE,
        "LASID" = student_studentNumber,
        "SASID" = student_stateID,
        "FIRSTNAME" = student_firstName,
        MIDDLENAME,
        "LASTNAME" = student_lastName,
        "GRADE" = student_grade,
        USERNAME,
        "PASSWORD" = student_studentNumber,
        ORGANIZATIONTYPEID,
        ORGANIZATIONID,
        PRIMARYEMAIL,
        HMHAPPLICATIONS
      ) -> USERS
    return(USERS)
  })
  
  #### HMH Class Assignments file ####
  HMHClassAssignments <- reactive({
    FilePrep() %>%
      mutate("CLASSLOCALID" = paste(cal_endYear, courseSection_courseID, sectionSchedule_periodStart, sep = "_")) %>%
      mutate(student_studentNumber, "ROLE" = ifelse(student_studentNumber >= 500000, "T", "S")) %>%
      mutate(student_studentNumber, "OSITION" = ifelse(student_studentNumber >= 500000, "L", NA)) %>%
      select(
        "SCHOOLYEAR" = cal_endYear,
        CLASSLOCALID,
        ROLE,
        POSITION
      ) -> CLASSASSIGNMENTS
    return(CLASSASSIGNMENTS)
  })
  
  
  
  ##### Waterford #####
  
  #### Waterford Prepped File ####
  WaterfordPrep <- reactive({
    df <- FileData()
    
    df %>%
      distinct(student_studentNumber, .keep_all = T) %>%
      mutate(student_grade = replace(student_grade, student_grade == "KF", "K")) %>%
      mutate(student_grade = replace(student_grade, student_grade == "01", "1st Grade")) %>%
      mutate(student_grade = replace(student_grade, student_grade == "02", "2nd Grade")) %>%
      mutate("firstName" = gsub(" ", "", str_remove_all(student_firstName, "[~!@#$%^&*(){}_+:<>?,./;'-]"))) %>%
      mutate("middleName" = gsub(" ", "", str_remove_all(student_middleName, "[~!@#$%^&*(){}_+:<>?,./;'-]"))) %>%
      mutate("lastName" = gsub(" ", "", str_remove_all(student_lastName, "[~!@#$%^&*(){}_+:<>?,./;'-]"))) %>%
      mutate("preferredName" = gsub(" ", "", str_remove_all(coalesce(student_alias, firstName), "[~!@#$%^&*(){}_+:<>?,./;'-]"))) %>%
      mutate("classGrade" = student_grade) %>%
      mutate("schoolName" = substr(student_calendarName, 6, 100000)) %>%
      add_column(
        "sisName" = NA,
        "sisID" = NA,
        "schoolSISName" = NA,
        "schoolSISID" = NA,
        "pLanguage" = NA,
        "Ethnicity" = NA,
        "Disabilities" = NA,
        "hStatus" = NA,
        "sp" = NA,
        "Email" = NA,
        "Username" = NA,
        "Password" = NA
      ) %>%
      select(
        "First Name" = firstName,
        "Middle Name" = middleName,
        "Last Name" = lastName,
        # "Prefered Name"=preferredName,
        "Unique Student ID" = student_studentNumber,
        "Grade" = student_grade,
        "Class Name" = student_homeroomTeacher,
        "Class Grade" = classGrade,
        "Class SIS NAME" = sisName,
        "Class SIS ID" = sisID,
        "School Name" = schoolName,
        "School SIS NAME" = schoolSISName,
        "School SIS ID" = schoolSISID,
        "Gender" = student_gender,
        "Birthday" = student_birthdate,
        "Primary Language" = pLanguage,
        Ethnicity,
        Disabilities,
        "Household Status" = hStatus,
        "Special Programs" = sp,
        Email,
        Username,
        Password
      ) -> waterford
    
    return(waterford)
  })
  
  
  ##### PLTW #####
  
  #### PLTW Elementary Classes ####
  ElementaryPLTW <- reactive({
    elemPLTW <- FileData()
    elemPLTW$schoolName <- substr(elemPLTW$student_calendarName, 7, length(elemPLTW$student_calendarName))
    
    
    start_date <- input$startDatePLTW
    end_date <- input$endDatePLTW
    
    
    elemPLTW %>%
      mutate(
        courseCode = case_when(
          student_grade == "KF" ~ "ELE_K",
          student_grade == "01" ~ "ELE_1",
          student_grade == "02" ~ "ELE_2",
          student_grade == "03" ~ "ELE_3",
          student_grade == "04" ~ "ELE_4",
          student_grade == "05" ~ "ELE_5",
          TRUE ~ "Error"
        ),
        startDate = start_date,
        endDate = end_date
      ) %>%
      filter(courseCode != "Error") %>%
      filter(schoolName == input$schoolNamePLTW) %>%
      select(
        "TEACHER EMAIL" = courseSection_TeacherEmail,
        "COURSE CODE" = courseCode,
        "COURSE BEGIN DATE" = startDate,
        "COURSE END DATE" = endDate,
        "STUDENT FIRST" = student_legalFirstName,
        "STUDENT LAST" = student_legalLastName,
        "STUDENT GRADE" = student_grade,
        "STUDENT STATE ID NUMBER" = student_stateID,
        "GENDER" = student_gender,
        "DOB" = student_birthdate
      )
  })
  
  
  ## Render the prepped data
  output$prepped <- renderTable({
    df <- FilePrep()
    
    return(df)
  })
  
  output$preppedIHT <- renderTable({
    
    if (input$SchoolDropdown == "Elementary") {
      return(ElementaryIHT())
    }
    else if (input$SchoolDropdown == "Junior High") {
      return(JrHighSchoolIHT())
    }
    else if (input$SchoolDropdown == "High School") {
      return(HighSchoolIHT())
    }
    else {
      return(NULL)
    }
    
  })
  
  
  #### EXPORT DATA ####
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$rosterType, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(FilePrep(), file, row.names = FALSE, na = "")
    }
  )
  
  #### UI ####
  
  ## select the level of institution for IHT
  output$schoolSelectIHT <- renderUI({

      
      items <- c("Elementary", "Junior High", "High School")
      selectInput("SchoolDropdown", "School", items)

  })
  
  ## select the teacher for elementary teachers in IHT
  output$teacherSelect <- renderUI({
    
    
    ## This gets the list of teachers
    
    

      df <- FileData() %>%
        as_tibble() %>%
        distinct(student_studentNumber, .keep_all = T) %>%
        mutate(student_grade = replace(student_grade, student_grade == "KF", "K")) %>%
        mutate(courseSection_teacherDisplay = str_replace_all(courseSection_teacherDisplay, ",", "-")) %>%
        separate(courseSection_teacherDisplay, into = c("peTeach", "elemteach"), sep = "-", extra = "drop")
      
      if (is.null(df)) {
        return(NULL)
      }
      if (input$SchoolDropdown != "Elementary") {
        return(NULL)
      }
      
      
      items <- unique(df$peTeach)
      selectInput("teacherDropdown", "Select the teacher", items)
    
  })
  
  ## Period select for high School IHT
  output$periodSelect <- renderUI({
    
    
   
      ## This gets the list of periods
      df <- FileData() %>%
        as_tibble() %>%
        distinct(student_studentNumber, .keep_all = T) %>%
        mutate(student_grade = replace(student_grade, student_grade == "KF", "K")) %>%
        mutate(courseSection_teacherDisplay = str_replace_all(courseSection_teacherDisplay, ",", "-")) %>%
        separate(function_IHTClassName, into = c("peTeach", "elemteach"), sep = 9, extra = "drop")
      
      if (is.null(df)) {
        return(NULL)
      }
      if (input$SchoolDropdown != "High School") {
        return(NULL)
      }
      
      
      items <- unique(df$peTeach)
      items <- items[order(nchar(items), items)]
      selectInput("periodDropdown", "Select the period", items)
   
  })
  
  
  ## Add the tabs that are ready to go
  observeEvent(input$append, {
    id <- paste0("Dropdown", input$append, "a")
    
    if (ReadyHMH() == T) {
      appendTab(
        inputId = "tabs",
        tabPanel(
          "HMH",
          # condition = "output.readyPLTW == true",
          titlePanel("HMH Rosters"),
          sidebarLayout(
            sidebarPanel(
              uiOutput("schoolSelectElemPLTW")
            ),
            
            ## this shows the uploaded data set
            mainPanel()
          )
        )
      )
    }
    if (ReadyIHT() == T) {
      appendTab(
        inputId = "tabs",
        tabPanel(
          "IHT",
          # condition = "output.readyPLTW == true",
          titlePanel("IHT Rosters"),
          sidebarLayout(
            sidebarPanel(
              uiOutput("schoolSelectIHT"),
              uiOutput("teacherSelect"),
              uiOutput("periodSelect")
            ),
            
            ## this shows the uploaded data set
            mainPanel(
              tableOutput("preppedIHT")
            )
          )
        )
      )
    }
    if (readyPLTW2() == T) {
      appendTab(
        inputId = "tabs",
        tabPanel(
          "PLTW",
          # condition = "output.readyPLTW == true",
          titlePanel("PLTW Rosters"),
          sidebarLayout(
            sidebarPanel(
              uiOutput("schoolSelectElemPLTW")
            ),
            
            ## this shows the uploaded data set
            mainPanel()
          )
        )
      )
    }
    if (ReadyWaterford() == T) {
      appendTab(
        inputId = "tabs",
        tabPanel(
          "Waterford",
          # condition = "output.readyPLTW == true",
          titlePanel("HMH Rosters"),
          sidebarLayout(
            sidebarPanel(
              uiOutput("schoolSelectElemPLTW")
            ),
            
            ## this shows the uploaded data set
            mainPanel()
          )
        )
      )
    }
    
    shinyjs::hide("append")
  })
  
  # eventReactive(input$file1, {
  #   if (readyPLTW2() == T) {
  #     insertTab(inputId = "tabs", )
  #   }
  #   else {
  #     hideTab(inputId = "tabs", target = "PLTW")
  #   }
  # })
  # 
  output$warningTitle <- renderUI({
    if (FileReady() == F) {
      return(NULL)
    }
    if (input$rosterType == "None") {
      
    }
    else {
      if (FileCorrect() == F) {
        titlePanel("MISSING ROWS")
      }
      else {
        titlePanel("All Rows Present")
      }
    }
  })
  
  ## renders the title for output page
  output$exportTitle <- renderUI({
    if (FileReady() == F) {
      return(NULL)
    }
    if (is.null(df)) {
      return(NULL)
    }
    
    if (input$rosterType == "IHT") {
      if (input$SchoolDropdown != "Junior High") {
        if (input$SchoolDropdown == "Elementary") {
          titlePanel(paste0(input$SchoolDropdown, " ", input$rosterType, ": ", input$teacherDropdown))
        }
        else {
          titlePanel(paste0(input$SchoolDropdown, " ", input$rosterType, ": ", input$periodDropdown))
        }
      }
      else {
        titlePanel(paste0(input$SchoolDropdown, " ", input$rosterType))
      }
    }
    return(NULL)
  })
  
  ## File options for HMH
  output$hmhFileOptions <- renderUI({
    if (FileReady() == F) {
      return(NULL)
    }
    if (FileCorrect() == F) {
      return(NULL)
    }
    
    
    if (input$rosterType == "HMH (Gov/Hist)") {
      items <- c("Class", "Users", "Class Assignments")
      radioButtons("hmhFileDropdown", "Select Output File", items)
    }
  })
  
  ## select the level of institution for PLTW
  output$schoolSelectElemPLTW <- renderUI({
    if (FileReady() == F) {
      return(NULL)
    }
    if (FileCorrect() == F) {
      return(NULL)
    }
    
    
    if (input$rosterType == "PLTW (Elementary)") {
      if (is.null(df)) {
        return(NULL)
      }
      elemPLTW <- FileData()
      
      elemPLTW$schoolName <- substr(elemPLTW$student_calendarName, 7, length(elemPLTW$student_calendarName))
      
      items <- unique(elemPLTW$schoolName)
      selectInput("schoolNamePLTW", "School", items)
    }
    else {
      return(NULL)
    }
  })
  
  
  
  
  ## does the file have all field for PLTW
  output$readyPLTW <- reactive({
    if (FileReady() == F) {
      return(FALSE)
    }
    
    check <- c(
      "student_grade", #
      "courseSection_TeacherEmail", #
      "student_calendarName", #
      "student_firstName", #
      "student_lastName", #
      "student_grade", #
      "student_stateID", #
      "student_gender", #
      "student_birthdate"
    )
    
    test <- check %in% names(FileData())
    
    if (all(test) == T) {
      return(TRUE)
    }
    else {
      return(FALSE)
    }
  })
  
  readyPLTW2 <- reactive({
    if (FileReady() == F) {
      return(NULL)
    }
    
    check <- c(
      "student_grade", #
      "courseSection_TeacherEmail", #
      "student_calendarName", #
      "student_firstName", #
      "student_lastName", #
      "student_grade", #
      "student_stateID", #
      "student_gender", #
      "student_birthdate"
    )
    
    
    test <- check %in% names(FileData())
    
    if (all(test) == T) {
      return(TRUE)
    }
    else {
      message("PLTW not ready to go")
      
      return(FALSE)
    }
  })
  
  ReadyIHT <- reactive({
    if (FileReady() == F) {
      return(NULL)
    }
    
    check <- c(
      "student_grade", #
      "student_homeroomTeacher", #
      "student_studentNumber", #
      "student_lastName", #
      "student_firstName", #
      "student_gender", #
      "student_birthdate", #
      "student_calendarName", #
      "courseSection_teacherDisplay", #
      "roster_endDate",
      "sectionSchedule_periodStart", #
      "sectionSchedule_scheduleStart", #
      "contacts_email", #
      "function_IHTClassName" #
    )
    
    
    test <- check %in% names(FileData())
    
    if (all(test) == TRUE) {
      return(TRUE)
    }
    else {
      message("IHT not ready to go")
      return(FALSE)
    }
  })
  
  ReadyHMH <- reactive({
    if (FileReady() == F) {
      return(NULL)
    }
    
    check <- c(
      "student_firstName",
      "student_lastName",
      "student_grade",
      "student_stateID",
      "courseSection_courseID",
      "student_endDate",
      "courseSection_courseNumber",
      "roster_endDate",
      "courseSection_teacherDisplay",
      "student_studentNumber",
      "courseSection_sectionNumber",
      "courseSection_courseName",
      "sectionSchedule_periodStart",
      "cal_endYear"
    )
    
    test <- check %in% names(FileData())
    
    if (all(test) == T) {
      return(TRUE)
    }
    else {
      message("HMH not ready to go")
      return(FALSE)
    }
    
  })
  
  ReadyWaterford <- reactive({
    if (FileReady() == F) {
      return(NULL)
    }
    
    check <- c(
      "student_firstName",
      "student_middleName",
      "student_lastName",
      "student_alias",
      "student_studentNumber",
      "student_grade",
      "student_gender",
      "student_birthdate",
      "student_calendarName",
      "student_homeroomTeacher"
    )
    test <- check %in% names(FileData())
    
    if (all(test) == T) {
      return(TRUE)
    }
    else {
      message("Waterford Not Ready To Go")
      return(FALSE)
    }
  })
  
  #### ERROR CHECKING ####
  
  ## is there a file ready and waiting?
  FileReady <- reactive({
    infile <- input$file1
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(FALSE)
    }
    return(TRUE)
  })
  
  FileCorrect <- reactive({
    if (FileReady() == F) {
      return(NULL)
    }
    
    
    if (input$rosterType == "IHT") {
      return(ReadyIHT())
    }
    else if (input$rosterType == "HMH (Gov/Hist)") {
      return(ReadyHMH())
    }
    else if (input$rosterType == "Waterford") {
      return(ReadyWaterford())
    }
    else if (input$rosterType == "PLTW (Elementary)"){
      return(readyPLTW2())
    }
    else {
      return(F)
    }
  })
  
  outputOptions(output, "readyPLTW", suspendWhenHidden = FALSE)
}
