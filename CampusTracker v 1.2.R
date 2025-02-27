library(shiny)
library(openxlsx)

# Define UI
ui <- fluidPage(
  titlePanel("CampusTracker 1.2"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Previous Excel File", accept = ".xlsx"),
      fileInput("team_file", "Upload Team List (.txt)", accept = ".txt"),
      uiOutput("team_dropdown"),
      textInput("record", "Record"),
      textInput("conference_record", "Conference Record"),
      selectInput("conference_tournament", "Conference Tournament?", choices = c("Yes", "No")),
      selectInput("national_tournament", "National Tournament?", choices = c("Yes", "No")),
      actionButton("submit", "Add Entry"),
      actionButton("edit_last", "Edit Last Entry"),
      downloadButton("download", "Download Excel")
    ),
    mainPanel(
      tableOutput("data_table")
    )
  )
)

# Define Server
server <- function(input, output, session) {
  data <- reactiveVal(data.frame(Year = integer(), Team = character(), Record = character(), Conference_Record = character(), Conference_Tournament = character(), National_Tournament = character(), stringsAsFactors = FALSE))
  teams <- reactiveVal(NULL)
  
  observeEvent(input$team_file, {
    req(input$team_file)
    team_list <- readLines(input$team_file$datapath, skip = 2)  # Skip first two rows
    team_names <- sapply(strsplit(team_list, ","), function(x) x[1])  # Extract first entry before delimiter
    teams(team_names)
  })
  
  output$team_dropdown <- renderUI({
    selectInput("team", "Team", choices = teams(), selected = NULL)
  })
  
  observeEvent(input$file, {
    req(input$file)
    uploaded_data <- read.xlsx(input$file$datapath)
    data(uploaded_data)
  })
  
  observeEvent(input$submit, {
    new_year <- ifelse(nrow(data()) == 0, 2019, max(data()$Year) + 1)
    new_entry <- data.frame(Year = new_year, Team = input$team, Record = input$record, Conference_Record = input$conference_record, Conference_Tournament = input$conference_tournament, National_Tournament = input$national_tournament, stringsAsFactors = FALSE)
    data(rbind(data(), new_entry))
  })
  
  observeEvent(input$edit_last, {
    req(nrow(data()) > 0)
    last_entry <- tail(data(), 1)
    updateSelectInput(session, "team", selected = last_entry$Team)
    updateTextInput(session, "record", value = last_entry$Record)
    updateTextInput(session, "conference_record", value = last_entry$Conference_Record)
    updateSelectInput(session, "conference_tournament", selected = last_entry$Conference_Tournament)
    updateSelectInput(session, "national_tournament", selected = last_entry$National_Tournament)
    data(data()[-nrow(data()), ])  # Remove last entry so it can be re-entered
  })
  
  output$data_table <- renderTable({
    data()
  })
  
  output$download <- downloadHandler(
    filename = function() { "entries.xlsx" },
    content = function(file) {
      write.xlsx(data(), file, rowNames = FALSE)
    }
  )
}

# Run App
shinyApp(ui, server)
