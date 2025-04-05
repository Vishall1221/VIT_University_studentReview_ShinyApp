# server.R

library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(stringr)
library(lubridate)
library(DBI)
library(RSQLite)
library(plotly)
library(ggplot2)
library(syuzhet)

# Load data
df <- read.csv("FINALVIT_Reviews_Students_Data.csv", stringsAsFactors = FALSE)

# Extract ID from Name and clean Name
df <- df %>%
  mutate(
    extracted_id = as.numeric(str_extract(Name, "^\\d+")),
    ID = ifelse(!is.na(extracted_id), extracted_id, row_number()),
    Name = str_trim(str_remove(Name, "^\\d+"))
  ) %>%
  select(-extracted_id) %>%
  relocate(ID, .before = Name)

# Initialize and write to SQLite database
conn <- dbConnect(SQLite(), "reviews.db")
dbExecute(conn, "DROP TABLE IF EXISTS reviews")
dbExecute(conn, "CREATE TABLE reviews (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  Name TEXT, 
  Rating REAL, 
  Degree TEXT, 
  Specialization TEXT,
  Review_Date TEXT, 
  Review_Text TEXT, 
  Word_Count INTEGER,
  Sentiment_Label TEXT, 
  Sentiment_Score REAL
)")
dbWriteTable(conn, "reviews", df, append = TRUE, row.names = FALSE)
dbDisconnect(conn)


ui <- dashboardPage(
  dashboardHeader(title = "VIT Reviews Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Add Review", tabName = "add", icon = icon("plus")),
      menuItem("Update Review", tabName = "update", icon = icon("edit")),
      menuItem("Delete Review", tabName = "delete", icon = icon("trash")),
      menuItem("Visualizations", tabName = "viz", icon = icon("chart-bar"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .dataTables_wrapper {
          width: 100%;
        }
        .dataTables_scrollBody {
          min-height: 500px;
        }
      "))
    ),
    tabItems(
      tabItem(tabName = "home",
              fluidRow(
                box(title = "Student Reviews", width = 12, status = "primary",
                    checkboxInput("show_id", "Show ID Column", value = FALSE),
                    DTOutput("reviews_table"))
              )
      ),
      
      tabItem(tabName = "add",
              fluidRow(
                column(width = 3),
                column(width = 6,
                       box(title = "Add Review", status = "primary", solidHeader = TRUE, width = 12,
                           textInput("new_name", "Name"),
                           numericInput("new_rating", "Rating", value = 0, min = 0, max = 5),
                           textInput("new_degree", "Degree"),
                           textInput("new_spec", "Specialization"),
                           dateInput("new_date", "Review Date"),
                           textAreaInput("new_text", "Review Text"),
                           actionButton("add_btn", "Submit Review", class = "btn-primary")
                       )
                ),
                column(width = 3)
              )
      ),
      
      tabItem(tabName = "update",
              fluidRow(
                box(title = "Update Review", width = 6, status = "warning",
                    numericInput("update_id", "Enter Review ID to Update", value = NA, min = 1),
                    verbatimTextOutput("existing_review_info"),
                    textAreaInput("update_text", "New Review Text", "", width = "100%"),
                    actionButton("update_btn", "Update Review", class = "btn-warning")
                )
              )
      ),
      
      tabItem(tabName = "delete",
              fluidRow(
                box(title = "Delete Review", width = 6, status = "danger",
                    numericInput("delete_id", "Enter Review ID to Delete", value = NA, min = 1),
                    actionButton("delete_btn", "Delete Review", class = "btn-danger")
                )
              )
      ),
      
      tabItem(tabName = "viz",
              fluidRow(
                box(title = "Filter Options", width = 12, status = "warning",
                    selectInput("year_filter", "Select Year:", choices = NULL, selected = "ALL")
                )
              ),
              fluidRow(
                box(title = "Monthly Rating Scatter Plot", width = 12, status = "info",
                    plotlyOutput("scatter_plot", height = "500px"))
              ),
              fluidRow(
                box(title = "Average Rating per Degree (Scrollable)", width = 12, status = "success",
                    selectInput("degree_sort", "Sort by:",
                                choices = c("Random", "Lowest to Highest", "Highest to Lowest"), selected = "Random"),
                    plotlyOutput("barplot_degree_scrollable", height = "500px"))
              ),
              fluidRow(
                box(title = "Trend Over Time by B.Tech Top Specialization", width = 12, status = "primary",
                    selectInput("time_period", "Time Period:",
                                choices = c("Past 1 Year", "Past 2 Years", "Past 3 Years", "All Time"), 
                                selected = "Past 1 Year"),
                    plotlyOutput("trend_line_chart", height = "500px"))
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  rv <- reactiveValues(data = df)
  
  analyze_sentiment <- function(text) {
    score <- get_sentiment(text, method = "syuzhet")
    label <- ifelse(score > 0, "Positive", ifelse(score < 0, "Negative", "Neutral"))
    return(list(label = label, score = score))
  }
  
  output$reviews_table <- renderDT({
    data <- rv$data
    if (!input$show_id) {
      data <- data[, !names(data) %in% "ID"]
    }
    datatable(data, 
              options = list(
                scrollX = TRUE,
                scrollY = "500px",
                pageLength = 10,
                autoWidth = TRUE
              ), 
              selection = "none",
              fillContainer = TRUE)
  })
  
  observeEvent(input$add_btn, {
    new_id <- ifelse(nrow(rv$data) == 0, 1, max(rv$data$ID, na.rm = TRUE) + 1)
    sentiment <- analyze_sentiment(input$new_text)
    new_row <- data.frame(
      ID = new_id,
      Name = input$new_name,
      Rating = input$new_rating,
      Degree = input$new_degree,
      Specialization = input$new_spec,
      Review_Date = as.character(input$new_date),
      Review_Text = input$new_text,
      Word_Count = str_count(input$new_text, "\\S+"),
      Sentiment_Label = sentiment$label,
      Sentiment_Score = sentiment$score
    )
    rv$data <- bind_rows(rv$data, new_row)
    df <<- rv$data
    write.csv(rv$data, "FINALVIT_Reviews_Students_Data.csv", row.names = FALSE)
    showNotification("Review added successfully!", type = "message")
  })
  
  observeEvent(input$delete_btn, {
    if (!is.na(input$delete_id) && input$delete_id %in% rv$data$ID) {
      rv$data <- rv$data[rv$data$ID != input$delete_id, ]
      df <<- rv$data
      write.csv(rv$data, "FINALVIT_Reviews_Students_Data.csv", row.names = FALSE)
      showNotification("Review deleted successfully.", type = "message")
    } else {
      showNotification("ID not found.", type = "error")
    }
  })
  
  observeEvent(input$update_btn, {
    if (!is.na(input$update_id) && input$update_id %in% rv$data$ID) {
      rv$data$Review_Text[rv$data$ID == input$update_id] <- input$update_text
      sentiment <- analyze_sentiment(input$update_text)
      rv$data$Sentiment_Label[rv$data$ID == input$update_id] <- sentiment$label
      rv$data$Sentiment_Score[rv$data$ID == input$update_id] <- sentiment$score
      rv$data$Word_Count[rv$data$ID == input$update_id] <- str_count(input$update_text, "\\S
 +")
      df <<- rv$data
      write.csv(rv$data, "FINALVIT_Reviews_Students_Data.csv", row.names = FALSE)
      showNotification("Review updated successfully.", type = "message")
    } else {
      showNotification("ID not found.", type = "error")
    }
  })
  
  observeEvent(input$update_id, {
    req(input$update_id)
    if (input$update_id %in% rv$data$ID) {
      selected_review <- rv$data[rv$data$ID == input$update_id, ]
      updateTextInput(session, "update_text", value = selected_review$Review_Text)
      output$existing_review_info <- renderText({
        paste0(
          "Name: ", selected_review$Name, "\n",
          "Rating: ", selected_review$Rating, "\n",
          "Degree: ", selected_review$Degree, "\n",
          "Specialization: ", selected_review$Specialization, "\n",
          "Date: ", selected_review$Review_Date, "\n",
          "Sentiment: ", selected_review$Sentiment_Label, " (", round(selected_review$Sentiment_Score, 2), ")"
        )
      })
    } else {
      output$existing_review_info <- renderText({ "ID not found." })
      updateTextInput(session, "update_text", value = "")
    }
  })
  
  observe({
    all_years <- sort(unique(format(as.Date(rv$data$Review_Date), "%Y")))
    updateSelectInput(session, "year_filter",
                      choices = c("ALL", all_years),
                      selected = "ALL")
  })
  
  output$scatter_plot <- renderPlotly({
    df_filtered <- rv$data
    if (input$year_filter != "ALL") {
      df_filtered <- df_filtered[format(as.Date(df_filtered$Review_Date), "%Y") == input$year_filter, ]
    }
    df_filtered$Month <- month(as.Date(df_filtered$Review_Date), label = TRUE, abbr = TRUE)
    df_filtered$Color <- ifelse(df_filtered$Rating > 4, "green",
                                ifelse(df_filtered$Rating >= 3, "yellow", "red"))
    
    p <- ggplot(df_filtered, aes(x = Month, y = Rating, color = Color)) +
      geom_point(size = 3, alpha = 0.7) +
      scale_color_identity() +
      labs(title = "Monthly Rating Scatter Plot", x = "Month", y = "Rating") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$barplot_degree_scrollable <- renderPlotly({
    df_bar <- rv$data %>%
      group_by(Degree) %>%
      summarise(Average_Rating = mean(Rating),
                Count = n(),
                Avg_Sentiment = mean(Sentiment_Score, na.rm = TRUE))
    
    if (input$degree_sort == "Lowest to Highest") {
      df_bar <- df_bar %>% arrange(Average_Rating)
      df_bar$Degree <- factor(df_bar$Degree, levels = df_bar$Degree)
    } else if (input$degree_sort == "Highest to Lowest") {
      df_bar <- df_bar %>% arrange(desc(Average_Rating))
      df_bar$Degree <- factor(df_bar$Degree, levels = df_bar$Degree)
    } else {
      random_order <- sample(df_bar$Degree)
      df_bar$Degree <- factor(df_bar$Degree, levels = random_order)
    }
    
    p <- ggplot(df_bar, aes(x = Degree, y = Average_Rating, fill = Count)) +
      geom_bar(stat = "identity") +
      scale_fill_gradient(low = "lightblue", high = "blue") +
      labs(title = "Average Rating per Degree", x = "Degree", y = "Average Rating", fill = "R
 eview Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  output$trend_line_chart <- renderPlotly({
    time_limit <- switch(input$time_period,
                         "Past 1 Year" = Sys.Date() - 365,
                         "Past 2 Years" = Sys.Date() - 365 * 2,
                         "Past 3 Years" = Sys.Date() - 365 * 3,
                         "All Time" = as.Date("1900-01-01"))
    
    # List of only the first 3 specializations we want to show
    target_specializations <- c(
      "Computer Science and Engineering",
      "Electronics & Communication Engineering",
      "Mechanical Engineering"
    )
    
    df_trend <- rv$data %>%
      filter(as.Date(Review_Date) >= time_limit) %>%
      # Filter to only include our target specializations
      filter(Specialization %in% target_specializations) %>%
      group_by(Specialization, Month = floor_date(as.Date(Review_Date), "month")) %>%
      summarise(Count = n(), Avg_Rating = mean(Rating))
    
    p <- ggplot(df_trend, aes(x = Month, y = Avg_Rating, color = Specialization)) +
      geom_line(size = 1) +
      geom_point(size = 3, alpha = 0.7) + labs(title = "Engineering Specialization Trends Over Time", 
                                               x = "Month", 
                                               y = "Average Rating") +
      theme_minimal() +
      scale_color_manual(values = c(
        "Computer Science and Engineering" = "darkblue",  # blue
        "Electronics & Communication Engineering" = "orange",  # pink/red
        "Mechanical Engineering" = "yellow"  # amber/yellow
      ))
    ggplotly(p)
  })
}


shinyApp(ui, server)