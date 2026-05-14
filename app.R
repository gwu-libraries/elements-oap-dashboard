library(shiny)
library(tidyverse)
library(lubridate)
library(DT)
library(forcats)

# Load the dataset once on startup
pubs <- readr::read_csv("user_pubs.csv", show_col_types = FALSE) %>%
  mutate(
    publication_date = lubridate::as_date(publication_date),
    online_publication_date = lubridate::as_date(online_publication_date),
    display_date = coalesce(online_publication_date, publication_date),
    year = lubridate::year(display_date),
    author = paste(user.first_name, user.last_name),
    open_access_status_clean = if_else(
      is.na(open_access_status) | open_access_status == "",
      "Unknown",
      open_access_status
    )
  )

pubs <- pubs %>%
  mutate(open_access_status_clean = factor(open_access_status_clean,
                                           c('Unknown',
                                             'Closed Access',
                                             'Open Access',
                                             'Hybrid OA',
                                             'Bronze OA',
                                             'Gold OA',
                                             'Green OA')))

# Helpers for UI choices
departments <- pubs %>% pull(user.department) %>% unique() %>% sort()
users <- pubs %>% pull(author) %>% unique() %>% sort()
pub_types <- pubs %>% pull(pub_type) %>% unique() %>% sort()
oa_status <- pubs %>% pull(open_access_status_clean) %>% unique() %>% sort()
year_range <- range(pubs$year, na.rm = TRUE)

ui <- fluidPage(
  titlePanel("Publication Dashboard"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
#      checkboxGroupInput("selected_pub_type", "Publication Type:",
#                         choices = pub_types, selected = pub_types),
      checkboxGroupInput("selected_oa", "Open Access Status:",
                         choices = oa_status, selected = oa_status),
      sliderInput("selected_year", "Publication Year:",
                  min = 2000, max = year_range[2],
                  value = year_range, sep = ""),
      hr(),
#      checkboxInput("show_only_missing_journal", "Show only records missing journal title", FALSE),
      downloadButton("download_filtered", "Download Filtered Data")
    ),
    mainPanel(
      width = 9,
      fluidRow(
        column(6,
               wellPanel(
                 h4("Total records"),
                 textOutput("total_records")
               )
        ),
        column(6,
               wellPanel(
                 h4("Unique authors"),
                 textOutput("total_authors")
               )
        )
      ),
      fluidRow(
#        column(6,
#               plotOutput("pubs_by_type", height = "320px")
#        ),
        column(6,
               plotOutput("oa_by_year", height = "320px")
        ),
        column(6,
               plotOutput("pubs_by_year", height = "320px")
        )
      ),
      fluidRow(
        column(12,
               plotOutput("oa_distribution", height = "320px")
        )
      ),
      fluidRow(
        column(12,
               h4("Top authors by publication count"),
               DTOutput("top_authors")
        )
      ),
      fluidRow(
        column(12,
               h4("Filtered publications"),
               DTOutput("filtered_table")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  filtered_data <- reactive({
    data <- pubs

    if (length(input$selected_pub_type) > 0) {
      data <- data %>% filter(pub_type %in% input$selected_pub_type)
    }
    if (length(input$selected_oa) > 0) {
      data <- data %>% filter(open_access_status_clean %in% input$selected_oa)
    }
    if (!is.null(input$selected_year)) {
      data <- data %>% filter(year >= input$selected_year[1], year <= input$selected_year[2])
    }
    # if (input$show_only_missing_journal) {
    #   data <- data %>% filter(is.na(journal_title) | journal_title == "")
    # }
    data
  })

  output$total_records <- renderText({
    scales::comma(nrow(filtered_data()))
  })

  output$total_authors <- renderText({
    scales::comma(filtered_data() %>% pull(author) %>% unique() %>% length())
  })

  # output$pubs_by_type <- renderPlot({
  #   filtered_data() %>%
  #     count(pub_type, name = "count") %>%
  #     ggplot(aes(x = reorder(pub_type, count), y = count, fill = pub_type)) +
  #     geom_col(show.legend = FALSE) +
  #     coord_flip() +
  #     labs(x = "Publication Type", y = "Count", title = "Publications by Type") +
  #     theme_minimal()
  # })

  output$pubs_by_year <- renderPlot({
    filtered_data() %>%
      count(year, name = "count") %>%
      ggplot(aes(x = year, y = count)) +
      geom_line(color = "#2c7fb8", size = 1.2) +
      geom_point(color = "#2c7fb8", size = 2) +
      labs(x = "Year", y = "Count", title = "Publication Trend by Year") +
      theme_minimal()
  })
  
  output$oa_by_year <- renderPlot({
    plot_data <- filtered_data() %>%
      count(year, open_access_status_clean, name = "count")
    
    plot_data %>%
      ggplot(aes(x = year, y = count, fill = open_access_status_clean)) +
      geom_area() +
  #    labs(x = "Year", y = "Count", title = "Publication Trend by Year") +
      theme_minimal() +
      scale_fill_manual(
        values = c(
          "Green OA" = "#238b45",
          "Gold OA" = "#d4af37",
          "Bronze OA" = "#cd7f32",
          "Unknown" = "grey",
          "Closed Access" = "red",
          "Open Access" = "blue",
          "Hybrid OA" = "purple"
        ),
        na.value = "#999999"
      ) +
      scale_x_continuous(
        breaks = seq(min(plot_data$year), max(plot_data$year), by = 1)
      )
  })

  output$oa_distribution <- renderPlot({
    filtered_data() %>%
      count(open_access_status_clean, name = "count") %>%
      ggplot(aes(x = reorder(open_access_status_clean, count), y = count, fill = open_access_status_clean)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      scale_fill_manual(
        values = c(
          "Green OA" = "#238b45",
          "Gold OA" = "#d4af37",
          "Bronze OA" = "#cd7f32",
          "Unknown" = "grey",
          "Closed Access" = "red",
          "Open Access" = "blue",
          "Hybrid OA" = "purple"
        ),
        na.value = "#999999"
      ) +
      labs(x = "Open Access Status", y = "Count", title = "Open Access Status Distribution") +
      theme_minimal(base_size = 14) +
      theme(
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 16, face = "bold")
      )
  })

  output$top_authors <- renderDT({
    filtered_data() %>%
      count(author, name = "publication_count") %>%
      arrange(desc(publication_count)) %>%
      slice_head(n = 15) %>%
      datatable(
        rownames = FALSE,
        options = list(pageLength = 15, dom = 'tp')
      )
  })

  output$filtered_table <- renderDT({
    filtered_data() %>%
      select(
        user.id, user.email_address, author, user.position,
        pub.id, pub_type, doi, title, journal_title, issn, eissn,
        open_access_status = open_access_status_clean, display_date, year
      ) %>%
      datatable(
        rownames = FALSE,
        filter = 'top',
        options = list(pageLength = 25, autoWidth = TRUE)
      )
  })

  output$download_filtered <- downloadHandler(
    filename = function() {
      paste0("filtered_publications_", Sys.Date(), ".csv")
    },
    content = function(file) {
      readr::write_csv(filtered_data(), file)
    }
  )
}

shinyApp(ui = ui, server = server)
