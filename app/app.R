options(renv.config.pak.enabled = TRUE)
library(renv)
library(pak)

library(shiny)
library(DBI)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(odbc)

# Create UI
ui <- dashboardPage(
  dashboardHeader(title = "ontime Flight Delay Explorer"),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem(
        "Flight Delays",
        tabName = "flight_delays",
        icon = icon("clock")
      ),
      menuItem(
        "Route Analysis",
        tabName = "route_analysis",
        icon = icon("plane")
      ),
      menuItem(
        "Airline Comparison",
        tabName = "airline_comparison",
        icon = icon("building")
      )
    ),

    # Filter sidebar
    div(
      style = "padding: 15px;",
      h4("Filters"),
      dateRangeInput(
        "date_range",
        "Date Range:",
        start = Sys.Date() - 30,
        end = Sys.Date()
      ),
      selectInput(
        "airline",
        "Airline:",
        choices = c("All", "AA", "DL", "UA", "WN", "AS", "B6", "NK", "F9"),
        selected = "All"
      ),
      selectInput(
        "weather",
        "Weather Conditions:",
        choices = c("All", "Clear", "Rain", "Snow", "Fog", "Thunderstorm"),
        selected = "All"
      ),
      actionButton("apply_filters", "Apply Filters", class = "btn-primary")
    ),

    div(
      style = "padding: 15px;",
      p(
        "The data included in this dashboard is synthetic and generated for demonstration purposes only."
      )
    )
  ),

  dashboardBody(
    tabItems(
      # Overview Tab
      tabItem(
        tabName = "overview",
        fluidRow(
          valueBoxOutput("total_flights_box"),
          valueBoxOutput("avg_delay_box"),
          valueBoxOutput("ontime_percent_box")
        ),
        fluidRow(
          box(
            title = "Delays by Day of Week",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            plotOutput("delay_by_day")
          ),
          box(
            title = "Delays by Weather Condition",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            plotOutput("delay_by_weather")
          )
        )
      ),

      # Flight Delays Tab
      tabItem(
        tabName = "flight_delays",
        fluidRow(
          box(
            title = "Delay Distribution",
            status = "primary",
            solidHeader = TRUE,
            plotOutput("delay_distribution")
          ),
          box(
            title = "Delay Patterns Over Time",
            status = "primary",
            solidHeader = TRUE,
            plotOutput("delay_time_series")
          )
        ),
        fluidRow(
          box(
            title = "Flights with Longest Delays",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            DT::dataTableOutput("delay_table")
          )
        )
      ),

      # Route Analysis Tab
      tabItem(
        tabName = "route_analysis",
        fluidRow(
          box(
            title = "Routes with Most Delays",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotOutput("route_delays")
          )
        ),
        fluidRow(
          box(
            title = "Route Details",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            DT::dataTableOutput("route_table")
          )
        )
      ),

      # Airline Comparison Tab
      tabItem(
        tabName = "airline_comparison",
        fluidRow(
          box(
            title = "Airline Performance Comparison",
            status = "primary",
            solidHeader = TRUE,
            plotOutput("airline_comparison")
          ),
          box(
            title = "Airline On-Time Percentages",
            status = "primary",
            solidHeader = TRUE,
            plotOutput("airline_ontime")
          )
        )
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  # Reactive value to track when filters are applied
  filters_applied <- reactiveVal(0)

  # When the apply filters button is clicked, increment the counter
  observeEvent(input$apply_filters, {
    filters_applied(filters_applied() + 1)
  })

  # Connect to the database using DSN when app starts
  # Important: This requires setting up a DSN named "ontime_db" in ODBC settings
  conn <- reactive({
    # Database connection with error handling
    tryCatch(
      {
        dbConnect(odbc::odbc(), "ontime_db")
      },
      error = function(e) {
        showNotification(
          paste("Database connection error:", e$message),
          type = "error",
          duration = NULL
        )
        return(NULL)
      }
    )
  })

  # Close database connection when app exits
  onSessionEnded(function() {
    if (!is.null(conn())) {
      dbDisconnect(conn())
    }
  })

  # Get filtered data
  flights_data <- reactive({
    # Only run query when filters are applied or first load
    filters_applied()

    if (is.null(conn())) {
      return(data.frame())
    }

    # Build SQL query based on filters
    query <- "SELECT f.*, a.name as airline_name,
              o.name as origin_name, o.city as origin_city, o.state as origin_state,
              d.name as destination_name, d.city as destination_city, d.state as destination_state
              FROM flights f
              JOIN airlines a ON f.airline_code = a.code
              JOIN airports o ON f.origin_airport = o.code
              JOIN airports d ON f.destination_airport = d.code
              WHERE 1=1"

    # Add date range filter
    if (!is.null(input$date_range)) {
      query <- paste0(
        query,
        " AND f.flight_date BETWEEN '",
        input$date_range[1],
        "' AND '",
        input$date_range[2],
        "'"
      )
    }

    # Add airline filter
    if (input$airline != "All") {
      query <- paste0(query, " AND f.airline_code = '", input$airline, "'")
    }

    # Add weather filter
    if (input$weather != "All") {
      query <- paste0(
        query,
        " AND f.weather_conditions = '",
        input$weather,
        "'"
      )
    }

    # Limit to 10000 records for performance
    query <- paste0(query, " LIMIT 10000")

    # Execute query
    tryCatch(
      {
        dbGetQuery(conn(), query)
      },
      error = function(e) {
        showNotification(
          paste("Query error:", e$message),
          type = "error",
          duration = NULL
        )
        return(data.frame())
      }
    )
  })

  # Overview tab outputs
  output$total_flights_box <- renderValueBox({
    data <- flights_data()
    if (nrow(data) == 0) {
      return(valueBox(0, "Flights", icon = icon("plane"), color = "blue"))
    }

    valueBox(
      nrow(data),
      "Total Flights",
      icon = icon("plane"),
      color = "blue"
    )
  })

  output$avg_delay_box <- renderValueBox({
    data <- flights_data()
    if (nrow(data) == 0) {
      return(valueBox(
        "0 min",
        "Avg Delay",
        icon = icon("clock"),
        color = "yellow"
      ))
    }

    avg_delay <- round(mean(data$arrival_delay, na.rm = TRUE), 1)
    valueBox(
      paste0(avg_delay, " min"),
      "Avg Arrival Delay",
      icon = icon("clock"),
      color = if (avg_delay > 15) "red" else "yellow"
    )
  })

  output$ontime_percent_box <- renderValueBox({
    data <- flights_data()
    if (nrow(data) == 0) {
      return(valueBox("0%", "On Time", icon = icon("check"), color = "green"))
    }

    on_time <- sum(data$arrival_delay <= 15, na.rm = TRUE)
    on_time_pct <- round(100 * on_time / nrow(data), 1)
    valueBox(
      paste0(on_time_pct, "%"),
      "On Time Percentage",
      icon = icon("check"),
      color = if (on_time_pct >= 80) "green" else "orange"
    )
  })

  output$delay_by_day <- renderPlot({
    data <- flights_data()
    if (nrow(data) == 0) {
      return(NULL)
    }

    # Convert day_of_week to factor to preserve order
    data$day_of_week <- factor(
      data$day_of_week,
      levels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
    )

    avg_delays <- data %>%
      group_by(day_of_week) %>%
      summarise(avg_delay = mean(arrival_delay, na.rm = TRUE))

    ggplot(
      avg_delays,
      aes(x = day_of_week, y = avg_delay, fill = day_of_week)
    ) +
      geom_col() +
      labs(x = "Day of Week", y = "Average Delay (minutes)") +
      theme_minimal() +
      theme(legend.position = "none") +
      scale_fill_brewer(palette = "Blues")
  })

  output$delay_by_weather <- renderPlot({
    data <- flights_data()
    if (nrow(data) == 0) {
      return(NULL)
    }

    avg_delays <- data %>%
      group_by(weather_conditions) %>%
      summarise(avg_delay = mean(arrival_delay, na.rm = TRUE), count = n()) %>%
      arrange(desc(avg_delay))

    ggplot(
      avg_delays,
      aes(
        x = reorder(weather_conditions, avg_delay),
        y = avg_delay,
        fill = count
      )
    ) +
      geom_col() +
      coord_flip() +
      labs(x = "Weather Condition", y = "Average Delay (minutes)") +
      theme_minimal() +
      scale_fill_gradient(low = "lightblue", high = "darkblue")
  })

  # Flight Delays tab outputs
  output$delay_distribution <- renderPlot({
    data <- flights_data()
    if (nrow(data) == 0) {
      return(NULL)
    }

    ggplot(data, aes(x = arrival_delay)) +
      geom_histogram(fill = "steelblue", bins = 30) +
      labs(x = "Arrival Delay (minutes)", y = "Number of Flights") +
      theme_minimal() +
      xlim(-20, 120) # Focus on the most common delay range
  })

  output$delay_time_series <- renderPlot({
    data <- flights_data()
    if (nrow(data) == 0) {
      return(NULL)
    }

    daily_delays <- data %>%
      group_by(flight_date) %>%
      summarise(avg_delay = mean(arrival_delay, na.rm = TRUE))

    ggplot(daily_delays, aes(x = flight_date, y = avg_delay)) +
      geom_line(color = "steelblue") +
      geom_smooth(method = "loess", span = 0.2, se = FALSE, color = "darkred") +
      labs(x = "Date", y = "Average Delay (minutes)") +
      theme_minimal()
  })

  output$delay_table <- DT::renderDataTable({
    data <- flights_data()
    if (nrow(data) == 0) {
      return(NULL)
    }

    # Get top delayed flights
    top_delays <- data %>%
      filter(arrival_delay > 0) %>%
      select(
        flight_date,
        flight_number,
        airline_name,
        origin_city,
        destination_city,
        departure_delay,
        arrival_delay,
        weather_conditions
      ) %>%
      arrange(desc(arrival_delay)) %>%
      head(50)

    DT::datatable(top_delays, options = list(pageLength = 10))
  })

  # Route Analysis tab outputs
  output$route_delays <- renderPlot({
    data <- flights_data()
    if (nrow(data) == 0) {
      return(NULL)
    }

    # Create route name
    data$route <- paste(data$origin_airport, "→", data$destination_airport)

    # Analyze routes
    route_stats <- data %>%
      group_by(route) %>%
      summarise(
        avg_delay = mean(arrival_delay, na.rm = TRUE),
        count = n()
      ) %>%
      filter(count >= 10) %>% # Only routes with sufficient data
      arrange(desc(avg_delay)) %>%
      head(15)

    ggplot(
      route_stats,
      aes(x = reorder(route, avg_delay), y = avg_delay, fill = count)
    ) +
      geom_col() +
      coord_flip() +
      labs(x = "Route", y = "Average Delay (minutes)", fill = "Flight Count") +
      theme_minimal() +
      scale_fill_gradient(low = "lightblue", high = "darkblue")
  })

  output$route_table <- DT::renderDataTable({
    data <- flights_data()
    if (nrow(data) == 0) {
      return(NULL)
    }

    # Create route name
    data$route <- paste(data$origin_city, "to", data$destination_city)

    # Analyze routes
    route_stats <- data %>%
      group_by(route, origin_airport, destination_airport) %>%
      summarise(
        flights = n(),
        avg_delay = round(mean(arrival_delay, na.rm = TRUE), 1),
        max_delay = max(arrival_delay, na.rm = TRUE),
        on_time_pct = round(
          100 * sum(arrival_delay <= 15, na.rm = TRUE) / n(),
          1
        )
      ) %>%
      arrange(desc(avg_delay))

    DT::datatable(route_stats, options = list(pageLength = 10))
  })

  # Airline Comparison tab outputs
  output$airline_comparison <- renderPlot({
    data <- flights_data()
    if (nrow(data) == 0) {
      return(NULL)
    }

    airline_stats <- data %>%
      group_by(airline_name) %>%
      summarise(
        avg_delay = mean(arrival_delay, na.rm = TRUE),
        count = n()
      ) %>%
      arrange(avg_delay)

    ggplot(
      airline_stats,
      aes(x = reorder(airline_name, avg_delay), y = avg_delay, fill = count)
    ) +
      geom_col() +
      coord_flip() +
      labs(x = "Airline", y = "Average Delay (minutes)") +
      theme_minimal() +
      scale_fill_gradient(low = "lightblue", high = "darkblue")
  })

  output$airline_ontime <- renderPlot({
    data <- flights_data()
    if (nrow(data) == 0) {
      return(NULL)
    }

    airline_ontime <- data %>%
      group_by(airline_name) %>%
      summarise(
        on_time_pct = 100 * sum(arrival_delay <= 15, na.rm = TRUE) / n()
      ) %>%
      arrange(desc(on_time_pct))

    ggplot(
      airline_ontime,
      aes(
        x = reorder(airline_name, on_time_pct),
        y = on_time_pct,
        fill = on_time_pct
      )
    ) +
      geom_col() +
      coord_flip() +
      labs(x = "Airline", y = "On-Time Percentage") +
      theme_minimal() +
      scale_fill_gradient(low = "orange", high = "darkgreen") +
      theme(legend.position = "none")
  })
}

# Run the app
shinyApp(ui = ui, server = server)
