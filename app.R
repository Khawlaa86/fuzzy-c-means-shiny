# ======================
# Fuzzy c_means   elabore par khawla Rahali
# ======================
library(shiny)
library(shinydashboard)
library(e1071)
library(ggplot2)
library(dplyr)
library(readr)
library(plotly)

# ======================
# Load data
# ======================
data <- read_csv(
  "C:/Users/khawla/OneDrive/Desktop/mall_customers.csv",
  show_col_types = FALSE
)

# ======================
# UI
# ======================
ui <- dashboardPage(
  dashboardHeader(title = "Fuzzy C-Means Evaluation"),
  
  dashboardSidebar(
    sliderInput("clusters", "Number of clusters", 2, 8, 5),
    sliderInput("m", "Fuzziness (m)", 1.1, 3, 2, step = 0.1),
    sliderInput(
      "customer_id",
      "Customer ID",
      min = min(data$customer_id),
      max = max(data$customer_id),
      value = min(data$customer_id),
      step = 1
    ),
    actionButton("run", "Run FCM")
  ),
  
  dashboardBody(
    
    fluidRow(
      valueBoxOutput("entropy")
    ),
    
    fluidRow(
      box(
        title = "Cluster Plot",
        width = 8,
        plotlyOutput("plot", height = "500px")
      ),
      box(
        title = "Cluster Centers",
        width = 4,
        tableOutput("centers")
      )
    ),
    
    fluidRow(
      box(
        title = "Membership Probabilities (Selected Customer)",
        width = 12,
        tableOutput("membership")
      )
    )
  )
)

# ======================
# SERVER
# ======================
server <- function(input, output) {
  
  # ---- Run FCM ----
  fcm <- eventReactive(input$run, {
    
    X <- data[, c("annual_income", "spending_score")]
    X_scaled <- scale(X)
    
    cmeans(
      X_scaled,
      centers = input$clusters,
      m = input$m,
      iter.max = 1000
    )
  })
  
  # ======================
  # Plot clusters
  # ======================
  output$plot <- renderPlotly({
    req(fcm())
    
    clusters <- fcm()$cluster
    
    df <- data.frame(
      income = data$annual_income,
      score = data$spending_score,
      cluster = factor(clusters),
      text = paste(
        "Customer ID:", data$customer_id,
        "<br>Income:", data$annual_income,
        "<br>Score:", data$spending_score
      )
    )
    
    X <- as.matrix(data[, c("annual_income", "spending_score")])
    centers_scaled <- fcm()$centers
    
    centers <- data.frame(
      income = centers_scaled[,1] * sd(X[,1]) + mean(X[,1]),
      score  = centers_scaled[,2] * sd(X[,2]) + mean(X[,2])
    )
    
    p <- ggplot(df, aes(income, score, color = cluster)) +
      geom_point(aes(text = text), size = 4, alpha = 0.8) +
      geom_point(
        data = centers,
        aes(income, score),
        color = "black",
        shape = 4,
        size = 6,
        stroke = 2
      ) +
      theme_minimal() +
      ggtitle("Fuzzy C-Means Clustering")
    
    ggplotly(p, tooltip = "text")
  })
  
  # ======================
  # Cluster centers table
  # ======================
  output$centers <- renderTable({
    req(fcm())
    
    X <- as.matrix(data[, c("annual_income", "spending_score")])
    centers_scaled <- fcm()$centers
    
    data.frame(
      Income = round(centers_scaled[,1] * sd(X[,1]) + mean(X[,1]), 2),
      Score  = round(centers_scaled[,2] * sd(X[,2]) + mean(X[,2]), 2)
    )
  })
  
  # ======================
  # Membership probabilities (selected customer)
  # ======================
  output$membership <- renderTable({
    req(fcm())
    
    selected_id <- input$customer_id
    
    U <- as.data.frame(fcm()$membership)
    colnames(U) <- paste0("Cluster_", 1:ncol(U))
    
    result <- cbind(
      CustomerID = data$customer_id,
      round(U, 3)
    )
    
    result %>% filter(CustomerID == selected_id)
  })
  
  # ======================
  # Fuzzy Cross-Entropy
  # ======================
  output$entropy <- renderValueBox({
    req(fcm())
    
    U <- fcm()$membership
    entropy <- -mean(rowSums(U * log(U + 1e-10)))
    
    valueBox(
      round(entropy, 4),
      subtitle = "Fuzzy Cross-Entropy",
      color = "purple"
    )
  })
}

shinyApp(ui, server)
