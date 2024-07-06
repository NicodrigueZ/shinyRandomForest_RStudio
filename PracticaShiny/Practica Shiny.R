install.packages("shinydashboard")
library(shiny)
library(shinydashboard)

#runExample("01_hello")


function(input, output, session) {
  
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    iris[, c(input$xcol, input$ycol)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  output$plot1 <- renderPlot({
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
  
}

vars <- setdiff(names(iris), "Species")

pageWithSidebar(
  headerPanel('Iris k-means clustering'),
  sidebarPanel(
    selectInput('xcol', 'X Variable', vars),
    selectInput('ycol', 'Y Variable', vars, selected = vars[[2]]),
    numericInput('clusters', 'Cluster count', 3, min = 1, max = 9)
  ),
  mainPanel(
    plotOutput('plot1')
  )
)




##################################################################################
##################################################################################
##################################################################################
                                #KMEANS
##################################################################################
##################################################################################
##################################################################################

library(broom)
library(factoextra)
library(readr)
library(corrplot)
library(readr)
install.packages("data.table")
library(data.table)

data_cart_abandonment <- read_csv("data_cart_abandonment.csv")

#SI ABANDONA
sum(data_cart_abandonment$Cart_Abandoned == '1')
#NO ABANDONA
sum(data_cart_abandonment$Cart_Abandoned == '0')


data <- fread("data_cart_abandonment.csv")
data <- data[,c(-1,-13)]
data <- na.omit(data)


data$Is_Product_Details_viewed[data$Is_Product_Details_viewed == "Yes"] <- 1
data$Is_Product_Details_viewed[data$Is_Product_Details_viewed == "No"] <- 0


data$Is_Product_Details_viewed <- suppressWarnings(as.numeric(data$Is_Product_Details_viewed))
data$Session_Activity_Count <- suppressWarnings(as.numeric(data$Session_Activity_Count))
data$No_Items_Added_InCart <- suppressWarnings(as.numeric(data$No_Items_Added_InCart))
data$No_Items_Removed_FromCart <- suppressWarnings(as.numeric(data$No_Items_Removed_FromCart))
data$No_Cart_Viewed <- suppressWarnings(as.numeric(data$No_Cart_Viewed))
data$No_Checkout_Confirmed <- suppressWarnings(as.numeric(data$No_Checkout_Confirmed))
data$No_Checkout_Initiated <- suppressWarnings(as.numeric(data$No_Checkout_Initiated))
data$No_Cart_Items_Viewed <- suppressWarnings(as.numeric(data$No_Cart_Items_Viewed))
data$No_Customer_Login <- suppressWarnings(as.numeric(data$No_Customer_Login))
data$No_Page_Viewed <- suppressWarnings(as.numeric(data$No_Page_Viewed))
data$Customer_Segment_Type <- suppressWarnings(as.numeric(data$Customer_Segment_Type))

dt_scaled <- data[, lapply(.SD, scale)]

View(dt_scaled)

set.seed(123)


kmeans_result_scale <- kmeans(dt_scaled, centers = 2, nstart = 25)

fviz_cluster(kmeans_result_scale, data = dt_scaled, geom = "point",
             ellipse.type = "norm", ggtheme = theme_minimal())

fviz_cluster(kmeans_result_scale, data = data, geom = "point",
             ellipse.type = "norm", ggtheme = theme_minimal())

fviz_nbclust(x = dt_scaled , FUNcluster = kmeans, method = "silhouette", k.max = 11) +
  labs(title = "Numero optimo de clusters")

#CENTERS ORIGINAL ESCALADO
print(kmeans_result_scale$centers)
print(kmeans_result_scale$cluster)
print(table(kmeans_result_scale$cluster))


inverse_transform <- function(scaled_data, original_data) {
  center <- sapply(original_data, mean, na.rm = TRUE)
  scale <- sapply(original_data, sd, na.rm = TRUE)
  t(t(scaled_data) * scale + center)
}

cluster_centers_scaled <- kmeans_result_scale$centers
cluster_centers_original <- inverse_transform(cluster_centers_scaled, data)

#CENTERS ESCALADO
print(cluster_centers_scaled)

#CENTERS ESCALA ORIGINAL
print(cluster_centers_original)

print("Hello world")
