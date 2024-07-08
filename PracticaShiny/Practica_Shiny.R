install.packages("shinydashboard")
library(shiny)
library(shinydashboard)
library(corrplot)
library(readr)
library(randomForest)
library(factoextra)
library(caret)
#runExample("01_hello")


##################################################################################
##################################################################################
##################################################################################
                                #RANDOMFOREST
##################################################################################
##################################################################################
##################################################################################

data_cart_abandonment <- read_csv("data_cart_abandonment.csv")
View(data_cart_abandonment)
attach(data_cart_abandonment)

data_tree <- data_cart_abandonment[,c(-1,-2)]
data_tree <- na.omit(data_tree)

View(data_tree)

data_tree$Is_Product_Details_viewed[data_tree$Is_Product_Details_viewed == "Yes"] <- 1
data_tree$Is_Product_Details_viewed[data_tree$Is_Product_Details_viewed == "No"] <- 0

data_tree$Customer_Segment_Type <- as.factor(data_tree$Customer_Segment_Type)
data_tree$Cart_Abandoned <- as.factor(data_tree$Cart_Abandoned)

set.seed(1250) # Numero inicial del cual comenzara a generar una secuencia aleatoria.
split_train_test <- createDataPartition(data_tree$Cart_Abandoned, p=0.7, list=FALSE) #p = porcentaje de los datos a usar, list = Si el resultado devolvera una lista o una matriz. 

dtrain<- data_tree[split_train_test,]
dtest<-  data_tree[-split_train_test,]

dtest$Cart_Abandoned <- factor(dtest$Cart_Abandoned, levels = levels(dtrain$Cart_Abandoned))

View(dtest)


#tr_fit <- rpart(Cart_Abandoned ~., data = dtrain, method="class") # Indicamos que deseamos un arbol de clasificacion, tambien podemos armar un arbol de regresion.
#tr_fit # Nuestro arbol obtenido.
#rpart.plot(tr_fit, tweak = 1.6) # Graficamos el arbol.

#aprender_rpart_pred <- predict(tr_fit, dtest, type="class")
#aprender_rpart_pred


rfModel <- randomForest(Cart_Abandoned ~ ., data = dtrain, 
                        importance = TRUE, ntree = 500)
print(rfModel)

predictions <- predict(rfModel, dtest)


# Calcular la matriz de confusión
confusionMatrix(predictions, dtest$Cart_Abandoned)

importance_values <- importance(rfModel)
print(importance_values)

# Visualizar la importancia de las variables
varImpPlot(rfModel)

importance_df <- as.data.frame(importance_values)
importance_df$Variable <- rownames(importance_df)

ggplot(importance_df, aes(x = reorder(Variable, MeanDecreaseAccuracy), y = MeanDecreaseAccuracy)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Importancia de las Variables (MeanDecreaseAccuracy)", x = "Variable", y = "Importancia (MeanDecreaseAccuracy)") +
  theme_minimal()

ggplot(importance_df, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Importancia de las Variables (MeanDecreaseGini)", x = "Variable", y = "Importancia (MeanDecreaseGini)") +
  theme_minimal()

##################################################################################
##################################################################################
##################################################################################
                                  #PREDICT
##################################################################################
##################################################################################
##################################################################################

# Evaluar el modelo en el conjunto de prueba

rfModel_shiny <- randomForest(Cart_Abandoned ~ No_Checkout_Confirmed +
                              Session_Activity_Count +
                              No_Customer_Login +
                              No_Checkout_Initiated, data = dtrain, 
                        importance = TRUE, ntree = 500)
print(rfModel_shiny)

predictions_shiny <- predict(rfModel_shiny, dtest)

resultados3 <- data.frame(Real = dtest$Cart_Abandoned, Predicho = predictions_shiny)

print(resultados3)

##################################################################################
##################################################################################
##################################################################################
                                  #SHINY
##################################################################################
##################################################################################
##################################################################################


ui <- fluidPage(
  titlePanel("Predicción de Abandono de Carrito"),
  
  sidebarLayout(
    sidebarPanel(
  
      numericInput("no_chck_conf", "Checkout confirmado", value = 0),
      numericInput("activity_count", "Actividad de sesión", value = 0),
      numericInput("no_login", "Registrado o no", value = 0),
      numericInput("no_chck_init", "Checkout iniciado", value = 0),
      
     
      
      actionButton("predict_button", "Predecir")
    ),
    
    mainPanel(
      h3("Resultados de la Predicción"),
      verbatimTextOutput("prediction_output")
    )
  )
)

server <- function(input, output) {
  # Función para realizar la predicción
  realizar_prediccion <- function(no_chck_conf, activity_count, no_login, no_chck_init) {
    # Preparar los datos para la predicción
    nueva_data <- data.frame(
      No_Checkout_Confirmed = no_chck_conf,
      Session_Activity_Count = activity_count,
      No_Customer_Login = no_login,
      No_Checkout_Initiated = no_chck_init
      # Añadir más variables predictoras según sea necesario
    )
    
    # Realizar la predicción usando el modelo entrenado
    pred <- predict(rfModel_shiny, newdata = nueva_data)
    return(pred)
  }
  
  # Evento para manejar la predicción cuando se hace clic en el botón
  observeEvent(input$predict_button, {
    predicciones <- realizar_prediccion(input$no_chck_conf, input$activity_count, input$no_login, input$no_chck_init)
    
    # Mostrar los resultados de la predicción
    output$prediction_output <- renderPrint({
      if (predicciones == 1) {
        "El carrito fue abandonado."
      } else {
        "El carrito no fue abandonado."
      }
    })
  })
}

shinyApp(ui = ui, server = server)
