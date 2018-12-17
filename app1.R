library(shiny)
ui=fluidPage(
  titlePanel("Welcome to game'24'"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "target",
                  label = "24 or other target",
                  value=24,min=20,max=50),
      checkboxGroupInput("operations", label = h4("operation sign"), 
                         choices = list("Adding" = "+", "Subtracting" = "-", 
                                        "Multiplying" = "*","Divding" = "/"),
                         selected = c("+","-","*","/")),
      
      
     
      fluidRow(column(4, verbatimTextOutput("operations"))),
      
      sliderInput(inputId = "num1",
                  label = "1st Card",
                  value=1,min=1,max=13),
      sliderInput(inputId = "num2",
                  label = "2nd Card",
                  value=2,min=1,max=13),
      sliderInput(inputId = "num3",
                  label = "3rd Card",
                  value=3,min=1,max=13),
      sliderInput(inputId = "num4",
                  label = "4th Card",
                  value=4,min=1,max=13),
      
     actionButton("go","Play")
    
      
    ),
    
    mainPanel(
      tableOutput("method")
    )
  )
)
server=function(input,output){
  
  data=eventReactive(input$go,{
    c(input$num1,
      input$num2,
      input$num3,
      input$num4)})
  
  output$method = renderTable({
    game24(data(), 
           input$target,
           input$operations)
  })
}
shinyApp(ui=ui,server=server)