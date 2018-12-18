library(shiny)

ui = fluidPage(
  titlePanel("Why game 24? Compare the probability now!!"),
  fluidRow(
    column(3, wellPanel(
      sliderInput("n", "Target number you are interested:", min = 20, max = 30,
                  value = 25, step = 1),
      radioButtons("plot_type", "Plot Type:",
                   c("bar_plot", "box_plot", "pie_plot")), 
      p("This is the reason why it is game 24 instead of game 27 or what ever. 
        By comparing the probability of individual cards as well as total probability for all cards,
        you will see the probability of getting 24 by 4 cards is the highest one.")
    )),
    column(6,
           plotOutput("plot", width = 1000, height = 700) 
    )
  )
)


server=function(input,output){
  
  output$plot = renderPlot({
    plot_prob(df,b = (input$n), type = input$plot_type)
  })
  

}
shinyApp(ui=ui,server=server)


