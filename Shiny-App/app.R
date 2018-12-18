
library(devtools)
install_github('PHP-2560/final-project-game24/game24',subdir="master",force=T)
library(game24)
library(shiny)
library(ggplot2)
library(reshape)

df = readRDS("data.rds")

#function of generating plots
plot_prob = function(df, b, type) {
  plot_df = df[, c(1,6,b-20+2)] #select the cards column, 24 column and user selected column
  colnames(plot_df) = c("cards","probability 24", "probability of user interest")
  
  if(type == "bar_plot") { #bar plot
    
    df <- reshape::melt(plot_df, id = c("cards")) #redo the dataframe by selecting "cards" as key
    plot = ggplot2::ggplot(df, aes(x=cards, y = value, fill = variable))+
      geom_bar(stat='identity', position='dodge', width =0.5) +
      #labs(x = "cards", y = "probability", title = "Why game24? Probability of each cards getting 24 vs getting other number")+
      scale_x_continuous(breaks = c(1:13))
    
    return(plot)
  } 
  
  else if(type == "box_plot") { #box plot
    plot_df = plot_df[,2:3] 
    plot = boxplot(plot_df, col = c("#F8766D", "#00BFC4"))
    return(plot)
  } 
  
  else if(type == "pie_plot") { #pie plot
    
    x = apply(plot_df[,2:3], 2, mean)
    lables = c("total probability of 24:", "total probability of user interest:")
    pct = round(x/sum(x)*100) #calculate the percentage
    lables <- paste(lables, pct) # add percents to labels
    lables <- paste(lables,"%",sep="") # ad % to labels
    plot = pie(x, lables, col = c("#F8766D", "#00BFC4"))
    return(plot)
  }
} 






#Use navbarPage as our general layout
ui=navbarPage(title="Game24", 
    #First Panel Layout
    tabPanel(title="Game24 Solver",
          sidebarLayout(  
            sidebarPanel(     
      sliderInput(inputId = "target",
                  label = "24 or other target",
                  value=24,min=20,max=50),
      checkboxGroupInput("operations", label = h4("operation sign"), 
                         choices = list("Addition" = "+", "Subtraction" = "-", 
                                        "Multiplication" = "*","Division" = "/"),
                         selected = c("+","-","*","/")),
   
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
      
      actionButton("go1","Solve it!")),
     
   
   mainPanel(
      tableOutput("method1")
      )
   )),
   #Second Panel Layout
    tabPanel(title="Game24 Player", titlePanel("Press 'Play' to get 4 cards"), 
             actionButton("go2","Play"),
             actionButton("result","Show result"),
             textOutput("cards"),
             tableOutput("method2"),
             tags$head(tags$style("#cards{color: blue;    
                                font-size: 40px;
                                  font-style: bold;
                                  font-family: 'Comic Sans MS', 'Comic Sans', cursive
                                  }",
                              "#method2{color: blue;    
                              font-size: 20px;
                              font-style: bold;
                              font-family: 'Comic Sans MS', 'Comic Sans', cursive
                              }",
                              "body{
                              background: url('');
                              background-size: cover;
                              }")
      
    
             
    )),
   #Third Panel Layout
   
   tabPanel(title="Why 24?",
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
       )),
   
   #Forth Panel Layout
   tabPanel(title="CHECKER",
            sidebarLayout(
              sidebarPanel(
                sliderInput(inputId = "numInputs",
                            label = "How many inputs you want",
                            min=1,max=5,value=4),
                
                hr(),
                helpText("Notice: Test 5 inputs may take you about 10 seconds"),
                numericInput(inputId = "Target",label="Target Value",value=24),
                actionButton("check","Check"),
                # place to hold dynamic inputs
                uiOutput("inputGroup")
              ),
              # this is just a demo to show the input values
              mainPanel(textOutput("inputValues"))
            ))
   
   
   
   
        )

server=function(input,output){
#######PART 1########
  #press "solve it" if we want to reset 4 different number or the target
  data=eventReactive(input$go1,{
    c(input$num1,
      input$num2,
      input$num3,
      input$num4)})
 
  data1=eventReactive(input$go1,{input$target})
  #generate the output by game24 function taking the data, data1 and the checkbox input as arguments
  output$method1 = renderTable({
    game24(data(), data1(),
           input$operations)
  })
  
#######PART 2########
  
  v <- reactiveValues(data = NULL)
  #obeserve the "Play" button, if user click it, 4 random numbers will generate 
  observeEvent(input$go2, {
    v$data <- sample(1:13, 4, replace = T)
  })
  
  
  
  output$cards <- renderText({
    
    v$data
  })
  
  #obeserve the "Show result" button, if user click it, the game24 function will take the 4 random numbers as arguments and generate methods.
  observeEvent(input$result, {
    v$data2 <- game24(v$data,24)
  })
  
  output$method2 <- renderTable({v$data2})
#######PART 3#######
  # observe changes in "numInputs", and create corresponding number of inputs
  observeEvent(input$numInputs, {
    output$inputGroup = renderUI({
      input_list <- lapply(1:input$numInputs, function(i) {
        # for each dynamically generated input, give a different name
        inputName <- paste("input", i, sep = "")
        numericInput(inputName, inputName, 1)
      })
      do.call(tagList, input_list)
    })
  })
  
  data3=eventReactive(input$check,{unlist(c(lapply(1:input$numInputs, function(i) {
    inputName <- paste("input", i, sep = "")
    input[[inputName]]
    
  })))})
  data4=eventReactive(input$check,{input$Target})
  
  # we call the game_check function with the all the input values
  output$inputValues <- renderText({
    game_check(data3(),data4())
  })  
  
########PART 4#######
  output$plot = renderPlot({
    plot_prob(df,b = (input$n), type = input$plot_type)
  })
  

}

  
shinyApp(ui=ui,server=server)
