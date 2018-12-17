#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Play Krypto"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
      actionButton("button", "Show me how to get Krypto!")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        textOutput("fieldCards1"),
        textOutput("objective"),
        tableOutput("solutions1"),
        tags$head(tags$style("#fieldCards1{color: blue;    
                                font-size: 40px;
                                font-style: bold;
                                font-family: 'Comic Sans MS', 'Comic Sans', cursive
                              }",
                             "#objective{color: blue;    
                                font-size: 40px;
                                font-style: bold;
                                font -family: 'Comic Sans MS', 'Comic Sans', cursive
                              }",
                             "#solutions1{color: blue;    
                                font-size: 20px;
                                font-style: bold;
                                font-family: 'Comic Sans MS', 'Comic Sans', cursive
                              }",
                              "body{
                              background: url('https://pbs.twimg.com/media/DefXfYlXkAAkR1q.jpg:large');
                              background-size: cover;
                  }"
          )
        )
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  library(combinat)
  library(stringr)
  
  num_sign=function(a,b,sign){
    if(sign==1){
      return(a+b)
    }
    else if(sign==2){
      return(a-b)
    }
    else if(sign==3){
      return(a*b)
    }
    else if(sign==4){
      return(a/b)
    }
    else if(sign==5){
      return(b-a)
    }
    else if(sign==6){
      return(b/a)
    }
  }
  
  
  trans=function(b,c,a){
    stopifnot(a==1 | a==2 | a==3 | a==4 | a==5 | a==6)
    if(a==1){
      k1=paste("(",b,"+",c,")")
      return(k1)
    }
    else if(a==2){
      k2=paste("(",b,"-",c,")")
      return(k2)
    }
    else if(a==3){
      k3=paste(b,"*",c)
      return(k3)
    }
    else if(a==4){
      k4=paste(b,"/",c)
      return(k4)
    }
    else if(a==5){
      k5=paste("(",c,"-",b,")")
      return(k5)
    }
    else{
      k6=paste(c,"/",b)
      return(k6)
    }
  }
  
  gameKrypto_makeCardsShiny=function(A = 0, b=0){
    if(A == 0){
    A = sample(1:13, 4, replace = TRUE)
    }
    output$fieldCards1 = renderText({
      paste(c("Make the objective card from these cards:", A), collapse = " ")
      })
    b = sample(1:13, 1)
    output$objective = renderText({
      paste("Your objective card is:", b)
    })
    
    len=length(A)
    B=combinat::permn(A)
    stopifnot(len==4)
    stopifnot(A[1]%%1==0 & A[2]%%1==0 & A[3]%%1==0 & A[4]%%1==0)
    method=vector(mode="character",length=0)
    # Initializing required vectors
    operation_1=vector(mode="character",length=0)
    operation_2=vector(mode="character",length=0)
    operation_3=vector(mode="character",length=0)
    rep=vector(mode="integer",length=0)
    result1=vector(mode="integer",length=0)
    result2=vector(mode="integer",length=0)
    # Iterative arithmetical operations
    for(s in 1:factorial(len)){
      for(i in 1:6){
        result1_temp=num_sign(B[[s]][1],B[[s]][2],i)
        for(j in 1:6){
          result2_temp=num_sign(result1_temp,B[[s]][3],j)
          for(k in 1:6){
            result3=num_sign(result2_temp,B[[s]][4],k)
            if(result3==b){
              operation_1=c(operation_1,trans(B[[s]][1],B[[s]][2],i))
              operation_2=c(operation_2,trans(result1_temp,B[[s]][3],j))
              operation_3=c(operation_3,trans(result2_temp,B[[s]][4],k))
              result1=c(result1,result1_temp)
              result2=c(result2,result2_temp)
            }
          }
        }
      }
    }
    
    if(length(result1) == 0){
      output$solutions1 <- eventReactive(input$button, 
        {paste("You cannot make", b, "with these cards.")})
      return(0)
    }
    
    for(q in 1:(length(result1)-1)){
      s=q+1
      for(p in s:length(result1)){
        if(result1[q]==result1[p]){
          if(result2[q]==result2[p]){
            rep=c(rep,p)
          }
        }
      }
    }
    operation_1=operation_1[-rep]
    operation_2=operation_2[-rep]
    operation_3=operation_3[-rep]
    result1=result1[-rep]
    result2=result2[-rep]
    
    #After delete the repetation, we store all of the method in a vector 
    for(f in 1:length(operation_1)){
      method=c(method,stringr::str_c(operation_1[f]," = ", result1[f], " then ",
                                     operation_2[f]," = ", result2[f], " then ",
                                     operation_3[f],  " = ",b))
    }
    
    df <- eventReactive(input$button, {df=data.frame("Method"=1:length(method),"Process"=method)})
    output$solutions1 <- renderTable({df()})
  }
  
  gameKrypto_makeCardsShiny()
  
}

# Run the application 
shinyApp(ui = ui, server = server)

