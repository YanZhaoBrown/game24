library(game24)
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
    lables = c("probability 24", "probability of user interest")
    pct = round(x/sum(x)*100) #calculate the percentage
    lables <- paste(lables, pct) # add percents to labels
    lables <- paste(lables,"%",sep="") # ad % to labels
    plot = pie(x, lables, col = c("#F8766D", "#00BFC4"))
    return(plot)
  }
} 


