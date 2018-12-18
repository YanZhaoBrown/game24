library(game24)

#This chunk is to calculate all of the individual cards' probability of getting targets from 20 to 30
df = 1:13
for (j in 20:30) { 
  probs = vector(mode = "numeric", length = length(cards))
  for (i in 1:13) {
    probs[i] = game24prob(i,j)
  }
  df = cbind(df, probs)
}
df = df[1:13, ]
df = as.data.frame(df)
saveRDS(df, file="data.rds")

