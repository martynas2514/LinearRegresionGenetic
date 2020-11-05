
whichBest <-  function(parents, x , y ){
  y_hat = apply(parents, 1, calcVals, x = x )
  SSE <-  apply(y_hat, 2, sse, y = y)
  return(which.min(SSE))
}

calcVals <-  function(c, x){
  y <- 0
  for (i in 1:length(c)) {
    y = y + unlist(c[i]) * (x^(i-1))
  }
  return(y)
}

sse <-  function(yHat, y){
  return(sum((yHat - y)^2))
}

crossover <- function(x, value, prob){
  index = sample(c(1:length(x)), floor( length(x) * prob))
  for (i in index) {
    x[i] <- value
  }
  return(x)
}

insertRow <- function(existingDF, newrow, r) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  existingDF
}

mutation <- function(df, bestIndex, prob, totalCycles, currentCycle){
 
  best <- unlist(df[bestIndex, ])
  parents <- df[-bestIndex, ]
  index = sample(c(1:nrow(parents)), floor( nrow(parents) * prob))
 
  for (i in index) {
    # create mutation with variance
    mut <- vector()
    for (j in 1:ncol(df)) {
      mut <- append(mut, rnorm(1, best[j], 20))
    }
    parents[i, ] <- mut
  }
  parents <- rbind(parents, best)
  return(parents)
}

newGen <-  function(parents, indexBest, probCross, probMut, totalCycles, currentCycle ){
  df <- parents

  # crossover
  for (i in 1:ncol(parents)) {
   df[, i] <- crossover(df[, i], parents[indexBest, i], probCross)
  }
    
  # mutation
  df <- mutation(df, indexBest, probMut, totalCycles, currentCycle)
  
  return(df)
}

fit <- function(parents, n, x, y){
  for (i in 1:n) {
    best <- whichBest(parents, x, y)
    parents = as.data.frame(newGen(parents, best, 0.6, 0.3, n, i))
  }
  return(parents[best, ])
}

fitPolynom <- function(df, n){
  parents <- data.frame(matrix(runif(10*n, -100, 100), ncol = n))
  param <- fit(parents, 50000, df$x, df$y)
  y_hat = calcVals(param, df$x)
  print(param)
  
  plot(df$x,df$y)  
  lines(df$x, y_hat, type = "l", col = "blue")
}


q <- seq(from=0, to=20, by=0.1)
y <- q
# noise <- rnorm(length(q), mean=10, sd=80)
# y <- y + noise

#y <-  5 * sin(x/10)
# plot(q,y, type = "p")
input <- data.frame(x = q, y = y)
result <- fitPolynom(input, 2)

# model <- lm(y ~ poly(q,3))
# predicted.intervals <- predict(model,data.frame(x=q),interval='confidence',
#                                level=0.99)
# lines(q,predicted.intervals[,1],col='green',lwd=3)


