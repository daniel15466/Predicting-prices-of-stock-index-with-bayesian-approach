library(ggplot2)

kghmluty <- read.csv(file = "merged.csv", header = TRUE, dec =".", sep = ",")

#rozklad a priori wzrostowych dni pi(1) = pi(2) = ... = pi(19) = 1/19
df <- data.frame(c(0))
for (i in 1:45)
{
  df[i] <- c(0)
  for (j in 1:19)
  {
    df[j,i] <- 0 
  }
}

#apriori 0
for (i in 1:19)
{
  df[i,1] <- 1/19
}
#petla do wyliczania funkcji wiarygodnosci, i rozkladów apesteriori
for (j in 2:43)
{
  for (i in 1:19)
  {
    if (kghmluty$priceIncreased[j] == 1)
    {
      df[i,45] <- i^2/19^2
    }
    else
    {
      df[i,45] <- 1-i^2/19^2
    }
    mian <- sum(df[j-1]*df[45])
    for (i in 1:19)
    {
      df[j] <- df[j-1]*df[45]/mian
    }
  }
  
}


#30 sesji od poczatku lutego
plot <- ggplot(df, aes(1:19)) +  
  geom_line(aes(y = df[,1]), color = "black") +
  geom_line(aes(y = df[,2]), color = "black") +
  geom_line(aes(y = df[,3]), color = "black") +
  geom_line(aes(y = df[,4]), color = "black") +
  geom_line(aes(y = df[,5]), color = "black") +
  geom_line(aes(y = df[,6]), color = "black") +
  geom_line(aes(y = df[,7]), color = "black") +
  geom_line(aes(y = df[,8]), color = "black") +
  geom_line(aes(y = df[,9]), color = "black") +
  geom_line(aes(y = df[,10]), color = "black") +
  geom_line(aes(y = df[,11]), color = "black") +
  geom_line(aes(y = df[,12]), color = "black") +
  geom_line(aes(y = df[,13]), color = "black") +
  geom_line(aes(y = df[,14]), color = "black") +
  geom_line(aes(y = df[,15]), color = "black") +
  geom_line(aes(y = df[,16]), color = "black") +
  geom_line(aes(y = df[,17]), color = "black") +
  geom_line(aes(y = df[,18]), color = "black") +
  geom_line(aes(y = df[,19]), color = "black") +
  geom_line(aes(y = df[,21]), color = "black") +
  geom_line(aes(y = df[,22]), color = "black") +
  geom_line(aes(y = df[,23]), color = "black") +
  geom_line(aes(y = df[,24]), color = "black") +
  geom_line(aes(y = df[,25]), color = "black") +
  geom_line(aes(y = df[,26]), color = "black") +
  geom_line(aes(y = df[,27]), color = "black") +
  geom_line(aes(y = df[,28]), color = "black") +
  geom_line(aes(y = df[,29]), color = "black") +
  geom_line(aes(y = df[,30]), color = "black") 
plot

#5 ostatnich sesji z marca
plot1 <- ggplot(df, aes(1:19)) +  
  geom_line(aes(y = df[,43]), color = "black") +
  geom_line(aes(y = df[,42]), color = "black") +
  geom_line(aes(y = df[,41]), color = "black") +
  geom_line(aes(y = df[,40]), color = "black") +
  geom_line(aes(y = df[,39]), color = "black") 
plot1

#apriori z lutego
plot(df[,21], type = 'l')
#apriori ciagly
plot(df[,1], type = 'l')
#apriori custom
estBetaParams <- function(mu, var) {
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(params = list(alpha = alpha, beta = beta))
}
vars <- estBetaParams(0.6, 0.01)
alfa <- vars$alpha
beta <- vars$beta


x <- (1:99)/100
rozklb <- c(0)

rozklb[1] <- 1
iter <- 1
for (i in x)
{
  rozklb[iter] <- i^(alfa-1)*(1-i)^(beta-1)/beta(alfa, beta)
  iter <- iter + 1
}
plot(rozklb)


