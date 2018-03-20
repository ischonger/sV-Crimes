m.simple <- glm.nb(crimes~density, data = crimes.data)
m.matrix <- model.matrix(m.simple, data = crimes.data)
head(m.matrix)
plot(crimes.data$crimes~crimes.data$density)

m.beta0.hat <- m.simple$coefficients[[1]]
m.beta1.hat <- m.simple$coefficients[[2]]
m.beta0.hat
m.beta1.hat
abline(m.beta0.hat, m.beta1.hat, col ="green")

# oder:
x <- cbind(1, crimes.data$crimes)
head(x)
beta.hat <- solve(t(x) %*% x) %*% t(x) %*% crimes.data$crimes
beta.hat
x <- crimes.data$density
y <- crimes.data$crimes
plot(x,y)
abline(beta.hat, col = 2, lwd = 3)

#install.packages("stats")
