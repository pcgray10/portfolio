## Logistic Regression for Final Project 

# Read in the dataset
fb_data <- read.csv("ANYAData1.csv")

# Remove extra rows of NA; because this is not a missing data problem
fb_revised <- fb_data[!is.na(fb_data$Rk),]

# For ANY/A

# ------------------ Visualization of the Data ------------------
plot(fb_revised$ANY.A,fb_revised$Superbowl,pch=16,
     xlab = "ANY/A", ylab = 'Playoff Success')

# Add a jitter to points to see multiple observations
plot(jitter(fb_revised$ANY.A,1),fb_revised$Superbowl,pch = 16,
     col = rgb(0,0,0,.5),xlab = "ANY/A", ylab = 'Playoff Success')

# ------------------ Calculate Probabilities ------------------
# Frequency of Made or Missed per Distance
tab <- table(fb_revised$ANY.A,fb_revised$Superbowl);tab
# Calculate probabilities
probs <- tab[,2]/rowSums(tab); probs 

# Visualization of Probability vs Distance
plot(unique(fb_revised$ANY.A), probs, pch = 16,xlab = 'ANY/A')

# ------------------ Construct Logistic Model ------------------
reg_output <- glm(Superbowl ~ ANY.A, data = fb_revised, family = binomial)
reg_output

# Compute the estimated probability
p_hat <- predict(reg_output, newdata = data.frame(ANY.A = seq(4.2,8.4, 0.1)),type = 'response')

# When we go to plot the data, we you are plotting the coordinates
# (4.2, predicted value at 4.2), (4.3, predicted value at 4.3), ...
lines(seq(4.2,8.4,0.1),p_hat,col = 'blue', lwd = 3)

# For TANY/A
# ------------------ Visualization of the Data ------------------
plot(fb_revised$TANY.A,fb_revised$Superbowl,pch=16,
     xlab = "TANY/A", ylab = 'Playoff Success')

# Add a jitter to points to see multiple observations
plot(jitter(fb_revised$TANY.A,1),fb_revised$Superbowl,pch = 16,
     col = rgb(0,0,0,.5),xlab = "TANY/A", ylab = 'Playoff Success')

# ------------------ Calculate Probabilities ------------------
# Frequency of Made or Missed per Distance
tab1 <- table(fb_revised$TANY.A,fb_revised$Superbowl);tab
# Calculate probabilities
probs <- tab[,2]/rowSums(tab); probs 

# Visualization of Probability vs Distance
plot(unique(fb_revised$TANY.A), probs, pch = 16,xlab = 'TANY/A')

# ------------------ Construct Logistic Model ------------------
reg_output1 <- glm(Superbowl ~ TANY.A, data = fb_revised, family = binomial)
reg_output1

# Compute the estimated probability
p_hat1 <- predict(reg_output1, newdata = data.frame(TANY.A = seq(4.2,8.4, 0.1)),type = 'response')

# When we go to plot the data, we you are plotting the coordinates
# (4.2, predicted value at 4.2), (4.3, predicted value at 4.3), ...
lines(seq(4.2,8.4,0.1),p_hat1,col = 'blue', lwd = 3)

