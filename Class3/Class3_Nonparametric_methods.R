# Laboratory 3 - Nonparametric regression models
# Statistical Learning Methods
# £ukasz Kraiñski 
# lukasz.krainski123@gmail.com
# https://github.com/KrainskiL/SLM_S1920/Class3

#### Smoothing spline and EDA####

library(corrplot)
DATA_SET = read.table("https://raw.githubusercontent.com/KrainskiL/SLM_S1920/master/Class3/DATA4-12.txt")
#Function to split plot box in line with number of plotted elements
OptimizeMfrow = function(elements) {
  columns = ceiling(sqrt(elements))
  rows = ifelse(columns * (columns - 1) < elements, columns, columns - 1)
  return(c(rows, columns))
}
#Function to draw histograms and density estimates
DoHistogram = function(values, values.name) {
  histogram = hist(values, plot = FALSE)
  density.estimate = density(values)
  y.maximum = max(histogram$density, density.estimate$y)
  plot(histogram, freq = FALSE, ylim = c(0, y.maximum), 
       xlab = values.name, ylab = "density", main = NULL)
  lines(density.estimate, lwd = 2)
}
#Function to draw scatterplots and splines
DoScatterplot = function(x.values, x.name) {
  plot(x.values, DATA_SET$MORT,
       xlab = x.name, ylab = "MORT", main = NULL, col = gray(0.4))
  lines(smooth.spline(x.values, DATA_SET$MORT), lwd = 2)
  abline(lm(DATA_SET$MORT ~ x.values)$coef, col = gray(0.7), lwd = 2, lty = 2)
}

#Function to draw boxplots
DoBoxplot = function(values, values.name) {
  boxplot(values, horizontal = TRUE, xlab = values.name)
  median.leg = paste("median = ", round(median(values), digits = 4))
  q25 = quantile(values, 0.25)
  q25.leg = paste("25th percentile =", round(q25,digits = 4),"\n")
  q75 = quantile(values, 0.75)
  q75.leg = paste("75th percentile =", round(q75,digits = 4))
  legend(x = "top", median.leg, bty = "n")
  legend(x = "bottom", paste(q25.leg, q75.leg, sep = ""), bty = "n")
}

names(DATA_SET) = c("MORT", "INCC", "POV", "EDU1", "EDU2", "ALCC",
                     "TOBC", "HEXC", "PHYS", "URB", "AGED")

cat("Variable names:\n", names(DATA_SET))
cat("\n\nBasic statistics:\n")
print(summary(DATA_SET))
par(mfrow = OptimizeMfrow(ncol(DATA_SET)),  mar = c(5, 4, 1, 1))
invisible(mapply(DoBoxplot, DATA_SET, names(DATA_SET)))
cat("\nCorrelation matrix:\n")
cormat = cor(DATA_SET)
print(cormat, digits = 1)

par(mfrow = c(1,1))
corrplot(cormat, method="circle")
corrplot(cormat, method="pie")
corrplot(cormat, method="color")

corrplot(cormat, type="upper")
corrplot(cormat, type="upper", method="pie")

corrplot(cormat, type="upper", method="color", order="AOE")
corrplot(cormat, type="lower", order="FPC", col=c("magenta", "yellow"),bg="lightblue")

cor.mtest = function(mat, ...) {
  mat = as.matrix(mat)
  n = ncol(mat)
  p.mat= matrix(NA, n, n)
  diag(p.mat) = 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp = cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] = p.mat[j, i] = tmp$p.value
    }
  }
  colnames(p.mat) = rownames(p.mat) = colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation
p.mat = cor.mtest(DATA_SET)

corrplot(cormat, type="upper", order="hclust", p.mat = p.mat, sig.level = 0.01)

#More examples:
#http://www.sthda.com/english/wiki/visualize-correlation-matrix-using-correlogram

par(mfrow = OptimizeMfrow(ncol(DATA_SET)),  mar = c(5, 4, 1, 1))
invisible(mapply(DoHistogram, DATA_SET, names(DATA_SET)))

par(mfrow = OptimizeMfrow(ncol(DATA_SET) - 1),  mar = c(5, 4, 1, 1))
invisible(mapply(DoScatterplot, DATA_SET[-1], names(DATA_SET[-1])))
plot.new()
plot.new()
legend("left", c("data", "linear", "smoothed"), bty = "n",
       col = c(gray(0.4), gray(0.7), "black"), pch = c(1, -1, -1),
       lty = c(-1, 2, 1), lwd = c(-1, 2, 2))

#### Cross Validation and LOESS ####

# Dataset:
# http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data

set.seed(1)

# Function for splitting dataset
GetSplitLabels = function(data.length, proportions) {
  proportioned.labels = rep(1:length(proportions), proportions)
  labels = rep(proportioned.labels,len = data.length)
  return(sample(labels, data.length))
}

CV_FOLDS = 10
SPAN_LEVELS = seq(from = 0.1, to = 1, length.out = 100)
RAW_DATA_SET = readLines("http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data")
LSTAT = as.numeric(substr(RAW_DATA_SET, 85, 90))
MEDV = as.numeric(substr(RAW_DATA_SET, 92, 97))
MERGED = data.frame(MEDV,LSTAT)

LSTAT.bounds = range(LSTAT)
LSTAT.grid = seq(from = LSTAT.bounds[1], to = LSTAT.bounds[2])

# "span" parameter defines the share of neighbours we take into account
fit = loess(MEDV~LSTAT, span=.2, data=MERGED) # s=0.2 (better local fit - curve is less smooth)
fit2 = loess(MEDV~LSTAT, span=.5, data=MERGED) # s=0.5 (lbetter global fit - smoother curve)

plot(LSTAT, MEDV, cex=.5, col="darkgrey")
title("Local Regression")
lines(LSTAT.grid,predict(fit,data.frame(LSTAT=LSTAT.grid)),col="red",lwd=2) # s=0.2
lines(LSTAT.grid,predict(fit2,data.frame(LSTAT=LSTAT.grid)),col="blue",lwd=2) # s=0.5
legend("topright",legend=c("Span=0.2","Span=0.5"),col=c("red","blue"),lty=1,lwd=2,cex=.8)

# Creation of labels for cross-validation
split.labels = GetSplitLabels(length(LSTAT), rep(1, CV_FOLDS))

sse = numeric(length(SPAN_LEVELS))
for (fold in 1:CV_FOLDS) {
  training.x = LSTAT[split.labels != fold]
  training.y = MEDV[split.labels != fold]
  testing.x = LSTAT[split.labels == fold]
  testing.y = MEDV[split.labels == fold]
  for (i in 1:length(SPAN_LEVELS)) {
    model = loess(training.y ~ training.x, span = SPAN_LEVELS[i],
                   control = loess.control(surface = "direct"))
    prediction = predict(model, data.frame(training.x = testing.x))
    sse[i] = sse[i] + sum((prediction - testing.y) ^ 2)
  }
}
optimal.span = SPAN_LEVELS[which.min(sse)]
model = loess(MEDV ~ LSTAT, span = optimal.span)

par(mfrow = c(1, 2))
plot(SPAN_LEVELS, sse, cex = 0.7, col = gray(0.6))
points(optimal.span, sse[which.min(sse)], pch = 19)
plot(LSTAT, MEDV, cex = 0.7, col = gray(0.6))
lines(sort(LSTAT), predict(model)[order(LSTAT)], lwd = 3)

#### GAM ####

library(mgcv)
library(ROCR)
library(Ecdat)

set.seed(1)
Participation

LABELS = factor(rep(c("train", "test"), length = nrow(Participation)))
random.labels = sample(LABELS)
split.data = split(Participation, random.labels)

glm.model = glm(lfp ~ lnnlinc + age,
                 family = binomial, data = split.data$train)
gam.model = gam(lfp ~ s(lnnlinc) + s(age),
                 family = binomial, data = split.data$train)
glm.prediction = predict(glm.model, newdata = split.data$test)
gam.prediction = as.vector(predict(gam.model, newdata = split.data$test))

plot(gam.model)

par(mfrow = c(1,1))

plot(performance(prediction(gam.prediction, split.data$test$lfp),"tpr", "fpr"),lwd = 2, lty = 2)
plot(performance(prediction(glm.prediction, split.data$test$lfp),"tpr", "fpr"),add = TRUE, lwd = 2, lty = 1)
legend("bottomright", c("GLM", "GAM"),lwd = 2, lty = 1:2)
