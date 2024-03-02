Dt <- read.csv('Sample Dataset.csv')

# Create function
finite_differences <- function(x, y) {
  n <- length(x)
  derivativeFunction <- vector(length = n)
  for (i in 2:n) {
    derivativeFunction[i-1] <- (y[i-1] - y[i]) / (x[i-1] - x[i])
  }
  derivativeFunction[n] <- (y[n] - y[n - 1]) / (x[n] - x[n - 1])
  return(derivativeFunction)
}

# Fit cox regression
fomula <- Surv(start, stop, secondInfection) ~ afterInitialInfection  # construct formula 
model <- coxph(fomula, cluster = PID, data = Dt)  # due to repeated measurement over time using robust variance (set cluster = PID)
Summary <- summary(model)  # Obtain model summary

# Create coefficient table
resultTable <- cbind(Summary$coefficients, Summary$conf.int[, c('lower .95', 'upper .95')])
round(resultTable, 4)

# Calculate numerical derivative
deri <- finite_differences(x = bhest$time, y = bhest$hazard)
baselineHazard <- cbind(bhest, deri)
colnames(baselineHazard) <- c('CumulativeBaselineHazard', 'Time', 'BaselineHazard')

# Non-parametric smooth curve for estimating baseline hazard function over time
smoothBH <- ss(x = baselineHazard$Time, y = baselineHazard$BaselineHazard, df = 4)

# Non-parametric smooth curve for estimating cumulative baseline hazard function over time
smoothCBH <- ss(x = baselineHazard$Time, y = baselineHazard$CumulativeBaselineHazard, df = 6)

Dt_Plot <- data.frame('Time' = baselineHazard$Time, 
                      'BH' = baselineHazard$BaselineHazard, 
                      'smoothBH' = smoothBH$y, 
                      'CBH' = baselineHazard$CumulativeBaselineHazard, 
                      'smoothCBH' = smoothCBH$y)

# Set up color for smooth curves
colors <- c("Baseline hazard" = "blue", "Cumulative baseline\nhazard" = "#FFA500")

# Create scale factor for second y-axis
scaleFactor <- max(Dt_Plot$BH) /  max(Dt_Plot$CBH)

# Create x and y labels
xlab <- seq(0, 1000, length.out = 11)
ylab <- seq(0, 0.04, length.out = 5)

# Create figure
p <- 
  ggplot(Dt_Plot, aes(x = Time)) + 
  geom_point(aes(y = BH, color = 'Baseline hazard'), size = 0.75, alpha = 0.3) + 
  geom_path(aes(y = BH, color = 'Baseline hazard')) + 
  geom_path(aes(y = CBH * scaleFactor, color = 'Cumulative baseline\nhazard')) + 
  geom_point(aes(y = CBH * scaleFactor, color = 'Cumulative baseline\nhazard'), size = 0.75, alpha = 0.3) + 
  labs(x = 'Time', y = 'Raw baseline hazard') + 
  scale_x_continuous(expand = c(0, 0.01), limits = c(0, 1000), breaks = xlab, labels = xlab) +
  scale_y_continuous(expand = c(0, 0.01), 
                     sec.axis = sec_axis(trans = ~ ./scaleFactor, name = "Raw cumulative baseline hazard")) + 
  scale_color_manual(values = colors) + 
  theme_bw() + 
  theme(panel.background = element_blank(),                    
        axis.line.x = element_line(),                               
        axis.line.y = element_line(),
        axis.text.x = element_text(color = "black", size = 7), 
        axis.text.y = element_text(color = 'black', size = 7),
        legend.title = element_blank())
p

# Create scale factor for second y-axis
scaleFactor <- max(Dt_Plot$smoothBH) /  max(Dt_Plot$smoothCBH)

# Create figure
p <- 
  ggplot(Dt_Plot, aes(x = Time)) + 
  geom_path(aes(y = smoothBH, color = 'Baseline hazard')) + 
  geom_path(aes(y = smoothCBH * scaleFactor, color = 'Cumulative baseline\nhazard')) + 
  labs(x = 'Time', y = 'Baseline hazard') + 
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1000), breaks = xlab, labels = xlab) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.04), breaks = ylab, labels = round(ylab, 3), 
                     sec.axis = sec_axis(trans = ~ ./scaleFactor, name = "Cumulative baseline hazard")) + 
  scale_color_manual(values = colors) + 
  theme_bw() + 
  theme(panel.background = element_blank(),                    
        axis.line.x = element_line(),                               
        axis.line.y = element_line(),
        axis.text.x = element_text(color = "black", size = 7), 
        axis.text.y = element_text(color = 'black', size = 7),
        legend.title = element_blank())
p