#Load required libraries
library(reshape2)
library(dplyr)
library(e1071)
library(car)
library(ggplot2)

unzip(zipfile = "data/ERWaiting.csv.zip", exdir = "data/")
ERread <- read.csv("data/ERWaiting.csv")
ER <- melt(ERread, id = "day")
print(ER)

# Descriptive summary
ER.df <- ER %>%
  summarise(
    group_by(variable) %>%
    count = n(),
    sum = sum(value, na.rm = TRUE),
    min = min(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    mean = mean(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    range = max - min,
    sd = sd(value, na.rm = TRUE),
    var = var(value, na.rm = TRUE),
    kurt = kurtosis(value, na.rm = TRUE),
    skew = skewness(value, na.rm = TRUE),
    shapiro = shapiro.test(value)$statistic[1]
  )

print(ERread.df)

## ANOVA
## calculate anova results

fit <- lm(data = ERread,variable ~ value)
ERread.aov <- ERread(fit)
ERread.aov.summary <- summary(ERread.aov)

## Print results
print(ER.aov.summary)

# Perform Tukey-Kramer
cars.tukey <- TukeyHSD(cars.aov)

##Print results
print(cars.tukey)

## Create a plot and save it into a variable
qqp <- ggplot(cars) +
  stat_qq(aes(sample = mpg, colour = factor(cyl))) +
  guides(col = guide_legend(title = "Cylinder"))

## Homogenity of variance
cars.levene <- leveneTest(fit)

# Get values for ploting
## Get the degrees of freedom
df <- anova(fit)[, "Df"]
names(df) <- c("between", "within")

# Set alpha error
alpha <- 0.05

## Get the f values
cars.f.crit <- qf(alpha, df["between"], df["within"], lower.tail = FALSE)
cars.f.value <- cars.aov.summary[[1]]$F[1]

# Plot ANOVA results
## Plot Boxplot of the data set
bp <- ggplot(cars, aes(x = cyl, y = mpg)) +
  stat_boxplot(geom = "errorbar") + # Add error bars to the boxplot
  geom_boxplot() + # Add boxplot
  labs(y = "MPG", x = "Cylinders") # Tify up the axis title


# Save plot
ggsave(filename = "graphs/CarsBoxplot.pdf", plot = bp)

# Plot F-distribution
### Display settings
ncp <- 0 # Non centrality parameter
frameEmpty <- "black" # Color for the empty frame
areaEmpty <- "white" # Color for the empty area
frameH0 <- "green4" # Color for the H0 frame
areaH0 <- "green3" # Color for the H0 area
frameH1 <- "red4" # Color for the H1 frame
areaH1 <- "red2" # Color for the H1 area

### Distribution specific settings
df1 <- df[1] # Degree of freedom first parameter
df2 <- df[2] # Degree of freedom second parameter
length <- 500 # number of elements

### Data preperation
x <- seq(from = 0, to = cars.f.value+2, length = length) # Set vector range 
dd <- data.frame(x = seq(from = 0, to = cars.f.value+2, length = length),  # Create data frame
                 y = df(x = x, df1 = df1, df2 = df2, ncp = 5))

### Create F-distribution plot
pf <- ggplot(data = dd) # Create the plot
pf <- pf + labs(y = "Relative frequency", x = "F-values") # Tidy up the axis title
pf <- pf + geom_area(aes(x = x, y = y),
                     color = frameH0, fill = areaH0) # Add the H0 area
pf <- pf + geom_area(data = subset(dd, x > cars.f.crit),
                     aes(x = x, y = y),
                     fill = areaH1, color = frameH1) # Add the H1 area
pf <- pf + geom_vline(xintercept = cars.f.crit, colour = frameH1, linetype = "longdash") # Add the F-critical value line
pf <- pf + geom_vline(xintercept = cars.f.value, colour = "black", linetype = "dotted") # Add the F-value line
pf <- pf + scale_x_continuous(breaks = sort(round(c(seq(from = min(dd$x), # Add tick marks for the F values
                                                        to = round(max(dd$x),0),
                                                        by = 2), cars.f.crit, cars.f.value),2)))
pf <- pf + annotate("text", y = .2, x = cars.f.value + 1,
                    label = paste("Pr(>F) = ", round(cars.aov.summary[[1]]$Pr[1],3))) # Add p-value to plot

#### Save plot
ggsave("graphs/fDistribution.pdf", pf)