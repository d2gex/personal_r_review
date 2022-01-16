library(readxl)
library(ggplot2)
ekev <- read_excel("ekev.xls", skip = 1)

# (1) Create linear regression model and plot it
plot(Ba ~ Distance, ekev)
modBa <- lm(Ba ~ Distance, ekev)
abline(modBa)

# (2) Apply logartithm transformation to variable BA
plot(log(Ba) ~ Distance, ekev)
modBa2 <- lm(log(Ba) ~ Distance, ekev)
abline(modBa2)


# (3) Compare two models 
fitting_model_colors <- c("dodgerblue", "tomato")

plot(Ba ~ Distance, ekev)
grid()
# lwd is line width of the fitting line
abline(modBa, lwd = 2, col = fitting_model_colors[1])
xs <- seq(0, 8, len = 100)
ys <- exp(predict(modBa2, newdata = data.frame(Distance = xs)))
lines(xs, ys, lwd = 2, col = fitting_model_colors[2])

legend("topright",
       c("linear", "semilog"),
       lwd = 2,
       col = fitting_model_colors,
       title = "model",
       inset = 0.02)

# (4) Same as 3 but using ggplot
preds2 <- data.frame(Distance = xs, Ba = ys)
# Look at this tutorial: https://youtu.be/HPJn1CMvtmI
# layer 0: x and y are the variables to be represented. "aes" is the aesthetic 
ggplot(data=ekev, mapping = aes(x=Distance, y=Ba)) +  
  # layer 1: size refers to that of the points and pch is the shape of it
  geom_point(pch = 21, size = 2) +
  # layer 2: add a smooth line over the point using a given method
  # method used: linear regression; standar error: none 
  geom_smooth(method = "lm", se = F, aes(col = "linear")) +
  # layer 3: this is another layer build on top of the graph hence data for this 
  # layer needs to be fed in again; we use line width and the aesthetics on top as 
  # the line is an individual object
  geom_line(data = preds2, lwd = 1, aes(col = "semilog")) +
  # layer 4: add colours blue and red to the two fitting lines
  scale_color_manual("model", values = c("linear" = "dodgerblue", "semilog" = "tomato")) +
  # layer 5: add a black and white background, it could be theme_x where x is the type of
  # the thme. Look at the doc to know how many of them are.
  theme_bw(base_size = 16)

plot(Pb ~ Distance, data = ekev)
plot(Pb ~ Distance, ekev, log = "xy")
modPb <- lm(log10(Pb) ~ log10(Distance), ekev)
abline(modPb)
summary(modPb)
