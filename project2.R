data(ToothGrowth)

summary(ToothGrowth)

g <- ggplot(ToothGrowth, aes(x = dose, y = len, group = factor(supp)))
g <- g + geom_line(size = 1, aes(colour = supp)) + 
    geom_point(size =10, pch = 21, fill = "salmon", alpha = .5)
g
