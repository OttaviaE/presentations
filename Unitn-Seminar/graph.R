library(ggplot2)
library(gganimate)
setwd("C:/Users/ottae/Documents/GitHub/presentations/Unitn-Seminar")
data = read.csv("irt-freq.csv")

p = ggplot(data[!data$Year %in% 2024, ], 
       aes(x = Year, y = Freq)) + 
  geom_point(size = 4) + geom_line() + ylab("Number of publications") + xlab("Year") + 
  theme_bw() + scale_x_continuous(n.breaks = 20) + 
  theme(axis.text = element_text(size = 24), 
        axis.title = element_text(size = 28), 
        axis.text.y = element_text(size = 22)) +   transition_reveal(Year)
p

animate(p, device = "png",
        renderer = file_renderer("~/gganim", 
                                 prefix = "gganim_plot", overwrite = TRUE), 
        width = 14.00, height = 9.00, units = "in", res = 150)
ggplot(data[!data$Year %in% 2024, ], 
      aes(x = Year, y = Freq)) + 
  geom_point(size = 1.2) + geom_line() + ylab("Number of publications") + xlab("Year") + 
  theme_bw() + scale_x_continuous(n.breaks = 20) + 
  theme(axis.text = element_text(size = 24), 
        axis.title = element_text(size = 28), 
        axis.text.y = element_text(size = 22))
