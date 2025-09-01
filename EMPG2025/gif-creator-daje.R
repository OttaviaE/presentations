# gif per la stanchezza 
library(tidyverse)
IRT <- function(theta, a = 1, b = 0, c = 0,e = 1) {
  y <- c + (e - c) * (exp(a * (theta - b)) / (1 + exp(a * (theta - b))))
  return(y)
}
# calcola l'IIF per un item specifico
# i_info <- function(b, a=1,c=0, e= 1,  theta = seq(-5,5,length.out=1000)){
#   Ii = (a^2)*IRT(theta, b = b, a = a, e = e, c=c)*(1- IRT(theta, b = b, a = a, e = e,c=c ))
#   return(Ii)
# }
i_info <- function(b, a=1,c=0, e= 1,  theta = seq(-5,5,length.out=1000)){
  P = IRT(theta, b = b, a = a, e = e, c=c)
  Q = 1 - P 
  # Ii = (a^2)*(Q/P)*((P-c)/(e-c))^2
  # Ii = (a^2)*(Q*P/e^2)
  num = (a^2)*((P-c)^2)*((e-P)^2)
  den = ((e-c)^2)*P*Q
  Ii = num/den
  return(Ii)
}
item_info <- function(ipar, theta = seq(-5,5,length.out=1000)){
  item <- NULL
  for(i in 1:nrow(ipar)){
    item[[i]] <- i_info(b = ipar[i, "b"],a = ipar[i, "a"], c = ipar[i, "c"], e = ipar[i, "e"], theta = theta)
  }
  item = data.frame(do.call("cbind", item))
  colnames(item) = rownames(ipar)
  return(item)
}

# uso sempre lo stesso item e faccio vedere come può cambiare la sua curva 
# in funzione di d che cresce 
# lo faccio sia per la ICC sia per la sua IIF con colori diversi 
theta = seq(-4,4,length.out = 1000)
itempar =  data.frame(a = rep(1.5, 7), 
                      b = rep(0, 7), 
                     c = rep(0,7), 
                     e = exp(-0.1*(0:6)))
iif = item_info(itempar, theta)
# colnames(iif) = paste0("r", 1:ncol(iif))
iifs = cbind(theta, iif)
ilong = pivot_longer(iifs, cols = !theta)
ilong$name = as.integer(ilong$name)

giif = ggplot() + 
  geom_line(data = ilong, aes(x = theta, y = value, color = factor(name)), 
            linewidth = 1.2) + 
  theme_light() + ylab("IIF") + xlab(expression(theta)) + 
  theme(axis.text = element_text(size = 20), 
        axis.title = element_text(size = 22), 
        legend.position = "none")
library(gganimate)
anim <- giif +
  transition_states(name, transition_length = 1, state_length = 1) + 
  labs(subtitle = "rank: {closest_state}") +
  shadow_mark(past = TRUE) +
  theme(
    plot.subtitle = element_text(size = 24)  # Cambia qui la dimensione
  )

animate(anim, renderer = gifski_renderer("iifgif.gif"))

# grafico delle IIFs 
iics = data.frame(matrix(nrow = length(theta), ncol = nrow(itempar)))

for (i in 1:ncol(iics)) {
  iics[, i] = IRT(theta, a = 1.5, b = 0, c = 0, e = itempar$e[i])
}

iics = cbind(theta, iics)
iilong = pivot_longer(iics, cols = !theta)
iilong$name = as.integer(gsub("X", "", iilong$name))
gicc = ggplot() + 
  geom_line(data = iilong, aes(x = theta, y = value, color = factor(name)), 
            linewidth = 1.2) + 
  theme_light() + ylab("P( x = 1)") + xlab(expression(theta)) + 
  theme(axis.text = element_text(size = 20), 
        axis.title = element_text(size = 22), 
        legend.position = "none")

anim <- gicc +
  transition_states(name, transition_length = 1, state_length = 1) + 
  labs(subtitle = "rank: {closest_state}") +
  shadow_mark(past = TRUE)+
  theme(
    plot.subtitle = element_text(size = 24)  # Cambia qui la dimensione
  )

animate(anim, renderer = gifski_renderer("iccgif.gif"))

