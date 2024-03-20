

rm(list = ls())

{if(!require(tidyverse)) install.packages('tidyverse')
  require(tidyverse)}

# Identidade --------------------------------------------------------------

x <- seq(-5,5,by = 0.1)
y <- x

ggplot(data.frame(x=x,y=y)) +
  geom_hline(yintercept = 0, linetype = "dashed") + geom_vline(xintercept = 0, linetype = "dashed") +
  geom_line(aes(x=x,y=y), colour = '#000000', size = 1.5) +
  labs(x = 'X', y = 'f(x)') + 
  theme_bw() 



# Fim ---------------------------------------------------------------------


# Logística (Sigmoide) ----------------------------------------------------

x <- seq(-5,5,by = 0.1)
y <- 1/(1+exp(-x))

ggplot(data.frame(x=x,y=y)) +
  geom_hline(yintercept = 0, linetype = "dashed") + geom_vline(xintercept = 0, linetype = "dashed") +
  geom_line(aes(x=x,y=y), colour = '#000000', size = 1.5) +
  labs(x = 'X', y = 'f(x)') + 
  theme_bw() 


# Fim ---------------------------------------------------------------------


# Tangente Hiperbólica ----------------------------------------------------

x <- seq(-5,5,by = 0.1)
y <- (exp(x)-exp(-x))/(exp(x)+exp(-x))

ggplot(data.frame(x=x,y=y)) +
  geom_hline(yintercept = 0, linetype = "dashed") + geom_vline(xintercept = 0, linetype = "dashed") +
  geom_line(aes(x=x,y=y), colour = '#000000', size = 1.5) +
  labs(x = 'X', y = 'f(x)') + 
  theme_bw() 


# Fim ---------------------------------------------------------------------


# ReLu (Rectified Linear) -------------------------------------------------

x <- seq(-5,5,by = 0.1)
y <- pmax(0,x)

ggplot(data.frame(x=x,y=y)) +
  geom_hline(yintercept = 0, linetype = "dashed") + geom_vline(xintercept = 0, linetype = "dashed") +
  geom_line(aes(x=x,y=y), colour = '#000000', size = 1.5) +
  labs(x = 'X', y = 'f(x)') + 
  theme_bw() 

# Fim ---------------------------------------------------------------------


# Leaky ReLu --------------------------------------------------------------

x <- seq(-5,5,by = 0.1)
y <- ifelse(x < 0, 0.1 * x, x)

ggplot(data.frame(x=x,y=y)) +
  geom_hline(yintercept = 0, linetype = "dashed") + geom_vline(xintercept = 0, linetype = "dashed") +
  geom_line(aes(x=x,y=y), colour = '#000000', size = 1.5) +
  labs(x = 'X', y = 'f(x)') + 
  theme_bw() 

# Fim ---------------------------------------------------------------------



