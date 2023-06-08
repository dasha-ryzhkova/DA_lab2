library(ggplot2)
library(dplyr)

ggplot(data = mean_adr) +
  geom_point(aes(x=area, y=mean), size = 4, color = "#0066cc") +
  geom_errorbar(aes(x=area, y=mean, ymin = a, ymax = b),
                width = 0.4, color = "#0066cc", size = 1) +
  labs(x = "Area", title = "Adr ~ area (mean)") +
  theme(text = element_text(size = 15))

ggplot(data = median_adr) +
  geom_point(aes(x=area, y=median), size = 4, color = "#0066cc") +
  geom_errorbar(aes(x=area, y=median, ymin = a, ymax = b),
                width = 0.4, color = "#0066cc", size = 1) +
  labs(x = "Area", title = "Adr ~ area (mediana)") +
  theme(text = element_text(size = 15))


df <- data %>% group_by(area) %>%
  summarise(mean_hat = mean(adr),
            var_hat = var(adr) / n())

# North ~ South
mean_hat_s <- df %>% filter(area == 'North') %>% pull(mean_hat)
mean_hat_ns <- df %>% filter(area == 'South') %>% pull(mean_hat)
var_hat_s <- df %>% filter(area == 'North') %>% pull(var_hat)
var_hat_ns <- df %>% filter(area == 'South') %>% pull(var_hat)
se <- sqrt(var_hat_s + var_hat_ns)
T <- (mean_hat_s - mean_hat_ns) / se
p_value <- pnorm(T, lower.tail = FALSE)
conf.int <- c(mean_hat_s - mean_hat_ns - qnorm(0.95)*se, Inf)

mean_hat_s
mean_hat_ns

var_hat_s
var_hat_ns

p_value

conf.int

# North ~ Centre
mean_hat_s <- df %>% filter(area == 'North') %>% pull(mean_hat)
mean_hat_ns <- df %>% filter(area == 'Centre') %>% pull(mean_hat)
var_hat_s <- df %>% filter(area == 'North') %>% pull(var_hat)
var_hat_ns <- df %>% filter(area == 'Centre') %>% pull(var_hat)
se <- sqrt(var_hat_s + var_hat_ns)
T <- (mean_hat_s - mean_hat_ns) / se
p_value <- pnorm(T, lower.tail = FALSE)
conf.int <- c(mean_hat_s - mean_hat_ns - qnorm(0.95)*se, Inf)

mean_hat_s
mean_hat_ns

var_hat_s
var_hat_ns

p_value

conf.int


# Centre ~ South
mean_hat_s <- df %>% filter(area == 'Centre') %>% pull(mean_hat)
mean_hat_ns <- df %>% filter(area == 'South') %>% pull(mean_hat)
var_hat_s <- df %>% filter(area == 'Centre') %>% pull(var_hat)
var_hat_ns <- df %>% filter(area == 'South') %>% pull(var_hat)
se <- sqrt(var_hat_s + var_hat_ns)
T <- (mean_hat_s - mean_hat_ns) / se
p_value <- pnorm(T, lower.tail = FALSE)
conf.int <- c(mean_hat_s - mean_hat_ns - qnorm(0.95)*se, Inf)

mean_hat_s
mean_hat_ns

var_hat_s
var_hat_ns

p_value

conf.int




