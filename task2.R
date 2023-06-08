mean_adr <- data %>% 
  group_by(season) %>%
  summarise(mean = mean(adr),
            sd = sd(adr),
            n = n(),
            a = mean(adr ) + qnorm(0.025) * sd(adr ) / sqrt(n()),
            b = mean(adr ) + qnorm(0.975) * sd(adr ) / sqrt(n())) %>% 
  mutate(a_t = mean + qt(0.025, df = n - 1) * sd / sqrt(n),
         b_t = mean + qt(0.975, df = n - 1) * sd / sqrt(n)) %>% 
  as.data.frame()

mean_adr

median_adr <- data %>% 
  group_by(season) %>%
  summarise(median = median(adr),
            a = median(adr) - 1.96 * sd(adr) / sqrt(n()),
            b = median(adr) + 1.96 * sd(adr) / sqrt(n())) %>% 
  as.data.frame()

median_adr


ggplot(data = mean_adr) +
  geom_point(aes(x=season, y=mean), size = 4, color = "#0066cc") +
  geom_errorbar(aes(x=season, y=mean, ymin = a, ymax = b),
                width = 0.4, color = "#0066cc", size = 1) +
  labs(x = "Month", title = "Adr ~ Month (mean)") +
  theme(text = element_text(size = 15))

ggplot(data = median_adr) +
  geom_point(aes(x=season, y=median), size = 4, color = "#0066cc") +
  geom_errorbar(aes(x=season, y=median, ymin = a, ymax = b),
                width = 0.4, color = "#0066cc", size = 1) +
  labs(x = "Month", title = "Adr ~ Month (mediana)") +
  theme(text = element_text(size = 15))


df <- data %>% group_by(season) %>%
  summarise(mean_hat = mean(adr),
            var_hat = var(adr) / n())


mean_hat_s <- df %>% filter(season == 'Warm') %>% pull(mean_hat)
mean_hat_ns <- df %>% filter(season == 'Cold') %>% pull(mean_hat)
var_hat_s <- df %>% filter(season == 'Warm') %>% pull(var_hat)
var_hat_ns <- df %>% filter(season == 'Cold') %>% pull(var_hat)
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



