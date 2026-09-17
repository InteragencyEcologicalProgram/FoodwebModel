#legnth weight regressions

library(tidyverse)
library(readxl)

LW = read_excel("data/legnthweight.xlsx")
x = c(0, 15, by = 0.1)

LW = mutate(LW, equation = paste("Y ~ ",A,"*x","^",B, sep = ""))


df_plot <- LW %>%
  mutate(
    formula_id = row_number()
  ) %>%
  crossing(X = seq(0, 15, by = 0.1)) %>%
  mutate(
    Y = A * X^B
  )%>%
  mutate(Y2 = case_when(WetDry == "Wet" ~ Y/10,
                        TRUE ~ Y),
         Preservative = case_when(is.na(Preservative) ~ "Unknown",
                                        TRUE ~ Preservative)) %>%
  filter()


ggplot(df_plot, aes(x = X, y = Y, group = formula_id)) +
  geom_line(aes(group = factor(formula_id)), linewidth = 1) +
  labs(
    x = "legnth",
    y = "Weight"
  ) +
  facet_wrap(WetDry~Taxon, scales = "free_y")

meanLW = group_by(LW, Taxon, WetDry) %>%
  filter(A != max(A), A != min(A)) %>%
  
  summarize(A = mean(A), B = mean(B))



df_plotmean <- meanLW  %>%
  mutate(
    formula_id = row_number()
  ) %>%
  crossing(X = seq(0, 15, by = 0.1)) %>%
  mutate(
    Y = A * X^B
  ) %>%
  mutate(Y2 = case_when(WetDry == "Wet" ~ Y/10,
                        TRUE ~ Y))


ggplot(df_plot, aes(x = X, y = Y2, group = formula_id)) +
  geom_line(aes(group = factor(formula_id), color = log(A)), linewidth = 1) +
  scale_color_viridis_c()+
  geom_line(data = df_plotmean, aes(group = factor(formula_id), x = X, y = Y2), linetype =2, color = "red",
            linewidth =1)+
  labs(
    x = "legnth",
    y = "Weight"
  ) +
  facet_wrap(WetDry~Taxon, scales = "free_y")


ggplot(df_plot, aes(x = X, y = Y2, group = formula_id, linetype = WetDry)) +
  geom_line(aes(group = factor(formula_id), color = log(A)), linewidth = 1) +
  scale_color_viridis_c()+
  geom_line(data = df_plotmean, aes(group = factor(formula_id), x = X, y = Y2), color = "red",
            linewidth =1)+
  labs(
    x = "legnth",
    y = "Weight"
  ) +
  facet_wrap(~Taxon, scales = "free_y")

ggplot(df_plot, aes(x = X, y = Y2, group = formula_id, linetype = WetDry)) +
  geom_line(aes(group = factor(formula_id), color = Preservative), linewidth = 1) +
  
  geom_line(data = df_plotmean, aes(group = factor(formula_id), x = X, y = Y2), color = "red",
            linewidth =1)+
  labs(
    x = "legnth",
    y = "Weight"
  ) +
  facet_wrap(~Taxon, scales = "free_y")

ggplot(filter(df_plot, Taxon == "Neomysis mercedis"), aes(x = X, y = Y2, group = formula_id, linetype = WetDry)) +
  geom_line(aes(group = factor(formula_id), color = Preservative), linewidth = 1) +
  
  geom_line(data = filter(df_plotmean, Taxon == "Neomysis mercedis"), aes(group = factor(formula_id), x = X, y = Y2), color = "black",
            linewidth =1)+
  labs(
    x = "legnth",
    y = "Weight"
  ) +
  facet_wrap(~Taxon, scales = "free_y")



ggplot(filter(df_plot, Taxon == "Hyperacanthomysis longirostris"), aes(x = X, y = Y2, group = formula_id, linetype = WetDry)) +
  geom_line(aes(group = factor(formula_id), color = Preservative), linewidth = 1) +
  
  geom_line(data = filter(df_plotmean, Taxon == "Hyperacanthomysis longirostris"), aes(group = factor(formula_id), x = X, y = Y2), color = "black",
            linewidth =1)+
  labs(
    x = "legnth",
    y = "Weight"
  ) +
  facet_wrap(~Taxon, scales = "free_y")
