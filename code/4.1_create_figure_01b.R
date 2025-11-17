library(ggplot2)
library(dplyr)
library(MetBrewer)

x_vals <- seq(0, 10, length.out = 200)

data <- rbind(
  data.frame(
    x = x_vals,
    abundance = 35 * (x_vals / max(x_vals))^3,  
    guild = "facil_strong"
  ),
  data.frame(
    x = x_vals,
    abundance = 50 * (x_vals / max(x_vals))^3, 
    guild = "facil_weak"
  ),
  data.frame(
    x = x_vals,
    abundance = 50 * ((max(x_vals) - x_vals) / max(x_vals))^3, 
    guild = "compet_strong"
  ),
  data.frame(
    x = x_vals,
    abundance = 35 * ((max(x_vals) - x_vals) / max(x_vals))^3, 
    guild = "compet_weak"
  )
)

#to add labels
data <- data %>%
  mutate(interaction = ifelse(guild == "facil_weak" | guild == "facil_strong", "facilitation", "competition"))
label_data <- data %>%
  filter(guild == "compet_strong" | guild == "facil_strong") %>%  
  group_by(guild) %>%
  { bind_rows(
    filter(., guild == "facil_strong") %>% slice_tail(n = 1),
    filter(., guild == "compet_strong") %>% slice_head(n = 1)
  )
  } %>%
  ungroup()


ggplot(data, aes(x = x, y = abundance, color = guild)) +
  geom_line(linewidth = 2) +
  theme_classic() +
  labs(
    x = "abundance of other species",
    y = "abundance of\na non-excavator",
    color = "guild"
  ) +
  #to add facilitation / competition labels
  # geom_text(
  #   data = label_data,
  #   aes(label = interaction),
  #   hjust = -0.2,
  #   vjust = 2,
  #   size = 6,
  #   show.legend = FALSE
  # ) +
  scale_color_manual(values = c(MetBrewer::MetPalettes$Isfahan1[[1]][c(2,4,5,7)])) +
  theme(
    axis.title = element_text(vjust = 0.5, size = 8, colour = "black"),
    axis.line = element_line(linewidth = 0.3, color = "black"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none"
  )  

ggsave(here::here("figures/figure_01b.png"), width = 2.5, height = 2, dpi = 600, units = "in")
