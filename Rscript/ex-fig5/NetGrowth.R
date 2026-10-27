rm(list=ls())
# load library
library(readxl)
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)
# library(extrafont)
# font_import(prompt = FALSE)
# loadfonts(device = "pdf")


# ========== 1. Load basic data and metadata ==========

path.MAG.info <- './data/ex-fig5/a_MAG_Genus_Merged.xlsx'
MAG.info <- as.data.frame(read_excel(path.MAG.info))

netgrowth_long <- MAG.info %>%
  select(Genus, MAG, Initial, Dark1, Dark2, Dark3, IR1, IR2, IR3) %>%
  pivot_longer(
    cols = c(Initial, Dark1, Dark2, Dark3, IR1, IR2, IR3),
    names_to = "Condition",
    values_to = "Abundance"
  ) %>%
  mutate(
    Group = case_when(
      grepl("^Dark", Condition) ~ "Dark",
      grepl("^IR", Condition) ~ "IR",
      Condition == "Initial" ~ "Initial"
    ),
    Replicate = str_extract(Condition, "[123]")
  )

initial_df <- netgrowth_long %>%
  filter(Condition == "Initial") %>%
  select(Genus, MAG, Initial_Abundance = Abundance)

netgrowth_long <- netgrowth_long %>%
  left_join(initial_df, by = c("Genus", "MAG")) %>%
  mutate(
    NetGrowth = Abundance - Initial_Abundance
  )

netgrowth_plotdf <- netgrowth_long %>%
  filter(Group %in% c("Dark", "IR"))

summary_df <- netgrowth_plotdf %>%
  group_by(Genus, Group) %>%
  summarise(
    Mean_NetGrowth = mean(NetGrowth, na.rm = TRUE),
    SD_NetGrowth = sd(NetGrowth, na.rm = TRUE),
    n = sum(!is.na(NetGrowth)),
    SE_NetGrowth = SD_NetGrowth / sqrt(n)
  ) %>%
  ungroup()

genus_netgrowth_mean <- summary_df %>%
  group_by(Genus) %>%
  summarise(NetGrowthMean = mean(Mean_NetGrowth, na.rm = TRUE)) %>%
  arrange(desc(NetGrowthMean)) %>%
  slice_head(n = 25)

top20_genus <- genus_netgrowth_mean$Genus

summary_df <- summary_df %>%
  filter(Genus %in% top20_genus)

summary_df$Genus <- factor(summary_df$Genus, levels = top20_genus)

summary_df <- summary_df %>%
  mutate(
    Mean_NetGrowth_log = sign(Mean_NetGrowth) * log10(abs(Mean_NetGrowth) + 1),
    SE_NetGrowth_log = SE_NetGrowth / (abs(Mean_NetGrowth) + 1) / log(10)  # 近似变换后的误差
  )

library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(scales)
library(ggsignif)

netgrowth_points <- netgrowth_plotdf %>%
  filter(Genus %in% levels(summary_df$Genus)) %>%
  mutate(
    NetGrowth_log = sign(NetGrowth) * log10(abs(NetGrowth) + 1)
  )

netgrowth_points$Genus <- factor(netgrowth_points$Genus, levels = levels(summary_df$Genus))

stat_test <- netgrowth_points %>%
  filter(Group %in% c("Dark", "IR")) %>%
  group_by(Genus) %>%
  summarise(
    p.value = tryCatch(
      t.test(NetGrowth_log ~ Group)$p.value,
      error = function(e) NA_real_
    ),
    .groups = "drop"
  ) %>%
  mutate(
    p.signif = case_when(
      is.na(p.value) ~ "",
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    )
  )

y_max_df <- summary_df %>%
  group_by(Genus) %>%
  summarise(y_max = max(Mean_NetGrowth_log + SE_NetGrowth_log, na.rm = TRUE), .groups = "drop")

stat_test <- stat_test %>%
  left_join(y_max_df, by = "Genus") %>%
  mutate(
    y.position = y_max + 0.18 
  )


# 设置CNS风格的主题
cns_theme <- theme(
  text = element_text(family = "Arial"),
  panel.background = element_rect(fill = "transparent"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.line = element_line(color = "black", size = 0.4),
  axis.ticks = element_line(color = "black", size = 0.4),
  axis.text.x = element_text(color = "black", size = 7, face = "plain",angle = 45, hjust = 1, vjust = 1), # hjust = 1 # angle = 30, 
  axis.text.y = element_text(color = "black", size = 7, face = "plain"),
  axis.title = element_text(color = "black", size = 7, face = "plain"),
  legend.position = "right",
  legend.title = element_blank(),
  legend.text = element_text(size = 7, face = "plain"),
  legend.background = element_rect(fill = "white", color = NA),
  legend.key = element_rect(fill = "white", color = NA),
  plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
  plot.title = element_text(size = 7, face = 'plain', hjust = 0.5)
)

cns_colors <- c(
  "Dark" = "#023270",  
  "IR" = "#D34947"      
)

p <- ggplot(summary_df, aes(x = Genus, y = Mean_NetGrowth_log, group = Group)) +
  geom_bar(
    aes(color = Group),
    stat = "identity",
    position = position_dodge(width = 0.8),
    width = 0.8,
    fill = NA,
    size = 0.75
  ) +

  geom_errorbar(
    aes(ymin = Mean_NetGrowth_log - SE_NetGrowth_log, ymax = Mean_NetGrowth_log + SE_NetGrowth_log, group = Group),
    position = position_dodge(width = 0.7),
    width = 0.4,
    size = 0.5,
    color = "black"
  ) +

  geom_point(
    data = netgrowth_points,
    aes(x = Genus, y = NetGrowth_log, color = Group, fill = Group),
    position = position_jitterdodge(jitter.width = 0.18, dodge.width = 0.7),
    size = 2,
    shape = 21,
    stroke = 0.7,
    alpha = 0.6
  ) +

  geom_text(
    data = stat_test,
    aes(x = Genus, y = y.position, label = p.signif),
    inherit.aes = FALSE,
    size = 5,
    vjust = 0,
    fontface = "bold",
    color = "#D7263D"
  ) +
  scale_color_manual(
    values = cns_colors,
    name = "Condition",
    breaks = c("Dark", "IR"),
    labels = c("Dark", "IR")
  ) +
  scale_fill_manual(
    values = cns_colors,
    name = "Condition",
    breaks = c("Dark", "IR"),
    labels = c("Dark", "IR")
  ) +
  labs(
    x = "",
    y = "Net Abundance Growth (log)",
    title = NULL
  ) +
  cns_theme +
  coord_cartesian(clip = "off") +
  guides(
    color = guide_legend(
      override.aes = list(size = 3, shape = 21), # , fill = cns_colors[c("Dark", "IR")]
      order = 1
    ),
    fill = "none"
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0.05, 0.22)),
    breaks = pretty_breaks(n = 7)
  )

ggsave("Fig2d_NetGrowth_Barplot.pdf", p, width = 9, height = 5, dpi = 400, units = "in", useDingbats = FALSE)

write.csv(summary_df, file = "results_data/Fig2/Fig2e_NetGrowth_summary_df.csv", row.names = FALSE)

print(p)
