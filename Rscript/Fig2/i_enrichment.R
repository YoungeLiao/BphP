rm(list=ls())
library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggbeeswarm)
library(ggpubr)

# 设置CNS风格的主题
theme_cns <- theme(
  panel.background = element_rect(fill = "transparent"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.line = element_line(color = "black", size = 0.4),
  axis.ticks = element_line(color = "black", size = 0.4),
  axis.text.x = element_text(color = "black", size = 7, face = "plain", 
                             hjust = 1), # angle = 30, 
  axis.text.y = element_text(color = "black", size = 7, face = "plain"),
  axis.title = element_text(color = "black", size = 7, face = "plain"),
  legend.position = "right", # "right"
  legend.title = element_blank(),
  legend.text = element_text(size = 7, face = "plain"),
  legend.background = element_rect(fill = "white", color = NA),
  legend.key = element_rect(fill = "white", color = NA),
  plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
  plot.title = element_text(size = 14, face = 'plain', hjust = 0.5)
)

# load data
path <- './data/Fig2/Fig2i_IRCandidate.xlsx'
sheet.name <- 'Metagenomics' 
growth <- data.frame(read_excel(path, sheet = sheet.name))
head(growth)

# ensure correct types
growth$Enrichment <- as.numeric(as.character(growth$Enrichment))
# growth$Enrichment <- as.numeric(as.character(growth$Enrichment))
growth$Group <- factor(growth$Group, levels = c("IR", "Dark"))

# CNS-like colors
main_colors <- c("Dark" = "#023270", "IR" = "#D34947")

library(dplyr)
library(ggplot2)
library(ggbeeswarm)
library(ggpubr)


# define genus_order (order genera by overall mean Enrichment, descending; fallback to existing levels)
genus_order <- growth %>%
  group_by(Genus) %>%
  summarise(meanEnrichment = mean(Enrichment, na.rm = TRUE), n = sum(!is.na(Enrichment)), .groups = "drop") %>%
  arrange(desc(-meanEnrichment)) %>%
  pull(Genus) %>%
  as.character()
if (length(genus_order) == 0) genus_order <- unique(as.character(growth$Genus))

# ---------- Growth: ORIGINAL SCALE with error bars (show points; center error bars, only upper part) ----------
summary_growth <- growth %>%
  group_by(Genus, Group) %>%
  summarise(
    meanGrowth = mean(Enrichment, na.rm = TRUE),
    sdGrowth = sd(Enrichment, na.rm = TRUE),
    n = sum(!is.na(Enrichment)),
    seGrowth = ifelse(n > 0, sdGrowth / sqrt(n), NA_real_),
    .groups = "drop"
  )

summary_growth$Genus <- factor(summary_growth$Genus, levels = genus_order)
growth$Genus <- factor(growth$Genus, levels = genus_order)

dodge_width <- 0.75

p_growth <- ggplot() +
  # bars (dodge by Group)
  geom_col(data = summary_growth,
           aes(x = Genus, y = meanGrowth, fill = Group, group = Group),
           position = position_dodge(width = dodge_width),
           width = 0.75,
           color = "grey30") +
  # error bars showing only the upper half (from mean to mean + SE)
  geom_errorbar(data = summary_growth,
                aes(x = Genus, ymin = meanGrowth, ymax = meanGrowth + seGrowth, group = Group),
                position = position_dodge(width = dodge_width),
                width = 0.25,
                color = "black", size = 0.6) +
  # raw data points (jitter within each dodged position)
  geom_point(data = growth,
             aes(x = Genus, y = Enrichment, fill = Group, group = Group),
             shape = 21, color = "grey20", size = 1.6, alpha = 0.9,
             position = position_jitterdodge(jitter.width = 0.15, dodge.width = dodge_width)) +
  coord_flip() +
  scale_fill_manual(values = main_colors, name = "Group") +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60") +
  labs(x = "", y = "Abundance Enrichment", title = "") +
  theme_pubr(base_size = 12) +
  theme_cns
  # theme(
  #   axis.text.y = element_text(size = 12),
  #   legend.position = "top",
  #   plot.title = element_text(hjust = 0.5, face = "bold"),
  #   panel.grid.major.y = element_blank()
  # )

print(p_growth)

# ---------- Growth: log transformation ----------
# 在数据预处理部分添加对数变换
growth$log_Enrichment <- log1p(growth$Enrichment)  # 使用log1p避免log(0)的问题

# 修改summary_growth的计算，使用对数变换后的值
summary_growth <- growth %>%
  group_by(Genus, Group) %>%
  summarise(
    meanGrowth = mean(log_Enrichment, na.rm = TRUE),
    sdGrowth = sd(log_Enrichment, na.rm = TRUE),
    n = sum(!is.na(log_Enrichment)),
    seGrowth = ifelse(n > 0, sdGrowth / sqrt(n), NA_real_),
    .groups = "drop"
  )

# 修改绘图部分，使用对数变换后的值
p_growth <- ggplot() +
  geom_col(data = summary_growth,
           aes(x = Genus, y = meanGrowth, fill = Group, group = Group),
           position = position_dodge(width = dodge_width),
           width = 0.75,
           color = "grey30") +
  geom_errorbar(data = summary_growth,
                aes(x = Genus, ymin = meanGrowth, ymax = meanGrowth + seGrowth, group = Group),
                position = position_dodge(width = dodge_width),
                width = 0.25,
                color = "black", size = 0.6) +
  geom_point(data = growth,
             aes(x = Genus, y = log_Enrichment, fill = Group, group = Group),
             shape = 21, color = "grey20", size = 1.6, alpha = 0.9,
             position = position_jitterdodge(jitter.width = 0.15, dodge.width = dodge_width)) +
  coord_flip() +
  scale_fill_manual(values = main_colors, name = "Group") +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60") +
  labs(x = "", y = "Log(Abundance Enrichment)", title = "") +
  theme_pubr(base_size = 12) +
  theme_cns



# ---------- Growth: symlog ----------
# 在数据预处理部分添加标准化
# 定义对称对数变换函数
symlog_trans <- function(x, threshold = 1) {
  sign(x) * log1p(abs(x) / threshold)
}

# 在数据预处理部分添加对称对数变换
growth$symlog_Enrichment <- symlog_trans(growth$Enrichment)

# 修改summary_growth的计算，使用对称对数变换后的值
summary_growth <- growth %>%
  group_by(Genus, Group) %>%
  summarise(
    meanGrowth = mean(symlog_Enrichment, na.rm = TRUE),
    sdGrowth = sd(symlog_Enrichment, na.rm = TRUE),
    n = sum(!is.na(symlog_Enrichment)),
    seGrowth = ifelse(n > 0, sdGrowth / sqrt(n), NA_real_),
    .groups = "drop"
  )

# 修改绘图部分，使用对称对数变换后的值
p_growth <- ggplot() +
  geom_col(data = summary_growth,
           aes(x = Genus, y = meanGrowth, fill = Group, group = Group),
           position = position_dodge(width = dodge_width),
           width = 0.75,
           color = "grey30") +
  geom_errorbar(data = summary_growth,
                aes(x = Genus, ymin = meanGrowth, ymax = meanGrowth + seGrowth, group = Group),
                position = position_dodge(width = dodge_width),
                width = 0.25,
                color = "black", size = 0.6) +
  geom_point(data = growth,
             aes(x = Genus, y = symlog_Enrichment, fill = Group, group = Group),
             shape = 21, color = "grey20", size = 1.6, alpha = 0.9,
             position = position_jitterdodge(jitter.width = 0.15, dodge.width = dodge_width)) +
  coord_flip() +
  scale_fill_manual(values = main_colors, name = "Group") +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60") +
  labs(x = "", y = "Symlog(Abundance Enrichment)", title = "") +
  theme_pubr(base_size = 12) +
  theme_cns

p_growth


# # ------------------ break ------------------
# # 首先加载ggbreak包
# library(ggbreak)
# 
# # 在绘图部分添加坐标轴打断
# p_growth <- ggplot() +
#   geom_col(data = summary_growth,
#            aes(x = Genus, y = meanGrowth, fill = Group, group = Group),
#            position = position_dodge(width = dodge_width),
#            width = 0.75,
#            color = "grey30") +
#   geom_errorbar(data = summary_growth,
#                 aes(x = Genus, ymin = meanGrowth, ymax = meanGrowth + seGrowth, group = Group),
#                 position = position_dodge(width = dodge_width),
#                 width = 0.25,
#                 color = "black", size = 0.6) +
#   geom_point(data = growth,
#              aes(x = Genus, y = Enrichment, fill = Group, group = Group),
#              shape = 21, color = "grey20", size = 1.6, alpha = 0.9,
#              position = position_jitterdodge(jitter.width = 0.15, dodge.width = dodge_width)) +
#   coord_flip() +
#   scale_fill_manual(values = main_colors, name = "Group") +
#   geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60") +
#   labs(x = "", y = "Abundance Enrichment", title = "") +
#   theme_pubr(base_size = 12) +
#   theme_cns +
#   # 添加y轴打断，在指定位置断开
#   scale_y_break(c(10, 260), space = 0.2) +  # 在10到100之间断开，space参数控制断开处的间距
#   scale_y_break(c(330, 550), space = 0.2)
# 
# p_growth
