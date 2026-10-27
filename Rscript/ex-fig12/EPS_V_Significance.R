# rm(list=ls())
# library(readxl)
# library(ggplot2)
# library(ggpubr)
# library(dplyr)
# library(scales)
# library(broom)
# 
# # 读取数据
# path <- './data/Fig4/EPS.xlsx'
# sheet.name <- 'CongoRed'
# rawdata <- data.frame(read_excel(path, sheet = sheet.name))
# 
# # 过滤掉Control组的数据，并只保留72小时以内的数据
# data_filtered <- rawdata %>%
#   filter(Strain != "Control", Time <= 72)
# # data_filtered <- rawdata %>%
# #   filter(Strain != "Control")
# 
# # 对每个菌株和光照条件下的EPS随时间变化进行线性拟合
# slopes <- data_filtered %>%
#   group_by(Strain, Light) %>%
#   do(tidy(lm(EPS ~ Time, data = .), conf.int = TRUE)) %>%
#   filter(term == "Time") %>%
#   select(Strain, Light, estimate, std.error, conf.low, conf.high) %>%
#   rename(slope = estimate, se = std.error)
# 
# # 进行ANOVA分析，检验不同光照条件下的变化速率差异是否显著
# significance <- data_filtered %>%
#   group_by(Strain) %>%
#   do(tidy(aov(lm(EPS ~ Time * Light, data = .)))) %>%
#   filter(grepl("Light", term)) %>%
#   select(Strain, term, p.value) %>%
#   mutate(sig_label = ifelse(p.value < 0.001, "***",
#                             ifelse(p.value < 0.01, "**",
#                                    ifelse(p.value < 0.05, "*", "ns"))))
# 
# # 计算每个菌株的最大斜率，用于确定显著性标记的位置
# max_slopes <- slopes %>%
#   group_by(Strain) %>%
#   summarise(max_slope = max(slope, na.rm = TRUE))
# 
# # 将最大斜率信息合并到显著性数据框中
# significance <- left_join(significance, max_slopes, by = "Strain")
# 
# # 创建CNS风格的柱状图
# p <- ggplot(slopes, aes(x = Strain, y = slope, fill = Light)) +
#   # 添加柱状图
#   geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.7) +
#   # 添加误差棒
#   geom_errorbar(aes(ymin = slope - se, ymax = slope + se),
#                 position = position_dodge(width = 0.9), width = 0.25) +
#   # 添加显著性标记
#   geom_text(data = significance, aes(x = Strain, y = max_slope * 1.1, label = sig_label),
#             inherit.aes = FALSE, size = 5) +
#   # 设置CNS风格的主题
#   theme_classic() +
#   theme(
#     text = element_text(size = 12, face = "plain"),  # 统一字体大小，不加粗
#     axis.title = element_text(size = 12, face = "plain"),
#     axis.text = element_text(size = 12, face = "plain"),
#     legend.title = element_text(size = 12, face = "plain"),
#     legend.text = element_text(size = 12, face = "plain"),
#     strip.text = element_text(size = 12, face = "plain"),
#     strip.background = element_blank(),
#     panel.grid.major = element_line(color = "grey90", size = 0.5),
#     panel.grid.minor = element_line(color = "grey95", size = 0.25)
#   ) +
#   # 设置坐标轴标签
#   labs(x = "Strain", y = "EPS Change Rate (a.u./h)", fill = "Light") +
#   # 设置颜色
#   scale_fill_manual(values = c("Dark" = "#4E79A7", "IR" = "#E15759"))
# 
# # 显示图表
# print(p)
# 
# # 保存图表（可选）
# # ggsave("./Fig4_EPS_rate.png", p, width = 8, height = 6, dpi = 300)
# 
# # ------------------------ v 1.2 -----------------------------
# rm(list=ls())
# library(readxl)
# library(ggplot2)
# library(ggpubr)
# library(dplyr)
# library(scales)
# library(broom)
# 
# # 读取数据
# path <- './data/Fig4/EPS.xlsx'
# sheet.name <- 'v'
# rawdata <- data.frame(read_excel(path, sheet = sheet.name))
# 
# # 过滤掉Control组的数据
# data_filtered <- rawdata %>%
#   filter(Strain != "Control")
# 
# # 计算归一化的EPS值
# data_normalized <- data_filtered %>%
#   group_by(Strain, Light) %>%
#   mutate(EPS_normalized = EPS / OD600[Time == 12])
# 
# # 对每个菌株和光照条件下的归一化EPS随时间变化进行线性拟合
# slopes <- data_normalized %>%
#   group_by(Strain, Light) %>%
#   do(tidy(lm(EPS_normalized ~ Time, data = .), conf.int = TRUE)) %>%
#   filter(term == "Time") %>%
#   select(Strain, Light, estimate, std.error, conf.low, conf.high) %>%
#   rename(slope = estimate, se = std.error)
# 
# # 进行ANOVA分析，检验不同光照条件下的变化速率差异是否显著
# significance <- data_normalized %>%
#   group_by(Strain) %>%
#   do(tidy(aov(lm(EPS_normalized ~ Time * Light, data = .)))) %>%
#   filter(grepl("Light", term)) %>%
#   select(Strain, term, p.value) %>%
#   mutate(sig_label = ifelse(p.value < 0.001, "***",
#                             ifelse(p.value < 0.01, "**",
#                                    ifelse(p.value < 0.05, "*", "ns"))))
# 
# # 计算每个菌株的最大斜率，用于确定显著性标记的位置
# max_slopes <- slopes %>%
#   group_by(Strain) %>%
#   summarise(max_slope = max(slope, na.rm = TRUE))
# 
# # 将最大斜率信息合并到显著性数据框中
# significance <- left_join(significance, max_slopes, by = "Strain")
# 
# # 创建CNS风格的柱状图
# p <- ggplot(slopes, aes(x = Strain, y = slope, fill = Light)) +
#   # 添加柱状图
#   geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.7) +
#   # 添加误差棒
#   geom_errorbar(aes(ymin = slope - se, ymax = slope + se),
#                 position = position_dodge(width = 0.9), width = 0.25) +
#   # 添加显著性标记
#   geom_text(data = significance, aes(x = Strain, y = max_slope * 1.1, label = sig_label),
#             inherit.aes = FALSE, size = 5) +
#   # 设置CNS风格的主题
#   theme_classic() +
#   theme(
#     text = element_text(size = 12, face = "plain"),  # 统一字体大小，不加粗
#     axis.title = element_text(size = 12, face = "plain"),
#     axis.text = element_text(size = 12, face = "plain"),
#     legend.title = element_text(size = 12, face = "plain"),
#     legend.text = element_text(size = 12, face = "plain"),
#     strip.text = element_text(size = 12, face = "plain"),
#     strip.background = element_blank(),
#     panel.grid.major = element_line(color = "grey90", size = 0.5),
#     panel.grid.minor = element_line(color = "grey95", size = 0.25)
#   ) +
#   # 设置坐标轴标签
#   labs(x = "Strain", y = "EPS Change Rate (a.u./h/OD600)", fill = "Light") +
#   # 设置颜色
#   scale_fill_manual(values = c("Dark" = "#4E79A7", "IR" = "#E15759"))
# 
# # 显示图表
# print(p)
# 
# # 保存图表（可选）
# # ggsave("./Fig4_EPS_rate.png", p, width = 8, height = 6, dpi = 300)



# # ### -------------------------- v2.1 -------------------------- ###
# rm(list=ls())
# library(readxl)
# library(ggplot2)
# library(ggpubr)
# library(dplyr)
# library(scales)
# 
# # 读取数据
# path <- './data/Fig4/EPS.xlsx'
# sheet.name <- 'v'
# rawdata <- data.frame(read_excel(path, sheet = sheet.name))
# 
# # 过滤掉Control组的数据
# data_filtered <- rawdata %>%
#   filter(Strain != "Control")
# 
# # 计算归一化的EPS值
# data_normalized <- data_filtered %>%
#   group_by(Strain, Light) %>%
#   mutate(EPS_normalized = EPS / OD600[Time == 12])
# 
# # 计算每个菌株和光照条件下，Time = 12和Time = 53之间的EPS归一化变化速率
# slopes <- data_normalized %>%
#   filter(Time %in% c(12, 53)) %>%
#   group_by(Strain, Light) %>%
#   arrange(Time) %>%
#   summarise(
#     slope = (EPS_normalized[Time == 53] - EPS_normalized[Time == 12]) / (53 - 12),
#     se = sd((EPS_normalized[Time == 53] - EPS_normalized[Time == 12]) / (53 - 12), na.rm = TRUE),
#     .groups = 'drop'
#   )
# 
# # 进行t检验，检验不同光照条件下的变化速率差异是否显著
# significance <- data_normalized %>%
#   filter(Time %in% c(12, 53)) %>%
#   group_by(Strain) %>%
#   do({
#     df <- .
#     dark_rate <- (df$EPS_normalized[df$Time == 53 & df$Light == "Dark"] - df$EPS_normalized[df$Time == 12 & df$Light == "Dark"]) / (53 - 12)
#     ir_rate <- (df$EPS_normalized[df$Time == 53 & df$Light == "IR"] - df$EPS_normalized[df$Time == 12 & df$Light == "IR"]) / (53 - 12)
#     t_test <- t.test(dark_rate, ir_rate)
#     data.frame(p.value = t_test$p.value)
#   }) %>%
#   mutate(sig_label = ifelse(p.value < 0.001, "***",
#                             ifelse(p.value < 0.01, "**",
#                                    ifelse(p.value < 0.05, "*", "ns"))))
# 
# # 计算每个菌株的最大斜率，用于确定显著性标记的位置
# max_slopes <- slopes %>%
#   group_by(Strain) %>%
#   summarise(max_slope = max(slope, na.rm = TRUE))
# 
# # 将最大斜率信息合并到显著性数据框中
# significance <- left_join(significance, max_slopes, by = "Strain")
# 
# # 创建CNS风格的柱状图
# p <- ggplot(slopes, aes(x = Strain, y = slope, fill = Light)) +
#   # 添加柱状图
#   geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.7) +
#   # 添加误差棒
#   geom_errorbar(aes(ymin = slope - se, ymax = slope + se),
#                 position = position_dodge(width = 0.9), width = 0.25) +
#   # 添加显著性标记
#   geom_text(data = significance, aes(x = Strain, y = max_slope * 1.1, label = sig_label),
#             inherit.aes = FALSE, size = 5) +
#   # 设置CNS风格的主题
#   theme_classic() +
#   theme(
#     text = element_text(size = 12, face = "plain"),  # 统一字体大小，不加粗
#     axis.title = element_text(size = 12, face = "plain"),
#     axis.text = element_text(size = 12, face = "plain"),
#     legend.title = element_text(size = 12, face = "plain"),
#     legend.text = element_text(size = 12, face = "plain"),
#     strip.text = element_text(size = 12, face = "plain"),
#     strip.background = element_blank(),
#     panel.grid.major = element_line(color = "grey90", size = 0.5),
#     panel.grid.minor = element_line(color = "grey95", size = 0.25)
#   ) +
#   # 设置坐标轴标签
#   labs(x = "Strain", y = "EPS Change Rate (a.u./h/OD600)", fill = "Light") +
#   # 设置颜色
#   scale_fill_manual(values = c("Dark" = "#4E79A7", "IR" = "#E15759"))
# 
# # 显示图表
# print(p)

# 保存图表（可选）
# ggsave("./Fig4_EPS_rate.png", p, width = 8, height = 6, dpi = 300)

## ----------------------- v2.2 ----------------------------
rm(list=ls())
library(readxl)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(scales)

# 设置CNS风格的主题
theme_cns <- theme(
  panel.background = element_rect(fill = "transparent"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.line = element_line(color = "black", size = 0.4),
  axis.ticks = element_line(color = "black", size = 0.4),
  axis.text.x = element_text(color = "black", size = 7, face = "plain", angle = 30, hjust = 1), # 
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

# 读取数据
path <- './data/ex-fig12/EPS.xlsx'
sheet.name <- 'v.R1'
rawdata <- data.frame(read_excel(path, sheet = sheet.name))

# 过滤掉Control组的数据
data_filtered <- rawdata %>%
  filter(Strain != "Control")

# 计算归一化的EPS值
data_normalized <- data_filtered %>%
  group_by(Strain, Light) %>%
  mutate(EPS_normalized = EPS / OD600[Time == 12])

# 计算每个菌株和光照条件下，Time = 12和Time = 53之间的EPS归一化变化速率
rates <- data_normalized %>%
  filter(Time %in% c(12, 53)) %>%
  group_by(Strain, Light) %>%
  arrange(Time) %>%
  summarise(
    rate = (EPS_normalized[Time == 53] - EPS_normalized[Time == 12]) / (53 - 12),
    .groups = 'drop'
  )

# 进行t检验，检验不同光照条件下的变化速率差异是否显著
significance <- rates %>%
  group_by(Strain) %>%
  do({
    df <- .
    dark_rate <- df$rate[df$Light == "Dark"]
    ir_rate <- df$rate[df$Light == "IR"]
    t_test <- t.test(dark_rate, ir_rate)
    data.frame(p.value = t_test$p.value)
  }) %>%
  mutate(sig_label = ifelse(p.value < 0.001, "***",
                            ifelse(p.value < 0.01, "**",
                                   ifelse(p.value < 0.05, "*", "ns"))))

# 计算每个菌株的最大速率，用于确定显著性标记的位置
max_rates <- rates %>%
  group_by(Strain) %>%
  summarise(max_rate = max(rate, na.rm = TRUE))

# 将最大速率信息合并到显著性数据框中
significance <- left_join(significance, max_rates, by = "Strain")

# 创建CNS风格的箱线图
p <- ggplot(rates, aes(x = Strain, y = rate, fill = Light)) +
  # 添加箱线图
  geom_boxplot(position = position_dodge(width = 0.9), width = 0.7, outlier.shape = NA) +
  # 添加原始数据点
  geom_jitter(position = position_jitterdodge(dodge.width = 0.9, jitter.width = 0.1),
              size = 2.5, alpha = 0.6) +
  # # 添加显著性标记
  # geom_text(data = significance, aes(x = Strain, y = max_rate * 1.1, label = sig_label),
  #           inherit.aes = FALSE, size = 5) +
  # 设置CNS风格的主题
  theme_classic() +
  theme_cns + 
  # 设置坐标轴标签
  labs(x = "", y = "Normalized EPS Secretion Rate", fill = "Light") +
  # 设置颜色
  scale_fill_manual(values = c("Dark" = "#023270", "IR" = "#D34947"))

# 显示图表
print(p)

# 保存图表（可选）
# ggsave("./Fig4_EPS_rate.png", p, width = 8, height = 6, dpi = 300)

