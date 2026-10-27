rm(list=ls())
library(readxl)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(scales)

# 读取数据
path <- './data/Fig3/NADH.xlsx'
sheet.name <- 'All'
rawdata <- data.frame(read_excel(path, sheet = sheet.name))
head(rawdata)
# Samples    Raw      NADH      Strain Light
# 1  B1 (D) 0.0435  3.974845 Deinococcus  Dark
# 2      B2 0.0299 -0.796987 Deinococcus  Dark
# 3      B3 0.0474  5.343238 Deinococcus  Dark
# 4      B4 0.0436  4.009932 Deinococcus    IR
# 5      B5 0.0515  6.781805 Deinococcus    IR
# 6      B6 0.0463  4.957281 Deinococcus    IR

# 计算统计量和标准误
data_summary <- rawdata %>%
  group_by(Strain, Light) %>%
  summarise(
    mean_value = mean(NADH, na.rm = TRUE),
    se_value = sd(NADH, na.rm = TRUE) / sqrt(n()),
    .groups = 'drop'
  )

# 设置CNS风格的主题
theme_cns <- theme(
  panel.background = element_rect(fill = "transparent"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.line = element_line(color = "black", size = 0.4),
  axis.ticks = element_line(color = "black", size = 0.4),
  axis.text.x = element_text(color = "black", size = 7, face = "plain", 
                             angle = 30, hjust = 1), # 
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




# 创建基础柱状图
p <- ggplot(data_summary, aes(x = Strain, y = mean_value, fill = Light)) +
  # 添加柱状图
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  # 添加误差棒
  geom_errorbar(aes(ymin = mean_value - se_value, ymax = mean_value + se_value),
                position = position_dodge(width = 0.8), width = 0.2, size = 0.6) +
  # 添加原始数据点
  geom_jitter(data = rawdata, aes(x = Strain, y = NADH, fill = Light),
              position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
              size = 3, alpha = 0.6, shape = 21) +
  # 设置颜色
  scale_fill_manual(values = c("Dark" = "#023270", "IR" = "#D34947"), # 青绿色 "#2CA25F"  # 深蓝（Caron Sequestration）："#2C7FB8" # 深灰色 "#6E6E6E"  #  橙色 "#F4A259"
                    labels = c("Dark" = "Dark", "IR" = "IR")) +
  # 设置坐标轴标签
  labs(x = "", y = "NADH (mM)", fill = "Light condition") +
  # 设置坐标轴范围，使柱子底部与x轴相贴
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  # scale_x_discrete(expand = expansion(mult = c(0, 0.0))) +
  # 应用CNS主题
  theme_cns

# 设置显著性显示方式：TRUE显示*标记，FALSE显示具体p值
show_star <- FALSE  # 可以修改为FALSE以显示具体p值

# 进行显著性分析
stat_compare <- list()
for (strain in unique(rawdata$Strain)) {
  subset_data <- rawdata[rawdata$Strain == strain, ]
  test_result <- t.test(NADH ~ Light, data = subset_data)
  p_value <- test_result$p.value
  
  # 根据p值设置显著性标记
  if (show_star) {
    if (p_value < 0.001) {
      sig_label <- "***"
    } else if (p_value < 0.01) {
      sig_label <- "**"
    } else if (p_value < 0.05) {
      sig_label <- "*"
    } else {
      sig_label <- "ns"
    }
  } else {
    # 显示具体p值，保留3位小数
    sig_label <- ifelse(p_value < 0.001, "p < 0.001", 
                        paste0("p = ", round(p_value, 3)))
  }
  
  # 计算标注位置
  max_y <- max(data_summary$mean_value[data_summary$Strain == strain], na.rm = TRUE)
  y_pos <- max_y + max(data_summary$se_value[data_summary$Strain == strain], na.rm = TRUE) * 2
  
  stat_compare[[strain]] <- data.frame(Strain = strain, y = y_pos, label = sig_label)
}

# 合并所有显著性标注
stat_compare_df <- do.call(rbind, stat_compare)
# 
# # 添加显著性标注
# p <- p + geom_text(data = stat_compare_df,
#                    aes(x = Strain, y = y, label = label),
#                    inherit.aes = FALSE,
#                    size = 4,
#                    position = position_dodge(width = 0.8))

# 显示图形
print(p)

# 保存图形
# ggsave("./output/Fig4_NADH.pdf", p, width = 8, height = 6, dpi = 300)

