rm(list=ls())
library(readxl)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(scales)

# 读取数据
path <- './data/Fig3/CCUS&S.xlsx'
sheet.name <- 'All'
rawdata <- data.frame(read_excel(path, sheet = sheet.name))

# 确保CCUS列是数值型
rawdata$CCUS <- as.numeric(as.character(rawdata$CCUS))

# 定义函数：识别并删除异常值
remove_outliers <- function(x, na.rm = TRUE, multiplier = 1.5, ...) { # multiplier = 1.5
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- multiplier * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

# 过滤NA值和异常值
# 使用更严格的阈值（multiplier = 1.0）来识别异常值
filtered_data <- rawdata %>%
  filter(!is.na(CCUS)) %>%
  group_by(Strain, Light) %>%
  mutate(CCUS_clean = remove_outliers(CCUS, multiplier = 0.5)) %>%
  ungroup() %>%
  filter(!is.na(CCUS_clean)) %>%
  mutate(CCUS = CCUS_clean) %>%
  select(-CCUS_clean)

# 设置CNS风格的主题
theme_cns <- theme(
  panel.background = element_rect(fill = "transparent"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.line = element_line(color = "black", size = 0.4),
  axis.ticks = element_line(color = "black", size = 0.4),
  axis.text.x = element_text(color = "black", size = 7, face = "plain", 
                             angle = 30, hjust = 1), # angle = 30, 
  axis.text.y = element_text(color = "black", size = 7, face = "plain"),
  axis.title = element_text(color = "black", size = 7, face = "plain"),
  legend.position = "right", # "right"
  legend.title = element_blank(),
  legend.text = element_text(size = 7, face = "plain"),
  legend.background = element_rect(fill = "white", color = NA),
  legend.key = element_rect(fill = "white", color = NA),
  plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
  plot.title = element_text(size = 8, face = 'plain', hjust = 0.5)
)

# 创建箱形图
p <- ggplot(filtered_data, aes(x = Strain, y = CCUS, fill = Light)) +
  geom_boxplot(width = 0.7, outlier.shape = NA, position = position_dodge(width = 0.8)) +
  # 添加原始数据点
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
              size = 2, alpha = 0.6, shape = 21) +
  scale_fill_manual(values = c("IR" = "#D34947", "Dark" = "#023270")) +
  labs(
    x = "",
    y = "CO2 Conversion Rate (mg·C·L-1·d-1)",
    title = ""
  ) +
  theme_cns

# 计算统计量和标准误（使用过滤后的数据）
data_summary <- filtered_data %>%
  group_by(Strain, Light) %>%
  summarise(
    mean_value = mean(CCUS, na.rm = TRUE),
    se_value = sd(CCUS, na.rm = TRUE) / sqrt(n()),
    .groups = 'drop'
  )

# 设置显著性显示方式：TRUE显示*标记，FALSE显示具体p值
show_star <- FALSE  # 可以修改为FALSE以显示具体p值

# 设置是否只标记显著的比较
show_significant_only <- FALSE  # TRUE只标记显著的，FALSE标记所有比较

# 进行显著性分析（使用过滤后的数据）
stat_compare <- list()
for (strain in unique(filtered_data$Strain)) {
  subset_data <- filtered_data[filtered_data$Strain == strain, ]
  test_result <- t.test(CCUS ~ Light, data = subset_data)
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
  
  # 如果设置了只标记显著的，则跳过不显著的比较
  if (show_significant_only && p_value >= 0.05) {
    next
  }
  
  # 计算标注位置
  max_y <- max(data_summary$mean_value[data_summary$Strain == strain], na.rm = TRUE)
  y_pos <- max_y + max(data_summary$se_value[data_summary$Strain == strain], na.rm = TRUE) * 2
  
  stat_compare[[strain]] <- data.frame(Strain = strain, y = y_pos+3.2, label = sig_label)
}

# 合并所有显著性标注
stat_compare_df <- do.call(rbind, stat_compare)

# 添加显著性标记到图表
p <- p + geom_text(data = stat_compare_df, aes(x = Strain, y = y, label = label), 
                   inherit.aes = FALSE, size = 2.5, vjust = -0.5)

# 显示图表
print(p)
