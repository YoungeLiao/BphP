rm(list=ls())
library(readxl)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(scales)

# 读取数据
path <- './data/Fig3/CCOx&BCA.xlsx'
sheet.name <- 'All'
rawdata <- data.frame(read_excel(path, sheet = sheet.name))
head(rawdata)

# Samples    Raw     CCO1      Strain Light    BCA OD600_BCA CCO  OD600
# 1  B1 (D) 0.0760 5.904855 Deinococcus  Dark 0.3993    0.2964   1.3471660 0.1618
# 2      B2 0.0652 5.943926 Deinococcus  Dark 0.3213    0.2147   1.4965068 0.1495
# 3      B3 0.0700 5.926561 Deinococcus  Dark 0.2583    0.1920   1.3453125 0.1529
# 4      B4 0.0708 5.923667 Deinococcus    IR 0.2933    0.1371   2.1393144 0.1536
# 5      B5 0.0682 5.933073 Deinococcus    IR 0.3603    0.2118   1.7011331 0.1455
# 6      B6 0.0734 5.914261 Deinococcus    IR 0.2491    0.3165   0.7870458 0.1525

# 设置CNS风格的主题
theme_cns <- theme(
  panel.background = element_rect(fill = "transparent"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.line = element_line(color = "black", size = 0.4),
  axis.ticks = element_line(color = "black", size = 0.4),
  axis.text.x = element_text(color = "black", size = 7, face = "plain", 
                             hjust = 1, angle = 30), # angle = 30, 
  axis.text.y = element_text(color = "black", size = 7, face = "plain"),
  axis.title = element_text(color = "black", size = 7, face = "plain"),
  legend.position = "right",
  legend.title = element_blank(),
  legend.text = element_text(size = 7, face = "plain"),
  legend.background = element_rect(fill = "white", color = NA),
  legend.key = element_rect(fill = "white", color = NA),
  plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
  plot.title = element_text(size = 14, face = 'plain', hjust = 0.5)
)

# 计算统计量和标准误
data_summary <- rawdata %>%
  group_by(Strain, Light) %>%
  summarise(
    mean_value = mean(CCO, na.rm = TRUE),
    se_value = sd(CCO, na.rm = TRUE) / sqrt(n()),
    .groups = 'drop'
  )


# 创建基础柱状图
p <- ggplot(data_summary, aes(x = Strain, y = mean_value, fill = Light)) +
  # 添加柱状图
  geom_col(position = position_dodge(width = 0.75), width = 0.7, alpha=1.0) +
  # 添加误差棒
  geom_errorbar(aes(ymin = mean_value - se_value, ymax = mean_value + se_value),
                position = position_dodge(width = 0.8), width = 0.2, size = 0.6) +
  # 添加原始数据点
  geom_jitter(data = rawdata, aes(x = Strain, y = CCO, fill = Light),
              position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.8),
              size = 3, alpha = 0.6, shape = 21) +
  # 设置颜色
  scale_fill_manual(values = c("Dark" = "#023270", "IR" = "#D34947"),
                    labels = c("Dark" = "Dark", "IR" = "IR")) +
  # 设置坐标轴标签
  labs(x = "", y = "CCO (mU·mg-1)", fill = "Light") +
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
  test_result <- t.test(CCO ~ Light, data = subset_data)
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

