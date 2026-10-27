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
  axis.text.x = element_text(color = "black", size = 7, face = "plain", 
                             hjust = 1), # angle = 30, 
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


# 读取数据
path <- './data/Fig3/EPS.xlsx'
sheet.name <- 'R_all'
rawdata <- data.frame(read_excel(path, sheet = sheet.name))
head(rawdata)
# Sample Time_Day Batch      Strain Light  Abs490     EPS  EPS_MEAN OD600 Abs490.1 Abs490.2
# 1  Control1        5  R1.5     Control  Dark 0.80945      NA        NA    NA   0.7923   0.8266
# 2  Control2        5  R1.5     Control  Dark 0.77075      NA        NA    NA   0.7763   0.7652
# 3  Control3        5  R1.5     Control  Dark 0.72560      NA        NA    NA   0.6661   0.7851
# 4 D-D1_0702        5  R1.5 Deinococcus  Dark 0.22045 0.54815 0.5503167    NA   0.2333   0.2076
# 5      D-D2        5  R1.5 Deinococcus  Dark 0.22560 0.54300        NA    NA   0.2306   0.2206
# 6      D-D3        5  R1.5 Deinococcus  Dark 0.20880 0.55980        NA    NA   0.2112   0.2064

# # 过滤掉Control组的数据
# data_filtered <- rawdata %>%
#   filter(Strain != "Control")
# 
# # 数据预处理：去除EPS为NA的行
# data_filtered <- data_filtered %>%
#   filter(!is.na(EPS))
# 
# # 创建箱形图
# p <- ggplot(data_filtered, aes(x = Strain, y = EPS, fill = Light)) +
#   geom_boxplot(width = 0.6, position = position_dodge(0.8),
#                outlier.shape = NA, alpha = 0.7) +
#   geom_jitter(width = 0.3, size = 1.5, alpha = 0.5,
#               position = position_dodge(0.8)) +
#   scale_fill_manual(values = c("Dark" = "#E0E0E0", "IR" = "#FF6B6B")) +
#   stat_compare_means(aes(group = Light),
#                      method = "wilcox.test",
#                      label = "p.signif",
#                      hide.ns = TRUE,
#                      size = 3) +
#   labs(y = "EPS content", x = "Strain") +
#   theme_cns +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# # 显示图形
# print(p)
# 
# # 保存图形（可选）
# # ggsave("./output/EPS_boxplot.pdf", p, width = 6, height = 4)



# 添加菌株过滤选项
filter_strain <- NULL  # 设置为NULL不过滤，或设置为特定菌株名称如"Deinococcus"来过滤

# 数据预处理
data_filtered <- rawdata %>%
  filter(Strain != "Control") %>%
  filter(!is.na(EPS)) %>%
  {if(!is.null(filter_strain)) filter(., Strain == filter_strain) else .}

# 独立进行显著性分析并保存结果
# 获取所有菌株
strains <- unique(data_filtered$Strain)

# 创建结果数据框
significance_results <- data.frame(
  Strain = character(),
  Comparison = character(),
  p_value = numeric(),
  Significance = character(),
  stringsAsFactors = FALSE
)

# 对每个菌株进行T检验
for (strain in strains) {
  # 提取当前菌株的数据
  strain_data <- data_filtered %>% filter(Strain == strain)
  
  # 执行T检验
  test_result <- t.test(EPS ~ Light, data = strain_data)
  
  # 确定显著性标记
  sig_label <- ifelse(test_result$p.value < 0.001, "***",
                      ifelse(test_result$p.value < 0.01, "**",
                             ifelse(test_result$p.value < 0.05, "*", "ns")))
  
  # 添加结果到数据框
  significance_results <- rbind(significance_results, data.frame(
    Strain = strain,
    Comparison = "IR vs Dark",
    p_value = test_result$p.value,
    Significance = sig_label,
    stringsAsFactors = FALSE
  ))
}

# 创建箱形图
p <- ggplot(data_filtered, aes(x = Strain, y = EPS, fill = Light)) +
  geom_boxplot(width = 0.6, position = position_dodge(0.8),
               outlier.shape = NA, alpha = 0.7) +
  geom_jitter(size = 1.5, alpha = 0.5, # width = 0.3, 
              position = position_jitterdodge(dodge.width = 0.6)) +
  scale_fill_manual(values = c("Dark" = "#023270", "IR" = "#D34947")) +
  # 使用保存的显著性结果添加显著性标记
  geom_text(data = significance_results, 
            aes(x = Strain, y = max(data_filtered$EPS) * 1.1, label = Significance),
            inherit.aes = FALSE, size = 3) +
  labs(y = "EPS content", x = "Strain") +
  theme_cns +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 显示图形
print(p)
