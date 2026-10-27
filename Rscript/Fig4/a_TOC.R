rm(list=ls())
library(readxl)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(scales)

# 设置CNS风格的主题
theme_cns <- theme(
  panel.background = element_rect(fill = "transparent"),
  # panel.grid.major = element_blank(),
  # panel.grid.minor = element_blank(),
  panel.grid.major = element_line(color = "grey90", size = 0.5),
  panel.grid.minor = element_line(color = "grey95", size = 0.25),
  # axis.line = element_line(color = "black", size = 0.4),
  # axis.ticks = element_line(color = "black", size = 0.4),
  axis.text.x = element_text(color = "black", size = 7, face = "plain", 
                             hjust = 1), # angle = 30, 
  axis.text.y = element_text(color = "black", size = 7, face = "plain"),
  axis.title = element_text(color = "black", size = 7, face = "plain"),
  
  legend.position = "right",
  legend.title = element_blank(),
  legend.text = element_text(size = 7, face = "plain"),
  # legend.background = element_rect(fill = "white", color = NA),
  legend.key = element_rect(fill = "white", color = NA),
  
  strip.text = element_text(size = 7, face = "plain"),
  strip.background = element_blank(),

  plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
  plot.title = element_text(size = 10, face = 'plain', hjust = 0.5)
)

theme_backup <- theme(
  text = element_text(size = 12, face = "plain"),  # 统一字体大小，不加粗
  axis.title = element_text(size = 12, face = "plain"),
  axis.text = element_text(size = 12, face = "plain"),
  legend.title = element_text(size = 12, face = "plain"),
  legend.text = element_text(size = 12, face = "plain"),
  strip.text = element_text(size = 12, face = "plain"),
  strip.background = element_blank(),
  panel.grid.major = element_line(color = "grey90", size = 0.5),
  panel.grid.minor = element_line(color = "grey95", size = 0.25)
)
  

# 读取数据
path <- './data/Fig4/TOC.xlsx'
sheet.name <- 'TOC'
rawdata <- data.frame(read_excel(path, sheet = sheet.name))
head(rawdata)
# Sample.Name Indicators Value Day Light       Group
# 1 D1-D_0819_R1_filt       NPOC 1.383  65  Dark Deinococcus
# 2              D1-D         IC 1.161  65  Dark Deinococcus
# 3              D2-D       NPOC 1.238  65  Dark Deinococcus
# 4              D2-D         IC 0.728  65  Dark Deinococcus
# 5              D3-D       NPOC 1.181  65  Dark Deinococcus
# 6              D3-D         IC 1.337  65  Dark Deinococcus


# 设置要可视化的指标
selected_indicator <- "NPOC"  # 可以修改为其他指标，如"IC"   "NPOC"

# 筛选数据，只保留选定的指标
data_filtered <- rawdata %>%
  filter(Indicator == selected_indicator)

# 计算每个时间点、每个分组和光照条件的平均值和标准误
data_summary <- data_filtered %>%
  group_by(Time, Group, Light) %>%
  summarise(
    mean_value = mean(Value, na.rm = TRUE),
    se_value = sd(Value, na.rm = TRUE) / sqrt(n()),
    .groups = 'drop'
  )

# 创建CNS风格的柱状图
p <- ggplot(data_summary, aes(x = factor(Time), y = mean_value, fill = Light)) +
  # 添加柱状图
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.7) +
  # 添加误差棒
  geom_errorbar(aes(ymin = mean_value - se_value, ymax = mean_value + se_value),
                position = position_dodge(width = 0.9), width = 0.25) +
  # 添加原始数据点
  geom_jitter(data = data_filtered, aes(x = factor(Time), y = Value, color = Light),
              position = position_jitterdodge(dodge.width = 0.9, jitter.width = 0.1),
              size = 2, alpha = 0.6) +
  # 按Group分面
  facet_wrap(~Group, ncol = 5) +
  # 设置CNS风格的主题
  theme_classic() +
  theme_cns +
  # theme(
  #   text = element_text(size = 12, face = "plain"),  # 统一字体大小，不加粗
  #   axis.title = element_text(size = 12, face = "plain"),
  #   axis.text = element_text(size = 12, face = "plain"),
  #   legend.title = element_text(size = 12, face = "plain"),
  #   legend.text = element_text(size = 12, face = "plain"),
  #   strip.text = element_text(size = 12, face = "plain"),
  #   strip.background = element_blank(),
  #   panel.grid.major = element_line(color = "grey90", size = 0.5),
  #   panel.grid.minor = element_line(color = "grey95", size = 0.25)
  # ) +
  # 设置坐标轴标签
  labs(x = "Time (Day)", y = paste(selected_indicator, "(mg/L)"), fill = "Light", color = "Light") +
  # 设置颜色
  scale_fill_manual(values = c("Dark" = "#023270", "IR" = "#D34947")) + # "Dark" = "#023270", "IR" = "#D34947"
  scale_color_manual(values = c("Dark" = "#023270", "IR" = "#D34947")) # "Dark" = "#4E79A7", "IR" = "#BF5D5A"

# 显示图表
print(p)

# 保存图表（可选）
# ggsave(paste0("./Fig5_", selected_indicator, ".png"), p, width = 10, height = 8, dpi = 300)


library(ggpubr)
library(broom)
# 同一时间组内不同光照条件(Dark与IR)的显著性分析
# 对每个Group和Time组合进行t检验
light_comparison <- data_filtered %>%
  group_by(Group, Time) %>%
  do(tidy(t.test(Value ~ Light, data = .))) %>%
  ungroup() %>%
  mutate(
    comparison = "Dark vs IR",
    y_position = max(data_summary$mean_value) * 1.1
  )

# 不同时间(T=0和T=65)之间的显著性分析
# 对每个Group和Light组合进行t检验
time_comparison <- data_filtered %>%
  filter(Time %in% c(0, 65)) %>%
  group_by(Group, Light) %>%
  do(tidy(t.test(Value ~ Time, data = .))) %>%
  ungroup() %>%
  mutate(
    comparison = "T0 vs T65",
    y_position = max(data_summary$mean_value) * 1.15
  )

# 合并显著性结果
significance_results <- bind_rows(light_comparison, time_comparison) %>%
  mutate(
    significance = ifelse(p.value < 0.001, p.value,
                          ifelse(p.value < 0.01, "**",
                                 ifelse(p.value < 0.05, p.value, p.value)))
  )

# 添加光照条件比较的显著性标记
p <- p + stat_compare_means(
  data = data_filtered,
  aes(group = Light),
  method = "t.test",
  label = "p.signif",
  hide.ns = TRUE,
  size = 3,
  step.increase = 0.1
)

# 添加时间比较的显著性标记（T=0 vs T=65）
# 对每个Group和Light条件分别添加
time_groups <- unique(data_filtered$Group)
light_conditions <- unique(data_filtered$Light)

for (group in time_groups) {
  for (light in light_conditions) {
    subset_data <- data_filtered %>%
      filter(Group == group, Light == light, Time %in% c(0, 65))
    
    if (nrow(subset_data) > 0) {
      p <- p + stat_compare_means(
        data = subset_data,
        comparisons = list(c("0", "65")),
        method = "t.test",
        label = "p.signif",
        hide.ns = TRUE,
        size = 3,
        step.increase = 0.1,
        tip.length = 0.01
      )
    }
  }
}
