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
  axis.text.x = element_text(color = "black", size = 7, face = "plain",), # 
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
sheet.name <- 'CongoRed.R1'
rawdata <- data.frame(read_excel(path, sheet = sheet.name))

# 过滤掉Control组的数据
data_filtered <- rawdata %>%
  filter(Strain != "Control")

# 计算每个时间点、每个菌株和光照条件下EPS的平均值和标准误
data_summary <- data_filtered %>%
  group_by(Time, Strain, Light) %>%
  summarise(
    mean_EPS = mean(EPS, na.rm = TRUE),
    se_EPS = sd(EPS, na.rm = TRUE) / sqrt(n()),
    .groups = 'drop'
  )

# 创建CNS风格的柱状图
p <- ggplot(data_summary, aes(x = factor(Time), y = mean_EPS, fill = Light)) +
  # 添加柱状图
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.7) +
  # 添加误差棒
  geom_errorbar(aes(ymin = mean_EPS - se_EPS, ymax = mean_EPS + se_EPS),
                position = position_dodge(width = 0.9), width = 0.4) +
  # 添加原始数据点
  geom_jitter(data = data_filtered, aes(x = factor(Time), y = EPS, color = Light),
              position = position_jitterdodge(dodge.width = 0.9, jitter.width = 0.1),
              size = 2, alpha = 0.6) +
  # 按Strain分面
  facet_wrap(~Strain, ncol = 5) +
  # 设置CNS风格的主题
  theme_cns +
  # 设置坐标轴标签
  labs(x = "Time (h)", y = "EPS (a.u.)", fill = "Light", color = "Light") +
  # 设置颜色
  scale_fill_manual(values = c("Dark" = "#023270",  "IR" = "#D34947")) +
  scale_color_manual(values = c("Dark" = "#023270",  "IR" = "#D34947"))

# 显示图表
print(p)

# 保存图表（可选）
# ggsave("./Fig4_EPS.png", p, width = 10, height = 8, dpi = 300)
