rm(list=ls())
# source('./Rscript/functions.R')
# load library
library(readxl)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(tidyr)
library(cowplot) 

# 设置CNS风格的主题
theme_cns <- theme(
  panel.background = element_rect(fill = "transparent"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.line = element_line(color = "black"),
  axis.text.x = element_text(color = "black", size = 7, face = "plain"),  # hjust = 1 # angle = 30, 
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

# load data
path <- './data/ex-fig2/b_co2.gc.xlsx'
sheet.name <- 'gl_co2_stack' 
rawdata.2 <- data.frame(read_excel(path, sheet = sheet.name))

path <- './data/ex-fig2/b_co2.gc.xlsx'
sheet.name <- 'gl_co2' 
rawdata.1 <- data.frame(read_excel(path, sheet = sheet.name))

head(rawdata.1)
# output:
# Samples Time    Group Batch GC_Area    CO2_ppm       C_m        TIC     TIC_m
# 1      D1    0 Dark-Gas Opto5   808.2 0.09979302 0.2464888  747.66656 22429.997
# 2      D2    0 Dark-Gas Opto6   498.4 0.05915872 0.1461220 1124.53604 33736.081
# 3      D3    0 Dark-Gas Opto6   366.8 0.04189767 0.1034872  954.02520 28620.756
# 4      I1    0   IR-Gas Opto5   514.7 0.06129668 0.1514028  -95.84776 -2875.433
# 5      I2    0   IR-Gas Opto6   484.8 0.05737490 0.1417160  779.77372 23393.212
# 6      I3    0   IR-Gas Opto6   486.8 0.05763723 0.1423640  875.78348 26273.504


# 1. 数据处理：将宽格式转换为长格式，并计算均值和标准误
plotdata <- rawdata.1 %>%
  select(Group, C_m, TIC_m) %>% 
  pivot_longer(cols = c(C_m, TIC_m), names_to = "Phase", values_to = "Carbon_Mass") %>% 
  group_by(Group, Phase) %>% 
  summarise(
    Mean = mean(Carbon_Mass, na.rm = TRUE),
    SE = sd(Carbon_Mass, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# 2. 计算误差棒的上下限（TIC_m在下层，C_m在上层，因此C_m的误差棒需要加上TIC_m的均值）
plotdata <- plotdata %>%
  group_by(Group) %>%
  mutate(
    ymin = ifelse(Phase == "TIC_m", Mean - SE, Mean - SE + Mean[Phase == "TIC_m"]),
    ymax = ifelse(Phase == "TIC_m", Mean + SE, Mean + SE + Mean[Phase == "TIC_m"])
  ) %>%
  ungroup()

# 3. 绘图
y.limit <- c(0, 36000)
y.BREAK <- 10000

main_plot <- ggplot(plotdata, aes(x = Group, y = Mean, fill = Phase, color = Phase)) +
  # 绘制堆叠柱状图（边框型）
  geom_bar(stat = "identity", position = "stack", width = 0.7, size = 0.5, fill = "transparent") +
  # 添加C_m（Gas）层的误差棒（仅在堆叠顶端显示）
  geom_errorbar(data = filter(plotdata, Phase == "TIC_m"), 
                aes(ymin = ymin, ymax = ymax), 
                width = 0.2, position = "stack") +
  # 添加数据点（使用转换后的长格式数据，保证颜色映射生效）
  geom_jitter(data = rawdata.1 %>% 
                pivot_longer(cols = c(C_m, TIC_m), names_to = "Phase", values_to = "Carbon_Mass"), 
              aes(x = Group, y = Carbon_Mass, color = Phase), 
              position = position_jitterdodge(jitter.width = 0.5, dodge.width = 0.7), 
              size = 3, alpha = 0.5, stroke = 1) +
  # 自定义颜色
  scale_fill_manual(values = c("C_m" = "#E57272", "TIC_m" = "#4489FE")) +
  scale_color_manual(values = c("C_m" = "#E57272", "TIC_m" = "#4489FE"), labels = c("C_m" = "Gas", "TIC_m" = "Liquid")) +
  scale_y_continuous(breaks = seq(from = y.limit[1], to = y.limit[2], by = y.BREAK)) + 
  labs(title = "",
       x = "",
       y = "CO2 equivalent (mg)",
       fill = "Phase") +
  theme_classic() +
  theme_cns

main_plot



zoom_plot <- ggplot(rawdata.2 %>% filter(Phase == "Gas"), aes(x = Group, y = C_ppm*10000)) + # fill = Group
    stat_summary(fun = mean, geom = "bar", position = "dodge", width = 0.7, color = '#E57272', fill = "transparent") +  # Bar plot showing mean values
    stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, position = position_dodge(0.7)) +  # Error bars showing standard error
    geom_jitter(width = 0.2, size = 3, alpha = 0.5, stroke = 1, color = '#E57272') +  # Scatter points
    scale_fill_manual(values =  c('Gas' = '#E57272')) +  # Custom fill colors
    labs(title = "",
             x = "",
             y = "CO2 (ppm)") +  # , fill = "Group"
    theme_cns +
    scale_y_continuous(sec.axis = dup_axis(name = "CO2 (ppm)"))  # Add secondary y-axis on the right
zoom_plot
