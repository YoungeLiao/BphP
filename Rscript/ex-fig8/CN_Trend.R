rm(list=ls())
library(readxl)
library(ggplot2)
library(ggpubr)
library(dplyr)  # For data filtering etc.

path <- './data/ex-fig8/CN_5StrainsIR.xlsx'  # CN_Opto2.xlsx
sheet.name <- 'R1'
rawdata <- data.frame(read_excel(path, sheet = sheet.name))
head(rawdata)
# Samples Time      Strain Light IC TOC TC TN      HCO3       NO2      NH4      NO3      TIN     CH3COO       S2O3
# 1 D1_D_0602    0 Deinococcus  Dark NA  NA NA NA 108.80471  40.13979 38.40269 2.179592 78.54248 -0.1081876 36.2125887
# 2      D2_D    0 Deinococcus  Dark NA  NA NA NA 173.81385  38.13146       NA       NA       NA -0.1087626 25.2571800
# 3      D3_D    0 Deinococcus  Dark NA  NA NA NA  84.59464 -15.69300       NA       NA       NA -0.1060536  0.6993432
# 4      D1_I    0 Deinococcus    IR NA  NA NA NA 110.00160  35.37033       NA       NA       NA -0.1100268 30.8144124
# 5      D2_I    0 Deinococcus    IR NA  NA NA NA  93.62156  36.70977 30.03003       NA       NA -0.1094850 25.9405248
# 6      D3_I    0 Deinococcus    IR NA  NA NA NA 113.35987 -15.69300 71.51908       NA       NA -0.1078596  1.1240388
# SO4 S2O3_Intake SO4_produce S_CRate_... e_efficiency NO2.NH4  OD600 CCUS CCUS_Efficiency CCUS_Rate X.NO2N X.NH4N
# 1  1.264268          NA          NA          NA           NA      NA 0.1424   NA              NA        NA     NA     NA
# 2  4.580887          NA          NA          NA           NA      NA     NA   NA              NA        NA     NA     NA
# 3 18.815577          NA          NA          NA           NA      NA     NA   NA              NA        NA     NA     NA
# 4  1.688788          NA          NA          NA           NA      NA     NA   NA              NA        NA     NA     NA
# 5  5.008531          NA          NA          NA           NA      NA     NA   NA              NA        NA     NA     NA
# 6 19.295405          NA          NA          NA           NA      NA     NA   NA              NA        NA     NA     NA
# X.S2O3 NLR.NRR  HCO3_raw   NO2_Raw   NH4_Raw   NO3_Raw Acetate_Raw    S_Raw   SO4_Raw  NH4_1  NH4_2  NH4_3 OD600.raw1
# 1     NA      NA 0.1415811 0.4751929 0.2335000 0.0246815 0.003218361 3.374257 0.4007519 0.2348 0.2334 0.2323     0.1403
# 2     NA      NA 0.1933717 0.4581000        NA 0.0100000 0.002900000 2.345000 1.4113000     NA     NA     NA     0.1242
# 3     NA      NA 0.1222937 0.0000000        NA 0.0100000 0.004400000 0.037800 5.7485000     NA     NA     NA     0.1259
# 4     NA      NA 0.1425346 0.4346000        NA 0.0100000 0.002200000 2.867100 0.5301000     NA     NA     NA     0.1260
# 5     NA      NA 0.1294852 0.4460000 0.1954667 0.0100000 0.002500000 2.409200 1.5416000 0.1951 0.1954 0.1959     0.1191
# 6     NA      NA 0.1452100 0.0000000 0.3839333 0.0100000 0.003400000 0.077700 5.8947000 0.3875 0.3819 0.3824     0.1233

# 指定要可视化的指标
indicator <- "S2O3"  # 可以修改为其他指标，如"HCO3", "NO2", "NH4", "NO3"等

# 获取所有菌株名称
strain_names <- unique(rawdata$Strain)

# 计算每个时间点、每个菌株和光照条件下指定指标的平均值和标准误
data_summary <- rawdata %>%
  group_by(Time, Strain, Light) %>%
  summarise(
    mean_value = mean(!!sym(indicator), na.rm = TRUE),
    se_value = sd(!!sym(indicator), na.rm = TRUE) / sqrt(n()),
    .groups = 'drop'
  )

# 计算全局的纵坐标范围
y_min <- min(data_summary$mean_value - data_summary$se_value, na.rm = TRUE)
y_max <- max(data_summary$mean_value + data_summary$se_value, na.rm = TRUE)

# 设置CNS风格的主题
theme_cns <- theme(
  panel.background = element_rect(fill = "transparent"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.line = element_line(color = "black", size = 0.5),
  axis.ticks = element_line(color = "black", size = 0.5),
  axis.text.x = element_text(color = "black", size = 7, face = "plain"), # hjust = 1 # angle = 30, 
  axis.text.y = element_text(color = "black", size = 7, face = "plain"),
  axis.title = element_text(color = "black", size = 7, face = "plain"),
  legend.position = "none", # "right",
  legend.title = element_blank(),
  legend.text = element_text(size = 7, face = "plain"),
  legend.background = element_rect(fill = "white", color = NA),
  legend.key = element_rect(fill = "white", color = NA),
  plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
  plot.title = element_text(size = 7, face = 'plain', hjust = 0.5)
)

# 为每个菌株创建指标变化图
plot_list <- list()
for (i in 1:length(strain_names)) {
  strain_name <- strain_names[i]
  strain_data <- data_summary[data_summary$Strain == strain_name, ]
  
  p <- ggplot(strain_data, aes(x = Time, y = mean_value, color = Light, shape = Light, group = Light)) +
    # 添加点和线
    geom_point(size = 2.5, alpha = 0.8) +
    geom_line(size = 0.75) +
    
    # 添加误差棒
    geom_errorbar(aes(ymin = mean_value - se_value, ymax = mean_value + se_value),
                  width = 10, size = 0.8) +
    # 设置颜色
    scale_color_manual(values = c("Dark" = "#377EB8", "IR" = "#E41A1C"),
                       labels = c("Dark" = "Dark", "IR" = "IR")) +
    # 设置坐标轴范围
    coord_cartesian(ylim = c(y_min, y_max)) +
    # 设置坐标轴标签
    labs(x = "Time (h)", y = indicator, title = strain_name) +
    # 应用CNS主题
    theme_cns +
    # 设置图例标题
    guides(color = guide_legend(title = "Light condition"))
  
  plot_list[[i]] <- p
}

# 将所有子图拼到一张大图中，设置为一行五列
combined_plot <- ggarrange(plotlist = plot_list, ncol = 5, nrow = 1, 
                           common.legend = TRUE, legend = "right",
                           align = "hv", vjust = 1, hjust = 1)

# 显示图形
print(combined_plot)

# 保存图形
# ggsave(paste0("./output/Fig4_", indicator, "_trend.pdf"), combined_plot, width = 20, height = 6, dpi = 300)
