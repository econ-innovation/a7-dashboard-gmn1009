# 导入所需包
library(ggplot2)
library(dplyr)

# 读取数据
pub_211 <- read.csv("pub_211.csv")


# 1.发文最多的3个学科随时间变化散点图
# 按学科和年份统计发文量
pubnum_by_subject_year <- pub_211 %>%
  group_by(cnsubject, pubyear) %>%
  summarise(total_pubnum = sum(pubnum))

# 选出发文量最多的三个学科
top3_subjects <- pubnum_by_subject_year %>%
  group_by(cnsubject) %>%
  summarise(total_pubnum = sum(total_pubnum)) %>%
  top_n(3, total_pubnum) %>%
  arrange(desc(total_pubnum))

# 筛选出发文量最多的三个学科的数据
top3_subjects_data <- pubnum_by_subject_year %>%
  filter(cnsubject %in% top3_subjects$cnsubject)

# 绘制散点图
scatter_plot <- ggplot(top3_subjects_data, aes(x = pubyear, y = total_pubnum, color = cnsubject)) +
  geom_point() +
  labs(x = "年份", y = "发文量", color = "学科", title = "发文量最多的三个学科随时间变化散点图") +
  theme_minimal() +
  theme(legend.position = "right")

# 打印图表
print(scatter_plot)


#2.一个交互图
# 绘制交互式散点图
scatter_plot <- plot_ly(top3_subjects_data, x = ~pubyear, y = ~total_pubnum, color = ~cnsubject, type = 'scatter', mode = 'markers') %>%
  layout(title = "发文量最多的三个学科随时间变化散点图",
         xaxis = list(title = "年份"),
         yaxis = list(title = "发文量"),
         legend = list(title = "学科"))

# 打印图表
print(scatter_plot)


#3.一个表格
gt_tbl <- gt(top3_subjects)

# 设置表格标题
gt_tbl <- gt_tbl %>%
  tab_header(title = "发文量最多的三个学科")

# 打印表格
print(gt_tbl)


#一些说明性文本


#4.年度发文动态图
library(gganimate)
# 筛选出发文量最多的三个学科的数据
top3_subjects_data <- pubnum_by_subject_year %>%
  filter(cnsubject %in% top3_subjects$cnsubject)

# 创建动态图
anim_plot <- ggplot(top3_subjects_data, aes(x = pubyear, y = total_pubnum, color = cnsubject)) +
  geom_line() +
  labs(x = "年份", y = "发文量", title = "发文量最多的三个学科随时间变化动态图") +
  transition_reveal(pubyear) +
  ease_aes('linear')

# 打印动态图
animate(anim_plot, nframes = 100)
