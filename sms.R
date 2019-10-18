rm(list = ls())

library(formattable)   # formattable()
library(corrplot)      # corrplot.mixed()
library(wordcloud)     # wordcloud()
library(wordcloud2)     # wordcloud()
library(dplyr)         # mutate()
library(stringr)       # str_length() / str_count()
library(randomForest)  # randomForest()
library(tm)            # 文本挖掘
library(ROSE)          # ovun.sample()
library(magrittr)      # %<>%
library(caret)         # createDataPartition()
library(SnowballC)
library(RColorBrewer)


# 读取数据，stringsAsFactors = F意味着不会将字符串转换为因子型
sms <- read.csv("spam.csv", stringsAsFactors = F)

sms <- sms[c(1, 2)]                    # 保留数据的前两列
names(sms) <- c('label', 'message')    # 重命名列名

# 将message列转换为utf-8格式，不能转换的字符使用其十六进制形式替换
# enc2utf8()将message列的内容转换为utf-8格式
# iconv(sub = 'byte')指定不能转换的字符用其十六进制形式替换
sms$message <- sapply(sms$message, function(x) iconv(enc2utf8(x), sub = "byte"))

## 重塑数据
# str_length()计算字符长度
# str_count() 计算字符数量
sms %<>% mutate(msg_length = str_length(message),
                pct_caps = str_count(message, "[A-Z]") / msg_length,
                pct_digits = str_count(message,"[0-9]") / msg_length,
                num_exclamations = str_count(message, "!"),
                num_caps = str_count(message, "[A-Z]"),
                num_digits = str_count(message,"[0-9]"),
                numeric_label = as.numeric(as.factor(label)))

## 对比不同短信的重塑数据
sms %>% 
  select(-message) %>%  # 剔除message列，只需对重塑的数据和类别操作
  group_by(label) %>%   # 按照label进行统计
  summarise_all(function(x) round(mean(x), 3)) %>% # 统计每列的均值，并保留三位小数 
  formattable(align = 'l') # 生成一个数据表，左对齐

## 绘制相关系数图
# tl.pos = 'lt'指定列名放在图的左边和上面
# diag = 'l'   指定对角线下面用数值填充

corr <- cor(sms[, -c(1, 2)])                      # 计算数据的相关系数
corrplot.mixed(corr, tl.pos = 'lt', diag = 'l') # 可视化相关系数

sms$numeric_label <- NULL # 剔除numeric_label列


## 创建文本处理函数
create_corpus <- function(x) {
  result_corpus <- VCorpus(VectorSource(x)) %>% 
    tm_map(tolower) %>%                    # 把所有字母转换为小写字母
    tm_map(PlainTextDocument) %>%          # 把文本转换为纯文本文档
    tm_map(removePunctuation) %>%          # 删除标点符号
    tm_map(removeWords, stopwords()) %>%   # 剔除停用词
    tm_map(removeNumbers) %>%              # 剔除数字
    tm_map(stripWhitespace) %>%            # 剔除空格
    tm_map(stemDocument)                   # 采用Porter's stemming算法提取词干
  return(result_corpus)
}

# 对正常短信进行文本处理
corpus_ham <- create_corpus(sms$message[sms$label == "ham"]) 
# 对垃圾短信进行文本处理
corpus_spam <- create_corpus(sms$message[sms$label == "spam"])

## 对比处理前后的文本
sms$message[1]
as.character(corpus_ham[[1]])


par(mfrow = c(1, 2)) # 将绘图窗口分为两份

## 绘制词云图
# min.freq = 10即词频低于10的词语不会绘制出来
# max.words = 100即词云图最多展示100个词
# scale单词的大小范围
# random.order = F即按照词频降序绘制词云图
wordcloud(sms[sms$label == 'ham', ]$message, min.freq = 10, max.words = 100, scale=c(3, 0.5), 
          random.order = FALSE, colors = c("springgreen1","springgreen2",
                                           "springgreen3","springgreen4"))

wordcloud(sms[sms$label == 'spam', ]$message, min.freq = 10, max.words = 100, scale=c(5, .5), 
          random.order = FALSE, colors = c("indianred1","indianred2",
                                           "indianred3","indianred"))

# 创建稀疏矩阵
dtm_ham <- DocumentTermMatrix(corpus_ham) %>%
  removeSparseTerms(0.99) # 降维，相当于剔除文本中的低频词汇

## 展示稀疏矩阵
dtm_ham
inspect(dtm_ham[35:45, 1:10])
as.matrix(dtm_ham[35:45, 1:10]) # 展示稀疏矩阵的一部分

# 创建稀疏矩阵
dtm_spam <- DocumentTermMatrix(corpus_spam) %>%
  removeSparseTerms(0.979)

## 展示稀疏矩阵
dtm_spam
inspect(dtm_spam[35:45, 1:10])
as.matrix(dtm_spam[35:45, 1:10])

# 封装函数---计算每个词出现次数
create_term_frequency_counts <- function(dtm) {
  m <- as.matrix(t(dtm))
  v <- sort(rowSums(m), decreasing = TRUE)
  d <- data.frame(word = names(v), freq = v, stringsAsFactors = FALSE)
  return(d)
}

# 计算dtm_ham稀疏矩阵的每个词的词频
wordfreq_ham <- create_term_frequency_counts(dtm_ham)
head(wordfreq_ham)

# 计算dtm_spam稀疏矩阵的每个词的词频
wordfreq_spam <- create_term_frequency_counts(dtm_spam)
head(wordfreq_spam)

# 合并两列数据
# by='word'指定按照word进行合并，新列名是原始列名+'_ham' / '_spam'
word_freq <- full_join(wordfreq_ham, wordfreq_spam, by = "word",
                       suffix =  c("_ham", "_spam"))
head(word_freq)


corpus <- create_corpus(sms$message)        # 对所有文本进行文本处理
dtm <- DocumentTermMatrix(corpus)           # 构建稀疏矩阵

# 把稀疏矩阵转换为数据框，并且只选择word_freq里面的word列里面特征值
dtm <- as.data.frame(as.matrix(dtm)) %>%
  select(word_freq$word)

wordfreq_dtm <- create_term_frequency_counts(dtm)

dtm2 <- suppressWarnings(cbind(dtm, sms[-2])) # 把稀疏矩阵和之前重塑的数据合并
dtm2$label <- as.factor(dtm2$label)           # 把类别转换为因子型
table(dtm2$label)                             # 查看类别个数
prop.table(table(dtm2$label))                 # 查看类别占比


## 剔除一半正常短信的样本
index <- rownames(dtm2[dtm2$label == 'ham', ]) # 获取正常短信样本行索引
set.seed(1234)
index2 <- sample(index, length(index)*0.5)     # 抽取其中的一半行索引
dtm3 <- dtm2[as.integer(index2), ]            # 剔除这一半样本
table(dtm3$label)                              # 查看剔除后的样本类别个数

## 对数据进行过采样
# data是数据源
# method = 'over'代表过采样
# N代表最后的样本个数
data_balanced_over <- ovun.sample(label ~ ., data = dtm,
                                  method = "over", N = 9652)$data ## 4826 
table(data_balanced_over$label)        # 查看采样后的样本类别个数

## 建立训练集和测试集---分层抽样
data_balanced_over <- select(data_balanced_over, label, everything())
set.seed(12)
index3 <- createDataPartition(data_balanced_over$label, p = 0.7, list = F)
traindata <- data_balanced_over[index3, ] # 建立训练集
testdata <- data_balanced_over[-index3, ] # 建立测试集

## 建立随机森林模型
set.seed(123) 
model_rf <- randomForest(traindata[-1], traindata[,1], importance = T, ntree = 100)

pred_rf <- predict(model_rf, testdata[-1])                  # 对测试集进行预测
confusionMatrix(pred_rf, testdata$label, positive = 'spam') # 建立混淆矩阵--99.24%

## 构建一个数据框--包含变量的重要性对应值
importance <- importance(model_rf)
head(importance)
var_importance <- data.frame(Variables = row.names(importance), 
                             Importance = round(importance[ , 'MeanDecreaseGini'], 2)) %>% 
  arrange(desc(Importance))

# 可视化变量重要性
ggplot(var_importance[1:15, ], 
       aes(x = reorder(Variables, Importance), y = Importance, fill = Variables)) +
  geom_bar(stat = 'identity') +
  labs(x = 'Variables') +
  coord_flip() +
  theme_minimal()+
  guides(fill = 'none')

# 利用重塑的六列数据进行建模
traindata2 <- 
  traindata %>% 
  select(label, num_digits, pct_digits, msg_length, num_caps, pct_caps, num_exclamations)

set.seed(123)
model_rf2 <- randomForest(traindata2[-1], traindata$label, importance = T, ntree = 100)
pred_rf2 <- predict(model_rf2, testdata[-1])                 # 对测试集进行预测
confusionMatrix(pred_rf2, testdata$label, positive = 'spam') # 建立混淆矩阵--98.82%

## 构建一个数据框--包含变量的重要性对应值
(importance2 <- importance(model_rf2))
var_importance2 <- data.frame(Variables = row.names(importance2), 
                              Importance = round(importance2[ , 'MeanDecreaseGini'], 2)) %>% 
  arrange(desc(Importance))

# 可视化变量重要性
ggplot(var_importance2, 
       aes(x = reorder(Variables, Importance), y = Importance, fill = Variables)) +
  geom_bar(stat = 'identity') +
  labs(x = 'Variables') +
  coord_flip() +
  theme_minimal() +
  guides(fill = 'none')

