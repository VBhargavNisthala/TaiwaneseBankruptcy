banking.df <- read.csv("data.csv")
head(banking.df)
glimpse(banking.df)
any(is.na(banking.df))

set.seed(1, sample.kind = "Rounding") # just to make the code reproducible

validation_index <- createDataPartition(y = banking.df$Bankrupt, times = 1, p = 0.2, list = FALSE)

validation <- banking.df[validation_index,]
banking.df <- banking.df[-validation_index,]

nzv <- nearZeroVar(banking.df)
nzv[!nzv %in% 1] #to avoid delete the variable of interest

banking.df <- df[,-nzv[!nzv %in% 1]]

banking.df$Bankrupt <- factor(df$Bankrupt, labels = c("non_bankruptcy", "bankruptcy"))
validation$Bankrupt <- factor(validation$Bankrupt, labels = c("non_bankruptcy", "bankruptcy"))

train_index <- createDataPartition(y = banking.df$Bankrupt, times = 1, p = 0.8, list = FALSE)

train_set <- banking.df[train_index,]
test_set <- banking.df[-train_index,]

summary(banking.df[,1:12])

banking.df %>%# Change the order just for aesthetics
  mutate(Bankrupt = relevel(banking.df$Bankrupt, "non_bankruptcy"))%>%
  group_by(Bankrupt)%>%#dplyr:: must be added for confusion with plyr package
  dplyr::summarise(n = n())%>% 
  mutate(percentage = n/sum(n)*100)%>%
  ggplot(aes(Bankrupt, n, fill=Bankrupt))+
  geom_bar(stat="identity")+
  geom_text(aes(label=n), vjust=-0.3, hjust = 1, size=4.5)+
  geom_text(aes(label=paste0("( ",round(percentage),"% )")), vjust=-0.3, hjust = -0.1, size=4.5)+
  ylab("cases")+ 
  scale_fill_manual(values = c("bankruptcy" = "#F8766D", "non_bankruptcy" = "#00BFC4"))+
  xlab("")+
  ggtitle("Bankrupt distribution")


