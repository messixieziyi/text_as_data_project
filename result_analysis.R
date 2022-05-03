
##########################
# result analysis
combined_result <- cbind(subset(merged[-ids_train, ], select=c(ym,pct_change,target_movement)),svm_linear_pred_topic)
colnames(combined_result) <- c('ym','pct_change','target','pred')
combined_result$same_or_not = as.numeric(combined_result$target == combined_result$pred)

#combined_result

# ggplot(combined_result, aes(x = pct_change, fill = same_or_not))+
#   geom_histogram(fill = "white", colour = "black") +
#   facet_grid(same_or_not ~ .)

combined_result$correctness <- recode_factor(combined_result$same_or_not, '0' = 'Wrong pred', '1' = 'Right pred')


ggplot(combined_result, aes(x = pct_change, fill = correctness)) +
  #geom_histogram(aes(y=..density..),position = "identity", alpha = 0.4)+
  geom_density(alpha=0.6)
  


ggplot(merged_topic,aes(x=date,y = Topic1))+geom_density(alpha = 0.6)

ggplot(merged_topic[1:50,], aes(x=date)) + 
  geom_line(aes(y = Topic1), color = "red") + 
  geom_line(aes(y = Topic2), color="blue") +
  geom_line(aes(y = Topic3), color="green")+
  geom_line(aes(y = Topic4), color="black")+
  geom_line(aes(y = Topic5), color="purple")+
  ylab("Proportions") 
  

       