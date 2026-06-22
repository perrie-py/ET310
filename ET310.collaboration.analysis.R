### ET310 ANALYSIS CODE ###

AI.data <- read.csv('https://raw.githubusercontent.com/perrie-py/ChatGPT-PP-Attachment-Ambiguity/refs/heads/main/collaboration.data.csv')
View(AI.data)


library(ggplot2)

#### REGULATING THE CATEGORICAL VARIBALES IN THE TABLE ####

unique(AI.data$PP_type)
unique(AI.data$ambiguity.)
AI.data$ambiguity.[AI.data$ambiguity. == 'Y '] <- 'Y'
AI.data$PP_type <- trimws(AI.data$PP_type)
unique(AI.data$PP_type)


# VISUALISATION OF PP TYPE 1ST QUESTION #
ggplot(AI.data, aes(x = PP_type, fill = X1attatchment)) +
  geom_bar(position = "fill") +
  facet_wrap(~ Regular) +
  ylab("Proportion") +
  xlab("PP Type") +
  ggtitle("Attachment Preferences by PP Type and Condition After Question 1")


#VISUALISATION OF PP TYPE RE-QUESTION

ggplot(AI.data, aes(x = PP_type, fill = X2attactchment)) +
  geom_bar(position = "fill") +
  facet_wrap(~ Regular) +
  ylab("Proportion") +
  xlab("PP Type") +
  ggtitle("Attachment Preferences by PP Type and Condition After Re-Questioning")

# STATISTICS ON CHATGPT'S ABILITY TO DETECT AMBIGUITY IN EACH PP-TYPE #
# TABLE 1
table_data <- table(AI.data$PP_type, AI.data$ambiguity.)
table_data
chisq.test(table_data)

# Statistics per PP type (residuals) #
chisq.test(table_data)$stdres
# > chisq.test(table_data)$stdres

#N          Y
#Descriptive  -2.2823391  2.2823391
    # AI is better at detecting ambiguity here
#Instrumental  2.6083875 -2.6083875
    # AI is worse than expcted at detecting ambiguity here
#Locative     -0.3260484  0.3260484
    # behaves as expected here

# residuals for regular / inverse pp type #
  # Regular
regular_data <- subset(AI.data, Regular == "Regular")
reg_table <- table(regular_data$PP_type, regular_data$ambiguity.)
chisq.test(reg_table)
chisq.test(reg_table)$stdres

  # Inverse
inverse_data <- subset(AI.data, Regular == "Inverse")
inv_table <- table(inverse_data$PP_type, inverse_data$ambiguity.)
chisq.test(inv_table)
chisq.test(inv_table)$stdres
fisher.test(inv_table)
