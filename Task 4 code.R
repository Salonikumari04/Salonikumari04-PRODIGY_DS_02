# Load required libraries
library(readr)
library(dplyr)
library(tidytext)

# Load the dataset
file_path <- "D:/Study/College/Sem 6/Prodigy/sentimentdataset.csv"  # Replace with the actual path
data <- read_csv(file_path)

# Preview the data
glimpse(data)

# Clean the text data
data_clean <- data %>%
  mutate(
    Text = gsub("[^[:alnum:][:space:]]", "", Text),  # Remove special characters
    Text = tolower(Text)  # Convert to lowercase
  )

# Preview the cleaned data
glimpse(data_clean)

# Tokenize the text
tokenized_data <- data_clean %>%
  unnest_tokens(word, Text)

# Preview tokenized data
head(tokenized_data)

data("stop_words")

filtered_data <- tokenized_data %>%
  anti_join(stop_words, by = "word")

# Preview filtered data
head(filtered_data)

# Perform sentiment analysis
sentiment_scores <- filtered_data %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(Sentiment, sentiment, sort = TRUE)

# View sentiment scores
print(sentiment_scores, n=700)

sentiment_by_platform <- filtered_data %>%
  inner_join(get_sentiments("bing"), by = "word", relationship = "many-to-many") %>%
  count(Platform, sentiment, sort = TRUE)

# Visualize using a bar plot
ggplot(sentiment_by_platform, aes(x = Platform, y = n, fill = sentiment)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Sentiment by Platform", x = "Platform", y = "Count") +
  theme_minimal()

# Summarize sentiment counts by positive/negative
sentiment_summary <- sentiment_scores %>%
  group_by(sentiment) %>%
  summarise(total_count = sum(n))

print(sentiment_summary)

# Visualize the sentiment distribution
ggplot(sentiment_scores, aes(x = sentiment, y = n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Sentiment Distribution", x = "Sentiment", y = "Count")

# Top 10 most frequent sentiments
top_sentiments <- sentiment_scores %>%
  arrange(desc(n)) %>%
  head(10)

print(top_sentiments)

# Group by specific sentiment categories
sentiment_by_category <- sentiment_scores %>%
  group_by(Sentiment) %>%
  summarise(total_count = sum(n))

print(sentiment_by_category)

sentiment_with_platform <- filtered_data %>%
  inner_join(sentiment_scores, by = "Sentiment") %>%
  count(Platform, sentiment, sort = TRUE)

ggplot(sentiment_with_platform, aes(x = Platform, y = n, fill = sentiment)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_minimal() +
  labs(title = "Emotion Distribution by Platform", x = "Platform", y = "Emotion Count") +
  scale_fill_manual(values = c("positive" = "lightblue", "negative" = "salmon"))
