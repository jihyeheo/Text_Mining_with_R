# 1.2 The unnest_tokens function
text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")
text
install.packages("dplyr")
library(dplyr)

# 데이터 프레임으로 바꾸는 작업
text_df <- tibble(line = 1:4, text = text)
text_df

install.packages("tidytext")
library(tidytext)
text_df %>%
  unnest_tokens(word, text) # 새로 만들어진 변수 이름 word
# 20개가 나왔고 다 소문자로 나왔다.
# 하지만 무조건 소문자로 하는 것은 비추 ~



# 1.3 Tidying the works of Jane Austen
install.packages("janeaustenr")
library(janeaustenr)
library(dplyr)
install.packages("stringr")
library(stringr)

original_books <- austen_books() %>%
  group_by(book) %>%
  # 새 컬럼
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, 
                                     # chapter라는 글자로 시작하고(^)
                                     # 숫자가 오거나(d)
                                     # 로마자로 되거나(ivxlc)
                                     # 인 애들만 추출하겠다.
                                     # 대문자 소문자 무시 ~
                                     regex("^chapter [\\divxlc]",
                                           ignore_case = TRUE)))) %>%
  ungroup()

original_books

library(tidytext)
tidy_books <- original_books %>%
  unnest_tokens(word, text)

tidy_books

data(stop_words)
# anti_join : 겹치는 거 제거!
# 불용어
tidy_books <- tidy_books %>%
  anti_join(stop_words)

tidy_books %>%
  count(word, sort = TRUE) 

install.packages("ggplot2")
library(ggplot2)
tidy_books %>%
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)



# 1.4 The gutenbergr package
# 1.5 Word Frequencies
install.packages("gutenbergr")
library(gutenbergr)
hgwells <- gutenberg_download(c(35, 36, 5230, 159))

tidy_hgwells <- hgwells %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_hgwells %>%
  count(word, sort = TRUE)


bronte <- gutenberg_download(c(1260, 768, 969, 9182, 767))
tidy_bronte <- bronte %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_bronte %>%
  count(word, sort = TRUE)

library(tidyr)

frequency <- bind_rows(mutate(tidy_bronte, author = "Bronte Sisters"),
                       mutate(tidy_hgwells, author = "H.G. Wells"), 
                       mutate(tidy_books, author = "Jane Austen")) %>% 
  # a부터 z까지 한 글자 이상 끄집어내다.
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = author, values_from = proportion) %>%
  pivot_longer(`Bronte Sisters`:`H.G. Wells`,
               names_to = "author", values_to = "proportion")

frequency


install.packages("scales")
library(scales)
library(ggplot2)
# expect a warning about rows with missing values being removed
ggplot(frequency, aes(x = proportion, y = `Jane Austen`, 
                      color = abs(`Jane Austen` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), 
                       low = "darkslategray4", high = "gray75") +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Jane Austen", x = NULL)


cor.test(data = frequency[frequency$author == "Bronte Sisters",],
         ~ proportion + `Jane Austen`)

cor.test(data = frequency[frequency$author == "H.G. Wells",], 
         ~ proportion + `Jane Austen`)

