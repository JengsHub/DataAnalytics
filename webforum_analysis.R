rm(list = ls())
# Redirect to your directory

# install required packages
install.packages("ggplot2")
library(ggplot2)
install.packages("plyr")
library(plyr)
install.packages("viridis")
library(viridis)
install.packages("data.table")
library(data.table)
install.packages("reshape2")
library(reshape2)
install.packages("ggpubr")
library(ggpubr)
install.packages("igraph")
library(igraph)

# To obtain unique seed
set.seed(29637996)
# Read the csv file
webforum = read.csv("webforum.csv")
# Obtain 20000 rows
webforum = webforum [sample(nrow(webforum), 20000), ]

# Convert the date from string to date time and add as new column
date_type = as.Date(webforum[,3], "%Y-%m-%d")
webforum = cbind(webforum, date_type)

# bind date, month, year in date time format as new columns
webforum = cbind(webforum, date = as.numeric(format(date_type, "%d")))
webforum = cbind(webforum, month = as.numeric(format(date_type, "%m")))
webforum = cbind(webforum, year = as.numeric(format(date_type, "%Y")))

# function to check for missing value in columns
missing_vals = function(x) {return(sum(is.na(x)))}
empty_cells = function(x) { return(sum(x==""))}

num_rows = length(colnames(webforum))
temp_table = data.frame(Missing_Values = integer(num_rows)) 

temp_table$Missing_Values = apply(webforum,2,missing_vals)
temp_table$Empty_Cells = apply(webforum,2,empty_cells)
rownames(temp_table) = colnames(webforum)

temp_table

# filter out author id -1 and check new data-set
no_anon_auth = webforum[webforum$AuthorID != "-1", ]
# show number of entries before and after removing anonymous authors
cat("Number of Entries before removal: ", nrow(webforum))

cat("Number of Entries after removal: ", nrow(no_anon_auth))

#Plotting the line chart of activity from 2002 to 2011 without anon authors
yr_monthly_counts_no_anon = count(no_anon_auth,c("year", "month"))
yr_monthly_counts_no_anon$dt <- with(yr_monthly_counts_no_anon, sprintf(
  "%d-%02d-1", year, month))
yr_monthly_counts_no_anon$dt <- as.Date(yr_monthly_counts_no_anon$dt,
                                        "%Y-%m-%d" )
yr_monthly_trend_no_anon = ggplot(yr_monthly_counts_no_anon, aes(dt, freq)) + 
  geom_line(color="#69b3a2", size = 2) +
  labs(y="Number of Threads", x="Years") +
  theme_classic() +
  ggtitle("Number of posts made over the months (2002 to 2011)
          (With no anonymous author)")
yr_monthly_trend_no_anon


# plot line chart of yearly growth with anon authors
yr_monthly_counts = count(webforum,c("year", "month"))
yr_monthly_counts$dt <- with(yr_monthly_counts, sprintf("%d-%02d-1",
                                                        year,
                                                        month))
yr_monthly_counts$dt <- as.Date(yr_monthly_counts$dt, "%Y-%m-%d" )
yr_monthly_trend = ggplot(yr_monthly_counts, aes(dt, freq)) + 
  geom_line(color="#69b3a2", size = 2) +
  labs(y="Number of Threads", x="Years") +
  theme_classic() + 
  ggtitle("Number of posts made over the months (2002 to 2011)")
yr_monthly_trend
# save plot
# yr_monthly_trend = yr_monthly_trend + ggsave("line.png")

# do a t test here for 2005 and 2006
subset_2005 = yr_monthly_counts[yr_monthly_counts$year == "2005", ]
subset_2006 = yr_monthly_counts[yr_monthly_counts$year == "2006", ]
# t.test on number of post made
t.test(subset_2006$freq, subset_2005$freq, "greater", conf.level = 0.95)

# plot area chart by month to show monthly trend
monthly_counts = count(webforum, "month")
monthly_trend = ggplot(monthly_counts, aes(month, freq))
monthly_trend = monthly_trend + geom_area(fill="#69b3a2", alpha=0.4) +
  geom_line(color="#69b3a2", size = 2) +
  geom_point(fill="#69b3a2", color = 2) +
  labs(y="Number of Threads", x="Months") +
  ggtitle("Number of posts made over the months (2002 to 2011)") +
  scale_x_continuous(breaks=1:12, labels = monthly_counts$month)
monthly_trend

# Investigating the change of linguistic variables with respect to time
var_sums = aggregate(webforum[,5:19], webforum[22], mean)
monthly_counts = count(webforum, "month")
monthly_WC_var = cbind(occurence = var_sums$WC, 
                       variable = rep("WC", times = nrow(var_sums)), 
                       month = var_sums$month)

monthly_Analtytic_var = cbind(occurence = var_sums$Analytic,  
                              variable = rep("Analytic", times = nrow(var_sums)),
                              month = var_sums$month)
monthly_clout_var = cbind(occurence = var_sums$Clout,  
                          variable = rep("Clout", times = nrow(var_sums)), 
                          month = var_sums$month)
monthly_Auth_var = cbind(occurence = var_sums$Authentic,  
                         variable = rep("Authentic", times = nrow(var_sums)),
                         month = var_sums$month)
monthly_Tone_var = cbind(occurence = var_sums$Tone,  
                         variable = rep("Tone", times = nrow(var_sums)), 
                         month = var_sums$month)
monthly_WPS_var = cbind(occurence = var_sums$WPS,  
                        variable = rep("WPS", times = nrow(var_sums)), 
                        month = var_sums$month)
monthly_i_var = cbind(occurence = var_sums$i,  
                      variable = rep("i", times = nrow(var_sums)),
                      month = var_sums$month)
monthly_we_var = cbind(occurence = var_sums$we,  
                       variable = rep("we", times = nrow(var_sums)), 
                       month = var_sums$month)
monthly_you_var = cbind(occurence = var_sums$you,  
                        variable = rep("you", times = nrow(var_sums)), 
                        month = var_sums$month)
monthly_they_var = cbind(occurence = var_sums$they,  
                         variable = rep("they", times = nrow(var_sums)),
                         month = var_sums$month)
monthly_number_var = cbind(occurence = var_sums$number, 
                           variable = rep("number", times = nrow(var_sums)),
                           month = var_sums$month)
monthly_affect_var = cbind(occurence = var_sums$affect,  
                           variable = rep("affect", times = nrow(var_sums)), 
                           month = var_sums$month)
monthly_posemo_var = cbind(occurence = var_sums$posemo,  
                           variable = rep("posemo", times = nrow(var_sums)), 
                           month = var_sums$month)
monthly_negemo_var = cbind(occurence = var_sums$negemo,  
                           variable = rep("negemo", times = nrow(var_sums)), 
                           month = var_sums$month)
monthly_anx_var = cbind(occurence = var_sums$anx,  
                        variable = rep("anx", times = nrow(var_sums)), 
                        month = var_sums$month)

monthly_vars = monthly_WC_var
monthly_vars = rbind(monthly_vars, monthly_Analtytic_var)
monthly_vars = rbind(monthly_vars, monthly_clout_var)
monthly_vars = rbind(monthly_vars, monthly_Auth_var)
monthly_vars = rbind(monthly_vars, monthly_Tone_var)
monthly_vars = rbind(monthly_vars, monthly_WPS_var)
monthly_vars = rbind(monthly_vars, monthly_i_var)
monthly_vars = rbind(monthly_vars, monthly_we_var)
monthly_vars = rbind(monthly_vars, monthly_you_var)
monthly_vars = rbind(monthly_vars, monthly_they_var)
monthly_vars = rbind(monthly_vars, monthly_number_var)
monthly_vars = rbind(monthly_vars, monthly_affect_var)
monthly_vars = rbind(monthly_vars, monthly_posemo_var)
monthly_vars = rbind(monthly_vars, monthly_negemo_var)
monthly_vars = rbind(monthly_vars, monthly_anx_var)

monthly_vars = as.data.frame(monthly_vars)
# plotting the monthly sum of linguistic variables
monthly_var_trend = ggplot(monthly_vars, aes(as.numeric(month),
                                             as.numeric(occurence), 
                                             group = 1)) +
  geom_line() +
  facet_grid(variable~.) + facet_wrap(~variable, ncol = 3, scales = "free") +
  scale_x_continuous(breaks=1:12, labels = monthly_counts$month) +
  labs(y="Average Usage", 
       x="Months") 
monthly_var_trend
# monthly_var_trend = monthly_var_trend + ggsave("LinVar.png")

# Analyzing the correlation between variables
correlations = cor(webforum[, 5:19])
correlations = melt(correlations)
cor_map = ggplot(correlations, aes(Var2, Var1, fill = value)) +
  geom_tile(color = "white") + scale_fill_gradient(low = "white", high="red") +
  ggtitle("Correlations between linguistic variables") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  labs(y = "Linguistic Variables",
       x = "Linguistic Variables")
cor_map
cor_map = cor_map + ggsave("cor_map.png")

# getting summary of threads present
summary(webforum[,5:19])

# Getting only top threads
top_threads = count(webforum,"ThreadID")
top_threads = top_threads[order(-top_threads$freq,decreasing = FALSE),]

# Getting the top 5 Threads with most activity
top_threads = top_threads[1:5, ]
top_threads = merge(top_threads, webforum, by = "ThreadID")

# Plotting a boxplot for each linguistic variable with different threads
top_threads$ThreadID = as.factor(top_threads$ThreadID)
posemo = ggplot(top_threads, aes(ThreadID, posemo)) + geom_boxplot() +
  stat_summary(fun=mean, geom="point", shape=23, size=4) +
  ggtitle("Presence of Positive Emotion") +
  labs(y="Positive Emotion (%)", x="Thread IDs")
posemo

negemo = ggplot(top_threads, aes(ThreadID, negemo)) + geom_boxplot() +
  stat_summary(fun=mean, geom="point", shape=23, size=4) +
  ggtitle("Presence of Negative Emotion") +
  labs(y="Negative Emotion (%)", x="Thread IDs")
negemo

Tone = ggplot(top_threads, aes(ThreadID, Tone)) + geom_boxplot() +
  stat_summary(fun=mean, geom="point", shape=23, size=4) +
  ggtitle("Presence of Emotional Tone") +
  labs(y="Emotional Tone (%)", x="Thread IDs")
Tone

authentic = ggplot(top_threads, aes(ThreadID, Authentic)) + geom_boxplot() +
  stat_summary(fun=mean, geom="point", shape=23, size=4) +
  ggtitle("Presence of Authenticity") +
  labs(y="Authenticity (%)", x="Thread IDs")
authentic

i = ggplot(top_threads, aes(ThreadID, i)) + geom_boxplot() +
  stat_summary(fun=mean, geom="point", shape=23, size=4) +
  ggtitle("Presence of First Person Pronoun") +
  labs(y="First Person Pronoun (%)", x="Thread IDs")
i

LIWC_figure = ggarrange(posemo, negemo, Tone, authentic, i,
                        ncol = 3, nrow = 2)
LIWC_figure

# use t test to prove that there is a difference in language used by 
# different groups
# Thread ID 252620 seems to have more posts with positive emotion compared to 
# other threads Prove that is true by comparing it with 
# second highest one (145223)
posemo_472752 = top_threads[top_threads$ThreadID == "472752", ]
posemo_283958 = top_threads[top_threads$ThreadID == "283958", ]
t.test(posemo_472752$posemo, 
       posemo_283958$posemo, 
       "greater", 
       conf.level = 0.95)

# Thread ID 252620 seems to have more posts with negative emotion compared to 
# other threads Prove that is true by comparing it with 
# second highest one (145223)
negemo_252620 = top_threads[top_threads$ThreadID == "252620", ]
negemo_145223 = top_threads[top_threads$ThreadID == "145223", ]
t.test(negemo_252620$negemo, 
       negemo_145223$negemo, 
       "greater", 
       conf.level = 0.95)

# Thread ID 472752 seems to have more posts with emotional tone compared to 
# other threads Prove that is true by comparing it with 
# second highest one (283958)
tone_472752 = top_threads[top_threads$ThreadID == "472752", ]
tone_283958 = top_threads[top_threads$ThreadID == "283958", ]
t.test(tone_472752$Tone, 
       tone_283958$Tone, 
       "greater", 
       conf.level = 0.95)

# Thread ID 472752 seems to have more authentic posts compared to other threads
# Prove that is true by comparing it with second highest one (283958)
# How many to prove
authentic_472752 = top_threads[top_threads$ThreadID == "472752", ]
authentic_283958 = top_threads[top_threads$ThreadID == "283958", ]
t.test(authentic_472752$Authentic, 
       authentic_283958$Authentic, 
       "greater", 
       conf.level = 0.95)

# Thread ID 472752 seems to have more usage of First Person pronouns in their 
# post compared to other threads
# Prove that is true by comparing it with second highest one (283958)
# How many to prove
i_472752 = top_threads[top_threads$ThreadID == "472752", ]
i_283958 = top_threads[top_threads$ThreadID == "283958", ]
t.test(i_472752$i, 
       i_283958$i, 
       "greater", 
       conf.level = 0.95)

# Does the language used within threads (or between threads) change over time?
# See the top 3 threads and its linguistic variables over time
top_3_threads = count(webforum,"ThreadID")
top_3_threads = top_3_threads[order(-top_3_threads$freq,decreasing = FALSE),]

# Getting the top 3 Threads with most activity
top_3_threads = top_3_threads[1:3, ]
top_3_threads = merge(top_3_threads, webforum, by = "ThreadID")
attach(top_3_threads)
top_3_threads = aggregate(cbind(posemo, negemo, Tone, Authentic, i) ~ 
                            ThreadID + month, 
                          top_3_threads, 
                          FUN = mean)
detach(top_3_threads)

# Getting the trend of Posemo in top 3 threads
monthly_posemo_trend = ggplot(top_3_threads, aes(as.numeric(month), posemo,
                                                 group = 1)) +
  geom_line() + 
  facet_grid(ThreadID~.) + facet_wrap(~ ThreadID, ncol = 3) + 
  scale_x_continuous(breaks=1:12, labels = c("1", "2", "3", "4", "5", "6",
                                             "7", "8", "9", "10", "11", "12")) +
  labs(y="Positive emotion (%)", 
       x="Months (cumulative of 2002 to 2011)") +
  ggtitle("Use of positive emotion (average) within top 3 threads")
monthly_posemo_trend

# Getting the trend of negemo in top 3 threads
monthly_negemo_trend = ggplot(top_3_threads, aes(as.numeric(month), negemo,
                                                 group = 1)) +
  geom_line() + 
  facet_grid(ThreadID~.) + facet_wrap(~ ThreadID, ncol = 3) + 
  scale_x_continuous(breaks=1:12, labels = c("1", "2", "3", "4", "5", "6",
                                             "7", "8", "9", "10", "11", "12")) +
  labs(y="Negative emotion (%)", 
       x="Months (cumulative of 2002 to 2011)") +
  ggtitle("Use of negative emotion (average) within top 3 threads")
monthly_negemo_trend

# Getting the trend of emotional tone in top 3 threads
monthly_tone_trend = ggplot(top_3_threads, aes(as.numeric(month), Tone,
                                               group = 1)) +
  geom_line() + 
  facet_grid(ThreadID~.) + facet_wrap(~ ThreadID, ncol = 3) + 
  scale_x_continuous(breaks=1:12, labels = c("1", "2", "3", "4", "5", "6",
                                             "7", "8", "9", "10", "11", "12")) +
  labs(y="Emotional tone (%)", 
       x="Months (cumulative of 2002 to 2011)") +
  ggtitle("Use of emotional tone (average) within top 3 threads")
monthly_tone_trend

# Getting the trend of authenticity in top 3 threads
monthly_auth_trend = ggplot(top_3_threads, aes(as.numeric(month), Authentic,
                                               group = 1)) +
  geom_line() + 
  facet_grid(ThreadID~.) + facet_wrap(~ ThreadID, ncol = 3) + 
  scale_x_continuous(breaks=1:12, labels = c("1", "2", "3", "4", "5", "6",
                                             "7", "8", "9", "10", "11", "12")) +
  labs(y="Presence of authenticity (%)", 
       x="Months (cumulative of 2002 to 2011)") +
  ggtitle("Presence of authenticity (average) within top 3 threads")
monthly_auth_trend

# Getting the trend of First person pronouns in top 3 threads
monthly_fpp_trend = ggplot(top_3_threads, aes(as.numeric(month), i, group=1)) +
  geom_line() + 
  facet_grid(ThreadID~.) + facet_wrap(~ ThreadID, ncol = 3) + 
  scale_x_continuous(breaks=1:12, labels = c("1", "2", "3", "4", "5", "6",
                                             "7", "8", "9", "10", "11", "12")) +
  labs(y="First person Pronoun (%)", 
       x="Months (cumulative of 2002 to 2011)") +
  ggtitle("Presence of first person pronoun (average) within top 3 threads")
monthly_fpp_trend

LIWC_trend_figure = ggarrange(monthly_posemo_trend, monthly_negemo_trend, 
                              monthly_tone_trend, monthly_auth_trend,
                              monthly_fpp_trend, ncol = 1, nrow = 5)
LIWC_trend_figure

# Filter data by months
# filtering webforum for only data in 2011, October
matrix_2011_10 =  webforum[webforum$year == "2011", ]
matrix_2011_10 =  matrix_2011_10[matrix_2011_10$month == "10", ]


# build adjacency matrix for 2011_10
# remove anonymous authors as they might not all represent same person
no_anonymous = matrix_2011_10[matrix_2011_10$AuthorID != "-1", ]
unique_authors = unique(no_anonymous$AuthorID)
unique_authors_df = as.data.frame(unique_authors)
nrow = nrow(unique_authors_df)

adj_matrix_2011_10 = matrix(data=rep(0,nrow[1]*nrow[1]),
                            nrow = nrow[1],
                            ncol = nrow[1],
                            dimnames = list(unique_authors, unique_authors))

unique_threads = unique(no_anonymous$ThreadID)
unique_threads = as.data.frame(unique_threads)

for(i in 1:nrow(unique_threads)){
  # get the authors in a thread and plot them in matrix as they have a 
  # connection
  authors_in_thread = no_anonymous[no_anonymous$ThreadID == unique_threads[i,],]
  # get unique authors
  authors_in_thread = as.data.frame(unique(authors_in_thread$AuthorID))
  # for each author, we loop list again to find the connected authors?
  for(j in 1: nrow(authors_in_thread)){
    curr_author = authors_in_thread[j, ]
    for(k in j: nrow(authors_in_thread)){
      if (curr_author != authors_in_thread[k, ]){
        adj_matrix_2011_10[as.character(curr_author), 
                           as.character(authors_in_thread[k, ])] = 1
      }
    }
  }
}

graph_df = graph_from_adjacency_matrix(adj_matrix_2011_10, mode = "undirected")
plot(graph_df, layout = layout.auto, node.size = 0.5,
     main = "Social Network of October 2011")

deg = degree(graph_df)
bet = betweenness(graph_df)
clos = closeness(graph_df)
dia = diameter(graph_df)
avg = average.path.length(graph_df)
EigF = evcent(graph_df)$vector
results = as.data.frame(rbind(deg, bet, clos, dia, avg, EigF))
results = t(results)
colnames(results) = c("Degree", 
                      "Betweenness", 
                      "Closeness",
                      "Diameter",
                      "Average Path Length",
                      "Eigenvector Centrality")

top_10_degree = results[order(-deg),]
top_10_degree[1:10, c("Degree") , drop = FALSE]

top_10_bet = results[order(-bet),]
top_10_bet[1:10, c("Betweenness") , drop = FALSE]

top_10_clos = results[order(-clos),]
top_10_clos[1:10, c("Closeness") , drop = FALSE]

top_10_EigF = results[order(-EigF),]
top_10_EigF[1:10, c("Eigenvector Centrality") , drop = FALSE]


# Repeat the same step for the following month
# 2011, November to show change in time
matrix_2011_11 =  webforum[webforum$year == "2011", ]
matrix_2011_11 =  matrix_2011_11[matrix_2011_11$month == "11", ]

# build adjacency matrix for 2011_11
# remove anonymous authors as they might not all represent same person
no_anonymous_sec = matrix_2011_11[matrix_2011_11$AuthorID != "-1", ]
unique_authors_sec = unique(no_anonymous_sec$AuthorID)
unique_authors_df_sec = as.data.frame(unique_authors_sec)
nrow = nrow(unique_authors_df_sec)

adj_matrix_2011_11 = matrix(data=rep(0,nrow[1]*nrow[1]),
                            nrow = nrow[1],
                            ncol = nrow[1],
                            dimnames = list(unique_authors_sec,
                                            unique_authors_sec))

unique_threads_sec = unique(no_anonymous_sec$ThreadID)
unique_threads_sec = as.data.frame(unique_threads_sec)

for(i in 1:nrow(unique_threads_sec)){
  # get the authors in a thread and plot them in matrix as they have a 
  # connection
  authors_in_thread_sec = no_anonymous_sec[no_anonymous_sec$ThreadID == 
                                             unique_threads_sec[i,],]
  # get unique authors
  authors_in_thread_sec = as.data.frame(unique(authors_in_thread_sec$AuthorID))
  # for each author, we loop list again to find the connected authors?
  for(j in 1: nrow(authors_in_thread_sec)){
    curr_author_sec = authors_in_thread_sec[j, ]
    for(k in j: nrow(authors_in_thread_sec)){
      if (curr_author_sec != authors_in_thread_sec[k, ]){
        adj_matrix_2011_11[as.character(curr_author_sec), 
                           as.character(authors_in_thread_sec[k, ])] = 1
      }
    }
  }
}

graph_df_sec = graph_from_adjacency_matrix(adj_matrix_2011_11, 
                                           mode = "undirected")
plot(graph_df_sec, layout = layout.auto, node.size = 0.5,
     main = "Social Network of November 2011")

deg = degree(graph_df_sec)
bet = betweenness(graph_df_sec)
clos = closeness(graph_df_sec)
dia = diameter(graph_df_sec)
avg = average.path.length(graph_df_sec)
EigF = evcent(graph_df_sec)$vector
results_sec = as.data.frame(rbind(deg, bet, clos, dia, avg, EigF))
results_sec = t(results_sec)
colnames(results_sec) = c("Degree", 
                          "Betweenness", 
                          "Closeness",
                          "Diameter",
                          "Average Path Length",
                          "Eigenvector Centrality")

top_10_degree = results_sec[order(-deg),]
top_10_degree[1:10, c("Degree") , drop = FALSE]

top_10_bet = results_sec[order(-bet),]
top_10_bet[1:10, c("Betweenness") , drop = FALSE]

top_10_clos = results_sec[order(-clos),]
top_10_clos[1:10, c("Closeness") , drop = FALSE]

top_10_EigF = results_sec[order(-EigF),]
top_10_EigF[1:10, c("Eigenvector Centrality") , drop = FALSE]
