# ----------------------------------------------------------------------------------------
# GROUP E - EXPLORATORY DATA ANALYSIS R SCRIPT
# ----------------------------------------------------------------------------------------

# ----------------------------------------------------------------------------------------
# Accomendation Analysis
# import libraries
library(data.table)
library(ggplot2)
library(gridExtra)
library(dplyr)


# import data
dt <- data.table(read.csv('madrid_transactions.csv'))
ct <- data.table(read.csv('continents.csv'))
# rename columns
colnames(ct) <- c('customer_country', 'language', 'continent')
# combine data
all <- merge(dt, ct, by='customer_country')
# dtype 
all$hour <- as.factor(all$hour)


# ----------------------------------------------------------------------------------------
## Graph 1 - Top Selling Categories

# Prepare data - group amount by category
category_plot <- aggregate(dt$amount, by=list(dt$category), FUN=sum)
colnames(category_plot) <- c("category", "sum")
category_plot <- category_plot[order(category_plot$sum), ]
category_plot$category <- factor(category_plot$category, levels = category_plot$category) #keep the order in plot
head(category_plot, 20)

# Create highligh column to tell ggplot which bar to highlight later
category_plot<- category_plot %>% mutate( highlight = ifelse( category == "Accommodation", "yes", "no" ) )

# Create tha Bar Chart
Graph1 <- ggplot(category_plot, aes(x=category, y=sum, fill= category_plot$highlight)) + 
  geom_bar(stat="identity", width=.5) + 
  labs(title="Categories", 
       subtitle="Top Selling Categories") + 
  coord_flip() +
  scale_fill_manual( values = c( "yes"="tomato", "no"="grey53" ), guide = FALSE ) +
  theme(axis.title= element_text(face="bold", size = 13), axis.text.y = element_text(face="bold")) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "Sum of value", labels = scales::comma)

Graph1

## Creating a dataset only for Accomodation
accommodation <- dt[ which( dt$category %in% "Accommodation") , ]
str(accommodation)

# ----------------------------------------------------------------------------------------
## Graph 2 - Industry Pulse 

# Percentage change of transactions per hour per category 
# find number of transactions per industry and hour
cat_trans_hour <- all[, list('transactions' = length(amount)),
                      by = c('category', 'hour')][order(hour)]

# calculate the % change in transaction per category per hour
cat_trans_hour[, trans_perc_change := lapply(hour, function(x) diff(transactions)/-length(transactions)), 
               by=c('category')]

# filter for Accommendation, Transport and food&beverage
sel_industry <- c('Accommodation', 'Transportation', 'Bars & restaurants')
sel_trans_hour <- cat_trans_hour[category %in% sel_industry, ]

### Plot number of transactions per category per hour (rate of change)
g1 <- ggplot(sel_trans_hour, aes(x=hour, y=trans_perc_change, group=category)) +
  geom_line(aes(color=category, size=category)) +
  # line size
  scale_size_manual(values = c(0.8, 0.6, 0.6)) +
  # axis-labs
  theme(axis.title = element_text(face="bold", size = 13)) +
  labs(title="Industry Pulse", 
       subtitle="Percentage change of transactions per hour per category",
       y='Transactions change %', x='Hour of Day') + 
  # y-axis to percentage
  scale_y_continuous(labels=scales::percent) +
  # Use brewer color palettes
  scale_color_brewer(palette="Dark2") + 
  # change legend poistion
  theme(legend.position="bottom")
# view plot  
g1


# ----------------------------------------------------------------------------------------
## Graph 3 - Most frequent visitors by the number of transactions

# Top 10 countries for Accommodation Industry with number of transactions

# Prepare the data - group countries by count of transactions
acc_country_count <- aggregate(accommodation$X, by=list(accommodation$customer_country), FUN=length)
colnames(acc_country_count) <- c("country", "transactions")
acc_country_count <- acc_country_count[order(-acc_country_count$transactions), ]
acc_country_count$country <- factor(acc_country_count$country, levels = acc_country_count$country)
acc_country_count <- head(acc_country_count, 10) #show only top 10

# Create a Bar Chart
Graph2 <- ggplot(acc_country_count, aes(x=country, y=transactions, fill = transactions)) + 
  geom_bar(stat="identity", width=.5) + 
  labs(title="Most frequent visitors", 
       subtitle="by the number of transactions") + 
  scale_fill_gradient(low="dodgerblue4", high="dodgerblue2") +
  theme(axis.title = element_text(face="bold", size = 13)) +
  scale_x_discrete(name = "Country") +
  scale_y_continuous(name = "Number of transactions")

Graph2


# ----------------------------------------------------------------------------------------
## Graph 4 - Most valuable visitors by the average value of transaction

# Top 10 profitable countries for Accommodation Industry - by average value

# Leaving only countries with at least 3 transactions or more - to avoid outliers on mean
accommodation2 <- accommodation %>% group_by(customer_country) %>% filter(n() > 3)

# Prepare the data - group countries by average value of transaction
acc_country_average <- aggregate(accommodation2$amount, by=list(accommodation2$customer_country), FUN=mean)
colnames(acc_country_average) <- c("country", "profitability")
acc_country_average <- acc_country_average[order(-acc_country_average$profitability), ]
acc_country_average$country <- factor(acc_country_average$country, levels = acc_country_average$country)
acc_country_average <- head(acc_country_average, 10) #show only top 10

# Create a Bar Chart
Graph3 <- ggplot(acc_country_average, aes(x=country, y=profitability, fill = profitability)) + 
  geom_bar(stat="identity", width=.5) + 
  labs(title="Most valuable visitors", 
       subtitle="by the average value of transaction") + 
  scale_fill_gradient(low="darkorange4", high="darkorange2") +
  theme(axis.title = element_text(face="bold", size = 13)) +
  scale_x_discrete(name = "Country") +
  scale_y_continuous(name = "Profitability")

Graph3


# ----------------------------------------------------------------------------------------
## Graph 5 - Service languages - revenue

# Ticket amount range for top 5 languages

## Data Filtering
# subset for accommodation cdata 
acc_data <- all[category == 'Accommodation',]
# find top languages by sum of transactions
top_lang_trans <- acc_data[, list('sum_trans' = length(amount)),
                           by = language][order(-sum_trans)]

# list top 3 languages as per transaction volume
top_lang_trans <- head(top_lang_trans$language, 3)
# subset acc data for desired languages
acc_top_lang_trans <- acc_data[language %in% top_lang_trans,  ]

### revenue
## total reve per language
tot_rev <- acc_data[, list('tot_rev' = sum(amount)),
                    by = language][order(-tot_rev)]
# subset top 10 names
tot_rev_lang <- head(tot_rev$language, 10)
# subset acc data for desired languages
acc_top_lang_tot_rev <- acc_data[language %in% tot_rev_lang,  ]

### add columns for sum of transcation by language
acc_data[, 'sum_trans' := length(amount), by = language]
### find total rev per langauge - min 10 trans (desired threshold)
avg_ticket <- acc_data[sum_trans > 10, list('med_rev' = median(amount)),
                       by = c('language')][order(-med_rev)]
# subset top 5 names
top_lang_rev <- head(avg_ticket$language, 5)
# subset acc data for desired languages
acc_top_lang_rev <- acc_data[language %in% top_lang_rev,  ]

# subset top 3 names
top_lang_rev_hour <- head(avg_ticket$language, 5)
# subset acc data for desired languages
acc_top_lang_rev_hour <- acc_data[language %in% top_lang_rev_hour,  ]


# plot - avg transaction amount 
g4 <- ggplot() + 
  # boxplot
  geom_boxplot(data=acc_top_lang_rev,
               aes(x=reorder(language, -amount, FUN = median), 
                   y=amount, group=language, fill=language), alpha=0.2) +
  # axis-labs
  labs(title="Service Languages - Revenue",
       subtitle="Ticket amount range for top 5 languges") + 
  xlab('Language') + ylab('Transaction Size') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(axis.title = element_text(face="bold", size = 13)) +
  # Use brewer color palettes
  scale_color_brewer(palette="Dark2") +
  theme(legend.position="none") 

g4


# ----------------------------------------------------------------------------------------
## Graph 6 - Service lingos - transactions

# Number of transactions per hour for high revenue languages

# plot nr transactions for high median ticket languages 
# -> who to hire and when they should work
g5 <- ggplot() +
  # freqplot
  geom_freqpoly(data=acc_top_lang_rev_hour,
                aes(x=hour, color=language, group=language),
                stat = 'count', size=0.8) +
  # axis-scale
  ylim(0, 20) +
  # axis-labs
  labs(title="Service Lingos - Transactions", 
       subtitle="Number of transactions per hour for high revenue languages") +
  xlab('Hour') + ylab('Number of Transactions') +
  theme(axis.title = element_text(face="bold", size = 13)) +
  # Use brewer color palettes
  scale_color_brewer(palette="Dark2") + theme(legend.position="none") + 
  # face grid
  facet_grid(language ~.)

g5


# ----------------------------------------------------------------------------------------
# non-used graphs 

# combine plots 
grid.arrange(g5, g4, ncol=2, nrow=1)

## Plotting
# plot - nr of transaction per top 3 trans volume languages per hour
g2 <- ggplot() + 
  # freqplot
  geom_freqpoly(data=acc_top_lang_trans,
                aes(x=hour, color=language, group=language),
                stat = 'count', size=0.8) +
  # axis-labs
  theme(axis.title = element_text(face="bold", size = 13)) +
  labs(title="Volume Languages - Transactions",
       subtitle="Number of transactions per hour for most common languages") + 
  xlab('Hour') + ylab('Number of Transactions') +
  # Use brewer color palettes
  scale_color_brewer(palette="Dark2") + 
  # legend
  theme(legend.position="bottom")

g2 

# plot - tot tranasaction amount 
g3 <- ggplot(data=head(tot_rev, 10),
             aes(x= reorder(language, -tot_rev, FUN = max),
                 y=tot_rev, fill=tot_rev)) +
  # barplot
  geom_bar(stat = 'identity', width=0.5) +
  # bar style
  scale_fill_gradient(low="dodgerblue4", high="dodgerblue2") +
  # axis-labs
  labs(title="Volume Languages - Revenue",
       subtitle="Total Revenue per top 10 languges") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab('Total Revenue') + xlab('Language') +
  theme(axis.title = element_text(face="bold", size = 13),
        # legend
        legend.position = 'None')
g3 

# combine volume langage plots 
grid.arrange(g2, g3, ncol=2, nrow=1)
