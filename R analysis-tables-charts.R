# import packages
library(tidyverse)
library(gt)
library(extrafont)
library(ggbeeswarm)
library(gghighlight)

# distribution and summary figures -----------------

# colour palettes
PolPalette <- c("#ffd914","#000aff","#ad6300", "#5bc200","#e60015", "#ffc42e","#159900", "#ffe329", "#32784d", "#0ac700", "#000000")

# importing data
MPsSummary1 <- read.csv("MPNodeListPArties1.csv", header = T, sep = ",", strip.white = T)
MPsSummary1 <- as_tibble(MPsSummary1)

# changes indy designation to most recent party
MPsSummary1[MPsSummary1$Name=="Jeremy Corbyn", "Party"] <- "Labour"
MPsSummary1[MPsSummary1$Name=="Margaret Ferrier", "Party"] <- "Conservative"
MPsSummary1[MPsSummary1$Name=="Claudia Webbe", "Party"] <- "Labour"
# corrects Sinn Fein wording - but doesnt have the accent
MPsSummary1$Party[MPsSummary1$Party=="Sinn FÃ©in"] <- "Sinn Fein"
# change na to zero
MPsSummary1[is.na(MPsSummary1)] <- 0

# selects the top 25 mps by mp followers and only shows a few columns
MPsSummary1Top25MPFollowers <- MPsSummary1 %>% 
  arrange(desc(MPFollowers)) %>% 
  slice(1:25) %>% 
  select(Name, id, Party, Constituency, MPFollowers, followed_followers, followed_status_count, followed_like_count) %>% 
  rename("Screen Name"="id", "MP Followers" = "MPFollowers", "Total Followers" = "followed_followers", "Total Statuses" = "followed_status_count", "Total Likes" = "followed_like_count") %>% 
  select(Name, Party, Constituency, "MP Followers", "Total Followers")

# summary table of top 25 mps
MPTopMPFollowersTable25 <- gt(data = MPsSummary1Top25MPFollowers) %>% 
  tab_header(title = "MPs followed by the most other MPs", subtitle = "Top 25, Feb 2021") %>% 
  tab_source_note("Source: Data from Twitter API, own analysis") %>% 
  data_color(columns = vars("MP Followers"), colors = scales::col_numeric(palette = c("#FDD3E7", "#f72585"), domain = NULL)) %>%
  data_color(columns = vars("Total Followers"), colors = scales::col_numeric(palette = c("#DBF4FC", "#4cc9f0"), domain = NULL)) %>%
  tab_options(table.font.names = "Open Sans", data_row.padding = px(2.5), table.width = pct(95),
              row.striping.include_table_body =FALSE,
              table.border.top.color = "white",
              table.border.bottom.color = "white",
              table.margin.left = pct(3),
              table.margin.right = pct(3)) %>% 
  tab_style(style = list(cell_text(font = "Open Sans", weight = "bold")),locations = list(cells_title(groups = "title"))) %>% 
  tab_style(style = list(cell_text(font = "Open Sans", weight = "bold")),locations = list(cells_column_labels(gt::everything()))) 
# view table
MPTopMPFollowersTable25
# export table
gtsave(MPTopMPFollowersTable25, filename = "MPTopFollowersTable25.png", expand = 30) 


# summary by party - median and IQR due to distribution being non-normal.
PartySummarybyFollowersMed <- MPsSummary1 %>% 
  group_by(Party) %>% 
  summarise(`Number of MPs` = n(),
            `Median MP Followers` = median(MPFollowers),
            `Median Total Followers` = median(followed_followers),
            `IQR MP Followers` = IQR(MPFollowers), 
            `MP With Most MP Followers` = Name[which(MPFollowers == max(MPFollowers))],
            `MP Followers` = max(MPFollowers),
            `MP With Most Total Followers` = Name[which(followed_followers == max(followed_followers))],
            `Total Followers` = max(followed_followers)) %>% 
  arrange(desc(`Number of MPs`))

# round values for table
PartySummarybyFollowersMed[c(3,4,5)] <- round(PartySummarybyFollowersMed[c(3,4,5)], 0)

# party summary table
PartySummaryTableMed <- gt(data = PartySummarybyFollowersMed) %>% 
  tab_header(title = "Summary of UK political parties' twitter followings", subtitle = "Feb 2021") %>% 
  tab_source_note("Source: Data from Twitter API, own analysis") %>% 
  data_color(columns = vars("MP Followers"), colors = scales::col_numeric(palette = c("#FDD3E7", "#f72585"), domain = NULL)) %>%
  data_color(columns = vars("Total Followers"), colors = scales::col_numeric(palette = c("#DBF4FC", "#4cc9f0"), domain = NULL)) %>%
  data_color(columns = vars("Median MP Followers"), colors = scales::col_numeric(palette = c("#FDD3E7", "#f72585"), domain = NULL)) %>%
  data_color(columns = vars("Median Total Followers"), colors = scales::col_numeric(palette = c("#DBF4FC", "#4cc9f0"), domain = NULL)) %>%
  tab_options(table.font.names = "Open Sans", data_row.padding = px(2.5), table.width = pct(95),
              row.striping.include_table_body =FALSE,
              table.border.top.color = "white",
              table.border.bottom.color = "white",
              table.margin.left = pct(3),
              table.margin.right = pct(3)) %>% 
  tab_style(style = list(cell_text(font = "Open Sans", weight = "bold")),locations = list(cells_title(groups = "title"))) %>% 
  tab_style(style = list(cell_text(font = "Open Sans", weight = "bold")),locations = list(cells_column_labels(gt::everything()))) 

# export table
gtsave(PartySummaryTableMed, filename = "PartySummaryTableMed.png", expand = 30) 

# beeswarm plot of distribution of mps by MP followers
Beeswarm1 <- MPsSummary1 %>% 
  ggplot(aes(x = MPFollowers, y= Party, colour = Party)) +
  geom_beeswarm(alpha = 1, groupOnX = F)+
  annotate(geom = "curve", x = 175, y = 7, xend = 153, yend = 5.8, 
           curvature = .2, arrow = arrow(length = unit(2, "mm")))+
  annotate(geom = "text", x = 225, y = 7, label = "Each point represents an MP \nand points are positioned using \nthe beeswarm algorithm \nto avoid overlaps", family = "Open Sans", size= 3) +
  scale_colour_manual(values = PolPalette) +
  scale_fill_manual(values = PolPalette) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title.position = "plot",
        panel.grid.minor =element_blank(),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(family = "Open Sans", hjust = 0.5, face = "bold"), 
        plot.subtitle = element_text(family = "Open Sans", hjust = 0.5), 
        axis.title = element_text(family = "Open Sans", face = "bold"),
        legend.title = element_text(family = "Open Sans", face = "bold"),
        text = element_text(family = "Open Sans"),
        legend.text = element_text(family = "Open Sans"),
        plot.caption = element_text(family = "Open Sans"),
        plot.margin = unit( c(0.25,0.25,0.25,0.25), units = "in")) +
  labs(title = "Distribution of MPs by Number of MP Followers", subtitle = "UK MPs, Feb 2021, by party", x = "MP Followers", y = "", caption = "Source: Twitter API, own analysis") 

ggsave(Beeswarm1, filename = "MPFollowersBeeswarm2.png", dpi = 400,
       width = 10, height = 8, units = "in", type = "cairo")

# scatterplot graph of total followers and MP followers
# with gghighlight facets
MPFollowersPointGraphFacet <- MPsSummary1 %>% ggplot(aes(x = MPFollowers, y = followed_followers)) +
  geom_point(aes(colour = Party), size = 1.2, alpha = 0.50) +
  # geom_smooth(method = lm, colour = "grey25", se = T) +
  scale_color_manual(values = PolPalette) +
  scale_y_continuous(trans = "log", labels = scales::label_number(accuracy = 1), breaks = c(0, 3000, 30000, 300000, 3000000)) +
  scale_x_continuous() +
  theme_minimal() +
  theme(legend.position = "bottom",
        #legend.position = c(0.85,0.25),
        panel.grid.minor =element_blank(),
        plot.title = element_text(family = "Open Sans", hjust = 0.5, face = "bold"), 
        plot.subtitle = element_text(family = "Open Sans", hjust = 0.5), 
        axis.title = element_text(family = "Open Sans", face = "bold"),
        legend.title = element_text(family = "Open Sans", face = "bold"),
        text = element_text(family = "Open Sans"),
        legend.text = element_text(family = "Open Sans"),
        plot.caption = element_text(family = "Open Sans"),
        plot.margin = unit( c(0.25,0.25,0.25,0.25), units = "in")) +
  labs(title = "Relationship between total followers and followers who are MPs", subtitle = "UK MPs, Feb 2021, by party", x = "MP Followers", y = "Total Followers", caption = "Source: Twitter API, own analysis") +
  gghighlight() +
  facet_wrap(~Party)

# export chart
ggsave(MPFollowersPointGraphFacet, filename = "MPFollowersPointGraphFacet3.png", dpi = 400,
       width = 10, height = 8, units = "in", type = "cairo")

# insularity of parties -------------------------------

# import edge list data (data of the individual relations between mps and other twitter users)
MPsEdges <- read.csv("MPfollowingUSERSOnlyMPS.csv",header = T, sep = ",", strip.white = T)
MPsEdges <- as_tibble(MPsEdges)
# selection only source and target variables
MPsEdges2 <- MPsEdges %>% 
  select(Source, Target)

#need to join by party of both source and target to find most insular MPs and their party relations.

# get sourceIDs and TargetIDs
SourceIDs <- MPsSummary1 %>% 
  select(id, Name, Party) %>% 
  rename(Source = id, SourceName = Name, SourceParty = Party)

TargetIDs <- MPsSummary1 %>% 
  select(id, Name, Party) %>% 
  rename(Target = id, TargetName = Name, TargetParty = Party)

# join
MPsEdges3 <- MPsEdges2 %>% inner_join(SourceIDs) %>% inner_join(TargetIDs)

# calculate following counts between parties
FollowingbyParty <- MPsEdges3 %>% 
  group_by(TargetParty, SourceParty) %>% 
  count(Source) %>% 
  ungroup() %>% 
  spread(key = TargetParty, value = n) %>% 
  replace(is.na(.), 0) %>% 
  mutate(TotalFollowing = rowSums(.[3:13])) %>% 
  gather('Alliance Party of Northern Ireland':'Speaker', key = TargetParty, value = n) %>% 
  mutate(PercTotalFollowing = n/TotalFollowing)

# view this
view(FollowingbyParty %>% 
       filter(SourceParty == TargetParty) %>% 
       arrange(desc(PercTotalFollowing)) )

# table of party insularity by % of MPs followed from own party.
PartyAveragePercFollow <- FollowingbyParty %>% 
  filter(SourceParty == TargetParty) %>% 
  arrange(desc(PercTotalFollowing)) %>% 
  group_by(SourceParty) %>% 
  summarise(MedianPercTotalFollowingOwnParty = median(PercTotalFollowing), IQRTotalFollowingOwnParty = IQR(PercTotalFollowing)) %>% 
  arrange(desc(MedianPercTotalFollowingOwnParty)) %>% 
  inner_join(PartySummaryTable1, by = c("SourceParty" = "Party")) %>% 
  select(SourceParty, MedianPercTotalFollowingOwnParty,IQRTotalFollowingOwnParty ,"Number of MPs") %>% 
  rename(Party = SourceParty, "Median % of MPs followed from own party" = MedianPercTotalFollowingOwnParty, "IQR of % of MPs followed from own party" = IQRTotalFollowingOwnParty) %>% 
  gt() %>% 
  tab_header(title = "Parties by Insularity of Following Relations", subtitle = "Feb 2021") %>% 
  tab_source_note("Source: Data from Twitter API, own analysis") %>% 
  data_color(columns = vars("Median % of MPs followed from own party"), colors = scales::col_numeric(palette = c("#FDD3E7", "#f72585"), domain = NULL)) %>%
  data_color(columns = vars("IQR of % of MPs followed from own party"), colors = scales::col_numeric(palette = c("#DBF4FC", "#4cc9f0"), domain = NULL, na.color = "white")) %>%
  tab_options(table.font.names = "Open Sans", data_row.padding = px(2.5), table.width = pct(95),
              row.striping.include_table_body =FALSE,
              table.border.top.color = "white",
              table.border.bottom.color = "white",
              table.margin.left = pct(3),
              table.margin.right = pct(3)) %>% 
  tab_style(style = list(cell_text(font = "Open Sans", weight = "bold")),locations = list(cells_title(groups = "title"))) %>% 
  tab_style(style = list(cell_text(font = "Open Sans", weight = "bold")),locations = list(cells_column_labels(gt::everything()))) %>% 
  fmt_percent(columns = c(2,3), decimals = 1)

# export 
gtsave(PartyAveragePercFollow, filename = "PartyAveragePercFollow2.png", expand = 30) 

# tile chart showing a matrix of parties by following relations to show minor parties more.
MPFollowingByParty <- MPsEdges3 %>% 
  group_by(SourceParty, TargetParty,) %>% 
  count() %>% 
  ungroup() %>% 
  complete(SourceParty, TargetParty, fill = list(n=0)) %>% 
  mutate(WrappedTargetParty = str_wrap(TargetParty, width = 10)) %>% 
  mutate(WrappedSourceParty = str_wrap(SourceParty, width = 10)) %>% 
  ggplot() +
  geom_tile(aes(y= WrappedSourceParty, x= WrappedTargetParty, fill = n)) +
  geom_text(aes(y= WrappedSourceParty, x = WrappedTargetParty, label = n), family = "Open Sans") +
  #scale_fill_gradient(low = "#DBF4FC",high = "#4cc9f0", trans = "pseudo_log") +
  scale_fill_gradient(low = "#DBF4FC",high = "#4cc9f0", trans = "log10", na.value = "white") +
  theme_minimal() +
  scale_y_discrete(limits = rev) +
  theme(panel.grid = element_blank(),
        plot.title = element_text(family = "Open Sans", face = "bold",hjust = 0.5), 
        plot.subtitle = element_text(family = "Open Sans", hjust = 0.5, ), 
        text = element_text(family = "Open Sans"),
        axis.title = element_text(family = "Open Sans", face = "bold"), 
        plot.caption = element_text(family = "Open Sans", face = "italic"),
        plot.margin = unit( c(0.25,0.25,0.25,0.25), units = "in"),
        legend.position = "right") +
  labs(title = "Number of Following Relationships Between Parties", subtitle = "UK MPs, Feb 2021", 
       x = "Followed (Target) Party", y = "Following (Source) Party",
       caption = "Source: Twitter API, own analysis",
       fill = "Count")

# export
ggsave(MPFollowingByParty, filename = "MPFollowingByParty1.png", dpi = 400,
       width = 10, height = 8, units = "in", type = "cairo")


