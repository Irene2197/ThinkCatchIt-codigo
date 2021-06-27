library(ggpubr)
dd$channelGrouping<-as.factor(dd$channelGrouping)

dd$channelGrouping = ifelse(dd$channelGrouping == "Email", "CRM", dd$channelGrouping)
dd$channelGrouping = ifelse(dd$channelGrouping == "Push", "CRM", dd$channelGrouping)
dd$channelGrouping = ifelse(dd$channelGrouping == "Organic Search", "IE", dd$channelGrouping)
dd$channelGrouping = ifelse(dd$channelGrouping == "Branded Paid Search", "IE", dd$channelGrouping)


ggboxplot(dd, x = "channelGrouping", y = "engagement_score",
          color = "channelGrouping", palette = "jco")+
  stat_compare_means(method = "anova")

dd2$Campaign.Name<-as.factor(dd2$Campaign.Name)
ggboxplot(dd2, x = "Campaign.Name", y = "engagement_score",
          color = "Campaign.Name")+
  stat_compare_means(method = "anova")


ggboxplot(dd[dd$Campaign.Name=="Verano 19",], x = "channelGrouping", y = "engagement_score",
          color = "channelGrouping", palette = "jco")
#+stat_compare_means(method = "anova")


library(sqldf)
dd2<-sqldf('SELECT * FROM dd WHERE "Campaign.Name" in ("Hallazgos","Houdinis","Kitchen Club","Navidad","Disneyland","Evento BF","Dias Cosmicos","Verano 19","Semana Santa 20","Pros Europa")')
  






