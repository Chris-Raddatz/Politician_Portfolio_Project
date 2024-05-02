## Examine spike from 09/2021 to end of 2021
all_politician_trade_data%>%
  filter(year(txDate)==2021 & month(txDate)>=9)%>%
  group_by(politician.party)%>%
  summarize(total_value=sum(price*size, na.rm=TRUE),
            trade_count=n())


## Examine when trades spiked
party_trades_by_month<-all_politician_trade_data%>%
  filter(txDate>='2017-07-01' & txDate<='2022-11-30')%>%
  group_by(year(txDate),month(txDate),format(txDate,'%m/%Y'),politician.party)%>%
  summarize(total_value=sum(price*size, na.rm=TRUE),
            trade_count=n())%>%
  rename('monthyear'=3)

ggplot(party_trades_by_month, aes(x=my(monthyear),y=total_value,fill=politician.party))+
  geom_bar(stat='identity', position = 'dodge')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_x_date(breaks = scales::pretty_breaks(n = 15))+
  scale_fill_manual(values=c('blue','green','red'))+
  ylab('Total Value of Investments')+
  xlab('Month')


## Examine when industries spiked
all_politician_trade_data%>%
  group_by(year(txDate),month(txDate),format(txDate,'%m/%Y'),issuer.sector)%>%
  summarize(total_value=sum(price*size, na.rm=TRUE),
            trade_count=n())


## Examine what industries democrats bought in August 2021
all_politician_trade_data%>%
  filter(politician.party=='democrat' & year(txDate)==2021 & month(txDate)==8)%>%
  group_by(issuer.sector)%>%
  summarize(total_value=sum(price*size, na.rm=TRUE),
            trade_count=n())%>%
  arrange(desc(total_value))

