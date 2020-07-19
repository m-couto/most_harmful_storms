# Question 1:
# What types of events are most harmful wrt population health?

# The important columns here are:
# EVTYPE
# FATALITIES
# INJURIES

dim(data)
# 201284 x 7

sum(data$FATALITIES==0)
# 196326
# The overwhelming majority of events has no fatalities

sum(data$FATALITIES>=1 & data$FATALITIES<=10)
# 4889

fatal <- data$FATALITIES[data$FATALITIES>10]

max(fatal)
# 158

hist(fatal, xlim=c(10,160), breaks=15)
# A histogram of all other nr of fatalities

health <- data %>% select(EVTYPE, FATALITIES, INJURIES) %>% 
    filter(FATALITIES!=0 | INJURIES!=0) %>% 
    #    mutate(EVTYPE = factor(EVTYPE)) %>%
    group_by(EVTYPE) %>%
    summarize(fatalsum = sum(FATALITIES), injsum = sum(INJURIES))

length(unique(health$EVTYPE))
#View(health)

# Events with most fatalities

mostfatal <- health %>% select(EVTYPE, fatalsum) %>% arrange(desc(fatalsum))
mostfatal$EVTYPE <- factor(mostfatal$EVTYPE, levels=mostfatal$EVTYPE)
    # I must specify the levels to keep them in this descending order

op <- par(no.readonly = TRUE)   # the default
par(mar=c(11, 4, 4, 2) + .1)
barplot(mostfatal$fatalsum ~ mostfatal$EVTYPE, xlab='',
        ylab='Number of fatalities', main='Number of fatalities per event', las=2)
par(op)

head(mostfatal,20)

top20fatal <- health %>% select(EVTYPE, fatalsum) %>%
    arrange(desc(fatalsum)) %>% top_n(20)
    # takes the top 20 entries
top20fatal$EVTYPE <- factor(top20fatal$EVTYPE, levels=top20fatal$EVTYPE)

op <- par(no.readonly = TRUE)   # the default
par(mar=c(11, 4, 4, 2) + .1)
barplot(top20fatal$fatalsum ~ top20fatal$EVTYPE, xlab='', ylab='Number of fatalities',
        main='Top 20 storms with most number of fatalities', las=2)
par(op)

#head(mostfatal, 20)
#View(mostfatal[1:30,])


# Events with most number of injuries

mostinj <- health %>% select(EVTYPE, injsum) %>% arrange(desc(injsum))
mostinj$EVTYPE <- factor(mostinj$EVTYPE, levels=mostinj$EVTYPE)
# I must specify the levels to keep them in this descending order

op <- par(no.readonly = TRUE)   # the default
par(mar=c(11, 4, 4, 2) + .1, mgp=c(3,.5,0))
barplot(mostinj$injsum ~ mostinj$EVTYPE, xlab='',
        ylab='Number of injured people', main='Number of injured people per event', las=2)
par(op)

head(mostinj,20)

top20inj <- health %>% select(EVTYPE, injsum) %>% arrange(desc(injsum)) %>% top_n(20)
    # takes the top 20 entries
top20inj$EVTYPE <- factor(top20inj$EVTYPE, levels=top20inj$EVTYPE)

op <- par(no.readonly = TRUE)   # the default
par(mar=c(11, 4, 4, 2) + .1)
barplot(top20inj$injsum ~ top20inj$EVTYPE, xlab='',
        ylab='Number of injured people', main='Top 20 storms with most number of injured people', las=2)
par(op)


# The most harmful events

most <- intersect(mostfatal$EVTYPE[1:10], mostinj$EVTYPE[1:10])
    # 7 events are in the top 10 most fatalities
    # and the top 10 most injuries

health %>% filter(EVTYPE %in% most) %>% arrange(desc(fatalsum))






# Question 2:
# What types of events have greatest economic consequences?

# The important columns here are:
# EVTYPE
# PROPDMG, PROPDMGEXP
# CROPDMG, CROPDMGEXP


unique(data$PROPDMGEXP)
unique(data$CROPDMGEXP)



economy <- data %>% select(EVTYPE, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP) %>%
    filter(PROPDMG!=0 | CROPDMG!=0)

temp <- economy[economy$PROPDMGEXP=="",]
sum(temp$PROPDMG)

head(economy[economy$CROPDMGEXP=="",])

head(economy)

# Turn K/M/B to numbers

economy$PROPDMGEXP[economy$PROPDMGEXP == ""] <- "0"
map <- setNames(c(0,1E3,1E6,1E9), c("0","K","M","B"))
economy$PROPDMGEXP <- map[economy$PROPDMGEXP]

economy$CROPDMGEXP[economy$CROPDMGEXP == ""] <- "0"
map <- setNames(c(0,1E3,1E6,1E9), c("0","K","M","B"))
economy$CROPDMGEXP <- map[economy$CROPDMGEXP]

economy <- economy %>% mutate(EVTYPE = factor(EVTYPE)) %>% group_by(EVTYPE) %>%
    summarize(propsum = sum(PROPDMG * PROPDMGEXP),
              cropsum = sum(CROPDMG * CROPDMGEXP),
              total = propsum + cropsum)

head(economy)

# Events with most property damage

mostpropdmg <- economy %>% select(EVTYPE, propsum) %>% arrange(desc(propsum))
# mostpropdmg$EVTYPE <- factor(mostpropdmg$EVTYPE, levels = mostpropdmg$EVTYPE)
# I specify the levels to keep them in this descending order

op <- par(no.readonly = TRUE)   # the default
par(mar=c(11, 6, 4, 2) + .1, mgp=c(4.5,1,0))
barplot(mostpropdmg$propsum ~ mostpropdmg$EVTYPE, xlab='',
        ylab='Property Damage Cost (USD)', main='Property damage cost per storm', las=2)
par(op)

head(mostpropdmg,20)

mostpropdmg <- economy %>% select(EVTYPE, propsum) %>% arrange(desc(propsum)) %>% top_n(20)
# takes the top 20 entries
# mostpropdmg$EVTYPE <- factor(mostpropdmg$EVTYPE, levels=mostpropdmg$EVTYPE)

op <- par(no.readonly = TRUE)   # the default
par(mar=c(11, 6, 4, 2) + .1, mgp=c(4.5,1,0))
barplot(mostpropdmg$propsum ~ mostpropdmg$EVTYPE, xlab='',
        ylab='Property Damage Cost (USD)', main='Top 20 events with most property damage cost', las=2)
par(op)


# Events with most crop damage

mostcropdmg <- economy %>% select(EVTYPE, cropsum) %>% arrange(desc(cropsum))
# mostcropdmg$EVTYPE <- factor(mostcropdmg$EVTYPE, levels = mostcropdmg$EVTYPE)
# I must specify the levels to keep them in this descending order

op <- par(no.readonly = TRUE)   # the default
par(mar=c(11, 6, 4, 2) + .1, mgp=c(4.5,1,0))
barplot(mostcropdmg$cropsum ~ mostcropdmg$EVTYPE, xlab='',
        ylab='Crop Damage Cost (USD)', main='Crop damage cost per storm', las=2)
par(op)

head(mostpropdmg,20)

mostcropdmg <- economy %>% select(EVTYPE, cropsum) %>% arrange(desc(cropsum)) %>% top_n(20)
# takes the top 20 entries
# mostcropdmg$EVTYPE <- factor(mostcropdmg$EVTYPE, levels=mostcropdmg$EVTYPE)

op <- par(no.readonly = TRUE)   # the default
par(mar=c(11, 6, 4, 2) + .1, mgp=c(4.5,1,0))
barplot(mostcropdmg$cropsum ~ mostcropdmg$EVTYPE, xlab='',
        ylab='Crop Damage Cost (USD)', main='Top 20 storms with most crop damage cost', las=2)
par(op)


# The most economically devastating events

top <- intersect(mostpropdmg$EVTYPE[1:15], mostcropdmg$EVTYPE[1:15])
# 8 events are in the top 10 most fatalities
# and the top 10 most injuries

mostdmg <- economy %>% filter(EVTYPE %in% top) %>% arrange(desc(propsum))
mostdmg$EVTYPE <- factor(mostdmg$EVTYPE, levels = top)

op <- par(no.readonly = TRUE)   # the default
par(mar=c(11, 6, 4, 2) + .1, mgp=c(4.5,1,0))
barplot(mostdmg$total ~ mostdmg$EVTYPE, xlab='',
        ylab='Total Damage Cost (USD)', main='Damage cost per type of event', las=2)
par(op)









