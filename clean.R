library(dplyr);
prop_exp <- as.vector(data$PROPDMGEXP)
crop_exp <- as.vector(data$CROPDMGEXP)

clean <- function(v) {
	
	for(i in 1:length(v)) {
		if (v[i] == "1") {
			v[i] <- "10";
		} else if (v[i] == "2" || v[i] == "H" || v[i] == "h") {
			v[i] <- "100";
		} else if (v[i] == "3" || v[i] == "K" || v[i] == "k") {
			v[i] <- "1000";
		} else if (v[i] == "4") {
			v[i] <- "10000";
		} else if (v[i] == "5") {
			v[i] <- "100000";
		} else if (v[i] == "6" || v[i] == "M" || v[i] == "m") {
			v[i] <- "1000000";
		} else if (v[i] == "7") {
			v[i] <- "10000000";
		} else if (v[i] == "8") {
			v[i] <- "100000000";
		} else if (v[i] == "B") {
			v[i] <- "1000000000";
		} else if (v[i] == "0"){
			v[i] <- "1";
		} else {
			v[i] <- NA;
		}
	}
	v
}
prop_exp <- as.integer(clean(prop_exp));
crop_exp <- as.integer(clean(crop_exp));

PROPDMG <- data$PROPDMG * prop_exp;
CROPDMG <- data$CROPDMG * crop_exp;

x <- as.data.frame(cbind(PROPDMG, CROPDMG));

y <- mutate(x, DMG = ifelse(is.na(PROPDMG) & is.na(CROPDMG), NA,
					   (ifelse(is.na(PROPDMG), 0, PROPDMG)) +
					   	(ifelse(is.na(CROPDMG), 0, CROPDMG))
));



DATA <- data[,c(7,8,23,24)];
DATA$PROPDMG <- y$PROPDMG;
DATA$CROPDMG <- y$CROPDMG;
DATA$DMG <- y$DMG;
write.csv(DATA, "clean.csv")

EV_FATALITIES <- arrange(aggregate(FATALITIES ~ EVTYPE, DATA, sum, na.rm=TRUE), desc(FATALITIES));
EV_INJURIES <- arrange(aggregate(INJURIES ~ EVTYPE, DATA, sum, na.rm=TRUE), desc(INJURIES));


EV_DMG <- arrange(aggregate(DMG ~ EVTYPE, DATA, sum, na.rm=TRUE), desc(DMG));
EV_PROPDMG <- arrange(aggregate(PROPDMG ~ EVTYPE, DATA, sum, na.rm=TRUE), desc(PROPDMG));
EV_CROPDMG <- arrange(aggregate(CROPDMG ~ EVTYPE, DATA, sum, na.rm=TRUE), desc(CROPDMG));

plot_fatalities <- as.data.frame(head(EV_FATALITIES));

ggplot(data = plot_fatalities, aes(x = plot_fatalities$EVTYPE, y = plot_fatalities$FATALITIES)) + geom_bar(stat = "identity") + 
	theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("Event Type") + 
	ylab("Economic Damage ($)") + ggtitle("NOAA Top 6: Highest Damage (Property + Crop), 1950-2011")

plot_injuries <- as.data.frame(head(EV_INJURIES))

ggplot(data = plot_injuries, aes(x = plot_injuries$EVTYPE, y = plot_injuries$INJURIES)) + geom_bar(stat = "identity") + 
	theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("Event Type") + 
	ylab("# of Injuries") + ggtitle("NOAA Top 6: Highest Injury Counts, 1950-2011")

plot_dmg <- as.data.frame(head(EV_DMG))

ggplot(data = plot_dmg, aes(x = plot_dmg$EVTYPE, y = plot_dmg$DMG)) + geom_bar(stat = "identity") + 
	theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("Event Type") + 
	ylab("Economic Damage ($)") + ggtitle("NOAA Top 6: Highest Damage (Property + Crop), 1950-2011")
