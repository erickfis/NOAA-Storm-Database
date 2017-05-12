# NOAA Storm Database - worst cases


*erickfis, 2017, May, 12th*

In this study we have analysed the NOAA Storm Database in order to
determine what are the worst natural catastrophic events, both in terms
of public health and in economic impact.

The U.S. National Oceanic and Atmospheric Administration's (NOAA) storm
database tracks characteristics of major storms and weather events in
the United States, including when and where they occur, as well as
estimates of any fatalities, injuries, and property damage.

The database currently contains data from January 1950 to January 2017,
as entered by NOAA's National Weather Service (NWS).

The database can be found on:

<https://www.ncdc.noaa.gov/stormevents/ftp.jsp>

RPubs version: <http://rpubs.com/erickfis/noaa>

GitHub version, with code included and pdf version:
<https://github.com/erickfis/NOAA-Storm-Database>

# Summary

-   [Introduction](#introduction)
-   [Objective](#objective)
-   [Methods](#methods)
-   [Data Processing](#data-processing)
-   [Human health: the most harmfull
    events](#human-health-the-most-harmfull-events)
    -   [Fatal Occurrences](#fatal-occurrences)
        -   [Most fatal in a single
            occurrence](#most-fatal-in-a-single-occurrence)
        -   [Most fatal in all time](#most-fatal-in-all-time)
        -   [Least fatal events](#least-fatal-events)
    -   [Injuring Occurrences](#injuring-occurrences)
        -   [Most injuring in a single
            occurrence](#most-injuring-in-a-single-occurrence)
        -   [Most injuring in all time](#most-injuring-in-all-time)
        -   [Least injuring events](#least-injuring-events)
-   [Economy: the the most harmfull
    events](#economy-the-the-most-harmfull-events)
    -   [Property losses](#property-losses)
        -   [Most Property Damaging event in a single
            occurrence](#most-property-damaging-event-in-a-single-occurrence)
        -   [Most Property Damaging event in all
            time](#most-property-damaging-event-in-all-time)
        -   [Least property damaging
            events](#least-property-damaging-events)
    -   [Crop losses](#crop-losses)
        -   [Most Crop Damaging event in a single
            occurrence](#most-crop-damaging-event-in-a-single-occurrence)
        -   [Most Crop Damaging event in all
            time](#most-crop-damaging-event-in-all-time)
        -   [Least crops damaging events](#least-crops-damaging-events)
-   [Most aflicted locations](#most-aflicted-locations)
    -   [Worst fatality count](#worst-fatality-count)
    -   [Worst injuries count](#worst-injuries-count)
    -   [Worst property losses](#worst-property-losses)
    -   [Worst crops losses](#worst-crops-losses)
-   [Results](#results)
    -   [Population Health](#population-health)
    -   [Economic Damages](#economic-damages)
    -   [Most aflicted locations](#most-aflicted-locations-1)
    -   [Distribution of data](#distribution-of-data)



Objective
=========

The goal of this study is to answer the questions:

1.  Across the United States, which types of events were the most
    harmful with respect to population health ever recorded in a single
    occurrence?

2.  Which types of events caused most harm to population health along
    all those years?

3.  Which types of events had the greatest economic consequences in a
    single occurrence?

4.  Which types of events had the greatest economic consequences along
    all those years?

5.  Which were the places that were subject to the greatest losses, both
    in terms of human health and economic losses.

Methods
=======

To answer each one of those questions, we did a very simple
**descriptive analysis** of data.

We used R tools to filter, sort and combine data, so we could get the
total sum of fatalities, injuries and economic losses.

Data Processing
===============

In order to answer our questions, the original database needed to be
treated from its raw form to a more useful format.

    library(RCurl)
    library(scales)
    library(stringr)
    library(data.table)
    library(chron)
    library(dplyr)
    library(lubridate)
    library(ggplot2)
    library(rmarkdown)
    library(RColorBrewer)
    library(gridExtra)
    library(grid)

    # looks at ftp repository listing files
    url <- "ftp://ftp.ncdc.noaa.gov/pub/data/swdi/stormevents/csvfiles/"
    arquivos.ftp <- getURL(url, ftp.use.epsv = FALSE,dirlistonly = TRUE) 

    arquivos.ftp <- paste(strsplit(arquivos.ftp, "\n")[[1]], sep = "")
    arquivos.ftp <- arquivos.ftp[grep("details", arquivos.ftp)]


    # compares ftp repo to files already downloaded
    arquivos.local <- dir("data/")
    arquivos.faltando <- arquivos.ftp[which(!(arquivos.ftp %in% arquivos.local))]

    baixa.arquivos <- arquivos.faltando # for in line R, so when I see this print on report I will let the triggers do their job.

    # arquivos.faltando <- NULL # I'm deleting those files now to save space and don't wanna do this check automaticaly so soon

    tem.novidade <- 0 # start the trigger

    # if the lists of files are different, download new ones 
    # and set the trigger for new raw processing

    if(length(arquivos.faltando)>0) {
            tem.novidade <- 1 # set trigger
            arquivo <- character()
                    for (i in 1:length(arquivos.faltando)) {
                            arquivo <- paste0(url, arquivos.faltando[i])
                            download.file(arquivo, 
                                    paste0("data/", arquivos.faltando[i]),
                                    method = "curl")
                    }
    }        

    # check if processed data is present:

    tem.rds <- grep("rds", arquivos.local)
    # if trigger is set, raw process again
    if(tem.novidade==1|length(tem.rds)==0) { # them process the data
            arquivos.local <- dir("data/")

            dados <- fread(sprintf("gzip -dc %s | tr -d '\\000'",
                                    paste0("data/", arquivos.local[1])),
                            na.strings = "")

            for (i in 2:length(arquivos.local)) {
                    dados <- bind_rows(
                            dados, fread(sprintf(
                                    "gzip -dc %s | tr -d '\\000'",
                                    paste0("data/", arquivos.local[i]),
                                    na.strings = "")
                                    )
                            )
            }
            
    # treating var names
            names(dados) <- gsub("_", ".", tolower(names(dados)))


    # treat vars
    # treating event types
            trata.eventos <- function(eventos) {
                    eventos <- tolower(as.character(eventos))
                    # first, need to see what are the event types
                    # unique(eventos)
                    # contagem <- sort(table(eventos))
                    
                    # them we create this list of terms
                    
                    lista.search <- c(
                            "dry",        
                            "fog",        
                            "wind",
                            "winter",
                            "slide",
                            "snow",
                            "flood",
                            "fld",
                            "cold|freez",
                            "hurricane",
                            "tornado",
                            "rain|precip",
                            "hail",
                            "heat|warm",
                            "tide",
                            "storm",
                            "record",
                            "blizzard",
                            "fire",
                            "funnel",
                            "surf")
                    
                    lista.replace <- c(
                            "drought",
                            "fog",        
                            "wind",
                            "winter",
                            "slide",
                            "snow",
                            "flood",
                            "flood",
                            "cold",
                            "hurricane",
                            "tornado",
                            "rain",
                            "hail",
                            "heat",
                            "tide",
                            "storm",
                            "record temperature",
                            "blizzard",
                            "fire",
                            "funnel",
                            "surf")
                    
                    
                    for (i in 1:length(lista.search)) {
                            eventos[grepl(lista.search[i], eventos)] <- lista.replace[i]
                            
                    }
                    
                    # lets group the events whose count is < 5 and call it "other"
                    contagem <- sort(table(eventos))
                    outros <- names(contagem[contagem<5])
                    eventos[eventos %in% outros] <- "other"
                    eventos
            }

    # treating dates types
            arruma.data <- function(dia) {
            #adicionar prefixo 19 se menor que 99, 20 se > 99
            # 20## if year < 50, 19 if not
                    condicao <- as.numeric(substr(dia,8,9))<50

                    dia[condicao] <- paste0(
                            # day-month
                            substr(dia[condicao],1,7),
                            #full year
                            paste0("20", substr(dia[condicao],8,9)),
                            # hours
                            substr(dia[condicao],10,
                                    nchar(dia[condicao])) 
                            )

                    dia[!condicao] <- paste0(
                            # day-month
                            substr(dia[!condicao],1,7),
                            #full year
                            paste0("19", substr(dia[!condicao],8,9)), 
                            # hours
                            substr(dia[!condicao],10,
                                    nchar(dia[!condicao])) 
                            )
                    dia
            }

    # Treating County Names
            treat.local <- function(cidades) {
                    cidades <- tolower(str_trim(cidades))
                    cidades <- gsub("_| |-", ".", cidades)
                    
                    # remove valley, coastal
                    cidades <-
                    gsub("valley|coastal|county", "", cidades)
                    
                    # remove ".."
                    # cidades[grep("\\.\\.$", cidades)] <-
                    #         gsub("\\.\\.", "", cidades[grep("\\.\\.$", cidades)])
                    cidades[grep("\\.+", cidades)] <-
                            gsub("\\.+", ".", cidades[grep("\\.+", cidades)])
                    cidades[grep("\\.$+", cidades)] <-
                            gsub("\\.+", "", cidades[grep("\\.$+", cidades)])
                    
                    # remove "> ( ,"
                    cidades <-
                            sapply(strsplit(cidades, split = ">", fixed = TRUE),
                                    function(x) (x[1]))
                    cidades <-
                            sapply(strsplit(cidades, split = "(", fixed = TRUE),
                                    function(x) (x[1]))
                    cidades <-
                            sapply(strsplit(cidades, split = ",", fixed = TRUE),
                                    function(x) (x[1]))
                    cidades
            }

    # fix exp data
            eval.dmg <- function(dmg) {
            # check for unique exp chars
            # exp <- str_extract(str_trim(harm.df$damage.crops), "[aA-zZ]+")
            # exp.unicos <- unique(exp)
            # exp <- str_extract(str_trim(harm.df$damage.property), "[aA-zZ]+")
            # exp.unicos <- unique(exp)  
                    # prepare exp, for 10^exp        
                    # remove white space and extract chars
                    exp <- str_extract(str_trim(dmg), "[aA-zZ]+")
                    exp[is.na(exp)] <- as.character(0) # NAs get 0
                    exp[which(exp %in% c("+","?", "-"))] <- as.character(0)
                    exp[which(exp %in% c("H","h"))] <- as.character(2)
                    exp[which(exp %in% c("K","k"))] <- as.character(3)
                    exp[which(exp %in% c("M","m"))] <- as.character(6)
                    exp[which(exp %in% c("B","b"))] <- as.character(9)
                    exp[which(exp %in% c("T","t"))] <- as.character(12)
                    exp <- as.numeric(exp)
                    
                    # prepare numbers for number*10^exp
                    number <- strsplit(dmg, split="[aA-zZ]", fixed=FALSE)
                    number <- sapply(number, function(x) (x[1]))
                    number <- as.numeric(number)
                    
                    # now evaluate them all
                    dmg <- number*10^exp
                    dmg
            }
            
            dados$begin.date.time <- arruma.data(dados$begin.date.time)
            dados$end.date.time <- arruma.data(dados$end.date.time)

            harm.df <- dados %>% 
                    transmute(
                            event = trata.eventos(event.type),
                            magnitude,
                            day = as.Date(dmy_hms(begin.date.time,
                                            locale = "en_US.utf8"),
                                    "%m/%d/%Y"),
                            duration = as.period(interval(
                                            dmy_hms(end.date.time, 
                                                    locale = "en_US.utf8"),
                                            dmy_hms(begin.date.time, 
                                                    locale = "en_US.utf8")
                                                    )
                                            ),
                            state = treat.local(state), county = treat.local(cz.name),
                            fatalities = as.numeric(deaths.direct) + 
                                    as.numeric(deaths.indirect),
                            injuries = as.numeric(injuries.direct) + 
                                    as.numeric(injuries.indirect),
                            prop.ev = eval.dmg(damage.property),
                            crop.ev = eval.dmg(damage.crops)
                    ) 

            rm(dados) # house cleanning
            
            # end of processing - save the data
            saveRDS(harm.df, "data/harm.rds")

    } else { # just loads processed data
            harm.df <- readRDS("data/harm.rds")
    }

<!-- ### New files to download and process data again: -->
<!--          -->
The necessary transformations were:

-   sanitized var names
-   evaluated duration of events, however they are not useful
-   evaluated damages values according to multipliers provided
-   sanitized and grouped similar events: strong snow, heavy snow and
    light snow all became just "snow"
-   sanitized county names

This database has 1419673 observations. Each observation corresponds to
an event occurrence.

To determine the most harmful events to human health, we checked the
variables related to human health, which are "fatalities" and
"injuries".

To determine the most harmful events to economy, we checked the
variables related to economic measures, from "propdmg" through
"cropdmgexp".

Also, in order to analyse various occurrences of the same event, we
measured the duration of the event, its magnitude and where the event
occurred (state and county name).

This is a really big database whose data has been being registered by a
lot of different people since 1950. Thus, as expected, there are
variations on how people registered events.

For example, the string "snow" was used to register a lot of events.
They are the same type of event, but count as different:

This is why we decided to filter those events: we grouped them by its
common strings.

Human health: the most harmfull events
======================================

We have determined what events did more harm to human health.

There were occurrences that caused zero fatalities but a lot of
injuries. The inverse is also true, so we did a separate analysis to
fatal and non-fatal events.

Fatal Occurrences
-----------------

### Most fatal in a single occurrence

Most fatal in a single occurrence

In order to determine what were the most fatal events in a single
occurrence, we need to see how fatalities are distributed along the
occurrences.

    # subset original data
    fatal.df <- harm.df %>% 
                    filter(!is.na(fatalities)) %>%
                    select(1:7)

      
    # quantiles
    qt <- quantile(fatal.df$fatalities, probs=seq(.9,1,0.001))

    # distribution plot

    plt.distr.fatal0 <- ggplot(fatal.df, aes(fatalities))

    plt.distr.fatal0 <- plt.distr.fatal0 + geom_density(aes(y=..scaled..)) + xlim(0,.5) + 
            labs(title="All events") +
            theme(plot.title = element_text(hjust = 0.5))

    # display only the qts next to fatal events
    #qt[(length(qt)-(length(qt[qt>=1])+1)): length(qt)]

Looking at this distribution, we can infer that the vast majority of
those occurrences were not fatal at all: **99.2% occurrences didn't
caused any fatalities.**

On the other hand, fatal occurrences had to have at least 1 fatality.

Now, among the fatal occurrences, we are interested in the ones whose
fatalities are beyond the confidence interval, ie. above 99% of the most
common values.

    # subset for fatal events
    fatal.df <- filter(fatal.df, fatalities > 0) %>%
                    arrange(desc(fatalities)) %>%
                    mutate(mean = mean(fatalities), 
                    median = median(fatalities),
                    rank = seq_len(length(event)))


    # quantiles, same as 
    # poisson.test(mean, conf.level = 0.95)

    qt <- quantile(fatal.df$fatalities, probs=seq(.999,1,0.005))
    #qt

Looking at this distribution, we can infer that **99.8% of the fatal
occurrences caused up to 57.04 fattalities**.

    # distribution plot
     
    plt.distr.fatal1 <- ggplot(fatal.df, aes(fatalities))

    plt.distr.fatal1 <- plt.distr.fatal1 + geom_density(aes(y=..scaled..)) + 
            xlim(0,(qt[1]/10)) + 
            labs(title="Fatal events") +
            theme(plot.title = element_text(hjust = 0.5))

    grid.arrange(plt.distr.fatal0, plt.distr.fatal1, nrow=1, ncol=2)
    grid.rect(gp=gpar(fill=NA))

![Population distribution for fatalities /
occurrences](readme_files/figure-markdown_strict/fatal-distr-4-1.png)

In this study, we looked on the 1% deadliest occurrences.

    # subset for 99% CI
    fatal95.df <- fatal.df %>% filter(fatalities>qt[1])
                    
    # create color pallete for all events
    colourCount.fatal.single = length(unique(fatal95.df$event))
    getPalette = colorRampPalette(brewer.pal(colourCount.fatal.single, "Set1"))


    # prepare text for inline R
    worst.fatal.single.ev <- fatal95.df$event[1]
    worst.fatal.single.st <- fatal95.df$state[1]
    worst.fatal.single.ct <- fatal95.df$county[1]
    worst.fatal.single.dt <- fatal95.df$day[1]
    worst.fatal.single.kill <- fatal95.df$fatalities[1]
    worst.fatal.single.mean <- round(fatal95.df$mean[1],2)
    worst.fatal.single.median <- round(fatal95.df$median[1],2)

    # print a table

    kable(fatal95.df[, c(10,1,3,5:7)], 
          caption=paste("Worst fatal occurrences, mean = ",
                        worst.fatal.single.mean, " and median = ",
                        worst.fatal.single.median))

<table>
<caption>Worst fatal occurrences, mean = 1.9 and median = 1</caption>
<thead>
<tr class="header">
<th align="right">rank</th>
<th align="left">event</th>
<th align="left">day</th>
<th align="left">state</th>
<th align="left">county</th>
<th align="right">fatalities</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="right">1</td>
<td align="left">hurricane</td>
<td align="left">2005-08-28</td>
<td align="left">louisiana</td>
<td align="left">orleans</td>
<td align="right">638</td>
</tr>
<tr class="even">
<td align="right">2</td>
<td align="left">tornado</td>
<td align="left">2011-05-22</td>
<td align="left">missouri</td>
<td align="left">jasper</td>
<td align="right">161</td>
</tr>
<tr class="odd">
<td align="right">3</td>
<td align="left">hurricane</td>
<td align="left">2005-08-28</td>
<td align="left">louisiana</td>
<td align="left">lower.st.bernard</td>
<td align="right">140</td>
</tr>
<tr class="even">
<td align="right">4</td>
<td align="left">tornado</td>
<td align="left">1953-06-08</td>
<td align="left">michigan</td>
<td align="left">genesee</td>
<td align="right">116</td>
</tr>
<tr class="odd">
<td align="right">5</td>
<td align="left">tornado</td>
<td align="left">1953-05-11</td>
<td align="left">texas</td>
<td align="left">mclennan</td>
<td align="right">114</td>
</tr>
<tr class="even">
<td align="right">6</td>
<td align="left">hurricane</td>
<td align="left">2005-08-28</td>
<td align="left">mississippi</td>
<td align="left">harrison</td>
<td align="right">97</td>
</tr>
<tr class="odd">
<td align="right">7</td>
<td align="left">heat</td>
<td align="left">1999-07-28</td>
<td align="left">illinois</td>
<td align="left">cook</td>
<td align="right">93</td>
</tr>
<tr class="even">
<td align="right">8</td>
<td align="left">tornado</td>
<td align="left">1953-06-09</td>
<td align="left">massachusetts</td>
<td align="left">worcester</td>
<td align="right">90</td>
</tr>
<tr class="odd">
<td align="right">9</td>
<td align="left">tornado</td>
<td align="left">1955-05-25</td>
<td align="left">kansas</td>
<td align="left">cowley</td>
<td align="right">75</td>
</tr>
<tr class="even">
<td align="right">10</td>
<td align="left">heat</td>
<td align="left">1999-07-04</td>
<td align="left">pennsylvania</td>
<td align="left">philadelphia</td>
<td align="right">58</td>
</tr>
</tbody>
</table>

    # the plot
    plt.fatal.single <- ggplot(fatal95.df, aes(day, fatalities, colour=event))

    plt.fatal.single <- plt.fatal.single + geom_point() +
            geom_text(aes(label=ifelse(rank <= 3,
                     paste0(as.character(day), ": ", fatalities, " killed") ,""),
                    hjust=-.03,vjust=0.5)) +

            # geom_hline(aes(yintercept = mean), linetype=2) +
            # geom_hline(aes(yintercept = median), linetype=3) +
            labs(title="Most Fatal",
                        y="", x="") +
        
            expand_limits(x=as.Date('2017-01-01'))+ #ok
            scale_colour_manual(values = getPalette(colourCount.fatal.single))+                
            theme(legend.title=element_blank()) +
            theme(legend.position="bottom") +
            guides(fill=guide_legend(nrow=5, byrow=TRUE)) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
            theme(plot.title = element_text(hjust = 0.5))                 

    plt.fatal.single + labs(title="Worst fatal occurrences",
                        y="Fatalities", x="")

![Worst fatal
occurrences](readme_files/figure-markdown_strict/fatal-plot-single-1.png)

The single most fatal event was a **hurricane, that occurred in
louisiana, orleans, on 2005-08-28, killing 638 people.**

However, if we compare this single awful event to the mean of fatalities
caused, we see that this is very unlikely to happen.

### Most fatal in all time

Most fatal in all time

Notice that are several occurrences of the same type of event along the
time.

Therefore, in order to know which is the worst type of event along all
the years, we summed up the fatalities caused by each one of occurrences
of this events.

Notice that we are interested only in the worst of them, ie, the ones
which are above the mean.

    # totals per event
    fatal.all.df <- fatal.df %>% group_by(event) %>%
            summarise(total = sum(fatalities)) %>% arrange(desc(total)) %>% 
            mutate(mean = mean(total), median = median(total),
                   rank = seq_len(length(event))) %>%
            filter(total > mean(total))

    # ordering events by rank, to plot later
    fatal.all.df$event <- factor(fatal.all.df$event,
                    levels = fatal.all.df$event[order(fatal.all.df$rank)])


    # create color pallete for all events
    colourCount.fatal.all = length(unique(fatal.all.df$event))
    getPalette = colorRampPalette(brewer.pal(colourCount.fatal.all, "Set1"))

    # prepare text for inline R
    worst.fatal.all.ev <- fatal.all.df$event[1]
    worst.fatal.all.kill <- fatal.all.df$total[1]
    worst.fatal.all.mean <- round(fatal.all.df$mean[1],2)
    worst.fatal.all.median <- round(fatal.all.df$median[1],2)

    # a table
    kable(fatal.all.df[,c(5,1:2)],
          caption=paste("Total fatalities by event, mean = ",
                        worst.fatal.all.mean, " and median = ",
                        worst.fatal.all.median))

<table>
<caption>Total fatalities by event, mean = 677.57 and median = 184</caption>
<thead>
<tr class="header">
<th align="right">rank</th>
<th align="left">event</th>
<th align="right">total</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="right">1</td>
<td align="left">tornado</td>
<td align="right">5873</td>
</tr>
<tr class="even">
<td align="right">2</td>
<td align="left">heat</td>
<td align="right">2854</td>
</tr>
<tr class="odd">
<td align="right">3</td>
<td align="left">wind</td>
<td align="right">2304</td>
</tr>
<tr class="even">
<td align="right">4</td>
<td align="left">flood</td>
<td align="right">1944</td>
</tr>
<tr class="odd">
<td align="right">5</td>
<td align="left">winter</td>
<td align="right">1196</td>
</tr>
<tr class="even">
<td align="right">6</td>
<td align="left">hurricane</td>
<td align="right">1128</td>
</tr>
<tr class="odd">
<td align="right">7</td>
<td align="left">lightning</td>
<td align="right">833</td>
</tr>
<tr class="even">
<td align="right">8</td>
<td align="left">rip current</td>
<td align="right">798</td>
</tr>
</tbody>
</table>

    # the plot
    plt.fatal.all <- ggplot(data=fatal.all.df, aes(event, total, fill=event))

    plt.fatal.all <- plt.fatal.all + geom_bar(stat="identity") +
            geom_text(aes(label=ifelse(total==max(total),
                    paste0(event, ": ", max(total), " killed"),'')),
                    hjust=0,vjust=2) +
            geom_hline(aes(yintercept = mean), linetype=1) +
            # geom_hline(aes(yintercept = median), linetype=2) +
            labs(title="All time", y="",
                 x="") + 
                    
            theme(legend.position="none") +        
            scale_colour_manual(values = getPalette(colourCount.fatal.all))+                
            theme(legend.title=element_blank()) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
            theme(plot.title = element_text(hjust = 0.5))                 

    plt.fatal.all + labs(title="Total Fatalities - All time",
                 subtitle="Most fatal events of all time",
                         y="Fatalities", x="")

![Total fatalities by
event](readme_files/figure-markdown_strict/fatal-plot-alltime-1.png)

The most fatal event along the time is the **tornado. It has killed 5873
people until now.**

### Least fatal events

Just for curiosity, these are the less fatal among the fatal events:

    # sort for less dangerous, had to subset again due to previous filtering
    fatal.all.df <- fatal.df %>% group_by(event) %>%
            summarise(total = sum(fatalities)) %>% arrange(total) %>% 
            mutate(mean = mean(total), median = median(total),
                   rank = seq(length(event),1, by=-1))

    # a table
    kable(fatal.all.df[1:10,c(5,1:2)], caption="Least fatal events")

<table>
<caption>Least fatal events</caption>
<thead>
<tr class="header">
<th align="right">rank</th>
<th align="left">event</th>
<th align="right">total</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="right">28</td>
<td align="left">tropical depression</td>
<td align="right">1</td>
</tr>
<tr class="even">
<td align="right">27</td>
<td align="left">dense smoke</td>
<td align="right">2</td>
</tr>
<tr class="odd">
<td align="right">26</td>
<td align="left">sleet</td>
<td align="right">2</td>
</tr>
<tr class="even">
<td align="right">25</td>
<td align="left">waterspout</td>
<td align="right">2</td>
</tr>
<tr class="odd">
<td align="right">24</td>
<td align="left">cold</td>
<td align="right">4</td>
</tr>
<tr class="even">
<td align="right">23</td>
<td align="left">dust devil</td>
<td align="right">4</td>
</tr>
<tr class="odd">
<td align="right">22</td>
<td align="left">slide</td>
<td align="right">4</td>
</tr>
<tr class="even">
<td align="right">21</td>
<td align="left">sneakerwave</td>
<td align="right">14</td>
</tr>
<tr class="odd">
<td align="right">20</td>
<td align="left">hail</td>
<td align="right">20</td>
</tr>
<tr class="even">
<td align="right">19</td>
<td align="left">tide</td>
<td align="right">22</td>
</tr>
</tbody>
</table>

    #kable(fatal.all.df[1:10,c(5,1:2)])

Injuring Occurrences
--------------------

### Most injuring in a single occurrence

Most injuring in a single occurrence

In order to determine what were the most injuring events in a single
occurrence, we need to see how injuries are distributed along the
occurrences.

    rm(fatal.df,fatal.all.df, fatal95.df, qt) # cleannig house


    injuring.df <- harm.df %>% filter(!is.na(injuries)) %>%
                    select(1:6,8)

    # quantiles
    qt <- quantile(injuring.df$injuries, probs=seq(.975,1,0.002))

    # distribution plot
    plt.distr.inj0 <- ggplot(injuring.df, aes(injuries))

    plt.distr.inj0 <- plt.distr.inj0 + geom_density(aes(y=..scaled..)) + xlim(0,0.5) + 
            labs(title="All events") +
            theme(plot.title = element_text(hjust = 0.5))


    # display only the qts next to injuring events
    #qt[(length(qt)-(length(qt[qt>=1])+1)): length(qt)]

Looking at this distribution, we can infer that the vast majority of
those occurrences were not injuring at all: **98.5% occurrences didn't
caused any injuries**

On the other hand, injuring occurrences had to have at least 1 injury.

Now, among the injuring occurrences, we are interested in the ones whose
harm is beyond the confidence interval, ie. above 99% of the most common
values.

    # subset for harm events
    injuring.df <- injuring.df %>% filter(injuries > 0) %>%
                    arrange(desc(injuries)) %>%
                    mutate(mean = mean(injuries), 
                    median = median(injuries),
                    rank = seq_len(length(event)))

    # quantiles, same as 
    # poisson.test(mean, conf.level = 0.95)

    qt <- quantile(injuring.df$injuries, probs=seq(.999,1,0.005))
    #qt

Looking at this distribution, we can infer that **99.8% of the injuring
occurrences caused up to 500 injuries**.

    # distribution plot
    plt.distr.inj1 <- ggplot(injuring.df, aes(injuries))

    plt.distr.inj1 <- plt.distr.inj1 + geom_density(aes(y=..scaled..)) + xlim(0,(qt[1]/20)) + 
            labs(title="Injuring events") +
            theme(plot.title = element_text(hjust = 0.5))       

    grid.arrange(plt.distr.inj0, plt.distr.inj1,
                 nrow=1, ncol=2)
    grid.rect(gp=gpar(fill=NA))

![Population distribution for Injuries /
occurrences](readme_files/figure-markdown_strict/inj-distribution-1.png)

In this study, we looked on the 1% most injuring occurrences.

    # subset for 99% CI
    injuring95.df <- filter(injuring.df, injuries>qt[1])

    # create color pallete for all events
    colourCount.inj.single = length(unique(injuring95.df$event))
    getPalette = colorRampPalette(brewer.pal(colourCount.inj.single, "Set1"))


    # prepare text for inline R
    worst.injuring.single.ev <- injuring95.df$event[1]
    worst.injuring.single.st <- injuring95.df$state[1]
    worst.injuring.single.ct <- injuring95.df$county[1]
    worst.injuring.single.dt <- injuring95.df$day[1]
    worst.injuring.single.inj <- injuring95.df$injuries[1]
    worst.injuring.single.mean <- round(injuring95.df$mean[1],2)
    worst.injuring.single.median <- round(injuring95.df$median[1],2)

    # print a table

    kable(injuring95.df[, c(10,1,3,5:7)], 
          caption=paste("Worst injuring occurrences, mean = ",
                        worst.injuring.single.mean, " and median = ",
                        worst.injuring.single.median))

<table>
<caption>Worst injuring occurrences, mean = 7.57 and median = 2</caption>
<thead>
<tr class="header">
<th align="right">rank</th>
<th align="left">event</th>
<th align="left">day</th>
<th align="left">state</th>
<th align="left">county</th>
<th align="right">injuries</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="right">1</td>
<td align="left">hurricane</td>
<td align="left">2008-09-12</td>
<td align="left">texas</td>
<td align="left">harris</td>
<td align="right">2400</td>
</tr>
<tr class="even">
<td align="right">2</td>
<td align="left">tornado</td>
<td align="left">1979-04-10</td>
<td align="left">texas</td>
<td align="left">wichita</td>
<td align="right">1700</td>
</tr>
<tr class="odd">
<td align="right">3</td>
<td align="left">tornado</td>
<td align="left">1953-06-09</td>
<td align="left">massachusetts</td>
<td align="left">worcester</td>
<td align="right">1228</td>
</tr>
<tr class="even">
<td align="right">4</td>
<td align="left">tornado</td>
<td align="left">1974-04-03</td>
<td align="left">ohio</td>
<td align="left">greene</td>
<td align="right">1150</td>
</tr>
<tr class="odd">
<td align="right">5</td>
<td align="left">tornado</td>
<td align="left">2011-05-22</td>
<td align="left">missouri</td>
<td align="left">jasper</td>
<td align="right">1150</td>
</tr>
<tr class="even">
<td align="right">6</td>
<td align="left">flood</td>
<td align="left">1998-10-17</td>
<td align="left">texas</td>
<td align="left">comal</td>
<td align="right">800</td>
</tr>
<tr class="odd">
<td align="right">7</td>
<td align="left">tornado</td>
<td align="left">2011-04-27</td>
<td align="left">alabama</td>
<td align="left">tuscaloosa</td>
<td align="right">800</td>
</tr>
<tr class="even">
<td align="right">8</td>
<td align="left">tornado</td>
<td align="left">1953-06-08</td>
<td align="left">michigan</td>
<td align="left">genesee</td>
<td align="right">785</td>
</tr>
<tr class="odd">
<td align="right">9</td>
<td align="left">hurricane</td>
<td align="left">2004-08-13</td>
<td align="left">florida</td>
<td align="left">charlotte</td>
<td align="right">700</td>
</tr>
<tr class="even">
<td align="right">10</td>
<td align="left">tornado</td>
<td align="left">2011-04-27</td>
<td align="left">alabama</td>
<td align="left">jefferson</td>
<td align="right">700</td>
</tr>
<tr class="odd">
<td align="right">11</td>
<td align="left">flood</td>
<td align="left">1998-10-17</td>
<td align="left">texas</td>
<td align="left">bexar</td>
<td align="right">600</td>
</tr>
<tr class="even">
<td align="right">12</td>
<td align="left">tornado</td>
<td align="left">1953-05-11</td>
<td align="left">texas</td>
<td align="left">mclennan</td>
<td align="right">597</td>
</tr>
<tr class="odd">
<td align="right">13</td>
<td align="left">tornado</td>
<td align="left">1965-04-11</td>
<td align="left">indiana</td>
<td align="left">howard</td>
<td align="right">560</td>
</tr>
<tr class="even">
<td align="right">14</td>
<td align="left">heat</td>
<td align="left">2007-08-04</td>
<td align="left">missouri</td>
<td align="left">st.louis</td>
<td align="right">519</td>
</tr>
<tr class="odd">
<td align="right">15</td>
<td align="left">tornado</td>
<td align="left">1966-03-03</td>
<td align="left">mississippi</td>
<td align="left">hinds</td>
<td align="right">504</td>
</tr>
</tbody>
</table>

    # the plot
    plt.inj.single <- ggplot(injuring95.df, aes(day, injuries, colour=event))

    plt.inj.single <- plt.inj.single + geom_point() +
            geom_text(aes(label=ifelse(rank <= 3,
                     paste0(as.character(day), ": ", injuries, " injuried") ,""),
                    hjust=-.03,vjust=0.5)) +

            # geom_hline(aes(yintercept = mean), linetype=2) +
            # geom_hline(aes(yintercept = median), linetype=3) +
            labs(title="Most Injuring",
                        y="", x="") +
                    
            expand_limits(x=as.Date('2017-01-01'))+ #ok
            scale_colour_manual(values = getPalette(colourCount.inj.single))+                
            theme(legend.title=element_blank()) +
            theme(legend.position="bottom") +
            guides(fill=guide_legend(nrow=5, byrow=TRUE)) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
            theme(plot.title = element_text(hjust = 0.5)) 

            # scale_y_continuous(expand = c(0, 0)) +
            # geom_hline(aes(yintercept = mean), linetype=2) +
            # geom_hline(aes(yintercept = median), linetype=3)
    plt.inj.single + labs(title="Worst injuring occurrences",
                        y="Injuries", x="") 

![Worst injuring
occurrences](readme_files/figure-markdown_strict/injuring-single-plot-1.png)

The single most injuring event was a **hurricane, that occurred in
texas, harris, on 2008-09-12, injuring 2400 people.**

However, if we compare this single awful event to the mean of injuries
caused, we see that this is very unlikely to happen.

### Most injuring in all time

Notice that are several occurrences of the same type of event along the
time.

Therefore, in order to know which is the worst type of event along all
the years, we summed up the injuries caused by each one of occurrences
of this events.

Notice that we are interested only in the worst of them, ie, the ones
which are above the mean.

    # totals per event
    injuring.all.df <- injuring.df %>% group_by(event) %>% 
            summarise(total = sum(injuries)) %>%
            arrange(desc(total)) %>% 
            mutate(mean = mean(total), median = median(total),
                   rank = seq_len(length(event)))  %>%
            filter(total >= mean(total))

    # ordering events by rank, to plot later
    injuring.all.df$event <- factor(injuring.all.df$event,
                    levels = injuring.all.df$event[order(injuring.all.df$rank)])


    # create color pallete for all events
    colourCount.inj.all = length(unique(injuring.all.df$event))
    getPalette = colorRampPalette(brewer.pal(colourCount.inj.all, "Set1"))

    # prepare text for inline R
    worst.injuring.all.ev <- injuring.all.df$event[1]
    worst.injuring.all.inj <- injuring.all.df$total[1]
    worst.injuring.all.mean <- round(injuring.all.df$mean[1],2)
    worst.injuring.all.median <- round(injuring.all.df$median[1],2)

    # a table
    kable(injuring.all.df[,c(5,1:2)],
          caption=paste("Total injuries by event, mean = ",
                        worst.injuring.all.mean, " and median = ",
                        worst.injuring.all.median))

<table>
<caption>Total injuries by event, mean = 5200.42 and median = 316</caption>
<thead>
<tr class="header">
<th align="right">rank</th>
<th align="left">event</th>
<th align="right">total</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="right">1</td>
<td align="left">tornado</td>
<td align="right">94614</td>
</tr>
<tr class="even">
<td align="right">2</td>
<td align="left">heat</td>
<td align="right">15436</td>
</tr>
<tr class="odd">
<td align="right">3</td>
<td align="left">wind</td>
<td align="right">13439</td>
</tr>
<tr class="even">
<td align="right">4</td>
<td align="left">flood</td>
<td align="right">8806</td>
</tr>
<tr class="odd">
<td align="right">5</td>
<td align="left">winter</td>
<td align="right">8224</td>
</tr>
</tbody>
</table>

The most injuring event along the time is the **tornado. It has injuried
94614 people until now.**

    # the plot
    plt.inj.all <- ggplot(data=injuring.all.df, aes(event, total, fill=event))

    plt.inj.all <- plt.inj.all + geom_bar(stat="identity") +
            geom_text(aes(label=ifelse(total==max(total),
                    paste0(event, ": ", max(total), " injuried"),'')),
                    hjust=0,vjust=2) +
            geom_hline(aes(yintercept = mean), linetype=1) +
            # geom_hline(aes(yintercept = median), linetype=2) +
            labs(title="All time", 
                 y="", x="") + 
                    
            theme(legend.position="none") +        
            scale_colour_manual(values = getPalette(colourCount.inj.all))+                
            theme(legend.title=element_blank()) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
            theme(plot.title = element_text(hjust = 0.5))                 
                    

    plt.inj.all  + labs(title="Total Injuries - All time",
                 subtitle="Most injuring events of all time",
                         y="Injuries", x="")

![Total Injuries by
event](readme_files/figure-markdown_strict/injuring-all-plot-1.png)

### Least injuring events

Just for curiosity, lets show now what are the less injuring among the
injuring events:

    # sort for less dangerous, had to subset again due to previous filtering
    injuring.all.df <- injuring.df %>% group_by(event) %>%
            summarise(total = sum(injuries)) %>% arrange(total) %>% 
            mutate(mean = mean(total), median = median(total),
                   rank = seq(length(event),1, by=-1))

    # a table
    kable(injuring.all.df[1:10,c(5,1:2)], caption="Least injuring events")

<table>
<caption>Least injuring events</caption>
<thead>
<tr class="header">
<th align="right">rank</th>
<th align="left">event</th>
<th align="right">total</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="right">31</td>
<td align="left">funnel</td>
<td align="right">3</td>
</tr>
<tr class="even">
<td align="right">30</td>
<td align="left">tropical depression</td>
<td align="right">3</td>
</tr>
<tr class="odd">
<td align="right">29</td>
<td align="left">waterspout</td>
<td align="right">3</td>
</tr>
<tr class="even">
<td align="right">28</td>
<td align="left">other</td>
<td align="right">4</td>
</tr>
<tr class="odd">
<td align="right">27</td>
<td align="left">drought</td>
<td align="right">8</td>
</tr>
<tr class="even">
<td align="right">26</td>
<td align="left">sleet</td>
<td align="right">10</td>
</tr>
<tr class="odd">
<td align="right">25</td>
<td align="left">sneakerwave</td>
<td align="right">11</td>
</tr>
<tr class="even">
<td align="right">24</td>
<td align="left">slide</td>
<td align="right">13</td>
</tr>
<tr class="odd">
<td align="right">23</td>
<td align="left">cold</td>
<td align="right">15</td>
</tr>
<tr class="even">
<td align="right">22</td>
<td align="left">dense smoke</td>
<td align="right">17</td>
</tr>
</tbody>
</table>

    #kable(injuring.all.df[1:10,c(5,1:2)])

Economy: the the most harmfull events
=====================================

We have determined what events did more harm to economy, both in terms
of property and crops damage.

There were events that causes zero property damage but a lot of crop
damage. The inverse is also true, so we did a separate analysis to
property VS crop damaging events.

Property losses
---------------

### Most Property Damaging event in a single occurrence

In order to determine what were the most property damaging events in a
single occurrence, we need to see how damages are distributed along the
occurrences.

    # property damages
    rm(injuring.df, injuring.all.df, injuring95.df) # cleannig house

    prop.df <- harm.df %>% filter(!is.na(prop.ev)) %>%
                            select(1:6,9)
                    

    # quantiles
    qt <- quantile(prop.df$prop.ev, probs=seq(.999,1,0.002))

    # distribution plot
    plt.distr.prop0 <- ggplot(prop.df, aes(log(prop.ev)))

    plt.distr.prop0 <- plt.distr.prop0 + geom_density(aes(y=..scaled..)) + #xlim(0,.5) + 
            labs(title="All events", x="log(amount $)") +
            theme(plot.title = element_text(hjust = 0.5))       


    # display only the qts next to harmfull events
    #qt

Looking at this distribution, we can infer that 99.8% of the occurrences
caused less than **$40,000,000 in losses**.

On the other hand, damaging occurrences had to have damages above zero.

Now, among the damaging occurrences, we are interested in the ones whose
damages are above 99.8% of the most common values.

    # subset for harm events
    prop.df <- prop.df %>% filter(prop.ev > 0) %>%
                    arrange(desc(prop.ev)) %>%
                    mutate(value = dollar(prop.ev),
                            media.raw = mean(prop.ev), 
                            mediana.raw = median(prop.ev),
                            mean = dollar(media.raw),
                            median = dollar(mediana.raw),
                            rank = seq_len(length(event)))
    # quantiles, same as 

    qt <- quantile(prop.df$prop.ev, probs=seq(.999,1,0.002))
    #qt

Looking at this distribution, we can infer that **99.8% of the damaging
occurrences caused up to $128,960,700 in losses**.

    # distribution plot
    plt.distr.prop1 <- ggplot(prop.df, aes(log(prop.ev)))

    plt.distr.prop1 <- plt.distr.prop1 + geom_density(aes(y=..scaled..)) + #xlim(0,100) + 
            labs(title="Damaging events", x="log(amount $)") +
            theme(plot.title = element_text(hjust = 0.5))     

    grid.arrange(plt.distr.prop0, plt.distr.prop1, nrow=1, ncol=2)
    grid.rect(gp=gpar(fill=NA))

![Population distribution for losses /
occurrences](readme_files/figure-markdown_strict/crop-distribution-1.png)

In this study, we looked on the 1% most harmful occurrences.

    # subset for 99% CI
    prop95.df <- filter(prop.df, prop.ev>qt[1])

    # create color pallete for all events
    colourCount.prop.single = length(unique(prop95.df$event))
    getPalette = colorRampPalette(brewer.pal(colourCount.prop.single, "Set1"))




    # prepare text for inline R
    worst.prop.single.ev <- prop95.df$event[1]
    worst.prop.single.st <- prop95.df$state[1]
    worst.prop.single.ct <- prop95.df$county[1]
    worst.prop.single.dt <- prop95.df$day[1]
    worst.prop.single.value <- prop95.df$value[1]
    worst.prop.single.mean <- prop95.df$mean[1]
    worst.prop.single.median <- prop95.df$median[1]

    # print table
    kable(prop95.df[1:20,c(13,1,3,5,6,8)],
          caption=paste("Worst property damaging occurrences, mean = ",
                        worst.prop.single.mean, " and median = ",
                        worst.prop.single.median))

<table>
<caption>Worst property damaging occurrences, mean = $1,137,697 and median = $10,000</caption>
<thead>
<tr class="header">
<th align="right">rank</th>
<th align="left">event</th>
<th align="left">day</th>
<th align="left">state</th>
<th align="left">county</th>
<th align="left">value</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="right">1</td>
<td align="left">tide</td>
<td align="left">2005-08-29</td>
<td align="left">louisiana</td>
<td align="left">orleans</td>
<td align="left">$17,900,000,000</td>
</tr>
<tr class="even">
<td align="right">2</td>
<td align="left">hurricane</td>
<td align="left">2005-10-24</td>
<td align="left">florida</td>
<td align="left">.palm.beach</td>
<td align="left">$10,000,000,000</td>
</tr>
<tr class="odd">
<td align="right">3</td>
<td align="left">flood</td>
<td align="left">2012-10-29</td>
<td align="left">new.jersey</td>
<td align="left">eastern.ocean</td>
<td align="left">$7,500,000,000</td>
</tr>
<tr class="even">
<td align="right">4</td>
<td align="left">tide</td>
<td align="left">2005-08-29</td>
<td align="left">mississippi</td>
<td align="left">harrison</td>
<td align="left">$5,630,000,000</td>
</tr>
<tr class="odd">
<td align="right">5</td>
<td align="left">storm</td>
<td align="left">2001-06-05</td>
<td align="left">texas</td>
<td align="left">harris</td>
<td align="left">$5,030,000,000</td>
</tr>
<tr class="even">
<td align="right">6</td>
<td align="left">flood</td>
<td align="left">2012-10-28</td>
<td align="left">new.jersey</td>
<td align="left">eastern.monmouth</td>
<td align="left">$5,000,000,000</td>
</tr>
<tr class="odd">
<td align="right">7</td>
<td align="left">flood</td>
<td align="left">2012-10-28</td>
<td align="left">new.jersey</td>
<td align="left">western.monmouth</td>
<td align="left">$5,000,000,000</td>
</tr>
<tr class="even">
<td align="right">8</td>
<td align="left">hurricane</td>
<td align="left">2004-09-13</td>
<td align="left">florida</td>
<td align="left">.escambia</td>
<td align="left">$4,000,000,000</td>
</tr>
<tr class="odd">
<td align="right">9</td>
<td align="left">tide</td>
<td align="left">2008-09-12</td>
<td align="left">texas</td>
<td align="left">galveston</td>
<td align="left">$4,000,000,000</td>
</tr>
<tr class="even">
<td align="right">10</td>
<td align="left">hurricane</td>
<td align="left">2005-08-28</td>
<td align="left">louisiana</td>
<td align="left">orleans</td>
<td align="left">$3,560,000,000</td>
</tr>
<tr class="odd">
<td align="right">11</td>
<td align="left">tide</td>
<td align="left">2005-08-29</td>
<td align="left">mississippi</td>
<td align="left">hancock</td>
<td align="left">$3,380,000,000</td>
</tr>
<tr class="even">
<td align="right">12</td>
<td align="left">tide</td>
<td align="left">2005-08-29</td>
<td align="left">louisiana</td>
<td align="left">st.tammany</td>
<td align="left">$3,030,000,000</td>
</tr>
<tr class="odd">
<td align="right">13</td>
<td align="left">tide</td>
<td align="left">2005-08-29</td>
<td align="left">louisiana</td>
<td align="left">lower.plaquemines</td>
<td align="left">$3,030,000,000</td>
</tr>
<tr class="even">
<td align="right">14</td>
<td align="left">tide</td>
<td align="left">2005-08-29</td>
<td align="left">louisiana</td>
<td align="left">lower.st.bernard</td>
<td align="left">$3,020,000,000</td>
</tr>
<tr class="odd">
<td align="right">15</td>
<td align="left">tide</td>
<td align="left">2005-08-29</td>
<td align="left">louisiana</td>
<td align="left">upper.st.bernard</td>
<td align="left">$3,020,000,000</td>
</tr>
<tr class="even">
<td align="right">16</td>
<td align="left">flood</td>
<td align="left">1997-04-18</td>
<td align="left">north.dakota</td>
<td align="left">grand.forks</td>
<td align="left">$3,000,000,000</td>
</tr>
<tr class="odd">
<td align="right">17</td>
<td align="left">hurricane</td>
<td align="left">1999-09-15</td>
<td align="left">north.carolina</td>
<td align="left">alamance</td>
<td align="left">$3,000,000,000</td>
</tr>
<tr class="even">
<td align="right">18</td>
<td align="left">hurricane</td>
<td align="left">2004-08-13</td>
<td align="left">florida</td>
<td align="left">charlotte</td>
<td align="left">$3,000,000,000</td>
</tr>
<tr class="odd">
<td align="right">19</td>
<td align="left">tide</td>
<td align="left">2008-09-12</td>
<td align="left">texas</td>
<td align="left">harris</td>
<td align="left">$3,000,000,000</td>
</tr>
<tr class="even">
<td align="right">20</td>
<td align="left">hurricane</td>
<td align="left">2005-08-28</td>
<td align="left">mississippi</td>
<td align="left">harrison</td>
<td align="left">$2,940,000,000</td>
</tr>
</tbody>
</table>

    plt.prop.single <- ggplot(prop95.df, aes(day, prop.ev, colour=event))

    plt.prop.single <- plt.prop.single + geom_point() +
            geom_text(aes(label=ifelse(rank <= 3,
                    as.character(day),""),
                    hjust=-.03,vjust=0.5)) +
            # geom_hline(aes(yintercept = media.raw), linetype=2) +
             # geom_hline(aes(yintercept = mediana.raw), linetype=3) +
            labs(title="Property Dammaging",
                        y="", x="") + 
                    
            expand_limits(x=as.Date('2017-01-01'))+ #ok
            scale_y_continuous(labels = dollar)+
            
            scale_colour_manual(values = getPalette(colourCount.prop.single))+                
            theme(legend.title=element_blank()) +
            theme(legend.position="bottom") +
            guides(fill=guide_legend(nrow=5, byrow=TRUE)) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
            theme(plot.title = element_text(hjust = 0.5)) 

    plt.prop.single + labs(title="Worst property damaging occurrences",
                        y="Losses", x="")

![Worst property damaging
occurrences](readme_files/figure-markdown_strict/prop-single-plot-1.png)

The single most economic damaging event to properties was a **tide, that
occurred in louisiana, orleans, on 2005-08-29, causing U$
$17,900,000,000 in losses**.

### Most Property Damaging event in all time

Notice that are several occurrences of the same type of event along the
time.

Therefore, in order to know which is the worst type of event along all
the years, we summed up the losses caused by each one of occurrences of
this events.

Notice that we are interested only in the worst of them, ie, the ones
which are above the mean.

    # totals per event
    prop.all.df <- prop.df %>% group_by(event) %>%
                    summarise(total.raw = sum(prop.ev)) %>%
                    arrange(desc(total.raw)) %>%
                    mutate(media.raw = mean(total.raw), 
                            mediana.raw = median(total.raw),
                            total = dollar(total.raw),
                            mean = dollar(media.raw),
                            median = dollar(mediana.raw),
                            rank = seq_len(length(event))) %>%
                    filter(total.raw > mean(total.raw))

    # ordering events by rank, to plot later
    prop.all.df$event <- factor(prop.all.df$event,
                    levels = prop.all.df$event[order(prop.all.df$rank)])
                    
    # create color pallete for all events
    colourCount.prop.all = length(unique(prop.all.df$event))
    getPalette = colorRampPalette(brewer.pal(colourCount.prop.all, "Set1"))

    # prepare text for inline R
    worst.prop.all.ev <- prop.all.df$event[1]
    worst.prop.total <- prop.all.df$total[1]

    worst.prop.all.mean <- prop.all.df$mean[1]
    worst.prop.all.median <- prop.all.df$median[1]

    # print table

    kable(prop.all.df[, c(8,1,5)], 
                caption=paste("Total property losses by event, mean = ",
                        worst.prop.all.mean, " and median = ",
                        worst.prop.all.median))

<table>
<caption>Total property losses by event, mean = $11,737,195,233 and median = $229,971,300</caption>
<thead>
<tr class="header">
<th align="right">rank</th>
<th align="left">event</th>
<th align="left">total</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="right">1</td>
<td align="left">hurricane</td>
<td align="left">$87,005,170,310</td>
</tr>
<tr class="even">
<td align="right">2</td>
<td align="left">flood</td>
<td align="left">$82,920,597,380</td>
</tr>
<tr class="odd">
<td align="right">3</td>
<td align="left">tornado</td>
<td align="left">$63,648,478,192</td>
</tr>
<tr class="even">
<td align="right">4</td>
<td align="left">tide</td>
<td align="left">$54,155,102,600</td>
</tr>
<tr class="odd">
<td align="right">5</td>
<td align="left">hail</td>
<td align="left">$25,360,544,274</td>
</tr>
<tr class="even">
<td align="right">6</td>
<td align="left">wind</td>
<td align="left">$24,868,014,778</td>
</tr>
<tr class="odd">
<td align="right">7</td>
<td align="left">storm</td>
<td align="left">$16,753,590,360</td>
</tr>
</tbody>
</table>

    plt.prop.all <- ggplot(data=prop.all.df, aes(event, total.raw, fill=event))

    plt.prop.all <- plt.prop.all + geom_bar(stat="identity") +
                    
            geom_text(aes(label=ifelse(total.raw==max(total.raw),
                    paste(event, dollar(max(total.raw)), sep=": "),'')),
                    hjust=0,vjust=2) +
            geom_hline(aes(yintercept = media.raw), linetype=1) +
            # geom_hline(aes(yintercept = mediana.raw), linetype=2) +
            labs(title="All time", y="", x="") + 
                    
            scale_y_continuous(labels = dollar)+
         
            theme(legend.position="none") +        
            scale_colour_manual(values = getPalette(colourCount.prop.all))+                
            theme(legend.title=element_blank()) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
            theme(plot.title = element_text(hjust = 0.5))                 

    plt.prop.all + labs(title="Total Damages - All time",
                 subtitle="Most property damaging events of all time",
                         y="Losses", x="")

![Total Property Damages by
event](readme_files/figure-markdown_strict/prop-all-plot-1.png)

The most property damaging event along the time is the **hurricane. It
has caused $87,005,170,310 in losses.**

### Least property damaging events

Just for curiosity, these are the less damaging events:

    prop.all.df <- prop.df %>% group_by(event) %>%
                    summarise(total.raw = sum(prop.ev)) %>%
                    arrange(total.raw) %>%
                    mutate(media.raw = mean(total.raw), 
                            mediana.raw = median(total.raw),
                            total = dollar(total.raw),
                            mean = dollar(media.raw),
                            median = dollar(mediana.raw),
                            rank = seq(length(event),1, by=-1))



    kable(prop.all.df[1:10, c(8,1,5)], caption="Least property damaging events")

<table>
<caption>Least property damaging events</caption>
<thead>
<tr class="header">
<th align="right">rank</th>
<th align="left">event</th>
<th align="left">total</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="right">32</td>
<td align="left">other</td>
<td align="left">$1,000</td>
</tr>
<tr class="even">
<td align="right">31</td>
<td align="left">funnel</td>
<td align="left">$123,100</td>
</tr>
<tr class="odd">
<td align="right">30</td>
<td align="left">dense smoke</td>
<td align="left">$130,000</td>
</tr>
<tr class="even">
<td align="right">29</td>
<td align="left">rip current</td>
<td align="left">$163,000</td>
</tr>
<tr class="odd">
<td align="right">28</td>
<td align="left">volcanic ash</td>
<td align="left">$500,000</td>
</tr>
<tr class="even">
<td align="right">27</td>
<td align="left">dust devil</td>
<td align="left">$1,147,430</td>
</tr>
<tr class="odd">
<td align="right">26</td>
<td align="left">seiche</td>
<td align="left">$1,402,000</td>
</tr>
<tr class="even">
<td align="right">25</td>
<td align="left">sleet</td>
<td align="left">$3,084,000</td>
</tr>
<tr class="odd">
<td align="right">24</td>
<td align="left">avalanche</td>
<td align="left">$4,058,050</td>
</tr>
<tr class="even">
<td align="right">23</td>
<td align="left">waterspout</td>
<td align="left">$5,748,200</td>
</tr>
</tbody>
</table>

    # kable(prop.all.df[1:10, c(8,1,5)])

Crop losses
-----------

### Most Crop Damaging event in a single occurrence

In order to determine what were the most crop damaging events in a
single occurrence, we need to see how damages are distributed along the
occurrences.

    # crop damages
    rm(prop.df, prop.all.df, prop95.df) # cleannig house

    crop.df <- harm.df %>% filter(!is.na(crop.ev)) %>%
                    select(1:6,10) 


    # quantiles
    qt <- quantile(crop.df$crop.ev, probs=seq(.998,1,0.002))

    # distribution plot
    plt.distr.crop0 <- ggplot(crop.df, aes(log(crop.ev)))

    plt.distr.crop0 <- plt.distr.crop0 + geom_density(aes(y=..scaled..)) + #xlim(0,.5) + 
            labs(title="All events", x="log(amount $)") +
            theme(plot.title = element_text(hjust = 0.5))  


    # display only the qts next to harmfull events
    #qt

<!-- Looking at this distribution, we can infer that 99% of the occurrences caused less than **$5,030,000 in losses**. -->
On the other hand, damaging occurrences had to have damages above zero.

Now, among the damaging occurrences, we are interested in the ones whose
damages are above 99% of the most common values.

    # subset for harm events
    crop.df <- crop.df %>% filter(crop.ev > 0) %>%
                    arrange(desc(crop.ev)) %>%
                    mutate(value = dollar(crop.ev),
                            media.raw = mean(crop.ev), 
                            mediana.raw = median(crop.ev),
                            mean = dollar(media.raw),
                            median = dollar(mediana.raw),
                            rank = seq_len(length(event)))

    # quantiles, same as 
    # poisson.test(mean, conf.level = 0.95)

    qt <- quantile(crop.df$crop.ev, probs=seq(.999,1,0.005))
    #qt

Looking at this distribution, we can infer that **99.8% of the damaging
occurrences caused up to $197,450,000 in losses.**

    # distribution plot
    plt.distr.crop1 <- ggplot(crop.df, aes(log(crop.ev)))

    plt.distr.crop1 <- plt.distr.crop1 + geom_density(aes(y=..scaled..)) + #xlim(0,qt[1]) + 
            labs(title="Damaging events", x="log(amount $)") +
            # scale_x_continuous(labels = dollar)+
            # theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
            theme(plot.title = element_text(hjust = 0.5))       

    grid.arrange(plt.distr.crop0, plt.distr.crop1,
                 nrow=1, ncol=2)
    grid.rect(gp=gpar(fill=NA))

![Population distribution for losses /
occurrences](readme_files/figure-markdown_strict/crop-distr-4-1.png)

In this study, we looked on the 1% most harmful occurrences.

    # subset for 99% CI
    crop95.df <- filter(crop.df, crop.ev>qt[1])

    # create color pallete for all events
    colourCount.crop.single = length(unique(crop95.df$event))
    getPalette = colorRampPalette(brewer.pal(colourCount.crop.single, "Set1"))

    worst.crop.single.ev <- crop95.df$event[1]
    worst.crop.single.st <- crop95.df$state[1]
    worst.crop.single.ct <- crop95.df$county[1]
    worst.crop.single.dt <- crop95.df$day[1]
    worst.crop.single.value <- crop95.df$value[1]
    worst.crop.single.mean <- crop95.df$mean[1]
    worst.crop.single.median <- crop95.df$median[1]


    # print a table

    kable(crop95.df[1:20,c(13,1,3,5,6,8)],
          caption=paste("Worst crops damaging occurrences, mean = ",
                        worst.crop.single.mean, " and median = ",
                        worst.crop.single.median))

<table>
<caption>Worst crops damaging occurrences, mean = $1,783,319 and median = $20,000</caption>
<thead>
<tr class="header">
<th align="right">rank</th>
<th align="left">event</th>
<th align="left">day</th>
<th align="left">state</th>
<th align="left">county</th>
<th align="left">value</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="right">1</td>
<td align="left">drought</td>
<td align="left">2014-12-01</td>
<td align="left">california</td>
<td align="left">northernsanjoaquin</td>
<td align="left">$1,500,000,000</td>
</tr>
<tr class="even">
<td align="right">2</td>
<td align="left">drought</td>
<td align="left">2011-06-01</td>
<td align="left">texas</td>
<td align="left">lubbock</td>
<td align="left">$1,050,000,000</td>
</tr>
<tr class="odd">
<td align="right">3</td>
<td align="left">drought</td>
<td align="left">2006-01-01</td>
<td align="left">texas</td>
<td align="left">montague</td>
<td align="left">$1,000,000,000</td>
</tr>
<tr class="even">
<td align="right">4</td>
<td align="left">drought</td>
<td align="left">2007-06-01</td>
<td align="left">mississippi</td>
<td align="left">warren</td>
<td align="left">$700,000,000</td>
</tr>
<tr class="odd">
<td align="right">5</td>
<td align="left">cold</td>
<td align="left">2007-01-11</td>
<td align="left">california</td>
<td align="left">sesj</td>
<td align="left">$568,600,000</td>
</tr>
<tr class="even">
<td align="right">6</td>
<td align="left">drought</td>
<td align="left">2000-11-01</td>
<td align="left">texas</td>
<td align="left">parmer</td>
<td align="left">$515,000,000</td>
</tr>
<tr class="odd">
<td align="right">7</td>
<td align="left">drought</td>
<td align="left">1998-07-06</td>
<td align="left">oklahoma</td>
<td align="left">choctaw</td>
<td align="left">$500,000,000</td>
</tr>
<tr class="even">
<td align="right">8</td>
<td align="left">drought</td>
<td align="left">1999-07-01</td>
<td align="left">pennsylvania</td>
<td align="left">potter</td>
<td align="left">$500,000,000</td>
</tr>
<tr class="odd">
<td align="right">9</td>
<td align="left">hurricane</td>
<td align="left">1999-09-15</td>
<td align="left">north.carolina</td>
<td align="left">alamance</td>
<td align="left">$500,000,000</td>
</tr>
<tr class="even">
<td align="right">10</td>
<td align="left">flood</td>
<td align="left">2000-10-03</td>
<td align="left">florida</td>
<td align="left">.dade</td>
<td align="left">$500,000,000</td>
</tr>
<tr class="odd">
<td align="right">11</td>
<td align="left">flood</td>
<td align="left">2007-07-01</td>
<td align="left">missouri</td>
<td align="left">henry</td>
<td align="left">$500,000,000</td>
</tr>
<tr class="even">
<td align="right">12</td>
<td align="left">wind</td>
<td align="left">1998-12-20</td>
<td align="left">california</td>
<td align="left">southernsanjoaquin</td>
<td align="left">$490,500,000</td>
</tr>
<tr class="odd">
<td align="right">13</td>
<td align="left">drought</td>
<td align="left">1998-12-01</td>
<td align="left">texas</td>
<td align="left">yoakum</td>
<td align="left">$450,000,000</td>
</tr>
<tr class="even">
<td align="right">14</td>
<td align="left">hurricane</td>
<td align="left">2005-08-25</td>
<td align="left">florida</td>
<td align="left">.dade</td>
<td align="left">$423,000,000</td>
</tr>
<tr class="odd">
<td align="right">15</td>
<td align="left">drought</td>
<td align="left">2001-12-01</td>
<td align="left">texas</td>
<td align="left">parmer</td>
<td align="left">$420,000,000</td>
</tr>
<tr class="even">
<td align="right">16</td>
<td align="left">drought</td>
<td align="left">2007-09-01</td>
<td align="left">georgia</td>
<td align="left">baldwin</td>
<td align="left">$344,000,000</td>
</tr>
<tr class="odd">
<td align="right">17</td>
<td align="left">drought</td>
<td align="left">2006-02-01</td>
<td align="left">texas</td>
<td align="left">fannin</td>
<td align="left">$300,000,000</td>
</tr>
<tr class="even">
<td align="right">18</td>
<td align="left">cold</td>
<td align="left">2010-01-10</td>
<td align="left">florida</td>
<td align="left">inlandcollier</td>
<td align="left">$300,000,000</td>
</tr>
<tr class="odd">
<td align="right">19</td>
<td align="left">cold</td>
<td align="left">2010-01-10</td>
<td align="left">florida</td>
<td align="left">inland.miami.dade</td>
<td align="left">$286,000,000</td>
</tr>
<tr class="even">
<td align="right">20</td>
<td align="left">drought</td>
<td align="left">1998-12-01</td>
<td align="left">texas</td>
<td align="left">andrews</td>
<td align="left">$250,000,000</td>
</tr>
</tbody>
</table>

    plt.crop.single <- ggplot(crop95.df, aes(day, crop.ev, colour=event))

    plt.crop.single <- plt.crop.single + geom_point() +
            geom_text(aes(label=ifelse(rank <= 3,
                    as.character(day),""),
                    hjust=-.03,vjust=0.5)) +
            # geom_hline(aes(yintercept = media.raw), linetype=2) +
            # geom_hline(aes(yintercept = mediana.raw), linetype=3) +
            labs(title="Crop Dammaging",
                        y="", x="") + 
                    
            expand_limits(x=as.Date('2017-01-01'))+ #ok
            scale_y_continuous(labels = dollar)+
            
            scale_colour_manual(values = getPalette(colourCount.crop.single))+                
            theme(legend.title=element_blank()) +
            theme(legend.position="bottom") +
            guides(fill=guide_legend(nrow=5, byrow=TRUE)) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
            theme(plot.title = element_text(hjust = 0.5)) 

    plt.crop.single + labs(title="Worst crops damaging occurrences",
                        y="Losses", x="")

![Worst crops damaging
occurrences](readme_files/figure-markdown_strict/crop-single-plot-1.png)

The single most economic damaging event to crops was a **drought, that
occurred in california, northernsanjoaquin, on 2014-12-01, causing U$
$1,500,000,000 in losses.**

### Most Crop Damaging event in all time

Notice that are several occurrences of the same type of event along the
time.

Therefore, in order to know which is the worst type of event along all
the years, we summed up the losses caused by each one of occurrences of
this events.

Notice that we are interested only in the worst of them, ie, the ones
which are above the mean.

    # totals per event
    crop.all.df <- crop.df %>% group_by(event) %>%
                    summarise(total.raw = sum(crop.ev)) %>%
                    arrange(desc(total.raw)) %>%
                    mutate(media.raw = mean(total.raw), 
                            mediana.raw = median(total.raw),
                            total = dollar(total.raw),
                            mean = dollar(media.raw),
                            median = dollar(mediana.raw),
                            rank = seq_len(length(event))) %>%
                    filter(total.raw > mean(total.raw))


    # ordering events by rank, to plot later
    crop.all.df$event <- factor(crop.all.df$event,
                    levels = crop.all.df$event[order(crop.all.df$rank)])

    # create color pallete for all events
    colourCount.crop.all = length(unique(crop.all.df$event))
    getPalette = colorRampPalette(brewer.pal(colourCount.crop.all, "Set1"))

    # prepare text for inline R
    worst.crop.all.ev <- crop.all.df$event[1]
    worst.crop.total <- crop.all.df$total[1]
    worst.crop.all.mean <- crop.all.df$mean[1]
    worst.crop.all.median <- crop.all.df$median[1]

    # print table

    kable(crop.all.df[, c(8,1,5)], 
                caption=paste("Total crops losses by event, mean = ",
                        worst.crop.all.mean, " and median = ",
                        worst.crop.all.median))

<table>
<caption>Total crops losses by event, mean = $2,957,587,920 and median = $450,448,110</caption>
<thead>
<tr class="header">
<th align="right">rank</th>
<th align="left">event</th>
<th align="left">total</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="right">1</td>
<td align="left">drought</td>
<td align="left">$27,454,862,620</td>
</tr>
<tr class="even">
<td align="right">2</td>
<td align="left">flood</td>
<td align="left">$7,750,252,370</td>
</tr>
<tr class="odd">
<td align="right">3</td>
<td align="left">hurricane</td>
<td align="left">$5,341,874,800</td>
</tr>
<tr class="even">
<td align="right">4</td>
<td align="left">cold</td>
<td align="left">$4,919,893,200</td>
</tr>
<tr class="odd">
<td align="right">5</td>
<td align="left">wind</td>
<td align="left">$3,679,520,230</td>
</tr>
<tr class="even">
<td align="right">6</td>
<td align="left">hail</td>
<td align="left">$3,657,650,043</td>
</tr>
</tbody>
</table>

    # plt.crop.all <- ggplot(data=crop.all.df, aes(x=reorder(event, total.raw), y=total.raw, fill=event))

    plt.crop.all <- ggplot(data=crop.all.df, aes(event, total.raw, fill=event))

    plt.crop.all <- plt.crop.all + geom_bar(stat="identity") +
                    
            geom_text(aes(label=ifelse(total.raw==max(total.raw),
                    paste(event, dollar(max(total.raw)), sep=": "),'')),
                    hjust=0,vjust=2) +
            geom_hline(aes(yintercept = media.raw), linetype=1) +
            # geom_hline(aes(yintercept = mediana.raw), linetype=2) +
            labs(title="All time", y="", x="") + 
                    
            scale_y_continuous(labels = dollar)+
         
            theme(legend.position="none") +        
            scale_colour_manual(values = getPalette(colourCount.crop.all))+                
            theme(legend.title=element_blank()) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
            theme(plot.title = element_text(hjust = 0.5))                 

    plt.crop.all + labs(title="Total Damages - All time",
                 subtitle="Most crops damaging events of all time",
                         y="Losses", x="")

![Total Crop Damages by
event](readme_files/figure-markdown_strict/crop-all-plot-1.png)

The most crop damaging event along the time is the **drought. It has
caused $27,454,862,620 in losses.**

### Least crops damaging events

Just for curiosity, lets show now what are the less damaging among the
events:

    crop.all.df <- crop.df %>% group_by(event) %>%
                    summarise(total.raw = sum(crop.ev)) %>%
                    arrange(total.raw) %>%
                    mutate(media.raw = mean(total.raw), 
                            mediana.raw = median(total.raw),
                            total = dollar(total.raw),
                            mean = dollar(media.raw),
                            median = dollar(mediana.raw),
                            rank = seq(length(event),1, by=-1))



    kable(crop.all.df[1:10, c(8,1,5)], caption="Least crops damaging events")

<table>
<caption>Least crops damaging events</caption>
<thead>
<tr class="header">
<th align="right">rank</th>
<th align="left">event</th>
<th align="left">total</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="right">19</td>
<td align="left">slide</td>
<td align="left">$17,000</td>
</tr>
<tr class="even">
<td align="right">18</td>
<td align="left">tsunami</td>
<td align="left">$20,000</td>
</tr>
<tr class="odd">
<td align="right">17</td>
<td align="left">tide</td>
<td align="left">$955,000</td>
</tr>
<tr class="even">
<td align="right">16</td>
<td align="left">blizzard</td>
<td align="left">$7,060,000</td>
</tr>
<tr class="odd">
<td align="right">15</td>
<td align="left">lightning</td>
<td align="left">$7,422,640</td>
</tr>
<tr class="even">
<td align="right">14</td>
<td align="left">debris flow</td>
<td align="left">$20,001,500</td>
</tr>
<tr class="odd">
<td align="right">13</td>
<td align="left">winter</td>
<td align="left">$46,924,000</td>
</tr>
<tr class="even">
<td align="right">12</td>
<td align="left">snow</td>
<td align="left">$91,145,900</td>
</tr>
<tr class="odd">
<td align="right">11</td>
<td align="left">fire</td>
<td align="left">$447,668,860</td>
</tr>
<tr class="even">
<td align="right">10</td>
<td align="left">tornado</td>
<td align="left">$450,448,110</td>
</tr>
</tbody>
</table>

Most aflicted locations
=======================

We have determined what locations had the worst outcome from those
events, both in terms of human health and economic losses.

Unfortunatelly, these has been the worst counties for living in:

    rm(crop.df, crop.all.df, crop95.df) # house cleanning

    # subset original data
    cities.df <- harm.df %>% 
                    filter(!(is.na(county) | is.na(state))) %>%
                    select(6,5,7:10) %>%
                    group_by(state, county) %>%
                    summarise(fatalities = sum(fatalities, na.rm=TRUE),
                            injuries = sum(injuries, na.rm=TRUE),
                            prop.dmg = sum(prop.ev, na.rm=TRUE),
                            crop.dmg = sum(crop.ev, na.rm=TRUE)
                    )

Worst fatality count
--------------------

    cities.fatal.df <- arrange(cities.df, desc(fatalities)) %>% ungroup(state, county) %>%
          mutate(rank = seq_len(length(fatalities)),
                    prop.dmg = dollar(prop.dmg),
                    crop.dmg = dollar(crop.dmg)
                 )

    kable(cities.fatal.df[1:10, c(7,1:6)], caption="Total fatalities by county")

<table>
<caption>Total fatalities by county</caption>
<thead>
<tr class="header">
<th align="right">rank</th>
<th align="left">state</th>
<th align="left">county</th>
<th align="right">fatalities</th>
<th align="right">injuries</th>
<th align="left">prop.dmg</th>
<th align="left">crop.dmg</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="right">1</td>
<td align="left">louisiana</td>
<td align="left">orleans</td>
<td align="right">649</td>
<td align="right">99</td>
<td align="left">$21,614,049,550</td>
<td align="left">$0</td>
</tr>
<tr class="even">
<td align="right">2</td>
<td align="left">illinois</td>
<td align="left">cook</td>
<td align="right">565</td>
<td align="right">912</td>
<td align="left">$670,237,350</td>
<td align="left">$0</td>
</tr>
<tr class="odd">
<td align="right">3</td>
<td align="left">pennsylvania</td>
<td align="left">philadelphia</td>
<td align="right">387</td>
<td align="right">455</td>
<td align="left">$52,680,980</td>
<td align="left">$0</td>
</tr>
<tr class="even">
<td align="right">4</td>
<td align="left">nevada</td>
<td align="left">lasvegas</td>
<td align="right">263</td>
<td align="right">601</td>
<td align="left">$13,162,000</td>
<td align="left">$0</td>
</tr>
<tr class="odd">
<td align="right">5</td>
<td align="left">texas</td>
<td align="left">harris</td>
<td align="right">216</td>
<td align="right">2825</td>
<td align="left">$10,890,439,870</td>
<td align="left">$7,442,000</td>
</tr>
<tr class="even">
<td align="right">6</td>
<td align="left">missouri</td>
<td align="left">jasper</td>
<td align="right">178</td>
<td align="right">1273</td>
<td align="left">$2,864,021,330</td>
<td align="left">$46,475,500</td>
</tr>
<tr class="odd">
<td align="right">7</td>
<td align="left">texas</td>
<td align="left">dallas</td>
<td align="right">149</td>
<td align="right">1757</td>
<td align="left">$1,946,192,730</td>
<td align="left">$1,405,000</td>
</tr>
<tr class="even">
<td align="right">8</td>
<td align="left">louisiana</td>
<td align="left">lower.st.bernard</td>
<td align="right">140</td>
<td align="right">0</td>
<td align="left">$4,845,022,000</td>
<td align="left">$0</td>
</tr>
<tr class="odd">
<td align="right">9</td>
<td align="left">texas</td>
<td align="left">mclennan</td>
<td align="right">127</td>
<td align="right">657</td>
<td align="left">$65,138,600</td>
<td align="left">$1,710,500</td>
</tr>
<tr class="even">
<td align="right">10</td>
<td align="left">michigan</td>
<td align="left">genesee</td>
<td align="right">123</td>
<td align="right">962</td>
<td align="left">$107,151,750</td>
<td align="left">$6,300,000</td>
</tr>
</tbody>
</table>

    #kable(cities.fatal.df[1:10, c(7,1:6)])

    worst.fatal.city.county <- cities.fatal.df$county[1]
    worst.fatal.city.st <- cities.fatal.df$state[1]
    worst.fatal.city.count <- cities.fatal.df$fatalities[1]

The county with the biggest fatality count is **orleans, in louisiana,
with 649 people killed.**

Worst injuries count
--------------------

    rm(cities.fatal.df) # house cleanning
    cities.inj.df <- arrange(cities.df, desc(injuries)) %>% ungroup(state, county) %>%
          mutate(rank = seq_len(length(injuries)),
                    prop.dmg = dollar(prop.dmg),
                    crop.dmg = dollar(crop.dmg)
                 )

    kable(cities.inj.df[1:10, c(7,1:6)], caption="Total injuries by county")

<table>
<caption>Total injuries by county</caption>
<thead>
<tr class="header">
<th align="right">rank</th>
<th align="left">state</th>
<th align="left">county</th>
<th align="right">fatalities</th>
<th align="right">injuries</th>
<th align="left">prop.dmg</th>
<th align="left">crop.dmg</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="right">1</td>
<td align="left">missouri</td>
<td align="left">st.louis</td>
<td align="right">65</td>
<td align="right">3144</td>
<td align="left">$1,461,882,880</td>
<td align="left">$10,500</td>
</tr>
<tr class="even">
<td align="right">2</td>
<td align="left">texas</td>
<td align="left">harris</td>
<td align="right">216</td>
<td align="right">2825</td>
<td align="left">$10,890,439,870</td>
<td align="left">$7,442,000</td>
</tr>
<tr class="odd">
<td align="right">3</td>
<td align="left">missouri</td>
<td align="left">st.louis.</td>
<td align="right">118</td>
<td align="right">2701</td>
<td align="left">$79,552,000</td>
<td align="left">$5,000</td>
</tr>
<tr class="even">
<td align="right">4</td>
<td align="left">texas</td>
<td align="left">wichita</td>
<td align="right">55</td>
<td align="right">1853</td>
<td align="left">$310,822,880</td>
<td align="left">$0</td>
</tr>
<tr class="odd">
<td align="right">5</td>
<td align="left">texas</td>
<td align="left">dallas</td>
<td align="right">149</td>
<td align="right">1757</td>
<td align="left">$1,946,192,730</td>
<td align="left">$1,405,000</td>
</tr>
<tr class="even">
<td align="right">6</td>
<td align="left">alabama</td>
<td align="left">jefferson</td>
<td align="right">117</td>
<td align="right">1699</td>
<td align="left">$2,037,082,100</td>
<td align="left">$3,355,000</td>
</tr>
<tr class="odd">
<td align="right">7</td>
<td align="left">massachusetts</td>
<td align="left">worcester</td>
<td align="right">96</td>
<td align="right">1292</td>
<td align="left">$286,072,530</td>
<td align="left">$0</td>
</tr>
<tr class="even">
<td align="right">8</td>
<td align="left">ohio</td>
<td align="left">greene</td>
<td align="right">40</td>
<td align="right">1278</td>
<td align="left">$289,967,757</td>
<td align="left">$540,000</td>
</tr>
<tr class="odd">
<td align="right">9</td>
<td align="left">missouri</td>
<td align="left">jasper</td>
<td align="right">178</td>
<td align="right">1273</td>
<td align="left">$2,864,021,330</td>
<td align="left">$46,475,500</td>
</tr>
<tr class="even">
<td align="right">10</td>
<td align="left">oklahoma</td>
<td align="left">oklahoma</td>
<td align="right">79</td>
<td align="right">1253</td>
<td align="left">$1,356,088,290</td>
<td align="left">$8,330,000</td>
</tr>
</tbody>
</table>

    #kable(cities.inj.df[1:10, c(7,1:6)])

    worst.inj.city.county <- cities.inj.df$county[1]
    worst.inj.city.st <- cities.inj.df$state[1]
    worst.inj.city.count <- cities.inj.df$injuries[1]

The county with the biggest injuries count is **st.louis, in missouri,
with 3144 people injuried.**

<!-- \newpage -->
Worst property losses
---------------------

    rm(cities.inj.df) # house cleanning

    cities.prop.df <- arrange(cities.df, desc(prop.dmg)) %>% ungroup(state, county) %>%
          mutate(rank = seq_len(length(prop.dmg)),
                    prop.dmg = dollar(prop.dmg),
                    crop.dmg = dollar(crop.dmg)
          )

    kable(cities.prop.df[1:10, c(7,1:6)], caption="Total property losses by county")

<table>
<caption>Total property losses by county</caption>
<thead>
<tr class="header">
<th align="right">rank</th>
<th align="left">state</th>
<th align="left">county</th>
<th align="right">fatalities</th>
<th align="right">injuries</th>
<th align="left">prop.dmg</th>
<th align="left">crop.dmg</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="right">1</td>
<td align="left">louisiana</td>
<td align="left">orleans</td>
<td align="right">649</td>
<td align="right">99</td>
<td align="left">$21,614,049,550</td>
<td align="left">$0</td>
</tr>
<tr class="even">
<td align="right">2</td>
<td align="left">texas</td>
<td align="left">harris</td>
<td align="right">216</td>
<td align="right">2825</td>
<td align="left">$10,890,439,870</td>
<td align="left">$7,442,000</td>
</tr>
<tr class="odd">
<td align="right">3</td>
<td align="left">florida</td>
<td align="left">.palm.beach</td>
<td align="right">5</td>
<td align="right">7</td>
<td align="left">$10,828,630,000</td>
<td align="left">$75,000,000</td>
</tr>
<tr class="even">
<td align="right">4</td>
<td align="left">mississippi</td>
<td align="left">harrison</td>
<td align="right">110</td>
<td align="right">90</td>
<td align="left">$8,870,659,460</td>
<td align="left">$0</td>
</tr>
<tr class="odd">
<td align="right">5</td>
<td align="left">new.jersey</td>
<td align="left">eastern.ocean</td>
<td align="right">16</td>
<td align="right">112</td>
<td align="left">$8,116,441,690</td>
<td align="left">$10</td>
</tr>
<tr class="even">
<td align="right">6</td>
<td align="left">new.jersey</td>
<td align="left">eastern.monmouth</td>
<td align="right">13</td>
<td align="right">397</td>
<td align="left">$6,527,278,550</td>
<td align="left">$0</td>
</tr>
<tr class="odd">
<td align="right">7</td>
<td align="left">louisiana</td>
<td align="left">st.tammany</td>
<td align="right">8</td>
<td align="right">89</td>
<td align="left">$5,677,622,950</td>
<td align="left">$0</td>
</tr>
<tr class="even">
<td align="right">8</td>
<td align="left">florida</td>
<td align="left">.escambia</td>
<td align="right">14</td>
<td align="right">0</td>
<td align="left">$5,632,695,000</td>
<td align="left">$25,300,000</td>
</tr>
<tr class="odd">
<td align="right">9</td>
<td align="left">texas</td>
<td align="left">galveston</td>
<td align="right">43</td>
<td align="right">259</td>
<td align="left">$5,358,909,770</td>
<td align="left">$109,602,000</td>
</tr>
<tr class="even">
<td align="right">10</td>
<td align="left">new.jersey</td>
<td align="left">western.monmouth</td>
<td align="right">6</td>
<td align="right">84</td>
<td align="left">$5,267,488,450</td>
<td align="left">$0</td>
</tr>
</tbody>
</table>

    #kable(cities.prop.df[1:10, c(7,1:6)])

    worst.prop.city.county <- cities.prop.df$county[1]
    worst.prop.city.st <- cities.prop.df$state[1]
    worst.prop.city.count <- cities.prop.df$prop.dmg[1]

The county with the biggest property losses is **orleans, in louisiana,
with $21,614,049,550 in losses.**

Worst crops losses
------------------

    rm(cities.prop.df) # house cleanning


    cities.crop.df <- arrange(cities.df, desc(crop.dmg)) %>% ungroup(state, county) %>%
          mutate(rank = seq_len(length(crop.dmg)),
                    prop.dmg = dollar(prop.dmg),
                    crop.dmg = dollar(crop.dmg)
          )

    kable(cities.crop.df[1:10, c(7,1:6)], caption="Total crops losses by county")

<table>
<caption>Total crops losses by county</caption>
<thead>
<tr class="header">
<th align="right">rank</th>
<th align="left">state</th>
<th align="left">county</th>
<th align="right">fatalities</th>
<th align="right">injuries</th>
<th align="left">prop.dmg</th>
<th align="left">crop.dmg</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="right">1</td>
<td align="left">texas</td>
<td align="left">lubbock</td>
<td align="right">37</td>
<td align="right">679</td>
<td align="left">$2,007,426,360</td>
<td align="left">$2,439,945,000</td>
</tr>
<tr class="even">
<td align="right">2</td>
<td align="left">texas</td>
<td align="left">montague</td>
<td align="right">5</td>
<td align="right">42</td>
<td align="left">$118,971,700</td>
<td align="left">$1,963,106,500</td>
</tr>
<tr class="odd">
<td align="right">3</td>
<td align="left">california</td>
<td align="left">northernsanjoaquin</td>
<td align="right">14</td>
<td align="right">25</td>
<td align="left">$5,948,500</td>
<td align="left">$1,520,000,000</td>
</tr>
<tr class="even">
<td align="right">4</td>
<td align="left">texas</td>
<td align="left">parmer</td>
<td align="right">1</td>
<td align="right">23</td>
<td align="left">$44,654,090</td>
<td align="left">$1,181,360,000</td>
</tr>
<tr class="odd">
<td align="right">5</td>
<td align="left">florida</td>
<td align="left">.dade</td>
<td align="right">10</td>
<td align="right">1</td>
<td align="left">$693,020,000</td>
<td align="left">$1,168,000,000</td>
</tr>
<tr class="even">
<td align="right">6</td>
<td align="left">california</td>
<td align="left">sesj</td>
<td align="right">28</td>
<td align="right">64</td>
<td align="left">$6,167,300</td>
<td align="left">$992,223,000</td>
</tr>
<tr class="odd">
<td align="right">7</td>
<td align="left">mississippi</td>
<td align="left">warren</td>
<td align="right">43</td>
<td align="right">341</td>
<td align="left">$312,674,880</td>
<td align="left">$728,657,000</td>
</tr>
<tr class="even">
<td align="right">8</td>
<td align="left">california</td>
<td align="left">ecentralsj</td>
<td align="right">43</td>
<td align="right">123</td>
<td align="left">$7,506,800</td>
<td align="left">$578,212,000</td>
</tr>
<tr class="odd">
<td align="right">9</td>
<td align="left">california</td>
<td align="left">southernsanjoaquin</td>
<td align="right">1</td>
<td align="right">22</td>
<td align="left">$18,657,000</td>
<td align="left">$517,800,000</td>
</tr>
<tr class="even">
<td align="right">10</td>
<td align="left">north.carolina</td>
<td align="left">alamance</td>
<td align="right">2</td>
<td align="right">8</td>
<td align="left">$3,005,157,200</td>
<td align="left">$503,166,000</td>
</tr>
</tbody>
</table>

    #kable(cities.crop.df[1:10, c(7,1:6)])


    worst.crop.city.county <- cities.crop.df$county[1]
    worst.crop.city.st <- cities.crop.df$state[1]
    worst.crop.city.count <- cities.crop.df$crop.dmg[1]

The county with the biggest croperty losses is **lubbock, in texas, with
$2,439,945,000 in losses.**

Results
=======

Population Health
-----------------

    rm(cities.df, cities.crop.df) # house cleanning

    # plist <- list(plt.fatal.single, plt.fatal.all, plt.inj.single, plt.inj.all)
    # n <- length(plist)
    # nCol <- floor(sqrt(n))
    # do.call("grid.arrange", c(plist, ncol=nCol))

    grid.arrange(plt.fatal.single, plt.fatal.all, plt.inj.single, plt.inj.all, 
                 nrow=2, ncol=2)
    grid.rect(gp=gpar(fill=NA))

![Population Health: fatalities and
injuries](readme_files/figure-markdown_strict/health-plot-1.png)

The single most fatal event was a **hurricane, that occurred in
louisiana, orleans, on 2005-08-28, killing 638 people.**

The most fatal event along the time is the **tornado. It has killed 5873
people until now.**

The single most injuring event was a **hurricane, that occurred in
texas, harris, on 2008-09-12, injuring 2400 people.**

The most injuring event along the time is the **tornado. It has injuried
94614 people until now.**

Economic Damages
----------------

    grid.arrange(plt.prop.single, plt.prop.all,
                 plt.crop.single, plt.crop.all,
                 nrow=2, ncol=2)
    grid.rect(gp=gpar(fill=NA))

![Economic Damages: property and
crops](readme_files/figure-markdown_strict/economic-plot-1.png)

The single most economic damaging event to properties was a **tide, that
occurred in louisiana, orleans, on 2005-08-29, causing U$
$17,900,000,000 in losses**.

The most property damaging event along the time is the **hurricane. It
has caused $87,005,170,310 in losses.**

The single most economic damaging event to crops was a **drought, that
occurred in california, northernsanjoaquin, on 2014-12-01, causing U$
$1,500,000,000 in losses**.

The most crop damaging event along the time is the **drought. It has
caused $27,454,862,620 in losses.**

Most aflicted locations
-----------------------

The county with the biggest fatality count is **orleans, in louisiana,
with 649 people killed.**

The county with the biggest injuries count is **st.louis, in missouri,
with 3144 people injuried.**

The county with the biggest property losses is **orleans, in louisiana,
with $21,614,049,550 in losses.**

The county with the biggest croperty losses is **lubbock, in texas, with
$2,439,945,000 in losses.**

Distribution of data
--------------------

    grid.arrange(plt.distr.fatal0, plt.distr.fatal1, plt.distr.inj0, plt.distr.inj1, 
                 nrow=1, ncol=4, 
                 bottom="Fatalities and injuries")
    grid.rect(gp=gpar(fill=NA))             

![Population
distribution](readme_files/figure-markdown_strict/distribution-1.png)

    grid.arrange(plt.distr.prop0, plt.distr.prop1, plt.distr.crop0, plt.distr.crop1, 
                 nrow=1, ncol=4, 
                 bottom="Property and crops losses")
    grid.rect(gp=gpar(fill=NA))   

![Population
distribution](readme_files/figure-markdown_strict/distribution-2.png)
