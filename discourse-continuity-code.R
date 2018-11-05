#######Load data
data = read.csv("discourse-continuity-data.csv")

#######Descriptive statistics
library(dplyr)
excludes <- data[data$dimension == "exclude" & data$egaze,]
incl_data <- data

#remove double-counting overlapping endpoints from excludes
for( i in 2:dim(excludes)[1]){
    if (excludes[i, "start"] < excludes[i-1, "end"] && excludes[i, "id"] == excludes[i-1, "id"]){
        print(excludes[(i-1):i,])
        excludes[i, "start"] = 1 + excludes[i-1, "end"]
    }
    if (excludes[i, "start"] == excludes[i-1, "end"]){
        print(excludes[(i-1):i,])
        excludes[i, "start"] = 1 + excludes[i, "start"]
    }
}
#remove gaze and hand events when excluded before counting durations
for( i in 1:dim(excludes)[1]){
    id <- excludes[i, "id"]
    start <- excludes[i, "start"]
    end <- excludes[i, "end"]
    egaze <- excludes[i, "egaze"]
    ehands <- excludes[i, "ehands"]
    if (egaze) {
        incl_data <- incl_data[incl_data$dimension != "gaze" | incl_data$id != id | incl_data$start > end | incl_data$end < start,]  
    }
    if (ehands){
        incl_data <- incl_data[incl_data$dimension != "hands" | incl_data$id != id | incl_data$start > end | incl_data$end < start,]  
    }
}

#count amounts of each code in each session
descriptive_stats <- incl_data %>%
    group_by(id) %>%
    summarize(
        transcription_time = (
            max(end * (dimension %in% c("utterance", "exclude_transcript")))
            - min(start)
            - sum(end * (dimension == "exclude_transcript"), na.rm = T)
            + sum(start * (dimension == "exclude_transcript"), na.rm = T)
            - sum(dimension == "exclude_transcript", na.rm = T)
        ),
        gaze_time = (
            max(end * (dimension %in% c("gaze", "exclude")))
            - min(start)
            - sum(end * (dimension == "exclude" & egaze), na.rm = T)
            + sum(start * (dimension == "exclude" & egaze), na.rm = T)
            - sum(dimension == "exclude" & egaze, na.rm = T)
        ),
        hands_time = (
            max(end * (dimension %in% c("hands", "exclude")))
            - min(start)
            - sum(end * (dimension == "exclude" & ehands), na.rm = T)
            + sum(start * (dimension == "exclude" & ehands), na.rm = T)
            - sum(dimension == "exclude" & ehands, na.rm = T)
        ),
        utts = 600*sum(dimension=="utterance")/transcription_time,
        d  = 600*sum(grepl("d", utype))/transcription_time,
        i  = 600*sum(grepl("i", utype))/transcription_time,
        q  = 600*sum(grepl("y|w", utype))/transcription_time,
        c  = 600*sum(grepl("c", utype))/transcription_time,
        a  = 600*sum(grepl("a", utype))/transcription_time,
        x  = 600*sum(grepl("x", utype))/transcription_time,
        n  = 600*sum(grepl("n", utype))/transcription_time,
        v  = 600*sum(grepl("v", utype))/transcription_time,
        p  = 600*sum(grepl("p", utype))/transcription_time,
        s  = 600*sum(grepl("s", utype))/transcription_time,
        toy = (
            (sum(end * (dimension == "gaze" & gtype=="object"), na.rm = T)
             - sum(start * (dimension == "gaze" & gtype=="object"), na.rm = T)
             + sum(dimension == "gaze" & gtype=="object", na.rm = T))
            /gaze_time
        ),
        face = (
            (sum(end * (dimension == "gaze" & gtype=="face"), na.rm = T)
             - sum(start * (dimension == "gaze" & gtype=="face"), na.rm = T)
             + sum(dimension == "gaze" & gtype=="face", na.rm = T))
            /gaze_time
        ),
        other = 1-toy-face,
        hands_one = (
            (sum(end * (htype == 1), na.rm = T)
             - sum(start * (htype == 1), na.rm = T)
             + sum(htype == 1, na.rm = T))
            /hands_time
        ),
        hands_two = (
            (sum(end * (htype == 2), na.rm = T)
             - sum(start * (htype == 2), na.rm = T)
             + sum(htype == 2, na.rm = T))
            /hands_time
        ),
        hands_zero = 1-hands_one-hands_two
        
    )
descriptive_stats <- data.frame(descriptive_stats)

#get descriptive stats for each code
apply(descriptive_stats, MARGIN  = 2, FUN= mean)
apply(descriptive_stats, MARGIN  = 2, FUN= sd)
apply(descriptive_stats, MARGIN  = 2, FUN= min)
apply(descriptive_stats, MARGIN  = 2, FUN= max)

#get co-occurrence rates of utterance types
patterns <- c("d", "i", "y|w", "c", "a", "x", "n", "v", "p", "s")
co = numeric(100)
i = 1
for (a in patterns){
    for (b in patterns){
        co[i] <- sum(grepl(a, data$utype) & grepl(b, data$utype))/sum(grepl(a, data$utype))
        i = 1+i
    }
}

######## Logistic regression sequence modeling

# first process data to represent prev_utype, gaze, hands, utype
library('stringr')
max_gap = 50 # 5 seconds
#max_gap = 99999 # all consecutive utterances considered
uttdata <- data[data$dimension == 'utterance',]
num_utts <- dim(uttdata)[1]
prev_utype = character(num_utts)
gaze = character(num_utts)
hands = character(num_utts)
utype = character(num_utts)
ids = character(num_utts)
time_diff = numeric(num_utts)
exact_repeat = logical(num_utts)
prop_words_repeated = numeric(num_utts)
gaze_shift = logical(num_utts)
hand_shift = logical(num_utts)
for (i in 1:dim(uttdata)[1]){
    #record the id
    ids[i] <- uttdata$id[i]
    # Find the prev_utype
    prev_utterance_within_threshold = T
    if (
        i == 1 
        || uttdata$id[i] != uttdata$id[i-1]
        || uttdata$start[i] > uttdata$end[i-1]+max_gap
    ){
        #no previous utterance
        prev_utype[i] = ""
        exact_repeat[i] = F
        prop_words_repeated[i] = NA
        gaze_shift[i] = NA
        hand_shift[i] = NA
        time_diff[i] = NA
        prev_utterance_within_threshold = F
    } else {
        prev_utype[i] = toString(uttdata$utype[i-1])
        exact_repeat[i] = toString(uttdata$utxt[i-1]) == toString(uttdata$utxt[i])
        prev_words = strsplit(str_replace_all(uttdata$utxt[i-1], "[!.?,]", ""), " ")[[1]]
        next_words = strsplit(str_replace_all(uttdata$utxt[i], "[!.?,]", ""), " ")[[1]]
        prop_words_repeated[i] = mean(next_words %in% prev_words)
    }
    #find the utype
    utype[i] <- toString(uttdata$utype[i])
    #Find the gaze and hands
    time = uttdata[i, "start"]
    id = uttdata[i, "id"]
    enclosing_hand_event_index <- min(which(
        data$dimension=='hands'
        & data$id == id
        & data$start <= time
        & data$end >= time))
    if(enclosing_hand_event_index == Inf){hands[i] <- '0'}
    else if(data[enclosing_hand_event_index, "htype"]==1){hands[i]='1'}
    else if(data[enclosing_hand_event_index, "htype"]==2){hands[i]='2'}
    enclosing_gaze_event_index <- min(which(
        data$dimension=='gaze'
        & data$id == id
        & data$start <= time
        & data$end >= time))
    if(enclosing_gaze_event_index == Inf){gaze[i] <- 'n'}
    else if(data[enclosing_gaze_event_index, "gtype"]=="face"){
        gaze[i]='f'
    }
    else if(data[enclosing_gaze_event_index, "gtype"]=="object"){
        gaze[i]='o'
    }
    if(prev_utterance_within_threshold){
        time_diff[i] <- uttdata[i, "start"] - uttdata[i-1, "end"]
        time = uttdata[i-1, "start"]
        prev_enclosing_hand_event_index = min(which(
            data$dimension=='hands'
            & data$id == id
            & data$start <= time
            & data$end >= time))
        prev_enclosing_gaze_event_index <- min(which(
            data$dimension=='gaze'
            & data$id == id
            & data$start <= time
            & data$end >= time))
        #check for gaze or hand shifts between previous and current utterance
        if (prev_enclosing_hand_event_index == Inf){
            before_hands = ""
            before_htype = "0"
        }
        else {
            before_hands = toString(data[prev_enclosing_hand_event_index, "hobj"])
            before_htype = toString(data[prev_enclosing_hand_event_index, "htype"])
        }
        if (enclosing_hand_event_index == Inf){
            after_hands = ""
            after_htype = "0"
        }
        else {
            after_hands = toString(data[enclosing_hand_event_index, "hobj"])
            after_htype = toString(data[enclosing_hand_event_index, "htype"])
        }
        if (prev_enclosing_gaze_event_index == Inf){
            before_gaze = ""
            before_gtype = "Other"
        }
        else {
            before_gaze = toString(data[prev_enclosing_gaze_event_index, "gobj"])
            before_gtype = toString(data[prev_enclosing_gaze_event_index, "gtype"])
        }
        if (enclosing_gaze_event_index == Inf){
            after_gaze = ""
            after_gtype = "Other"
        }
        else {
            after_gaze = toString(data[enclosing_gaze_event_index, "gobj"])
            after_gtype = toString(data[enclosing_gaze_event_index, "gtype"])
        }
        #split before_hands and after_hands into components
        before_hands = c(
            grepl("rin", before_hands),grepl("blo", before_hands),grepl("cup", before_hands),
            grepl("bal", before_hands),grepl("duc", before_hands),grepl("tur", before_hands),
            grepl("dol", before_hands),grepl("boa", before_hands),grepl("par", before_hands),
            grepl("oth", before_hands)
        )
        after_hands = c(
            grepl("rin", after_hands),grepl("blo", after_hands),grepl("cup", after_hands),
            grepl("bal", after_hands),grepl("duc", after_hands),grepl("tur", after_hands),
            grepl("dol", after_hands),grepl("boa", after_hands),grepl("par", after_hands),
            grepl("oth", after_hands)
        )
        before_gaze = c(
            grepl("rin", before_gaze),grepl("blo", before_gaze),grepl("cup", before_gaze),
            grepl("bal", before_gaze),grepl("duc", before_gaze),grepl("tur", before_gaze),
            grepl("dol", before_gaze),grepl("boa", before_gaze),grepl("par", before_gaze),
            grepl("oth", before_gaze),grepl("amb", before_gaze)
        )
        after_gaze = c(
            grepl("rin", after_gaze),grepl("blo", after_gaze),grepl("cup", after_gaze),
            grepl("bal", after_gaze),grepl("duc", after_gaze),grepl("tur", after_gaze),
            grepl("dol", after_gaze),grepl("boa", after_gaze),grepl("par", after_gaze),
            grepl("oth", after_gaze),grepl("amb", after_gaze)
        )
        #check if the set of components is identical
        gaze_shift[i] = (!all(before_gaze == after_gaze)) || before_gtype != after_gtype
        hand_shift[i] = (!all(before_hands == after_hands)) || before_htype != after_htype
    }
    
}
logreg_data <- data.frame(prev_utype, utype, exact_repeat, prop_words_repeated, gaze_shift, hand_shift, time_diff)
logreg_data$id <- ids
logreg_data$time <- uttdata$start
logreg_data$d <- grepl("d", logreg_data$prev_utype, fixed = T)
logreg_data$i <- grepl("i", logreg_data$prev_utype, fixed = T)
logreg_data$q <- grepl("w|y", logreg_data$prev_utype)
logreg_data$c <- grepl("c", logreg_data$prev_utype, fixed = T)
logreg_data$a <- grepl("a", logreg_data$prev_utype, fixed = T)
logreg_data$x <- grepl("x", logreg_data$prev_utype, fixed = T)
logreg_data$n <- grepl("n", logreg_data$prev_utype, fixed = T)
logreg_data$v <- grepl("v", logreg_data$prev_utype, fixed = T)
logreg_data$p <- grepl("p", logreg_data$prev_utype, fixed = T)
logreg_data$s <- grepl("s", logreg_data$prev_utype, fixed = T)
logreg_data$gap <- logreg_data$prev_utype == ""
#columns for following content type
logreg_data$next_d <- grepl("d", logreg_data$utype, fixed = T)
logreg_data$next_i <- grepl("i", logreg_data$utype, fixed = T)
logreg_data$next_q <- grepl("w|y", logreg_data$utype)
logreg_data$next_c <- grepl("c", logreg_data$utype, fixed = T)
logreg_data$next_a <- grepl("a", logreg_data$utype, fixed = T)
logreg_data$next_x <- grepl("x", logreg_data$utype, fixed = T)
logreg_data$next_n <- grepl("n", logreg_data$utype, fixed = T)
logreg_data$next_v <- grepl("v", logreg_data$utype, fixed = T)
logreg_data$next_p <- grepl("p", logreg_data$utype, fixed = T)
logreg_data$next_s <- grepl("s", logreg_data$utype, fixed = T)
#add in gaze and hand data
utterance_data = data[data$dimension == "utterance", c("id", "start", "utype")]
#separate column for each content type t/f
utterance_data$d = grepl("d",utterance_data$utype)
utterance_data$i = grepl("i",utterance_data$utype)
utterance_data$q = grepl("y|w",utterance_data$utype)
utterance_data$c = grepl("c",utterance_data$utype)
utterance_data$a = grepl("a",utterance_data$utype)
utterance_data$x = grepl("x",utterance_data$utype)
utterance_data$n = grepl("n",utterance_data$utype)
utterance_data$v = grepl("v",utterance_data$utype)
utterance_data$p = grepl("p",utterance_data$utype)
utterance_data$s = grepl("s",utterance_data$utype)
utterance_data$u = grepl("u",utterance_data$utype)
#find gaze and hand state for each start time
gaze_states = character(dim(utterance_data)[1])
hand_states = character(dim(utterance_data)[1])
for(i in 1:dim(utterance_data)[1]){
    print(i)
    id = utterance_data$id[i]
    time = utterance_data$start[i]
    #grab gaze state
    if (dim(data[data$id == id & data$dimension == "exclude" & data$start <= time & data$end >= time & data$egaze,])[1] > 0){
        #gaze excluded
        gaze_state = NA
    }
    else{
        gaze_state = data[data$id == id & data$dimension == "gaze" & data$start <= time & data$end >= time,"gtype"]
        if(length(gaze_state) == 0){
            gaze_state = "other"
        }
        else if(gaze_state != "object" && gaze_state != "face"){
            gaze_state = "other"
        }
    }
    #grab hand state
    if (dim(data[data$id == id & data$dimension == "exclude" & data$start <= time & data$end >= time & data$ehands,])[1] > 0){
        #gaze excluded
        hand_state = NA
    }
    else{
        hand_state = data[data$id == id & data$dimension == "hands" & data$start <= time & data$end >= time,"htype"]
        if(length(hand_state) == 0){
            hand_state = "0"
        }
        if(hand_state != "1" && hand_state != "2"){
            hand_state = "0"
        }
    }
    #record
    gaze_states[i] = as.character(gaze_state)
    hand_states[i] = as.character(hand_state)
}
utterance_data$gaze = gaze_states
utterance_data$hands = hand_states
#combine cross-modal and transition datasets
logreg_data$gaze = utterance_data$gaze
logreg_data$hands = utterance_data$hands


#mixed effects regression analysis of transitions
#go through each utterance type and model it with all other types as predictors,
#and then again removing each utterance type from the predictor list.
#use the anova function to perform likelihood ratio tests.
#if 5-second cutoff removed, results are the same except S->X becomes significant, X->P goes from marginal to nothing
#and P->Q goes from nothing to marginal
library(lme4)
d_trans = glmer(next_d ~ d+i+q+c+a+x+n+v+p+s + (1 | id), data = logreg_data, 
                family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
d_d = glmer(next_d ~ i+q+c+a+x+n+v+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
d_i = glmer(next_d ~ d+q+c+a+x+n+v+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
d_q = glmer(next_d ~ d+i+c+a+x+n+v+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
d_c = glmer(next_d ~ d+i+q+a+x+n+v+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
d_a = glmer(next_d ~ d+i+q+c+x+n+v+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
d_x = glmer(next_d ~ d+i+q+c+a+n+v+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
d_n = glmer(next_d ~ d+i+q+c+a+x+v+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
d_v = glmer(next_d ~ d+i+q+c+a+x+n+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
d_p = glmer(next_d ~ d+i+q+c+a+x+n+v+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
d_s = glmer(next_d ~ d+i+q+c+a+x+n+v+p + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
anova(d_d, d_trans)
anova(d_i, d_trans)
anova(d_q, d_trans)
anova(d_c, d_trans)
anova(d_a, d_trans)
anova(d_x, d_trans)
anova(d_n, d_trans)
anova(d_v, d_trans)
anova(d_p, d_trans)
anova(d_s, d_trans)

i_trans = glmer(next_i ~ d+i+q+c+a+x+n+v+p+s + (1 | id), data = logreg_data, 
                family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
i_d = glmer(next_i ~ i+q+c+a+x+n+v+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
i_i = glmer(next_i ~ d+q+c+a+x+n+v+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
i_q = glmer(next_i ~ d+i+c+a+x+n+v+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
i_c = glmer(next_i ~ d+i+q+a+x+n+v+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
i_a = glmer(next_i ~ d+i+q+c+x+n+v+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
i_x = glmer(next_i ~ d+i+q+c+a+n+v+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
i_n = glmer(next_i ~ d+i+q+c+a+x+v+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
i_v = glmer(next_i ~ d+i+q+c+a+x+n+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
i_p = glmer(next_i ~ d+i+q+c+a+x+n+v+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
i_s = glmer(next_i ~ d+i+q+c+a+x+n+v+p + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
anova(i_d, i_trans)
anova(i_i, i_trans)
anova(i_q, i_trans)
anova(i_c, i_trans)
anova(i_a, i_trans)
anova(i_x, i_trans)
anova(i_n, i_trans)
anova(i_v, i_trans)
anova(i_p, i_trans)
anova(i_s, i_trans)

q_trans = glmer(next_q ~ d+i+q+c+a+x+n+v+p+s + (1 | id), data = logreg_data, 
                family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
q_d = glmer(next_q ~ i+q+c+a+x+n+v+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
q_i = glmer(next_q ~ d+q+c+a+x+n+v+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
q_q = glmer(next_q ~ d+i+c+a+x+n+v+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
q_c = glmer(next_q ~ d+i+q+a+x+n+v+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
q_a = glmer(next_q ~ d+i+q+c+x+n+v+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
q_x = glmer(next_q ~ d+i+q+c+a+n+v+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
q_n = glmer(next_q ~ d+i+q+c+a+x+v+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
q_v = glmer(next_q ~ d+i+q+c+a+x+n+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
q_p = glmer(next_q ~ d+i+q+c+a+x+n+v+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
q_s = glmer(next_q ~ d+i+q+c+a+x+n+v+p + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
anova(q_d, q_trans)
anova(q_i, q_trans)
anova(q_q, q_trans)
anova(q_c, q_trans)
anova(q_a, q_trans)
anova(q_x, q_trans)
anova(q_n, q_trans)
anova(q_v, q_trans)
anova(q_p, q_trans)
anova(q_s, q_trans)

c_trans = glmer(next_c ~ d+i+q+c+a+x+n+v+p+s + (1 | id), data = logreg_data, 
                family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
c_d = glmer(next_c ~ i+q+c+a+x+n+v+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
c_i = glmer(next_c ~ d+q+c+a+x+n+v+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
c_q = glmer(next_c ~ d+i+c+a+x+n+v+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
c_c = glmer(next_c ~ d+i+q+a+x+n+v+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
c_a = glmer(next_c ~ d+i+q+c+x+n+v+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
c_x = glmer(next_c ~ d+i+q+c+a+n+v+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
c_n = glmer(next_c ~ d+i+q+c+a+x+v+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
c_v = glmer(next_c ~ d+i+q+c+a+x+n+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
c_p = glmer(next_c ~ d+i+q+c+a+x+n+v+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
c_s = glmer(next_c ~ d+i+q+c+a+x+n+v+p + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
anova(c_d, c_trans)
anova(c_i, c_trans)
anova(c_q, c_trans)
anova(c_c, c_trans)
anova(c_a, c_trans)
anova(c_x, c_trans)
anova(c_n, c_trans)
anova(c_v, c_trans)
anova(c_p, c_trans)
anova(c_s, c_trans)

a_trans = glmer(next_a ~ d+i+q+c+a+x+n+v+p+s + (1 | id), data = logreg_data, 
                family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
a_d = glmer(next_a ~ i+q+c+a+x+n+v+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
a_i = glmer(next_a ~ d+q+c+a+x+n+v+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
a_q = glmer(next_a ~ d+i+c+a+x+n+v+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
a_c = glmer(next_a ~ d+i+q+a+x+n+v+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
a_a = glmer(next_a ~ d+i+q+c+x+n+v+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
a_x = glmer(next_a ~ d+i+q+c+a+n+v+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
a_n = glmer(next_a ~ d+i+q+c+a+x+v+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
a_v = glmer(next_a ~ d+i+q+c+a+x+n+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
a_p = glmer(next_a ~ d+i+q+c+a+x+n+v+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
a_s = glmer(next_a ~ d+i+q+c+a+x+n+v+p + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
anova(a_d, a_trans)
anova(a_i, a_trans)
anova(a_q, a_trans)
anova(a_c, a_trans)
anova(a_a, a_trans)
anova(a_x, a_trans)
anova(a_n, a_trans)
anova(a_v, a_trans)
anova(a_p, a_trans)
anova(a_s, a_trans)

x_trans = glmer(next_x ~ d+i+q+c+a+x+n+v+p+s + (1 | id), data = logreg_data, 
                family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
x_d = glmer(next_x ~ i+q+c+a+x+n+v+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
x_i = glmer(next_x ~ d+q+c+a+x+n+v+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
x_q = glmer(next_x ~ d+i+c+a+x+n+v+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
x_c = glmer(next_x ~ d+i+q+a+x+n+v+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
x_a = glmer(next_x ~ d+i+q+c+x+n+v+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
x_x = glmer(next_x ~ d+i+q+c+a+n+v+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
x_n = glmer(next_x ~ d+i+q+c+a+x+v+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
x_v = glmer(next_x ~ d+i+q+c+a+x+n+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
x_p = glmer(next_x ~ d+i+q+c+a+x+n+v+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
x_s = glmer(next_x ~ d+i+q+c+a+x+n+v+p + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
anova(x_d, x_trans)
anova(x_i, x_trans)
anova(x_q, x_trans)
anova(x_c, x_trans)
anova(x_a, x_trans)
anova(x_x, x_trans)
anova(x_n, x_trans)
anova(x_v, x_trans)
anova(x_p, x_trans)
anova(x_s, x_trans)

n_trans = glmer(next_n ~ d+i+q+c+a+x+n+v+p+s + (1 | id), data = logreg_data, 
                family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
n_d = glmer(next_n ~ i+q+c+a+x+n+v+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
n_i = glmer(next_n ~ d+q+c+a+x+n+v+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
n_q = glmer(next_n ~ d+i+c+a+x+n+v+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
n_c = glmer(next_n ~ d+i+q+a+x+n+v+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
n_a = glmer(next_n ~ d+i+q+c+x+n+v+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
n_x = glmer(next_n ~ d+i+q+c+a+n+v+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
n_n = glmer(next_n ~ d+i+q+c+a+x+v+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
n_v = glmer(next_n ~ d+i+q+c+a+x+n+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
n_p = glmer(next_n ~ d+i+q+c+a+x+n+v+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
n_s = glmer(next_n ~ d+i+q+c+a+x+n+v+p + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
anova(n_d, n_trans)
anova(n_i, n_trans)
anova(n_q, n_trans)
anova(n_c, n_trans)
anova(n_a, n_trans)
anova(n_x, n_trans)
anova(n_n, n_trans)
anova(n_v, n_trans)
anova(n_p, n_trans)
anova(n_s, n_trans)

v_trans = glmer(next_v ~ d+i+q+c+a+x+n+v+p+s + (1 | id), data = logreg_data, 
                family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
v_d = glmer(next_v ~ i+q+c+a+x+n+v+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
v_i = glmer(next_v ~ d+q+c+a+x+n+v+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
v_q = glmer(next_v ~ d+i+c+a+x+n+v+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
v_c = glmer(next_v ~ d+i+q+a+x+n+v+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
v_a = glmer(next_v ~ d+i+q+c+x+n+v+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
v_x = glmer(next_v ~ d+i+q+c+a+n+v+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
v_n = glmer(next_v ~ d+i+q+c+a+x+v+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
v_v = glmer(next_v ~ d+i+q+c+a+x+n+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
v_p = glmer(next_v ~ d+i+q+c+a+x+n+v+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
v_s = glmer(next_v ~ d+i+q+c+a+x+n+v+p + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
anova(v_d, v_trans)
anova(v_i, v_trans)
anova(v_q, v_trans)
anova(v_c, v_trans)
anova(v_a, v_trans)
anova(v_x, v_trans)
anova(v_n, v_trans)
anova(v_v, v_trans)
anova(v_p, v_trans)
anova(v_s, v_trans)

p_trans = glmer(next_p ~ d+i+q+c+a+x+n+v+p+s + (1 | id), data = logreg_data, 
                family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
p_d = glmer(next_p ~ i+q+c+a+x+n+v+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
p_i = glmer(next_p ~ d+q+c+a+x+n+v+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
p_q = glmer(next_p ~ d+i+c+a+x+n+v+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
p_c = glmer(next_p ~ d+i+q+a+x+n+v+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
p_a = glmer(next_p ~ d+i+q+c+x+n+v+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
p_x = glmer(next_p ~ d+i+q+c+a+n+v+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
p_n = glmer(next_p ~ d+i+q+c+a+x+v+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
p_v = glmer(next_p ~ d+i+q+c+a+x+n+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
p_p = glmer(next_p ~ d+i+q+c+a+x+n+v+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
p_s = glmer(next_p ~ d+i+q+c+a+x+n+v+p + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
anova(p_d, p_trans)
anova(p_i, p_trans)
anova(p_q, p_trans)
anova(p_c, p_trans)
anova(p_a, p_trans)
anova(p_x, p_trans)
anova(p_n, p_trans)
anova(p_v, p_trans)
anova(p_p, p_trans)
anova(p_s, p_trans)

s_trans = glmer(next_s ~ d+i+q+c+a+x+n+v+p+s + (1 | id), data = logreg_data, 
                family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
s_d = glmer(next_s ~ i+q+c+a+x+n+v+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
s_i = glmer(next_s ~ d+q+c+a+x+n+v+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
s_q = glmer(next_s ~ d+i+c+a+x+n+v+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
s_c = glmer(next_s ~ d+i+q+a+x+n+v+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
s_a = glmer(next_s ~ d+i+q+c+x+n+v+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
s_x = glmer(next_s ~ d+i+q+c+a+n+v+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
s_n = glmer(next_s ~ d+i+q+c+a+x+v+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
s_v = glmer(next_s ~ d+i+q+c+a+x+n+p+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
s_p = glmer(next_s ~ d+i+q+c+a+x+n+v+s + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
s_s = glmer(next_s ~ d+i+q+c+a+x+n+v+p + (1 | id), data = logreg_data, 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
anova(s_d, s_trans)
anova(s_i, s_trans)
anova(s_q, s_trans)
anova(s_c, s_trans)
anova(s_a, s_trans)
anova(s_x, s_trans)
anova(s_n, s_trans)
anova(s_v, s_trans)
anova(s_p, s_trans)
anova(s_s, s_trans)

#check if robust to exact repeats
#same as above, except when exact repeat, remove from regression
#i becomes marginal and p goes away, all others are robust
d_rep = glmer(next_d ~ d+i+q+c+a+x+n+v+p+s + (1 | id), data = logreg_data[!logreg_data$exact_repeat,], # p < .001
              family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
i_rep = glmer(next_i ~ d+i+q+c+a+x+n+v+p+s + (1 | id), data = logreg_data[!logreg_data$exact_repeat,], # p < .05 (MARGINAL)
              family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
q_rep = glmer(next_q ~ d+i+q+c+a+x+n+v+p+s + (1 | id), data = logreg_data[!logreg_data$exact_repeat,], # p < .001
              family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
c_rep = glmer(next_c ~ d+i+q+c+a+x+n+v+p+s + (1 | id), data = logreg_data[!logreg_data$exact_repeat,], # p < .001
              family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
a_rep = glmer(next_a ~ d+i+q+c+a+x+n+v+p+s + (1 | id), data = logreg_data[!logreg_data$exact_repeat,], # p < .001
              family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
x_rep = glmer(next_x ~ d+i+q+c+a+x+n+v+p+s + (1 | id), data = logreg_data[!logreg_data$exact_repeat,], # p < .001
              family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
n_rep = glmer(next_n ~ d+i+q+c+a+x+n+v+p+s + (1 | id), data = logreg_data[!logreg_data$exact_repeat,], # p < .001
              family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
v_rep = glmer(next_v ~ d+i+q+c+a+x+n+v+p+s + (1 | id), data = logreg_data[!logreg_data$exact_repeat,], # p < .01
              family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
p_rep = glmer(next_p ~ d+i+q+c+a+x+n+v+p+s + (1 | id), data = logreg_data[!logreg_data$exact_repeat,], # ns
              family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
s_rep = glmer(next_s ~ d+i+q+c+a+x+n+v+p+s + (1 | id), data = logreg_data[!logreg_data$exact_repeat,], # p < .001
              family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)

d_rep_base = glmer(next_d ~ i+q+c+a+x+n+v+p+s + (1 | id), data = logreg_data[!logreg_data$exact_repeat,], # p < .001
                   family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
i_rep_base = glmer(next_i ~ d+q+c+a+x+n+v+p+s + (1 | id), data = logreg_data[!logreg_data$exact_repeat,], # p < .05 (MARGINAL)
                   family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
q_rep_base = glmer(next_q ~ d+i+c+a+x+n+v+p+s + (1 | id), data = logreg_data[!logreg_data$exact_repeat,], # p < .001
                   family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
c_rep_base = glmer(next_c ~ d+i+q+a+x+n+v+p+s + (1 | id), data = logreg_data[!logreg_data$exact_repeat,], # p < .001
                   family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
a_rep_base = glmer(next_a ~ d+i+q+c+x+n+v+p+s + (1 | id), data = logreg_data[!logreg_data$exact_repeat,], # p < .001
                   family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
x_rep_base = glmer(next_x ~ d+i+q+c+a+n+v+p+s + (1 | id), data = logreg_data[!logreg_data$exact_repeat,], # p < .001
                   family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
n_rep_base = glmer(next_n ~ d+i+q+c+a+x+v+p+s + (1 | id), data = logreg_data[!logreg_data$exact_repeat,], # p < .001
                   family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
v_rep_base = glmer(next_v ~ d+i+q+c+a+x+n+p+s + (1 | id), data = logreg_data[!logreg_data$exact_repeat,], # p < .01
                   family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
p_rep_base = glmer(next_p ~ d+i+q+c+a+x+n+v+s + (1 | id), data = logreg_data[!logreg_data$exact_repeat,], # ns
                   family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
s_rep_base = glmer(next_s ~ d+i+q+c+a+x+n+v+p + (1 | id), data = logreg_data[!logreg_data$exact_repeat,], # p < .001
                   family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)

anova(d_rep_base, d_rep)
anova(i_rep_base, i_rep)
anova(q_rep_base, q_rep)
anova(c_rep_base, c_rep)
anova(a_rep_base, a_rep)
anova(x_rep_base, x_rep)
anova(n_rep_base, n_rep)
anova(v_rep_base, v_rep)
anova(p_rep_base, p_rep)
anova(s_rep_base, s_rep)

#next analysis: do repeats depend on infant gaze/hand shifts?
#models with gaze and no hands
shift_d_gaze = glmer(next_d ~ time_diff + gaze_shift + (1 | id), data = logreg_data[logreg_data$d & !is.na(logreg_data$gaze) & !is.na(logreg_data$hands),], 
                     family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)

shift_i_gaze = glmer(next_i ~ time_diff + gaze_shift + (1 | id), data = logreg_data[logreg_data$i & !is.na(logreg_data$gaze) & !is.na(logreg_data$hands),], 
                     family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)

shift_q_gaze = glmer(next_q ~ time_diff + gaze_shift + (1 | id), data = logreg_data[logreg_data$q & !is.na(logreg_data$gaze) & !is.na(logreg_data$hands),], 
                     family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)

shift_c_gaze = glmer(next_c ~ time_diff + gaze_shift + (1 | id), data = logreg_data[logreg_data$c & !is.na(logreg_data$gaze) & !is.na(logreg_data$hands),], 
                     family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)

shift_a_gaze = glmer(next_a ~ time_diff + gaze_shift + (1 | id), data = logreg_data[logreg_data$a & !is.na(logreg_data$gaze) & !is.na(logreg_data$hands),], 
                     family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)

shift_x_gaze = glmer(next_x ~ time_diff + gaze_shift + (1 | id), data = logreg_data[logreg_data$x & !is.na(logreg_data$gaze) & !is.na(logreg_data$hands),], 
                     family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)

shift_n_gaze = glmer(next_n ~ time_diff + gaze_shift + (1 | id), data = logreg_data[logreg_data$n & !is.na(logreg_data$gaze) & !is.na(logreg_data$hands),], 
                     family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)

shift_v_gaze = glmer(next_v ~ time_diff + gaze_shift + (1 | id), data = logreg_data[logreg_data$v & !is.na(logreg_data$gaze) & !is.na(logreg_data$hands),], 
                     family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)

shift_p_gaze = glmer(next_p ~ time_diff + gaze_shift + (1 | id), data = logreg_data[logreg_data$p & !is.na(logreg_data$gaze) & !is.na(logreg_data$hands),], 
                     family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)

shift_s_gaze = glmer(next_s ~ time_diff + gaze_shift + (1 | id), data = logreg_data[logreg_data$s & !is.na(logreg_data$gaze) & !is.na(logreg_data$hands),], 
                     family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)

#models with hands and no gaze
shift_d_hands = glmer(next_d ~ time_diff + hand_shift + (1 | id), data = logreg_data[logreg_data$d & !is.na(logreg_data$gaze) & !is.na(logreg_data$hands),], 
                      family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)

shift_i_hands = glmer(next_i ~ time_diff + hand_shift + (1 | id), data = logreg_data[logreg_data$i & !is.na(logreg_data$gaze) & !is.na(logreg_data$hands),], 
                      family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)

shift_q_hands = glmer(next_q ~ time_diff + hand_shift + (1 | id), data = logreg_data[logreg_data$q & !is.na(logreg_data$gaze) & !is.na(logreg_data$hands),], 
                      family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)

shift_c_hands = glmer(next_c ~ time_diff + hand_shift + (1 | id), data = logreg_data[logreg_data$c & !is.na(logreg_data$gaze) & !is.na(logreg_data$hands),], 
                      family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)

shift_a_hands = glmer(next_a ~ time_diff + hand_shift + (1 | id), data = logreg_data[logreg_data$a & !is.na(logreg_data$gaze) & !is.na(logreg_data$hands),], 
                      family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)

shift_x_hands = glmer(next_x ~ time_diff + hand_shift + (1 | id), data = logreg_data[logreg_data$x & !is.na(logreg_data$gaze) & !is.na(logreg_data$hands),], 
                      family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)

shift_n_hands = glmer(next_n ~ time_diff + hand_shift + (1 | id), data = logreg_data[logreg_data$n & !is.na(logreg_data$gaze) & !is.na(logreg_data$hands),], 
                      family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)

shift_v_hands = glmer(next_v ~ time_diff + hand_shift + (1 | id), data = logreg_data[logreg_data$v & !is.na(logreg_data$gaze) & !is.na(logreg_data$hands),], 
                      family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)

shift_p_hands = glmer(next_p ~ time_diff + hand_shift + (1 | id), data = logreg_data[logreg_data$p & !is.na(logreg_data$gaze) & !is.na(logreg_data$hands),], 
                      family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)

shift_s_hands = glmer(next_s ~ time_diff + hand_shift + (1 | id), data = logreg_data[logreg_data$s & !is.na(logreg_data$gaze) & !is.na(logreg_data$hands),], 
                      family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)

#models with time removed to test significance of delay time
shift_d_notime = glmer(next_d ~ gaze_shift + hand_shift + (1 | id), data = logreg_data[logreg_data$d & !is.na(logreg_data$gaze) & !is.na(logreg_data$hands),], 
                       family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)

shift_i_notime = glmer(next_i ~ gaze_shift + hand_shift + (1 | id), data = logreg_data[logreg_data$i & !is.na(logreg_data$gaze) & !is.na(logreg_data$hands),], 
                       family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)

shift_q_notime = glmer(next_q ~ gaze_shift + hand_shift + (1 | id), data = logreg_data[logreg_data$q & !is.na(logreg_data$gaze) & !is.na(logreg_data$hands),], 
                       family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)

shift_c_notime = glmer(next_c ~ gaze_shift + hand_shift + (1 | id), data = logreg_data[logreg_data$c & !is.na(logreg_data$gaze) & !is.na(logreg_data$hands),], 
                       family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)

shift_a_notime = glmer(next_a ~ gaze_shift + hand_shift + (1 | id), data = logreg_data[logreg_data$a & !is.na(logreg_data$gaze) & !is.na(logreg_data$hands),], 
                       family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)

shift_x_notime = glmer(next_x ~ gaze_shift + hand_shift + (1 | id), data = logreg_data[logreg_data$x & !is.na(logreg_data$gaze) & !is.na(logreg_data$hands),], 
                       family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)

shift_n_notime = glmer(next_n ~ gaze_shift + hand_shift + (1 | id), data = logreg_data[logreg_data$n & !is.na(logreg_data$gaze) & !is.na(logreg_data$hands),], 
                       family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)

shift_v_notime = glmer(next_v ~ gaze_shift + hand_shift + (1 | id), data = logreg_data[logreg_data$v & !is.na(logreg_data$gaze) & !is.na(logreg_data$hands),], 
                       family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)

shift_p_notime = glmer(next_p ~ gaze_shift + hand_shift + (1 | id), data = logreg_data[logreg_data$p & !is.na(logreg_data$gaze) & !is.na(logreg_data$hands),], 
                       family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)

shift_s_notime = glmer(next_s ~ gaze_shift + hand_shift + (1 | id), data = logreg_data[logreg_data$s & !is.na(logreg_data$gaze) & !is.na(logreg_data$hands),], 
                       family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)

#full models
shift_d = glmer(next_d ~ time_diff + gaze_shift + hand_shift + (1 | id), data = logreg_data[logreg_data$d & !is.na(logreg_data$gaze) & !is.na(logreg_data$hands),], 
                family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
summary(shift_d)

shift_i = glmer(next_i ~ time_diff + gaze_shift + hand_shift + (1 | id), data = logreg_data[logreg_data$i & !is.na(logreg_data$gaze) & !is.na(logreg_data$hands),], 
                family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
summary(shift_i)

shift_q = glmer(next_q ~ time_diff + gaze_shift + hand_shift + (1 | id), data = logreg_data[logreg_data$q & !is.na(logreg_data$gaze) & !is.na(logreg_data$hands),], 
                family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
summary(shift_q)

shift_c = glmer(next_c ~ time_diff + gaze_shift + hand_shift + (1 | id), data = logreg_data[logreg_data$c & !is.na(logreg_data$gaze) & !is.na(logreg_data$hands),], 
                family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
summary(shift_c)

shift_a = glmer(next_a ~ time_diff + gaze_shift + hand_shift + (1 | id), data = logreg_data[logreg_data$a & !is.na(logreg_data$gaze) & !is.na(logreg_data$hands),], 
                family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
summary(shift_a)

shift_x = glmer(next_x ~ time_diff + gaze_shift + hand_shift + (1 | id), data = logreg_data[logreg_data$x & !is.na(logreg_data$gaze) & !is.na(logreg_data$hands),], 
                family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
summary(shift_x)

shift_n = glmer(next_n ~ time_diff + gaze_shift + hand_shift + (1 | id), data = logreg_data[logreg_data$n & !is.na(logreg_data$gaze) & !is.na(logreg_data$hands),], 
                family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
summary(shift_n)

shift_v = glmer(next_v ~ time_diff + gaze_shift + hand_shift + (1 | id), data = logreg_data[logreg_data$v & !is.na(logreg_data$gaze) & !is.na(logreg_data$hands),], 
                family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
summary(shift_v)

shift_p = glmer(next_p ~ time_diff + gaze_shift + hand_shift + (1 | id), data = logreg_data[logreg_data$p & !is.na(logreg_data$gaze) & !is.na(logreg_data$hands),], 
                family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
summary(shift_p)

shift_s = glmer(next_s ~ time_diff + gaze_shift + hand_shift + (1 | id), data = logreg_data[logreg_data$s & !is.na(logreg_data$gaze) & !is.na(logreg_data$hands),], 
                family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
summary(shift_s)

#p values for shifts: for each variable/content type combination, likelihood ratio test against full model
anova(shift_d, shift_d_hands)
anova(shift_i, shift_i_hands)
anova(shift_q, shift_q_hands)
anova(shift_c, shift_c_hands)
anova(shift_a, shift_a_hands)
anova(shift_x, shift_x_hands)
anova(shift_n, shift_n_hands)
anova(shift_v, shift_v_hands)
anova(shift_p, shift_p_hands)
anova(shift_s, shift_s_hands)

anova(shift_d, shift_d_gaze)
anova(shift_i, shift_i_gaze)
anova(shift_q, shift_q_gaze)
anova(shift_c, shift_c_gaze)
anova(shift_a, shift_a_gaze)
anova(shift_x, shift_x_gaze)
anova(shift_n, shift_n_gaze)
anova(shift_v, shift_v_gaze)
anova(shift_p, shift_p_gaze)
anova(shift_s, shift_s_gaze)

anova(shift_d, shift_d_notime)
anova(shift_i, shift_i_notime)
anova(shift_q, shift_q_notime)
anova(shift_c, shift_c_notime)
anova(shift_a, shift_a_notime)
anova(shift_x, shift_x_notime)
anova(shift_n, shift_n_notime)
anova(shift_v, shift_v_notime)
anova(shift_p, shift_p_notime)
anova(shift_s, shift_s_notime)

#next: how many repeats are exact
d_exact = mean(logreg_data$exact_repeat[logreg_data$next_d & logreg_data$d], na.rm = T)
i_exact = mean(logreg_data$exact_repeat[logreg_data$next_i & logreg_data$i], na.rm = T)
q_exact = mean(logreg_data$exact_repeat[logreg_data$next_q & logreg_data$q], na.rm = T)
c_exact = mean(logreg_data$exact_repeat[logreg_data$next_c & logreg_data$c], na.rm = T)
a_exact = mean(logreg_data$exact_repeat[logreg_data$next_a & logreg_data$a], na.rm = T)
x_exact = mean(logreg_data$exact_repeat[logreg_data$next_x & logreg_data$x], na.rm = T)
n_exact = mean(logreg_data$exact_repeat[logreg_data$next_n & logreg_data$n], na.rm = T)
v_exact = mean(logreg_data$exact_repeat[logreg_data$next_v & logreg_data$v], na.rm = T)
p_exact = mean(logreg_data$exact_repeat[logreg_data$next_p & logreg_data$p], na.rm = T)
s_exact = mean(logreg_data$exact_repeat[logreg_data$next_s & logreg_data$s], na.rm = T)

#how many repeats are only structural, i.e. no specific words are overlapping
d_no_overlap = mean(logreg_data$prop_words_repeated[logreg_data$next_d & logreg_data$d] == 0, na.rm = T)
i_no_overlap = mean(logreg_data$prop_words_repeated[logreg_data$next_i & logreg_data$i] == 0, na.rm = T)
q_no_overlap = mean(logreg_data$prop_words_repeated[logreg_data$next_q & logreg_data$q] == 0, na.rm = T)
c_no_overlap = mean(logreg_data$prop_words_repeated[logreg_data$next_c & logreg_data$c] == 0, na.rm = T)
a_no_overlap = mean(logreg_data$prop_words_repeated[logreg_data$next_a & logreg_data$a] == 0, na.rm = T)
x_no_overlap = mean(logreg_data$prop_words_repeated[logreg_data$next_x & logreg_data$x] == 0, na.rm = T)
n_no_overlap = mean(logreg_data$prop_words_repeated[logreg_data$next_n & logreg_data$n] == 0, na.rm = T)
v_no_overlap = mean(logreg_data$prop_words_repeated[logreg_data$next_v & logreg_data$v] == 0, na.rm = T)
p_no_overlap = mean(logreg_data$prop_words_repeated[logreg_data$next_p & logreg_data$p] == 0, na.rm = T)
s_no_overlap = mean(logreg_data$prop_words_repeated[logreg_data$next_s & logreg_data$s] == 0, na.rm = T)

#how many repeats are partial
d_partial = 1 - d_exact - d_no_overlap
i_partial = 1 - i_exact - i_no_overlap
q_partial = 1 - q_exact - q_no_overlap
c_partial = 1 - c_exact - c_no_overlap
a_partial = 1 - a_exact - a_no_overlap
x_partial = 1 - x_exact - x_no_overlap
n_partial = 1 - n_exact - n_no_overlap
v_partial = 1 - v_exact - v_no_overlap
p_partial = 1 - p_exact - p_no_overlap
s_partial = 1 - s_exact - s_no_overlap

##how much are non-exact repeats overlapping in words
mean(logreg_data$prop_words_repeated[logreg_data$next_d & logreg_data$d & !exact_repeat], na.rm = T)
mean(logreg_data$prop_words_repeated[logreg_data$next_i & logreg_data$i & !exact_repeat], na.rm = T)
mean(logreg_data$prop_words_repeated[logreg_data$next_q & logreg_data$q & !exact_repeat], na.rm = T)
mean(logreg_data$prop_words_repeated[logreg_data$next_c & logreg_data$c & !exact_repeat], na.rm = T)
mean(logreg_data$prop_words_repeated[logreg_data$next_a & logreg_data$a & !exact_repeat], na.rm = T)
mean(logreg_data$prop_words_repeated[logreg_data$next_x & logreg_data$x & !exact_repeat], na.rm = T)
mean(logreg_data$prop_words_repeated[logreg_data$next_n & logreg_data$n & !exact_repeat], na.rm = T)
mean(logreg_data$prop_words_repeated[logreg_data$next_v & logreg_data$v & !exact_repeat], na.rm = T)
mean(logreg_data$prop_words_repeated[logreg_data$next_p & logreg_data$p & !exact_repeat], na.rm = T)
mean(logreg_data$prop_words_repeated[logreg_data$next_s & logreg_data$s & !exact_repeat], na.rm = T)

#next: fit models for gaze and hands
library(multcomp)
#define pairwise comparison structure
K_gaze <- rbind("other-toy" = c(0,0,0,0,0,0,0,0,0,0,0, -1,  1),     
                "toy - face" = c(0,0,0,0,0,0,0,0,0,0,0,  1,  0),       
                "other-face" = c(0,0,0,0,0,0,0,0,0,0,0,  0, 1))
K_hands <- rbind("2-1" = c(0,0,0,0,0,0,0,0,0,0,0, -1,  1),     
                 "1 - 0" = c(0,0,0,0,0,0,0,0,0,0,0,  1,  0),       
                 "2-0" = c(0,0,0,0,0,0,0,0,0,0,0,  0, 1))
#fit models
d_utt = glmer(next_d ~ d+i+q+c+a+x+n+v+p+s + (1 | id), data = logreg_data[!is.na(logreg_data$gaze),], 
              family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
d_g = glmer(next_d ~ d+i+q+c+a+x+n+v+p+s+gaze + (1 | id), data = logreg_data[!is.na(logreg_data$gaze),], 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
d_h = glmer(next_d ~ d+i+q+c+a+x+n+v+p+s+hands + (1 | id), data = logreg_data[!is.na(logreg_data$gaze),], 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
d_d = glmer(next_d ~ i+q+c+a+x+n+v+p+s + (1 | id), data = logreg_data[!is.na(logreg_data$gaze),], 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
d_c = glmer(next_d ~ d+i+q+a+x+n+v+p+s + (1 | id), data = logreg_data[!is.na(logreg_data$gaze),], 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
d_a = glmer(next_d ~ d+i+q+c+x+n+v+p+s + (1 | id), data = logreg_data[!is.na(logreg_data$gaze),], 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
sum((resid(d_utt)[2:6763]-resid(d_utt)[1:6762])^2)/sum(resid(d_utt)[2:6763]^2)
#compare with likelihood ratio tests and with post-hoc pairwise tests
anova(d_utt, d_g)
anova(d_utt, d_h)
summary(glht(d_g, linfct = K_gaze))
summary(glht(d_h, linfct = K_hands))

#repeat with all other utterance content types
i_utt = glmer(next_i ~ d+i+q+c+a+x+n+v+p+s + (1 | id), data = logreg_data[!is.na(logreg_data$gaze),], 
              family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
i_g = glmer(next_i ~ d+i+q+c+a+x+n+v+p+s+gaze + (1 | id), data = logreg_data[!is.na(logreg_data$gaze),], 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
i_h = glmer(next_i ~ d+i+q+c+a+x+n+v+p+s+hands + (1 | id), data = logreg_data[!is.na(logreg_data$gaze),], 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
sum((resid(i_utt)[2:6763]-resid(i_utt)[1:6762])^2)/sum(resid(i_utt)[2:6763]^2)
anova(i_utt, i_g)
anova(i_utt, i_h)
summary(glht(i_g, linfct = K_gaze))
summary(glht(i_h, linfct = K_hands))

q_utt = glmer(next_q ~ d+i+q+c+a+x+n+v+p+s + (1 | id), data = logreg_data[!is.na(logreg_data$gaze),], 
              family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
q_g = glmer(next_q ~ d+i+q+c+a+x+n+v+p+s+gaze + (1 | id), data = logreg_data[!is.na(logreg_data$gaze),], 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
q_h = glmer(next_q ~ d+i+q+c+a+x+n+v+p+s+hands + (1 | id), data = logreg_data[!is.na(logreg_data$gaze),], 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
sum((resid(q_utt)[2:6763]-resid(q_utt)[1:6762])^2)/sum(resid(q_utt)[2:6763]^2)
anova(q_utt, q_g)
anova(q_utt, q_h)
summary(glht(q_g, linfct = K_gaze))
summary(glht(q_h, linfct = K_hands))

c_utt = glmer(next_c ~ d+i+q+c+a+x+n+v+p+s + (1 | id), data = logreg_data[!is.na(logreg_data$gaze),], 
              family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
c_g = glmer(next_c ~ d+i+q+c+a+x+n+v+p+s+gaze + (1 | id), data = logreg_data[!is.na(logreg_data$gaze),], 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
c_h = glmer(next_c ~ d+i+q+c+a+x+n+v+p+s+hands + (1 | id), data = logreg_data[!is.na(logreg_data$gaze),], 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
sum((resid(c_utt)[2:6763]-resid(c_utt)[1:6762])^2)/sum(resid(c_utt)[2:6763]^2)
anova(c_utt, c_g)
anova(c_utt, c_h)
summary(glht(c_g, linfct = K_gaze))
summary(glht(c_h, linfct = K_hands))

a_utt = glmer(next_a ~ d+i+q+c+a+x+n+v+p+s + (1 | id), data = logreg_data[!is.na(logreg_data$gaze),], 
              family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
a_g = glmer(next_a ~ d+i+q+c+a+x+n+v+p+s+gaze + (1 | id), data = logreg_data[!is.na(logreg_data$gaze),], 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
a_h = glmer(next_a ~ d+i+q+c+a+x+n+v+p+s+hands + (1 | id), data = logreg_data[!is.na(logreg_data$gaze),], 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
sum((resid(a_utt)[2:6763]-resid(a_utt)[1:6762])^2)/sum(resid(a_utt)[2:6763]^2)
anova(a_utt, a_g)
anova(a_utt, a_h)
summary(glht(a_g, linfct = K_gaze))
summary(glht(a_h, linfct = K_hands))

x_utt = glmer(next_x ~ d+i+q+c+a+x+n+v+p+s + (1 | id), data = logreg_data[!is.na(logreg_data$gaze),], 
              family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
x_g = glmer(next_x ~ d+i+q+c+a+x+n+v+p+s+gaze + (1 | id), data = logreg_data[!is.na(logreg_data$gaze),], 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
x_h = glmer(next_x ~ d+i+q+c+a+x+n+v+p+s+hands + (1 | id), data = logreg_data[!is.na(logreg_data$gaze),], 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
sum((resid(x_utt)[2:6763]-resid(x_utt)[1:6762])^2)/sum(resid(x_utt)[2:6763]^2)
anova(x_utt, x_g)
anova(x_utt, x_h)
summary(glht(x_g, linfct = K_gaze))
summary(glht(x_h, linfct = K_hands))

n_utt = glmer(next_n ~ d+i+q+c+a+x+n+v+p+s + (1 | id), data = logreg_data[!is.na(logreg_data$gaze),], 
              family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
n_g = glmer(next_n ~ d+i+q+c+a+x+n+v+p+s+gaze + (1 | id), data = logreg_data[!is.na(logreg_data$gaze),], 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
n_h = glmer(next_n ~ d+i+q+c+a+x+n+v+p+s+hands + (1 | id), data = logreg_data[!is.na(logreg_data$gaze),], 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
sum((resid(n_utt)[2:6763]-resid(n_utt)[1:6762])^2)/sum(resid(n_utt)[2:6763]^2)
anova(n_utt, n_g)
anova(n_utt, n_h)
summary(glht(n_g, linfct = K_gaze))
summary(glht(n_h, linfct = K_hands))

v_utt = glmer(next_v ~ d+i+q+c+a+x+n+v+p+s + (1 | id), data = logreg_data[!is.na(logreg_data$gaze),], 
              family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
v_g = glmer(next_v ~ d+i+q+c+a+x+n+v+p+s+gaze + (1 | id), data = logreg_data[!is.na(logreg_data$gaze),], 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
v_h = glmer(next_v ~ d+i+q+c+a+x+n+v+p+s+hands + (1 | id), data = logreg_data[!is.na(logreg_data$gaze),], 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
sum((resid(v_utt)[2:6763]-resid(v_utt)[1:6762])^2)/sum(resid(v_utt)[2:6763]^2)
anova(v_utt, v_g)
anova(v_utt, v_h)
summary(glht(v_g, linfct = K_gaze))
summary(glht(v_h, linfct = K_hands))

p_utt = glmer(next_p ~ d+i+q+c+a+x+n+v+p+s + (1 | id), data = logreg_data[!is.na(logreg_data$gaze),], 
              family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
p_g = glmer(next_p ~ d+i+q+c+a+x+n+v+p+s+gaze + (1 | id), data = logreg_data[!is.na(logreg_data$gaze),], 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
p_h = glmer(next_p ~ d+i+q+c+a+x+n+v+p+s+hands + (1 | id), data = logreg_data[!is.na(logreg_data$gaze),], 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
sum((resid(p_utt)[2:6763]-resid(p_utt)[1:6762])^2)/sum(resid(p_utt)[2:6763]^2)
anova(p_utt, p_g)
anova(p_utt, p_h)
summary(glht(p_g, linfct = K_gaze))
summary(glht(p_h, linfct = K_hands))

s_utt = glmer(next_s ~ d+i+q+c+a+x+n+v+p+s + (1 | id), data = logreg_data[!is.na(logreg_data$gaze),], 
              family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
s_g = glmer(next_s ~ d+i+q+c+a+x+n+v+p+s+gaze + (1 | id), data = logreg_data[!is.na(logreg_data$gaze),], 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
s_h = glmer(next_s ~ d+i+q+c+a+x+n+v+p+s+hands + (1 | id), data = logreg_data[!is.na(logreg_data$gaze),], 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
sum((resid(s_utt)[2:6763]-resid(s_utt)[1:6762])^2)/sum(resid(s_utt)[2:6763]^2)
anova(s_utt, s_g)
anova(s_utt, s_h)
summary(glht(s_g, linfct = K_gaze))
summary(glht(s_h, linfct = K_hands))

library(dplyr)
#Reference input: the mean value for the indicators of the content types in the previous utterance
utype_props =  logreg_data[!is.na(logreg_data$gaze),] %>%
    group_by(id) %>%
    summarize(
        d = mean(d),
        i = mean(i),
        q = mean(q),
        c = mean(c),
        a = mean(a),
        x = mean(x),
        n = mean(n),
        v = mean(v),
        p = mean(p),
        s = mean(s)
    )
reference_input = colMeans(utype_props[2:11])

#prepare for plotting gaze
type = rep(c('Declarative', 'Imperative', 'Question', 'Description', 'Attention', 'Action', 
             'Object Name', "Infant's Name", "Affirmation", "Social Routine"), each = 3)
gaze = rep(c('Face', 'Toy', 'Other'), 10)
type <- factor(type, levels = c("Declarative", "Imperative", "Question",
                                "Description", "Attention", "Action",
                                "Object Name", "Infant's Name","Affirmation", "Social Routine"))
gaze = factor(gaze, levels = c("Face", "Toy", "Other"))
#compute the means and error bars for the proportions of each utterance type, evaluated at the reference input
mean_prop = c(
    exp(fixef(d_g)[1] + sum(reference_input*fixef(d_g)[2:11]))/(1 + exp(fixef(d_g)[1] + sum(reference_input*fixef(d_g)[2:11]))),
    exp(fixef(d_g)[1] + fixef(d_g)[12] + sum(reference_input*fixef(d_g)[2:11]))/(1 + exp(fixef(d_g)[1] + fixef(d_g)[2] + sum(reference_input*fixef(d_g)[2:11]))),
    exp(fixef(d_g)[1] + fixef(d_g)[13] + sum(reference_input*fixef(d_g)[2:11]))/(1 + exp(fixef(d_g)[1] + fixef(d_g)[3] + sum(reference_input*fixef(d_g)[2:11]))),
    exp(fixef(i_g)[1] + sum(reference_input*fixef(i_g)[2:11]))/(1 + exp(fixef(i_g)[1] + sum(reference_input*fixef(i_g)[2:11]))),
    exp(fixef(i_g)[1] + fixef(i_g)[12] + sum(reference_input*fixef(i_g)[2:11]))/(1 + exp(fixef(i_g)[1] + fixef(i_g)[2] + sum(reference_input*fixef(i_g)[2:11]))),
    exp(fixef(i_g)[1] + fixef(i_g)[13] + sum(reference_input*fixef(i_g)[2:11]))/(1 + exp(fixef(i_g)[1] + fixef(i_g)[3] + sum(reference_input*fixef(i_g)[2:11]))),
    exp(fixef(q_g)[1] + sum(reference_input*fixef(q_g)[2:11]))/(1 + exp(fixef(q_g)[1] + sum(reference_input*fixef(q_g)[2:11]))),
    exp(fixef(q_g)[1] + fixef(q_g)[12] + sum(reference_input*fixef(q_g)[2:11]))/(1 + exp(fixef(q_g)[1] + fixef(q_g)[2] + sum(reference_input*fixef(q_g)[2:11]))),
    exp(fixef(q_g)[1] + fixef(q_g)[13] + sum(reference_input*fixef(q_g)[2:11]))/(1 + exp(fixef(q_g)[1] + fixef(q_g)[3] + sum(reference_input*fixef(q_g)[2:11]))),
    exp(fixef(c_g)[1] + sum(reference_input*fixef(c_g)[2:11]))/(1 + exp(fixef(c_g)[1] + sum(reference_input*fixef(c_g)[2:11]))),
    exp(fixef(c_g)[1] + fixef(c_g)[12] + sum(reference_input*fixef(c_g)[2:11]))/(1 + exp(fixef(c_g)[1] + fixef(c_g)[2] + sum(reference_input*fixef(c_g)[2:11]))),
    exp(fixef(c_g)[1] + fixef(c_g)[13] + sum(reference_input*fixef(c_g)[2:11]))/(1 + exp(fixef(c_g)[1] + fixef(c_g)[3] + sum(reference_input*fixef(c_g)[2:11]))),
    exp(fixef(a_g)[1] + sum(reference_input*fixef(a_g)[2:11]))/(1 + exp(fixef(a_g)[1] + sum(reference_input*fixef(a_g)[2:11]))),
    exp(fixef(a_g)[1] + fixef(a_g)[12] + sum(reference_input*fixef(a_g)[2:11]))/(1 + exp(fixef(a_g)[1] + fixef(a_g)[2] + sum(reference_input*fixef(a_g)[2:11]))),
    exp(fixef(a_g)[1] + fixef(a_g)[13] + sum(reference_input*fixef(a_g)[2:11]))/(1 + exp(fixef(a_g)[1] + fixef(a_g)[3] + sum(reference_input*fixef(a_g)[2:11]))),
    exp(fixef(x_g)[1] + sum(reference_input*fixef(x_g)[2:11]))/(1 + exp(fixef(x_g)[1] + sum(reference_input*fixef(x_g)[2:11]))),
    exp(fixef(x_g)[1] + fixef(x_g)[12] + sum(reference_input*fixef(x_g)[2:11]))/(1 + exp(fixef(x_g)[1] + fixef(x_g)[2] + sum(reference_input*fixef(x_g)[2:11]))),
    exp(fixef(x_g)[1] + fixef(x_g)[13] + sum(reference_input*fixef(x_g)[2:11]))/(1 + exp(fixef(x_g)[1] + fixef(x_g)[3] + sum(reference_input*fixef(x_g)[2:11]))),
    exp(fixef(n_g)[1] + sum(reference_input*fixef(n_g)[2:11]))/(1 + exp(fixef(n_g)[1] + sum(reference_input*fixef(n_g)[2:11]))),
    exp(fixef(n_g)[1] + fixef(n_g)[12] + sum(reference_input*fixef(n_g)[2:11]))/(1 + exp(fixef(n_g)[1] + fixef(n_g)[2] + sum(reference_input*fixef(n_g)[2:11]))),
    exp(fixef(n_g)[1] + fixef(n_g)[13] + sum(reference_input*fixef(n_g)[2:11]))/(1 + exp(fixef(n_g)[1] + fixef(n_g)[3] + sum(reference_input*fixef(n_g)[2:11]))),
    exp(fixef(v_g)[1] + sum(reference_input*fixef(v_g)[2:11]))/(1 + exp(fixef(v_g)[1] + sum(reference_input*fixef(v_g)[2:11]))),
    exp(fixef(v_g)[1] + fixef(v_g)[12] + sum(reference_input*fixef(v_g)[2:11]))/(1 + exp(fixef(v_g)[1] + fixef(v_g)[2] + sum(reference_input*fixef(v_g)[2:11]))),
    exp(fixef(v_g)[1] + fixef(v_g)[13] + sum(reference_input*fixef(v_g)[2:11]))/(1 + exp(fixef(v_g)[1] + fixef(v_g)[3] + sum(reference_input*fixef(v_g)[2:11]))),
    exp(fixef(p_g)[1] + sum(reference_input*fixef(p_g)[2:11]))/(1 + exp(fixef(p_g)[1] + sum(reference_input*fixef(p_g)[2:11]))),
    exp(fixef(p_g)[1] + fixef(p_g)[12] + sum(reference_input*fixef(p_g)[2:11]))/(1 + exp(fixef(p_g)[1] + fixef(p_g)[2] + sum(reference_input*fixef(p_g)[2:11]))),
    exp(fixef(p_g)[1] + fixef(p_g)[13] + sum(reference_input*fixef(p_g)[2:11]))/(1 + exp(fixef(p_g)[1] + fixef(p_g)[3] + sum(reference_input*fixef(p_g)[2:11]))),
    exp(fixef(s_g)[1] + sum(reference_input*fixef(s_g)[2:11]))/(1 + exp(fixef(s_g)[1] + sum(reference_input*fixef(s_g)[2:11]))),
    exp(fixef(s_g)[1] + fixef(s_g)[12] + sum(reference_input*fixef(s_g)[2:11]))/(1 + exp(fixef(s_g)[1] + fixef(s_g)[2] + sum(reference_input*fixef(s_g)[2:11]))),
    exp(fixef(s_g)[1] + fixef(s_g)[13] + sum(reference_input*fixef(s_g)[2:11]))/(1 + exp(fixef(s_g)[1] + fixef(s_g)[3] + sum(reference_input*fixef(s_g)[2:11])))
)
mean_param = log(mean_prop/(1-mean_prop))
std_err = c(
    sqrt(vcov(d_g)[1,1] + sum(outer(reference_input, reference_input)*vcov(d_g)[2:11,2:11]) + sum(2*reference_input*vcov(d_g)[1, 2:11])),
    sqrt(vcov(d_g)[1,1] + sum(outer(reference_input, reference_input)*vcov(d_g)[2:11,2:11]) + sum(2*reference_input*vcov(d_g)[1, 2:11]) + 
             vcov(d_g)[12,12] + 2*vcov(d_g)[1,12] + sum(2*reference_input*vcov(d_g)[2:11,12])),
    sqrt(vcov(d_g)[1,1] + sum(outer(reference_input, reference_input)*vcov(d_g)[2:11,2:11]) + sum(2*reference_input*vcov(d_g)[1, 2:11]) + 
             vcov(d_g)[13,13] + 2*vcov(d_g)[1,13] + sum(2*reference_input*vcov(d_g)[2:11,13])),
    sqrt(vcov(i_g)[1,1] + sum(outer(reference_input, reference_input)*vcov(i_g)[2:11,2:11]) + sum(2*reference_input*vcov(i_g)[1, 2:11])),
    sqrt(vcov(i_g)[1,1] + sum(outer(reference_input, reference_input)*vcov(i_g)[2:11,2:11]) + sum(2*reference_input*vcov(i_g)[1, 2:11]) + 
             vcov(i_g)[12,12] + 2*vcov(i_g)[1,12] + sum(2*reference_input*vcov(i_g)[2:11,12])),
    sqrt(vcov(i_g)[1,1] + sum(outer(reference_input, reference_input)*vcov(i_g)[2:11,2:11]) + sum(2*reference_input*vcov(i_g)[1, 2:11]) + 
             vcov(i_g)[13,13] + 2*vcov(i_g)[1,13] + sum(2*reference_input*vcov(i_g)[2:11,13])),
    sqrt(vcov(q_g)[1,1] + sum(outer(reference_input, reference_input)*vcov(q_g)[2:11,2:11]) + sum(2*reference_input*vcov(q_g)[1, 2:11])),
    sqrt(vcov(q_g)[1,1] + sum(outer(reference_input, reference_input)*vcov(q_g)[2:11,2:11]) + sum(2*reference_input*vcov(q_g)[1, 2:11]) + 
             vcov(q_g)[12,12] + 2*vcov(q_g)[1,12] + sum(2*reference_input*vcov(q_g)[2:11,12])),
    sqrt(vcov(q_g)[1,1] + sum(outer(reference_input, reference_input)*vcov(q_g)[2:11,2:11]) + sum(2*reference_input*vcov(q_g)[1, 2:11]) + 
             vcov(q_g)[13,13] + 2*vcov(q_g)[1,13] + sum(2*reference_input*vcov(q_g)[2:11,13])),
    sqrt(vcov(c_g)[1,1] + sum(outer(reference_input, reference_input)*vcov(c_g)[2:11,2:11]) + sum(2*reference_input*vcov(c_g)[1, 2:11])),
    sqrt(vcov(c_g)[1,1] + sum(outer(reference_input, reference_input)*vcov(c_g)[2:11,2:11]) + sum(2*reference_input*vcov(c_g)[1, 2:11]) + 
             vcov(c_g)[12,12] + 2*vcov(c_g)[1,12] + sum(2*reference_input*vcov(c_g)[2:11,12])),
    sqrt(vcov(c_g)[1,1] + sum(outer(reference_input, reference_input)*vcov(c_g)[2:11,2:11]) + sum(2*reference_input*vcov(c_g)[1, 2:11]) + 
             vcov(c_g)[13,13] + 2*vcov(c_g)[1,13] + sum(2*reference_input*vcov(c_g)[2:11,13])),
    sqrt(vcov(a_g)[1,1] + sum(outer(reference_input, reference_input)*vcov(a_g)[2:11,2:11]) + sum(2*reference_input*vcov(a_g)[1, 2:11])),
    sqrt(vcov(a_g)[1,1] + sum(outer(reference_input, reference_input)*vcov(a_g)[2:11,2:11]) + sum(2*reference_input*vcov(a_g)[1, 2:11]) + 
             vcov(a_g)[12,12] + 2*vcov(a_g)[1,12] + sum(2*reference_input*vcov(a_g)[2:11,12])),
    sqrt(vcov(a_g)[1,1] + sum(outer(reference_input, reference_input)*vcov(a_g)[2:11,2:11]) + sum(2*reference_input*vcov(a_g)[1, 2:11]) + 
             vcov(a_g)[13,13] + 2*vcov(a_g)[1,13] + sum(2*reference_input*vcov(a_g)[2:11,13])),
    sqrt(vcov(x_g)[1,1] + sum(outer(reference_input, reference_input)*vcov(x_g)[2:11,2:11]) + sum(2*reference_input*vcov(x_g)[1, 2:11])),
    sqrt(vcov(x_g)[1,1] + sum(outer(reference_input, reference_input)*vcov(x_g)[2:11,2:11]) + sum(2*reference_input*vcov(x_g)[1, 2:11]) + 
             vcov(x_g)[12,12] + 2*vcov(x_g)[1,12] + sum(2*reference_input*vcov(x_g)[2:11,12])),
    sqrt(vcov(x_g)[1,1] + sum(outer(reference_input, reference_input)*vcov(x_g)[2:11,2:11]) + sum(2*reference_input*vcov(x_g)[1, 2:11]) + 
             vcov(x_g)[13,13] + 2*vcov(x_g)[1,13] + sum(2*reference_input*vcov(x_g)[2:11,13])),
    sqrt(vcov(n_g)[1,1] + sum(outer(reference_input, reference_input)*vcov(n_g)[2:11,2:11]) + sum(2*reference_input*vcov(n_g)[1, 2:11])),
    sqrt(vcov(n_g)[1,1] + sum(outer(reference_input, reference_input)*vcov(n_g)[2:11,2:11]) + sum(2*reference_input*vcov(n_g)[1, 2:11]) + 
             vcov(n_g)[12,12] + 2*vcov(n_g)[1,12] + sum(2*reference_input*vcov(n_g)[2:11,12])),
    sqrt(vcov(n_g)[1,1] + sum(outer(reference_input, reference_input)*vcov(n_g)[2:11,2:11]) + sum(2*reference_input*vcov(n_g)[1, 2:11]) + 
             vcov(n_g)[13,13] + 2*vcov(n_g)[1,13] + sum(2*reference_input*vcov(n_g)[2:11,13])),
    sqrt(vcov(v_g)[1,1] + sum(outer(reference_input, reference_input)*vcov(v_g)[2:11,2:11]) + sum(2*reference_input*vcov(v_g)[1, 2:11])),
    sqrt(vcov(v_g)[1,1] + sum(outer(reference_input, reference_input)*vcov(v_g)[2:11,2:11]) + sum(2*reference_input*vcov(v_g)[1, 2:11]) + 
             vcov(v_g)[12,12] + 2*vcov(v_g)[1,12] + sum(2*reference_input*vcov(v_g)[2:11,12])),
    sqrt(vcov(v_g)[1,1] + sum(outer(reference_input, reference_input)*vcov(v_g)[2:11,2:11]) + sum(2*reference_input*vcov(v_g)[1, 2:11]) + 
             vcov(v_g)[13,13] + 2*vcov(v_g)[1,13] + sum(2*reference_input*vcov(v_g)[2:11,13])),
    sqrt(vcov(p_g)[1,1] + sum(outer(reference_input, reference_input)*vcov(p_g)[2:11,2:11]) + sum(2*reference_input*vcov(p_g)[1, 2:11])),
    sqrt(vcov(p_g)[1,1] + sum(outer(reference_input, reference_input)*vcov(p_g)[2:11,2:11]) + sum(2*reference_input*vcov(p_g)[1, 2:11]) + 
             vcov(p_g)[12,12] + 2*vcov(p_g)[1,12] + sum(2*reference_input*vcov(p_g)[2:11,12])),
    sqrt(vcov(p_g)[1,1] + sum(outer(reference_input, reference_input)*vcov(p_g)[2:11,2:11]) + sum(2*reference_input*vcov(p_g)[1, 2:11]) + 
             vcov(p_g)[13,13] + 2*vcov(p_g)[1,13] + sum(2*reference_input*vcov(p_g)[2:11,13])),
    sqrt(vcov(s_g)[1,1] + sum(outer(reference_input, reference_input)*vcov(s_g)[2:11,2:11]) + sum(2*reference_input*vcov(s_g)[1, 2:11])),
    sqrt(vcov(s_g)[1,1] + sum(outer(reference_input, reference_input)*vcov(s_g)[2:11,2:11]) + sum(2*reference_input*vcov(s_g)[1, 2:11]) + 
             vcov(s_g)[12,12] + 2*vcov(s_g)[1,12] + sum(2*reference_input*vcov(s_g)[2:11,12])),
    sqrt(vcov(s_g)[1,1] + sum(outer(reference_input, reference_input)*vcov(s_g)[2:11,2:11]) + sum(2*reference_input*vcov(s_g)[1, 2:11]) + 
             vcov(s_g)[13,13] + 2*vcov(s_g)[1,13] + sum(2*reference_input*vcov(s_g)[2:11,13]))
)

lower_bound = exp(mean_param-std_err)/(1 + exp(mean_param-std_err))
upper_bound = exp(mean_param+std_err)/(1 + exp(mean_param+std_err))

library('ggplot2')
#make plot of utterance types by gaze
gaze_plot_data = data.frame(type, gaze, mean_prop, lower_bound, upper_bound)
ggplot(data = gaze_plot_data, aes(x = gaze, y = mean_prop, ymin = lower_bound, ymax = upper_bound)) + 
    geom_bar(position = "dodge", stat = "identity", fill = "indianred3") +
    geom_errorbar(position = "dodge", size = 1) +
    facet_wrap(~type, nrow = 2) + 
    labs(y = "content type frequency",
         x = "gaze target") +
    coord_cartesian(ylim = c(0, .46), xlim = c(0.4,3.6), expand = 0) +
    theme(text = element_text(size=22))

#repeat the above steps to prepare for plotting utterance types by hands
type = rep(c('Declarative', 'Imperative', 'Question', 'Description', 'Attention', 'Action', 
             'Object Name', "Infant's Name", "Affirmation", "Social Routine"), each = 3)
hands = rep(c('0', '1', '2'), 10)
type <- factor(type, levels = c("Declarative", "Imperative", "Question",
                                "Description", "Attention", "Action",
                                "Object Name", "Infant's Name","Affirmation", "Social Routine"))
hands = factor(hands, levels = c("0", "1", "2"))
mean_prop = c(
    exp(fixef(d_h)[1] + sum(reference_input*fixef(d_h)[2:11]))/(1 + exp(fixef(d_h)[1] + sum(reference_input*fixef(d_h)[2:11]))),
    exp(fixef(d_h)[1] + fixef(d_h)[12] + sum(reference_input*fixef(d_h)[2:11]))/(1 + exp(fixef(d_h)[1] + fixef(d_h)[2] + sum(reference_input*fixef(d_h)[2:11]))),
    exp(fixef(d_h)[1] + fixef(d_h)[13] + sum(reference_input*fixef(d_h)[2:11]))/(1 + exp(fixef(d_h)[1] + fixef(d_h)[3] + sum(reference_input*fixef(d_h)[2:11]))),
    exp(fixef(i_h)[1] + sum(reference_input*fixef(i_h)[2:11]))/(1 + exp(fixef(i_h)[1] + sum(reference_input*fixef(i_h)[2:11]))),
    exp(fixef(i_h)[1] + fixef(i_h)[12] + sum(reference_input*fixef(i_h)[2:11]))/(1 + exp(fixef(i_h)[1] + fixef(i_h)[2] + sum(reference_input*fixef(i_h)[2:11]))),
    exp(fixef(i_h)[1] + fixef(i_h)[13] + sum(reference_input*fixef(i_h)[2:11]))/(1 + exp(fixef(i_h)[1] + fixef(i_h)[3] + sum(reference_input*fixef(i_h)[2:11]))),
    exp(fixef(q_h)[1] + sum(reference_input*fixef(q_h)[2:11]))/(1 + exp(fixef(q_h)[1] + sum(reference_input*fixef(q_h)[2:11]))),
    exp(fixef(q_h)[1] + fixef(q_h)[12] + sum(reference_input*fixef(q_h)[2:11]))/(1 + exp(fixef(q_h)[1] + fixef(q_h)[2] + sum(reference_input*fixef(q_h)[2:11]))),
    exp(fixef(q_h)[1] + fixef(q_h)[13] + sum(reference_input*fixef(q_h)[2:11]))/(1 + exp(fixef(q_h)[1] + fixef(q_h)[3] + sum(reference_input*fixef(q_h)[2:11]))),
    exp(fixef(c_h)[1] + sum(reference_input*fixef(c_h)[2:11]))/(1 + exp(fixef(c_h)[1] + sum(reference_input*fixef(c_h)[2:11]))),
    exp(fixef(c_h)[1] + fixef(c_h)[12] + sum(reference_input*fixef(c_h)[2:11]))/(1 + exp(fixef(c_h)[1] + fixef(c_h)[2] + sum(reference_input*fixef(c_h)[2:11]))),
    exp(fixef(c_h)[1] + fixef(c_h)[13] + sum(reference_input*fixef(c_h)[2:11]))/(1 + exp(fixef(c_h)[1] + fixef(c_h)[3] + sum(reference_input*fixef(c_h)[2:11]))),
    exp(fixef(a_h)[1] + sum(reference_input*fixef(a_h)[2:11]))/(1 + exp(fixef(a_h)[1] + sum(reference_input*fixef(a_h)[2:11]))),
    exp(fixef(a_h)[1] + fixef(a_h)[12] + sum(reference_input*fixef(a_h)[2:11]))/(1 + exp(fixef(a_h)[1] + fixef(a_h)[2] + sum(reference_input*fixef(a_h)[2:11]))),
    exp(fixef(a_h)[1] + fixef(a_h)[13] + sum(reference_input*fixef(a_h)[2:11]))/(1 + exp(fixef(a_h)[1] + fixef(a_h)[3] + sum(reference_input*fixef(a_h)[2:11]))),
    exp(fixef(x_h)[1] + sum(reference_input*fixef(x_h)[2:11]))/(1 + exp(fixef(x_h)[1] + sum(reference_input*fixef(x_h)[2:11]))),
    exp(fixef(x_h)[1] + fixef(x_h)[12] + sum(reference_input*fixef(x_h)[2:11]))/(1 + exp(fixef(x_h)[1] + fixef(x_h)[2] + sum(reference_input*fixef(x_h)[2:11]))),
    exp(fixef(x_h)[1] + fixef(x_h)[13] + sum(reference_input*fixef(x_h)[2:11]))/(1 + exp(fixef(x_h)[1] + fixef(x_h)[3] + sum(reference_input*fixef(x_h)[2:11]))),
    exp(fixef(n_h)[1] + sum(reference_input*fixef(n_h)[2:11]))/(1 + exp(fixef(n_h)[1] + sum(reference_input*fixef(n_h)[2:11]))),
    exp(fixef(n_h)[1] + fixef(n_h)[12] + sum(reference_input*fixef(n_h)[2:11]))/(1 + exp(fixef(n_h)[1] + fixef(n_h)[2] + sum(reference_input*fixef(n_h)[2:11]))),
    exp(fixef(n_h)[1] + fixef(n_h)[13] + sum(reference_input*fixef(n_h)[2:11]))/(1 + exp(fixef(n_h)[1] + fixef(n_h)[3] + sum(reference_input*fixef(n_h)[2:11]))),
    exp(fixef(v_h)[1] + sum(reference_input*fixef(v_h)[2:11]))/(1 + exp(fixef(v_h)[1] + sum(reference_input*fixef(v_h)[2:11]))),
    exp(fixef(v_h)[1] + fixef(v_h)[12] + sum(reference_input*fixef(v_h)[2:11]))/(1 + exp(fixef(v_h)[1] + fixef(v_h)[2] + sum(reference_input*fixef(v_h)[2:11]))),
    exp(fixef(v_h)[1] + fixef(v_h)[13] + sum(reference_input*fixef(v_h)[2:11]))/(1 + exp(fixef(v_h)[1] + fixef(v_h)[3] + sum(reference_input*fixef(v_h)[2:11]))),
    exp(fixef(p_h)[1] + sum(reference_input*fixef(p_h)[2:11]))/(1 + exp(fixef(p_h)[1] + sum(reference_input*fixef(p_h)[2:11]))),
    exp(fixef(p_h)[1] + fixef(p_h)[12] + sum(reference_input*fixef(p_h)[2:11]))/(1 + exp(fixef(p_h)[1] + fixef(p_h)[2] + sum(reference_input*fixef(p_h)[2:11]))),
    exp(fixef(p_h)[1] + fixef(p_h)[13] + sum(reference_input*fixef(p_h)[2:11]))/(1 + exp(fixef(p_h)[1] + fixef(p_h)[3] + sum(reference_input*fixef(p_h)[2:11]))),
    exp(fixef(s_h)[1] + sum(reference_input*fixef(s_h)[2:11]))/(1 + exp(fixef(s_h)[1] + sum(reference_input*fixef(s_h)[2:11]))),
    exp(fixef(s_h)[1] + fixef(s_h)[12] + sum(reference_input*fixef(s_h)[2:11]))/(1 + exp(fixef(s_h)[1] + fixef(s_h)[2] + sum(reference_input*fixef(s_h)[2:11]))),
    exp(fixef(s_h)[1] + fixef(s_h)[13] + sum(reference_input*fixef(s_h)[2:11]))/(1 + exp(fixef(s_h)[1] + fixef(s_h)[3] + sum(reference_input*fixef(s_h)[2:11])))
)
mean_param = log(mean_prop/(1-mean_prop))
std_err = c(
    sqrt(vcov(d_h)[1,1] + sum(outer(reference_input, reference_input)*vcov(d_h)[2:11,2:11]) + sum(2*reference_input*vcov(d_h)[1, 2:11])),
    sqrt(vcov(d_h)[1,1] + sum(outer(reference_input, reference_input)*vcov(d_h)[2:11,2:11]) + sum(2*reference_input*vcov(d_h)[1, 2:11]) + 
             vcov(d_h)[12,12] + 2*vcov(d_h)[1,12] + sum(2*reference_input*vcov(d_h)[2:11,12])),
    sqrt(vcov(d_h)[1,1] + sum(outer(reference_input, reference_input)*vcov(d_h)[2:11,2:11]) + sum(2*reference_input*vcov(d_h)[1, 2:11]) + 
             vcov(d_h)[13,13] + 2*vcov(d_h)[1,13] + sum(2*reference_input*vcov(d_h)[2:11,13])),
    sqrt(vcov(i_h)[1,1] + sum(outer(reference_input, reference_input)*vcov(i_h)[2:11,2:11]) + sum(2*reference_input*vcov(i_h)[1, 2:11])),
    sqrt(vcov(i_h)[1,1] + sum(outer(reference_input, reference_input)*vcov(i_h)[2:11,2:11]) + sum(2*reference_input*vcov(i_h)[1, 2:11]) + 
             vcov(i_h)[12,12] + 2*vcov(i_h)[1,12] + sum(2*reference_input*vcov(i_h)[2:11,12])),
    sqrt(vcov(i_h)[1,1] + sum(outer(reference_input, reference_input)*vcov(i_h)[2:11,2:11]) + sum(2*reference_input*vcov(i_h)[1, 2:11]) + 
             vcov(i_h)[13,13] + 2*vcov(i_h)[1,13] + sum(2*reference_input*vcov(i_h)[2:11,13])),
    sqrt(vcov(q_h)[1,1] + sum(outer(reference_input, reference_input)*vcov(q_h)[2:11,2:11]) + sum(2*reference_input*vcov(q_h)[1, 2:11])),
    sqrt(vcov(q_h)[1,1] + sum(outer(reference_input, reference_input)*vcov(q_h)[2:11,2:11]) + sum(2*reference_input*vcov(q_h)[1, 2:11]) + 
             vcov(q_h)[12,12] + 2*vcov(q_h)[1,12] + sum(2*reference_input*vcov(q_h)[2:11,12])),
    sqrt(vcov(q_h)[1,1] + sum(outer(reference_input, reference_input)*vcov(q_h)[2:11,2:11]) + sum(2*reference_input*vcov(q_h)[1, 2:11]) + 
             vcov(q_h)[13,13] + 2*vcov(q_h)[1,13] + sum(2*reference_input*vcov(q_h)[2:11,13])),
    sqrt(vcov(c_h)[1,1] + sum(outer(reference_input, reference_input)*vcov(c_h)[2:11,2:11]) + sum(2*reference_input*vcov(c_h)[1, 2:11])),
    sqrt(vcov(c_h)[1,1] + sum(outer(reference_input, reference_input)*vcov(c_h)[2:11,2:11]) + sum(2*reference_input*vcov(c_h)[1, 2:11]) + 
             vcov(c_h)[12,12] + 2*vcov(c_h)[1,12] + sum(2*reference_input*vcov(c_h)[2:11,12])),
    sqrt(vcov(c_h)[1,1] + sum(outer(reference_input, reference_input)*vcov(c_h)[2:11,2:11]) + sum(2*reference_input*vcov(c_h)[1, 2:11]) + 
             vcov(c_h)[13,13] + 2*vcov(c_h)[1,13] + sum(2*reference_input*vcov(c_h)[2:11,13])),
    sqrt(vcov(a_h)[1,1] + sum(outer(reference_input, reference_input)*vcov(a_h)[2:11,2:11]) + sum(2*reference_input*vcov(a_h)[1, 2:11])),
    sqrt(vcov(a_h)[1,1] + sum(outer(reference_input, reference_input)*vcov(a_h)[2:11,2:11]) + sum(2*reference_input*vcov(a_h)[1, 2:11]) + 
             vcov(a_h)[12,12] + 2*vcov(a_h)[1,12] + sum(2*reference_input*vcov(a_h)[2:11,12])),
    sqrt(vcov(a_h)[1,1] + sum(outer(reference_input, reference_input)*vcov(a_h)[2:11,2:11]) + sum(2*reference_input*vcov(a_h)[1, 2:11]) + 
             vcov(a_h)[13,13] + 2*vcov(a_h)[1,13] + sum(2*reference_input*vcov(a_h)[2:11,13])),
    sqrt(vcov(x_h)[1,1] + sum(outer(reference_input, reference_input)*vcov(x_h)[2:11,2:11]) + sum(2*reference_input*vcov(x_h)[1, 2:11])),
    sqrt(vcov(x_h)[1,1] + sum(outer(reference_input, reference_input)*vcov(x_h)[2:11,2:11]) + sum(2*reference_input*vcov(x_h)[1, 2:11]) + 
             vcov(x_h)[12,12] + 2*vcov(x_h)[1,12] + sum(2*reference_input*vcov(x_h)[2:11,12])),
    sqrt(vcov(x_h)[1,1] + sum(outer(reference_input, reference_input)*vcov(x_h)[2:11,2:11]) + sum(2*reference_input*vcov(x_h)[1, 2:11]) + 
             vcov(x_h)[13,13] + 2*vcov(x_h)[1,13] + sum(2*reference_input*vcov(x_h)[2:11,13])),
    sqrt(vcov(n_h)[1,1] + sum(outer(reference_input, reference_input)*vcov(n_h)[2:11,2:11]) + sum(2*reference_input*vcov(n_h)[1, 2:11])),
    sqrt(vcov(n_h)[1,1] + sum(outer(reference_input, reference_input)*vcov(n_h)[2:11,2:11]) + sum(2*reference_input*vcov(n_h)[1, 2:11]) + 
             vcov(n_h)[12,12] + 2*vcov(n_h)[1,12] + sum(2*reference_input*vcov(n_h)[2:11,12])),
    sqrt(vcov(n_h)[1,1] + sum(outer(reference_input, reference_input)*vcov(n_h)[2:11,2:11]) + sum(2*reference_input*vcov(n_h)[1, 2:11]) + 
             vcov(n_h)[13,13] + 2*vcov(n_h)[1,13] + sum(2*reference_input*vcov(n_h)[2:11,13])),
    sqrt(vcov(v_h)[1,1] + sum(outer(reference_input, reference_input)*vcov(v_h)[2:11,2:11]) + sum(2*reference_input*vcov(v_h)[1, 2:11])),
    sqrt(vcov(v_h)[1,1] + sum(outer(reference_input, reference_input)*vcov(v_h)[2:11,2:11]) + sum(2*reference_input*vcov(v_h)[1, 2:11]) + 
             vcov(v_h)[12,12] + 2*vcov(v_h)[1,12] + sum(2*reference_input*vcov(v_h)[2:11,12])),
    sqrt(vcov(v_h)[1,1] + sum(outer(reference_input, reference_input)*vcov(v_h)[2:11,2:11]) + sum(2*reference_input*vcov(v_h)[1, 2:11]) + 
             vcov(v_h)[13,13] + 2*vcov(v_h)[1,13] + sum(2*reference_input*vcov(v_h)[2:11,13])),
    sqrt(vcov(p_h)[1,1] + sum(outer(reference_input, reference_input)*vcov(p_h)[2:11,2:11]) + sum(2*reference_input*vcov(p_h)[1, 2:11])),
    sqrt(vcov(p_h)[1,1] + sum(outer(reference_input, reference_input)*vcov(p_h)[2:11,2:11]) + sum(2*reference_input*vcov(p_h)[1, 2:11]) + 
             vcov(p_h)[12,12] + 2*vcov(p_h)[1,12] + sum(2*reference_input*vcov(p_h)[2:11,12])),
    sqrt(vcov(p_h)[1,1] + sum(outer(reference_input, reference_input)*vcov(p_h)[2:11,2:11]) + sum(2*reference_input*vcov(p_h)[1, 2:11]) + 
             vcov(p_h)[13,13] + 2*vcov(p_h)[1,13] + sum(2*reference_input*vcov(p_h)[2:11,13])),
    sqrt(vcov(s_h)[1,1] + sum(outer(reference_input, reference_input)*vcov(s_h)[2:11,2:11]) + sum(2*reference_input*vcov(s_h)[1, 2:11])),
    sqrt(vcov(s_h)[1,1] + sum(outer(reference_input, reference_input)*vcov(s_h)[2:11,2:11]) + sum(2*reference_input*vcov(s_h)[1, 2:11]) + 
             vcov(s_h)[12,12] + 2*vcov(s_h)[1,12] + sum(2*reference_input*vcov(s_h)[2:11,12])),
    sqrt(vcov(s_h)[1,1] + sum(outer(reference_input, reference_input)*vcov(s_h)[2:11,2:11]) + sum(2*reference_input*vcov(s_h)[1, 2:11]) + 
             vcov(s_h)[13,13] + 2*vcov(s_h)[1,13] + sum(2*reference_input*vcov(s_h)[2:11,13]))
)

lower_bound = exp(mean_param-std_err)/(1 + exp(mean_param-std_err))
upper_bound = exp(mean_param+std_err)/(1 + exp(mean_param+std_err))

library('ggplot2')
hands_plot_data = data.frame(type, hands, mean_prop, lower_bound, upper_bound)
ggplot(data = hands_plot_data, aes(x = hands, y = mean_prop, ymin = lower_bound, ymax = upper_bound)) + 
    geom_bar(position = "dodge", stat = "identity", fill = "steelblue1") +
    geom_errorbar(position = "dodge", size = 1) +
    facet_wrap(~type, nrow = 2) + 
    labs(y = "content type frequency",
         x = "toys handled") +
    coord_cartesian(ylim = c(0, .46), xlim = c(0.4,3.6), expand = 0) +
    theme(text = element_text(size=22))

#next: analyze the degree of predictability of each utterance type using gaze+hands, previous utterance, or both as predictors
#divide into training and test data
library(ROCR)
library(lme4)

predict_from_utt <- function(model, newdata){
    coefs = fixef(model)
    logodds = (coefs[1] 
               + newdata$d * coefs[2]
               + newdata$i * coefs[3]
               + newdata$q * coefs[4]
               + newdata$c * coefs[5]
               + newdata$a * coefs[6]
               + newdata$x * coefs[7]
               + newdata$n * coefs[8]
               + newdata$v * coefs[9]
               + newdata$p * coefs[10]
               + newdata$s * coefs[11])
    probs = exp(logodds)/(1 + exp(logodds))
    probs
}

predict_from_gh <- function(model, newdata){
    coefs = fixef(model)
    logodds = (coefs[1] 
               + (newdata$gaze=="object") * coefs[12]
               + (newdata$gaze=="other") * coefs[13]
               + (newdata$hands=="1") * coefs[14]
               + (newdata$hands=="2") * coefs[15])
    probs = exp(logodds)/(1 + exp(logodds))
    probs
}

predict_from_full <- function(model, newdata){
    coefs = fixef(model)
    logodds = (coefs[1] 
               + newdata$d * coefs[2]
               + newdata$i * coefs[3]
               + newdata$q * coefs[4]
               + newdata$c * coefs[5]
               + newdata$a * coefs[6]
               + newdata$x * coefs[7]
               + newdata$n * coefs[8]
               + newdata$v * coefs[9]
               + newdata$p * coefs[10]
               + newdata$s * coefs[11]
               + (newdata$gaze=="object") * coefs[12]
               + (newdata$gaze=="other") * coefs[13]
               + (newdata$hands=="1") * coefs[14]
               + (newdata$hands=="2") * coefs[15])
    probs = exp(logodds)/(1 + exp(logodds))
    probs
}

set.seed(29486161)
session_folds = sample(c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5,6,6,6,6,7,7,7,7,8,8,8,8,9,9,9,10,10,10))
require('plyr')
folds = mapvalues(logreg_data$id, from = unique(logreg_data$id), to = session_folds)
aucs = array(rep(NaN, 3*10*10), c(10,3,10))
for (fold in 1:10) {
    print(fold)
    train = logreg_data[!folds == fold,]
    test = logreg_data[folds == fold,]
    d_model = glmer(next_d ~ d+i+q+c+a+x+n+v+p+s+gaze+hands+ (1 | id), data = train, 
                    family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
    i_model = glmer(next_i ~ d+i+q+c+a+x+n+v+p+s+gaze+hands+ (1 | id), data = train, 
                    family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
    q_model = glmer(next_q ~ d+i+q+c+a+x+n+v+p+s+gaze+hands+ (1 | id), data = train, 
                    family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
    c_model = glmer(next_c ~ d+i+q+c+a+x+n+v+p+s+gaze+hands+ (1 | id), data = train, 
                    family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
    a_model = glmer(next_a ~ d+i+q+c+a+x+n+v+p+s+gaze+hands+ (1 | id), data = train, 
                    family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
    x_model = glmer(next_x ~ d+i+q+c+a+x+n+v+p+s+gaze+hands+ (1 | id), data = train, 
                    family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
    n_model = glmer(next_n ~ d+i+q+c+a+x+n+v+p+s+gaze+hands+ (1 | id), data = train, 
                    family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
    v_model = glmer(next_v ~ d+i+q+c+a+x+n+v+p+s+gaze+hands+ (1 | id), data = train, 
                    family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
    p_model = glmer(next_p ~ d+i+q+c+a+x+n+v+p+s+gaze+hands+ (1 | id), data = train, 
                    family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
    s_model = glmer(next_s ~ d+i+q+c+a+x+n+v+p+s+gaze+hands+ (1 | id), data = train, 
                    family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
    probs_gh_d = predict_from_gh(d_model, test)
    probs_gh_i = predict_from_gh(i_model, test)
    probs_gh_q = predict_from_gh(q_model, test)
    probs_gh_c = predict_from_gh(c_model, test)
    probs_gh_a = predict_from_gh(a_model, test)
    probs_gh_x = predict_from_gh(x_model, test)
    probs_gh_n = predict_from_gh(n_model, test)
    probs_gh_v = predict_from_gh(v_model, test)
    probs_gh_p = predict_from_gh(p_model, test)
    probs_gh_s = predict_from_gh(s_model, test)
    probs_utt_d = predict_from_utt(d_model, test)
    probs_utt_i = predict_from_utt(i_model, test)
    probs_utt_q = predict_from_utt(q_model, test)
    probs_utt_c = predict_from_utt(c_model, test)
    probs_utt_a = predict_from_utt(a_model, test)
    probs_utt_x = predict_from_utt(x_model, test)
    probs_utt_n = predict_from_utt(n_model, test)
    probs_utt_v = predict_from_utt(v_model, test)
    probs_utt_p = predict_from_utt(p_model, test)
    probs_utt_s = predict_from_utt(s_model, test)
    probs_full_d = predict_from_full(d_model, test)
    probs_full_i = predict_from_full(i_model, test)
    probs_full_q = predict_from_full(q_model, test)
    probs_full_c = predict_from_full(c_model, test)
    probs_full_a = predict_from_full(a_model, test)
    probs_full_x = predict_from_full(x_model, test)
    probs_full_n = predict_from_full(n_model, test)
    probs_full_v = predict_from_full(v_model, test)
    probs_full_p = predict_from_full(p_model, test)
    probs_full_s = predict_from_full(s_model, test)
    
    targets_d = test$next_d
    targets_i = test$next_i
    targets_q = test$next_q
    targets_c = test$next_c
    targets_a = test$next_a
    targets_x = test$next_x
    targets_n = test$next_n
    targets_v = test$next_v
    targets_p = test$next_p
    targets_s = test$next_s
    
    pred_utt_d = prediction(probs_utt_d, 1*targets_d)
    pred_utt_i = prediction(probs_utt_i, 1*targets_i)
    pred_utt_q = prediction(probs_utt_q, 1*targets_q)
    pred_utt_c = prediction(probs_utt_c, 1*targets_c)
    pred_utt_a = prediction(probs_utt_a, 1*targets_a)
    pred_utt_x = prediction(probs_utt_x, 1*targets_x)
    pred_utt_n = prediction(probs_utt_n, 1*targets_n)
    pred_utt_v = prediction(probs_utt_v, 1*targets_v)
    pred_utt_p = prediction(probs_utt_p, 1*targets_p)
    pred_utt_s = prediction(probs_utt_s, 1*targets_s)
    
    pred_gh_d = prediction(probs_gh_d, 1*targets_d)
    pred_gh_i = prediction(probs_gh_i, 1*targets_i)
    pred_gh_q = prediction(probs_gh_q, 1*targets_q)
    pred_gh_c = prediction(probs_gh_c, 1*targets_c)
    pred_gh_a = prediction(probs_gh_a, 1*targets_a)
    pred_gh_x = prediction(probs_gh_x, 1*targets_x)
    pred_gh_n = prediction(probs_gh_n, 1*targets_n)
    pred_gh_v = prediction(probs_gh_v, 1*targets_v)
    pred_gh_p = prediction(probs_gh_p, 1*targets_p)
    pred_gh_s = prediction(probs_gh_s, 1*targets_s)
    
    pred_full_d = prediction(probs_full_d, 1*targets_d)
    pred_full_i = prediction(probs_full_i, 1*targets_i)
    pred_full_q = prediction(probs_full_q, 1*targets_q)
    pred_full_c = prediction(probs_full_c, 1*targets_c)
    pred_full_a = prediction(probs_full_a, 1*targets_a)
    pred_full_x = prediction(probs_full_x, 1*targets_x)
    pred_full_n = prediction(probs_full_n, 1*targets_n)
    pred_full_v = prediction(probs_full_v, 1*targets_v)
    pred_full_p = prediction(probs_full_p, 1*targets_p)
    pred_full_s = prediction(probs_full_s, 1*targets_s)
    
    aucs[1,1,fold] = (performance(pred_gh_d, measure = "auc")@y.values[[1]])
    aucs[2,1,fold] = (performance(pred_gh_i, measure = "auc")@y.values[[1]])
    aucs[3,1,fold] = (performance(pred_gh_q, measure = "auc")@y.values[[1]])
    aucs[4,1,fold] = (performance(pred_gh_c, measure = "auc")@y.values[[1]])
    aucs[5,1,fold] = (performance(pred_gh_a, measure = "auc")@y.values[[1]])
    aucs[6,1,fold] = (performance(pred_gh_x, measure = "auc")@y.values[[1]])
    aucs[7,1,fold] = (performance(pred_gh_n, measure = "auc")@y.values[[1]])
    aucs[8,1,fold] = (performance(pred_gh_v, measure = "auc")@y.values[[1]])
    aucs[9,1,fold] = (performance(pred_gh_p, measure = "auc")@y.values[[1]])
    aucs[10,1,fold]= (performance(pred_gh_s, measure = "auc")@y.values[[1]])
    
    aucs[1,2,fold] = (performance(pred_utt_d, measure = "auc")@y.values[[1]])
    aucs[2,2,fold] = (performance(pred_utt_i, measure = "auc")@y.values[[1]])
    aucs[3,2,fold] = (performance(pred_utt_q, measure = "auc")@y.values[[1]])
    aucs[4,2,fold] = (performance(pred_utt_c, measure = "auc")@y.values[[1]])
    aucs[5,2,fold] = (performance(pred_utt_a, measure = "auc")@y.values[[1]])
    aucs[6,2,fold] = (performance(pred_utt_x, measure = "auc")@y.values[[1]])
    aucs[7,2,fold] = (performance(pred_utt_n, measure = "auc")@y.values[[1]])
    aucs[8,2,fold] = (performance(pred_utt_v, measure = "auc")@y.values[[1]])
    aucs[9,2,fold] = (performance(pred_utt_p, measure = "auc")@y.values[[1]])
    aucs[10,2,fold]= (performance(pred_utt_s, measure = "auc")@y.values[[1]])
    
    
    aucs[1,3,fold] = (performance(pred_full_d, measure = "auc")@y.values[[1]])
    aucs[2,3,fold] = (performance(pred_full_i, measure = "auc")@y.values[[1]])
    aucs[3,3,fold] = (performance(pred_full_q, measure = "auc")@y.values[[1]])
    aucs[4,3,fold] = (performance(pred_full_c, measure = "auc")@y.values[[1]])
    aucs[5,3,fold] = (performance(pred_full_a, measure = "auc")@y.values[[1]])
    aucs[6,3,fold] = (performance(pred_full_x, measure = "auc")@y.values[[1]])
    aucs[7,3,fold] = (performance(pred_full_n, measure = "auc")@y.values[[1]])
    aucs[8,3,fold] = (performance(pred_full_v, measure = "auc")@y.values[[1]])
    aucs[9,3,fold] = (performance(pred_full_p, measure = "auc")@y.values[[1]])
    aucs[10,3,fold]= (performance(pred_full_s, measure = "auc")@y.values[[1]])
}

mean_aucs = matrix(rep(0, 60),nrow = 10, ncol = 3)
for (i in 1:10){
    for (j in 1:3){
        mean_aucs[i,j] = mean(aucs[i, j, 1:10])
    }
}

##Plot 2-D visualization of content type predictability

require(ggplot2)
name = c("DECLARATIVE", "IMPERATIVE", "QUESTION", 
         "DESCRIPTION", "ATTENTION", "ACTION", 
         "OBJECT NAME", "INFANT'S NAME", "AFFIRMATION",
         "SOCIAL ROUTINE")
gh = c(.54, .54, .51, .54, .53, .52, .53, .60, .56, .49)
utt = c(.61,.64,.60,.69,.59,.65,.67,.58,.57,.59)
df = data.frame(name, gh, utt)

ggplot(df, aes(x = gh, y = utt)) +
    geom_hline(yintercept = .5, linetype = "dashed") +
    geom_vline(xintercept = .5, linetype = "dashed") +
    geom_point() + 
    geom_text(label = name, nudge_y = .005) +
    coord_cartesian(xlim = c(.48, .67), ylim = c(.50, .69)) +
    labs(x = "Predictability by infant's gaze and object handling state (AUC)",
         y = "Predictability by previous utterance content (AUC)") +
    theme_bw() +
    scale_x_continuous(breaks = c(.50, .55, .60, .65, .70)) +
    theme(text=element_text(size=14))

###Find prototypical sequence
#sequence = c('v', 'a', 'd', 'p')
#which(grepl("v", data[1:(dim(data)[1]-5),"utype"]) &
#        grepl("a", data[2:(dim(data)[1]-4),"utype"]) &
#        grepl("d", data[3:(dim(data)[1]-3),"utype"]) &
#        grepl("p", data[4:(dim(data)[1]-2),"utype"]))


#Autocorrelation values: to make sure models aren't contaminated by autocorrelation at longer lags
cor(resid(d_trans)[2:7432],resid(d_trans)[1:7431])
cor(resid(i_trans)[2:7432],resid(i_trans)[1:7431])
cor(resid(q_trans)[2:7432],resid(q_trans)[1:7431])
cor(resid(c_trans)[2:7432],resid(c_trans)[1:7431])
cor(resid(a_trans)[2:7432],resid(a_trans)[1:7431])
cor(resid(x_trans)[2:7432],resid(x_trans)[1:7431])
cor(resid(n_trans)[2:7432],resid(n_trans)[1:7431])
cor(resid(v_trans)[2:7432],resid(v_trans)[1:7431])
cor(resid(p_trans)[2:7432],resid(p_trans)[1:7431])
cor(resid(s_trans)[2:7432],resid(s_trans)[1:7431])

cor(resid(d_g)[2:6763],resid(d_g)[1:6762])
cor(resid(i_g)[2:6763],resid(i_g)[1:6762])
cor(resid(q_g)[2:6763],resid(q_g)[1:6762])
cor(resid(c_g)[2:6763],resid(c_g)[1:6762])
cor(resid(a_g)[2:6763],resid(a_g)[1:6762])
cor(resid(x_g)[2:6763],resid(x_g)[1:6762])
cor(resid(n_g)[2:6763],resid(n_g)[1:6762])
cor(resid(v_g)[2:6763],resid(v_g)[1:6762])
cor(resid(p_g)[2:6763],resid(p_g)[1:6762])
cor(resid(s_g)[2:6763],resid(s_g)[1:6762])

cor(resid(d_h)[2:6763],resid(d_h)[1:6762])
cor(resid(i_h)[2:6763],resid(i_h)[1:6762])
cor(resid(q_h)[2:6763],resid(q_h)[1:6762])
cor(resid(c_h)[2:6763],resid(c_h)[1:6762])
cor(resid(a_h)[2:6763],resid(a_h)[1:6762])
cor(resid(x_h)[2:6763],resid(x_h)[1:6762])
cor(resid(n_h)[2:6763],resid(n_h)[1:6762])
cor(resid(v_h)[2:6763],resid(v_h)[1:6762])
cor(resid(p_h)[2:6763],resid(p_h)[1:6762])
cor(resid(s_h)[2:6763],resid(s_h)[1:6762])

cor(resid(shift_d)[2:(length(resid(shift_d)))], resid(shift_d)[1:(length(resid(shift_d))-1)])
cor(resid(shift_i)[2:(length(resid(shift_i)))], resid(shift_i)[1:(length(resid(shift_i))-1)])
cor(resid(shift_q)[2:(length(resid(shift_q)))], resid(shift_q)[1:(length(resid(shift_q))-1)])
cor(resid(shift_c)[2:(length(resid(shift_c)))], resid(shift_c)[1:(length(resid(shift_c))-1)])
cor(resid(shift_a)[2:(length(resid(shift_a)))], resid(shift_a)[1:(length(resid(shift_a))-1)])
cor(resid(shift_x)[2:(length(resid(shift_x)))], resid(shift_x)[1:(length(resid(shift_x))-1)])
cor(resid(shift_n)[2:(length(resid(shift_n)))], resid(shift_n)[1:(length(resid(shift_n))-1)])
cor(resid(shift_v)[2:(length(resid(shift_v)))], resid(shift_v)[1:(length(resid(shift_v))-1)])
cor(resid(shift_p)[2:(length(resid(shift_p)))], resid(shift_p)[1:(length(resid(shift_p))-1)])
cor(resid(shift_s)[2:(length(resid(shift_s)))], resid(shift_s)[1:(length(resid(shift_s))-1)])

