# Exemple of sequence analysis with Traminer

# generate random sequence data

library(dplyr)

ss <- data.frame(id = as.integer(rep(x, sample(1:10, length(x), replace = T))))
events_ <- c('criteo', 'natural', 'google_search', 'google_add', 'facebook_CA', 'ap_nexus', 'google_maps', 'relload_page', 'snapchat', 'twitter') 
ss <- ss %>% 
  group_by(id) %>% 
  mutate(timestamp = floor(10 * (seq(1, n(), 1) + rnorm(n())**2)), 
         event = factor(as.character(c(sample(1:10, n(), replace = T))), levels = as.character(1:10), labels = events_),
         result = factor(rep(sample(c('win', 'fail'), 1), n())))

summary(ss)

library(TraMineR)
ss.seq <- seqecreate(id = ss$id, timestamp = ss$timestamp, event = ss$event)

# count most frequent sequences
ss.reccurent <- seqefsub(ss.seq, min.support = 10)
plot(seqefsub(ss.seq, min.support = 10)[1:10])

# trouver des sequences discriminantes 
seq.win <- ss %>% group_by(id) %>% arrange(id) %>% summarise(win = any(result == 'win')) %>% pull(win)
seq.win <- factor(seq.win, levels = c(TRUE, FALSE), labels = c('win', 'fail'))
seq.groups <- seqecmpgroup(ss.reccurent, group = seq.win, method = "bonferroni")
seq.groups[1:10]

plot(seq.groups[1:10])
