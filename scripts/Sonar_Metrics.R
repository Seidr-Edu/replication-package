library(gplots)
df<-read.csv(file.path(dirname(sys.frame(1)$ofile), "../data/exported/sonar_projects.csv"))
df$complexity<-df$complexity/df$ncloc #complexity, higher number is worse,  normalized per ncloc
df$cognitive_complexity<-df$cognitive_complexity/df$ncloc #complexity, higher number is worse,  normalized per ncloc
 
d1<-df[df$variant %in% c("generated", "original"),]
d2<-df[df$variant %in% c("generated", "v2"),]
d3<-df[df$variant %in% c("generated", "v3"),]
d4<-df[df$variant %in% c("v2", "original"),]
d5<-df[df$variant %in% c("v3", "original"),]
d6<-df[df$variant %in% c("v2", "v3"),]
 
 
anova(lm(df$complexity~df$variant*df$agent))
anova(lm(df[df$agent %in% c("claude","codex"),]$complexity~df[df$agent %in% c("claude","codex"),]$agent))
plotmeans(df$complexity~df$variant)
plotmeans(df[df$agent %in% c("claude","codex"),]$complexity~df[df$agent %in% c("claude","codex"),]$agent)
anova(lm(d1$complexity~d1$variant))
anova(lm(d2$complexity~d2$variant))
anova(lm(d3$complexity~d3$variant))
anova(lm(d4$complexity~d4$variant))
anova(lm(d5$complexity~d5$variant))
anova(lm(d6$complexity~d6$variant))
 
 
  
anova(lm(df$code_smells~df$variant*df$agent))
anova(lm(df[df$agent %in% c("claude","codex"),]$code_smells~df[df$agent %in% c("claude","codex"),]$agent))
plotmeans(df$code_smells~df$variant)
plotmeans(df[df$agent %in% c("claude","codex"),]$code_smells~df[df$agent %in% c("claude","codex"),]$agent)
 
 

anova(lm(df$sqale_index~df$variant*df$agent))
anova(lm(df[df$agent %in% c("claude","codex"),]$sqale_index~df[df$agent %in% c("claude","codex"),]$agent))
plotmeans(df$sqale_index~df$variant)
plotmeans(df[df$agent %in% c("claude","codex"),]$sqale_index~df[df$agent %in% c("claude","codex"),]$agent)
anova(lm(d1$sqale_index~d1$variant))
anova(lm(d2$sqale_index~d2$variant))
anova(lm(d3$sqale_index~d3$variant))
anova(lm(d4$sqale_index~d4$variant))
anova(lm(d5$sqale_index~d5$variant))
anova(lm(d6$sqale_index~d6$variant))
 
 
anova(lm(df$sqale_rating~df$variant*df$agent))
anova(lm(df[df$agent %in% c("claude","codex"),]$sqale_rating~df[df$agent %in% c("claude","codex"),]$agent))
plotmeans(df$sqale_rating~df$variant)
plotmeans(df[df$agent %in% c("claude","codex"),]$sqale_rating~df[df$agent %in% c("claude","codex"),]$agent)
 
 
 
 
anova(lm(df$cognitive_complexity~df$variant*df$agent))
anova(lm(df[df$agent %in% c("claude","codex"),]$cognitive_complexity~df[df$agent %in% c("claude","codex"),]$agent))
plotmeans(df$cognitive_complexity~df$variant)
plotmeans(df[df$agent %in% c("claude","codex"),]$cognitive_complexity~df[df$agent %in% c("claude","codex"),]$agent)
 
 
 
 
anova(lm(df$bugs~df$variant*df$agent))
anova(lm(df[df$agent %in% c("claude","codex"),]$bugs~df[df$agent %in% c("claude","codex"),]$agent))
plotmeans(df$bugs~df$variant)
plotmeans(df[df$agent %in% c("claude","codex"),]$bugs~df[df$agent %in% c("claude","codex"),]$agent)
anova(lm(d1$bugs~d1$variant))
anova(lm(d2$bugs~d2$variant))
anova(lm(d3$bugs~d3$variant))
anova(lm(d4$bugs~d4$variant))
anova(lm(d5$bugs~d5$variant))
anova(lm(d6$bugs~d6$variant))
 
 
anova(lm(df$reliability_rating~df$variant*df$agent))
anova(lm(df[df$agent %in% c("claude","codex"),]$reliability_rating~df[df$agent %in% c("claude","codex"),]$agent))
plotmeans(df$reliability_rating~df$variant)
plotmeans(df[df$agent %in% c("claude","codex"),]$reliability_rating~df[df$agent %in% c("claude","codex"),]$agent)
plotmeans(df[df$agent %in% c("claude","codex"),]$bugs~df[df$agent %in% c("claude","codex"),]$agent)
anova(lm(d1$reliability_rating~d1$variant))
anova(lm(d2$reliability_rating~d2$variant))
anova(lm(d3$reliability_rating~d3$variant))
anova(lm(d4$reliability_rating~d4$variant))
anova(lm(d5$reliability_rating~d5$variant))
anova(lm(d6$reliability_rating~d6$variant))
 
 
 
 
anova(lm(df$vulnerabilities~df$variant*df$agent))
anova(lm(df[df$agent %in% c("claude","codex"),]$vulnerabilities~df[df$agent %in% c("claude","codex"),]$agent))
plotmeans(df$vulnerabilities~df$variant)
plotmeans(df[df$agent %in% c("claude","codex"),]$vulnerabilities~df[df$agent %in% c("claude","codex"),]$agent)
anova(lm(d1$vulnerabilities~d1$variant))
anova(lm(d2$vulnerabilities~d2$variant))
anova(lm(d3$vulnerabilities~d3$variant))
anova(lm(d4$vulnerabilities~d4$variant))
anova(lm(d5$vulnerabilities~d5$variant))
anova(lm(d6$vulnerabilities~d6$variant))
 
 
 
anova(lm(df$security_hotspots~df$variant*df$agent))
anova(lm(df[df$agent %in% c("claude","codex"),]$security_hotspots~df[df$agent %in% c("claude","codex"),]$agent))
plotmeans(df$security_hotspots~df$variant)
plotmeans(df[df$agent %in% c("claude","codex"),]$security_hotspots~df[df$agent %in% c("claude","codex"),]$agent)
anova(lm(d1$security_hotspots~d1$variant))
anova(lm(d2$security_hotspots~d2$variant))
anova(lm(d3$security_hotspots~d3$variant))
anova(lm(d4$security_hotspots~d4$variant))
anova(lm(d5$security_hotspots~d5$variant))
anova(lm(d6$security_hotspots~d6$variant))
 
 
anova(lm(df$security_rating~df$variant*df$agent))
anova(lm(df[df$agent %in% c("claude","codex"),]$security_rating~df[df$agent %in% c("claude","codex"),]$agent))
plotmeans(df$security_rating~df$variant)
plotmeans(df[df$agent %in% c("claude","codex"),]$security_rating~df[df$agent %in% c("claude","codex"),]$agent)
 
 
 
 
 
anova(lm(df$duplicated_lines_density~df$variant*df$agent))
anova(lm(df[df$agent %in% c("claude","codex"),]$duplicated_lines_density~df[df$agent %in% c("claude","codex"),]$agent))
plotmeans(df$duplicated_lines_density~df$variant)
plotmeans(df[df$agent %in% c("claude","codex"),]$duplicated_lines_density~df[df$agent %in% c("claude","codex"),]$agent)
 
 
anova(lm(df$duplicated_lines~df$variant*df$agent))
anova(lm(df[df$agent %in% c("claude","codex"),]$duplicated_lines~df[df$agent %in% c("claude","codex"),]$agent))
plotmeans(df$duplicated_lines~df$variant)
plotmeans(df[df$agent %in% c("claude","codex"),]$duplicated_lines~df[df$agent %in% c("claude","codex"),]$agent)
anova(lm(d1$duplicated_lines~d1$variant))
anova(lm(d2$duplicated_lines~d2$variant))
anova(lm(d3$duplicated_lines~d3$variant))
anova(lm(d4$duplicated_lines~d4$variant))
anova(lm(d5$duplicated_lines~d5$variant))
anova(lm(d6$duplicated_lines~d6$variant))
 
 
anova(lm(df$duplicated_files~df$variant*df$agent))
anova(lm(df[df$agent %in% c("claude","codex"),]$duplicated_files~df[df$agent %in% c("claude","codex"),]$agent))
plotmeans(df$duplicated_files~df$variant)
plotmeans(df[df$agent %in% c("claude","codex"),]$duplicated_files~df[df$agent %in% c("claude","codex"),]$agent)
 
 
anova(lm(df$duplicated_blocks~df$variant*df$agent))
anova(lm(df[df$agent %in% c("claude","codex"),]$duplicated_blocks~df[df$agent %in% c("claude","codex"),]$agent))
plotmeans(df$duplicated_blocks~df$variant)
plotmeans(df[df$agent %in% c("claude","codex"),]$duplicated_blocks~df[df$agent %in% c("claude","codex"),]$agent)
 
 
 
 
 
anova(lm(df$comment_lines~df$variant*df$agent))
anova(lm(df[df$agent %in% c("claude","codex"),]$comment_lines~df[df$agent %in% c("claude","codex"),]$agent))
plotmeans(df$comment_lines~df$variant)
plotmeans(df[df$agent %in% c("claude","codex"),]$comment_lines~df[df$agent %in% c("claude","codex"),]$agent)
anova(lm(d1$comment_lines~d1$variant))
anova(lm(d2$comment_lines~d2$variant))
anova(lm(d3$comment_lines~d3$variant))
anova(lm(d4$comment_lines~d4$variant))
anova(lm(d5$comment_lines~d5$variant))
anova(lm(d6$comment_lines~d6$variant))
 
 
anova(lm(df$comment_lines_density~df$variant*df$agent))
anova(lm(df[df$agent %in% c("claude","codex"),]$comment_lines_density~df[df$agent %in% c("claude","codex"),]$agent))
plotmeans(df$comment_lines_density~df$variant)
plotmeans(df[df$agent %in% c("claude","codex"),]$comment_lines_density~df[df$agent %in% c("claude","codex"),]$agent)
anova(lm(d1$comment_lines_density~d1$variant))
anova(lm(d2$comment_lines_density~d2$variant))
anova(lm(d3$comment_lines_density~d3$variant))
anova(lm(d4$comment_lines_density~d4$variant))
anova(lm(d5$comment_lines_density~d5$variant))
anova(lm(d6$comment_lines_density~d6$variant))
 
 
 
 
 
 
anova(lm(df$blocker_violations~df$variant*df$agent))
anova(lm(df[df$agent %in% c("claude","codex"),]$blocker_violations~df[df$agent %in% c("claude","codex"),]$agent))
plotmeans(df$blocker_violations~df$variant)
plotmeans(df[df$agent %in% c("claude","codex"),]$blocker_violations~df[df$agent %in% c("claude","codex"),]$agent)
 
 
 
anova(lm(df$critical_violations~df$variant*df$agent))
anova(lm(df[df$agent %in% c("claude","codex"),]$critical_violations~df[df$agent %in% c("claude","codex"),]$agent))
plotmeans(df$critical_violations~df$variant)
plotmeans(df[df$agent %in% c("claude","codex"),]$critical_violations~df[df$agent %in% c("claude","codex"),]$agent)
 
 
 
anova(lm(df$major_violations~df$variant*df$agent))
anova(lm(df[df$agent %in% c("claude","codex"),]$major_violations~df[df$agent %in% c("claude","codex"),]$agent))
plotmeans(df$major_violations~df$variant)
plotmeans(df[df$agent %in% c("claude","codex"),]$major_violations~df[df$agent %in% c("claude","codex"),]$agent)
 
 
 
anova(lm(df$minor_violations~df$variant*df$agent))
anova(lm(df[df$agent %in% c("claude","codex"),]$minor_violations~df[df$agent %in% c("claude","codex"),]$agent))
plotmeans(df$minor_violations~df$variant)
plotmeans(df[df$agent %in% c("claude","codex"),]$minor_violations~df[df$agent %in% c("claude","codex"),]$agent)
 
 
 
anova(lm(df$info_violations~df$variant*df$agent))
anova(lm(df[df$agent %in% c("claude","codex"),]$info_violations~df[df$agent %in% c("claude","codex"),]$agent))
plotmeans(df$info_violations~df$variant)
plotmeans(df[df$agent %in% c("claude","codex"),]$info_violations~df[df$agent %in% c("claude","codex"),]$agent)
 
 
 
 
 
anova(lm(df$files~df$variant*df$agent))
anova(lm(df[df$agent %in% c("claude","codex"),]$files~df[df$agent %in% c("claude","codex"),]$agent))
plotmeans(df$files~df$variant)
plotmeans(df[df$agent %in% c("claude","codex"),]$files~df[df$agent %in% c("claude","codex"),]$agent)
 
 
anova(lm(df$classes~df$variant*df$agent))
anova(lm(df[df$agent %in% c("claude","codex"),]$classes~df[df$agent %in% c("claude","codex"),]$agent))
plotmeans(df$classes~df$variant)
plotmeans(df[df$agent %in% c("claude","codex"),]$classes~df[df$agent %in% c("claude","codex"),]$agent)
 
 
anova(lm(df$functions~df$variant*df$agent))
anova(lm(df[df$agent %in% c("claude","codex"),]$functions~df[df$agent %in% c("claude","codex"),]$agent))
plotmeans(df$functions~df$variant)
plotmeans(df[df$agent %in% c("claude","codex"),]$functions~df[df$agent %in% c("claude","codex"),]$agent)
 
 
anova(lm(df$statements~df$variant*df$agent)) 
anova(lm(df[df$agent %in% c("claude","codex"),]$statements~df[df$agent %in% c("claude","codex"),]$agent))
plotmeans(df$statements~df$variant)
plotmeans(df[df$agent %in% c("claude","codex"),]$statements~df[df$agent %in% c("claude","codex"),]$agent)
anova(lm(d1$statements~d1$variant))
anova(lm(d2$statements~d2$variant))
anova(lm(d3$statements~d3$variant))
anova(lm(d4$statements~d4$variant))
anova(lm(d5$statements~d5$variant))
anova(lm(d6$statements~d6$variant))
 
 
 
anova(lm(df$ncloc~df$variant*df$agent))
anova(lm(df[df$agent %in% c("claude","codex"),]$ncloc~df[df$agent %in% c("claude","codex"),]$agent))
plotmeans(df$ncloc~df$variant)
plotmeans(df[df$agent %in% c("claude","codex"),]$ncloc~df[df$agent %in% c("claude","codex"),]$agent)
anova(lm(d1$ncloc~d1$variant))
anova(lm(d2$ncloc~d2$variant))
anova(lm(d3$ncloc~d3$variant))
anova(lm(d4$ncloc~d4$variant))
anova(lm(d5$ncloc~d5$variant))
anova(lm(d6$ncloc~d6$variant))
 
 
 
 
df<-df[df$coverage > 0, ]
anova(lm(df$coverage~df$variant*df$agent))
anova(lm(df[df$agent %in% c("claude","codex"),]$coverage~df[df$agent %in% c("claude","codex"),]$agent))
plotmeans(df$coverage~df$variant)
plotmeans(df[df$agent %in% c("claude","codex"),]$coverage~df[df$agent %in% c("claude","codex"),]$agent)