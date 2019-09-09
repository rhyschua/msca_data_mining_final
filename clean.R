library(dplyr)
library(tidyr)
library(magrittr)

final_data <- read.csv("sdoh_final_data.csv")


##2012 health rate
final_data$num_ppl_normal.bp_2012_r <- final_data$num_ppl_normal.bp_2012/final_data$total_pat_2012
final_data$num_ppl_normal.pre.ctrl_2012_r <- final_data$num_ppl_normal.pre.ctrl_2012/final_data$total_pat_2012
final_data$num_ppl_pre.htn_2012_r <- final_data$num_ppl_pre.htn_2012/final_data$total_pat_2012
final_data$num_ppl_stage1.ctrl_2012_r <- final_data$num_ppl_stage1.ctrl_2012/final_data$total_pat_2012
final_data$num_ppl_stage1.unctrl_2012_r <- final_data$num_ppl_stage1.unctrl_2012/final_data$total_pat_2012
final_data$num_ppl_stage2.ctrl_2012_r <- final_data$num_ppl_stage2.ctrl_2012/final_data$total_pat_2012
final_data$num_ppl_stage2.unctrl_2012_r <- final_data$num_ppl_stage2.unctrl_2012/final_data$total_pat_2012
final_data$final_htn_num_2012_r <- final_data$final_htn_num_2012/final_data$total_pat_2012

###2014 health rate
final_data$num_ppl_normal.bp_2014_r <- final_data$num_ppl_normal.bp_2014/final_data$total_pat_2014
final_data$num_ppl_normal.pre.ctrl_2014_r <- final_data$num_ppl_normal.pre.ctrl_2014/final_data$total_pat_2014
final_data$num_ppl_pre.htn_2014_r <- final_data$num_ppl_pre.htn_2014/final_data$total_pat_2014
final_data$num_ppl_stage1.ctrl_2014_r <- final_data$num_ppl_stage1.ctrl_2014/final_data$total_pat_2014
final_data$num_ppl_stage1.unctrl_2014_r <- final_data$num_ppl_stage1.unctrl_2014/final_data$total_pat_2014
final_data$num_ppl_stage2.ctrl_2014_r <- final_data$num_ppl_stage2.ctrl_2014/final_data$total_pat_2014
final_data$num_ppl_stage2.unctrl_2014_r <- final_data$num_ppl_stage2.unctrl_2014/final_data$total_pat_2014
final_data$final_htn_num_2014_r <- final_data$final_htn_num_2014/final_data$total_pat_2014

###2016 health rate
final_data$num_ppl_normal.bp_2016_r <- final_data$num_ppl_normal.bp_2016/final_data$total_pat_2016
final_data$num_ppl_normal.pre.ctrl_2016_r <- final_data$num_ppl_normal.pre.ctrl_2016/final_data$total_pat_2016
final_data$num_ppl_pre.htn_2016_r <- final_data$num_ppl_pre.htn_2016/final_data$total_pat_2016
final_data$num_ppl_stage1.ctrl_2016_r <- final_data$num_ppl_stage1.ctrl_2016/final_data$total_pat_2016
final_data$num_ppl_stage1.unctrl_2016_r <- final_data$num_ppl_stage1.unctrl_2016/final_data$total_pat_2016
final_data$num_ppl_stage2.ctrl_2016_r <- final_data$num_ppl_stage2.ctrl_2016/final_data$total_pat_2016
final_data$num_ppl_stage2.unctrl_2016_r <- final_data$num_ppl_stage2.unctrl_2016/final_data$total_pat_2016
final_data$final_htn_num_2016_r <- final_data$final_htn_num_2016/final_data$total_pat_2016

write.csv(final_data, "sdoh_final_data.csv", row.names = FALSE)
