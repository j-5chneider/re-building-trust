# generating randomized blocks for the 12 conditions
# that'll be pulled in formr in survey "rbt_0" in variable "treat" (row 2)
set.seed(100)
planned_sample_size <- 2400
groups <- 12
randblocks <- psych::block.random(planned_sample_size, groups)
write.csv(randblocks, file = "4_questionnaire/randblocks.csv", row.names = F)

# then the file 'randblocks.csv' needs to be pushed to github from where formr grabs the file