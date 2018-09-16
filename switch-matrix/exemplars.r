F1 = c(0,0,0,0,1,1,1,1)
F2 = c(0,0,1,1,0,0,1,1)
F3 = c(0,1,0,1,0,1,0,1)
exemplars = data.frame(F1,F2,F3)
exemplarstr = apply(exemplars,1,paste,collapse="")
rownames(exemplars) = exemplarstr

Type1 = c(1,1,1,1,2,2,2,2)
Type2 = c(1,1,2,2,2,2,1,1)
Type3 = c(1,1,2,1,1,2,2,2)
Type4 = c(1,1,1,2,1,2,2,2)
Type5 = c(1,2,2,2,2,1,1,1)
Type6 = c(1,2,2,1,2,1,1,2)
assignments = data.frame(Type1,Type2,Type3,Type4,Type5,Type6)
rownames(assignments) = exemplarstr


#   Type Two
# 000 010
# 001 011
# 111 100
# 110 101

#   Type Three  
# 000 111
# 001 110
# 010 011
# 101 100

#   Type Four
# 000 111
# 001 110
# 010 101
# 100 011