# AK: These two lines should be moved into 1_analysis.r
# also don't be shy to add comments to tell what these numbers mean
sum(is.na(df_long_publishers$type))
# This number tells us how many times type of media was not defined automatically.
unique(df_long_publishers$publisher[is.na(df_long_publishers$type)])
# The list of publishers left can be used to assign types manually.