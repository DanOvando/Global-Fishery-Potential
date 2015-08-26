quartz()

a <- ggplot(subset(UnlumpedProjectionData, Policy == "Business As Usual Pessimistic" ), aes(BvBmsy, FvFmsy,color = Year, group = IdOrig), alpha = 0.3) + geom_point()