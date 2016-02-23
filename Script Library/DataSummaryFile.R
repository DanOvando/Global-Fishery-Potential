########################################################################################
#
# Script containing data summary calculations for the upside model
#
########################################################################################

## Number and % of global fisheries below and above B/Bmsy and F/Fmsy of 0.8 ----
ProjectionData %>%
  filter(Year==2012 & is.na(BvBmsy)==F) %>%
  group_by(Year) %>%
  summarize(NumBelowBof1=length(BvBmsy[BvBmsy<0.8]),
            NumAboveBof1=length(BvBmsy[BvBmsy>=0.8]), 
            NumBelowFof1=length(FvFmsy[FvFmsy<1]),
            NumAboveFof1=length(FvFmsy[FvFmsy>=1]),
            PercBelowBof1=100*(length(BvBmsy[BvBmsy<0.8])/length(BvBmsy)),
            PercAboveBof1=100*(length(BvBmsy[BvBmsy>=0.8])/length(BvBmsy)),
            PercBelowFof1=100*(length(FvFmsy[FvFmsy<1])/length(FvFmsy)),
            PercAboveFof1=100*(length(FvFmsy[FvFmsy>=1])/length(FvFmsy))) %>%
  ungroup()

UnlumpedProjectionData %>%
  filter(Year==2012 & is.na(BvBmsy)==F) %>%
  group_by(Year) %>%
  summarize(NumBelowBof1=length(BvBmsy[BvBmsy<1]),
            NumAboveBof1=length(BvBmsy[BvBmsy>=1]), 
            NumBelowFof1=length(FvFmsy[FvFmsy<1]),
            NumAboveFof1=length(FvFmsy[FvFmsy>=1]),
            PercBelowBof1=100*(length(BvBmsy[BvBmsy<1])/length(BvBmsy)),
            PercAboveBof1=100*(length(BvBmsy[BvBmsy>=1])/length(BvBmsy)),
            PercBelowFof1=100*(length(FvFmsy[FvFmsy<1])/length(FvFmsy)),
            PercAboveFof1=100*(length(FvFmsy[FvFmsy>=1])/length(FvFmsy))) %>%
  ungroup()

## Number and % of global fisheries below and above B/Bmsy of 0.8 and F/Fmsy of 1 by Policy in 2050 ----
ProjectionData %>%
  filter(Year==2050 & is.na(BvBmsy)==F) %>%
  group_by(Year, Policy) %>%
  summarize(NumBelowBof1=length(BvBmsy[BvBmsy<0.8]),
            NumAboveBof1=length(BvBmsy[BvBmsy>=0.8]), 
            NumBelowFof1=length(FvFmsy[FvFmsy<1]),
            NumAboveFof1=length(FvFmsy[FvFmsy>=1]),
            PercBelowBof1=100*(length(BvBmsy[BvBmsy<0.8])/length(BvBmsy)),
            PercAboveBof1=100*(length(BvBmsy[BvBmsy>=0.8])/length(BvBmsy)),
            PercBelowFof1=100*(length(FvFmsy[FvFmsy<1])/length(FvFmsy)),
            PercAboveFof1=100*(length(FvFmsy[FvFmsy>=1])/length(FvFmsy))) %>%
  ungroup()

