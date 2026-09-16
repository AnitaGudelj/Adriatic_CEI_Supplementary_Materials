winter_nat <- winter %>%
  filter(UdioNat_SeaArea0 > 0)

summer_nat <- summer %>%
  filter(UdioNat_SeaArea0 > 0)

nrow(winter_nat)
nrow(summer_nat)

summary(winter_nat$UdioNat_SeaArea0)
summary(winter_nat$tr_mean)
summary(winter_nat$depth_sea)
summary(winter_nat$CEI)