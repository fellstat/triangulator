# Tau Multiplier

The "Confidence" values specified for each study in the first tab add uncertainty
to the estimates due to potential study bias not accounted for in the sampling design.

The value of tau is the scale of how much bias exists across studies after accounting for the "Confidence" values. We set the prior scale for tau to be equal to a "multiplier" that is multiplied against the standard deviation of the estimates.

We typically want tau to be very small, indicating a belief that between study biases
have been accounted for by specification of the "Confidence" values. However,
if the data is inconsistent with that belief this should be picked up. A tau
multiplier of 0.01 serves well in most cases.

