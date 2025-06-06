# Prior Specification

Priors are a way of defining expert knowledge about a population independent of
the empirical estimates that are going to be triangulated. These beliefs are
then updated based on the empirical evidence.

## Prior Types

Two types of priors are supported. The first is the "uninformative" prior, which
just puts a flat distribution over all potential values. Note that this is flat 
in the transformed space. Behind the scenes, the triangulator preforms a log transform
on population size estimates and a logit transform on proportion estimates.

The second type is an "informative" prior, which is a normal distribution in the
transformed space.

In most cases, stakeholders will have some knowledge of the population under study,
so an informative prior is recommended as a best practice. However, if stakeholders
are unwilling or uncomfortable providing a prior, using an uninformative prior is
a good alternative.


## Eliciting A Good Informative Prior
Determining a good prior is an art form. One way to elicit a good prior for this
application is to gather the stakeholders together and work through the following
steps:

1. Have each stakeholder produce a value that they think is their best guess
of the truth. This should be a priori, so the studies and estimates you are
combining should be ignored as much as possible in the process.
2. Discuss the guesses produced and come up with a single guess for the group.
Input this value into the "Median" field.
3. Each stakeholder recognizes that there is uncertainty in their guess. The 
next step is to quantify that uncertainty. What is the number where there is a 75%
chance that the true value is below it, and a 25% chance it is above it? Have
stakeholders discuss and come up with a value.
4. Finally, what are the upper and lower bounds, above (or below) which is 
implausible. For example, in a population size estimate we know that the population
size is greater than the size of the largest study done in the population and
below the population size of the world.
5. Now look at the distribution plot. Does this reasonably reflect the groups
beliefs about what the true value could be? Build consensus around this. Feel free
to run through the steps again to dial in the distribution to something that 
everyone can agree on.
