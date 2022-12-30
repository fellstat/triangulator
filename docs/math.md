<script type="text/x-mathjax-config">
    MathJax.Hub.Config({
      tex2jax: {
        skipTags: ['script', 'noscript', 'style', 'textarea', 'pre'],
        inlineMath: [['$','$']]
      }
    });
  </script>
  <script src="https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML" type="text/javascript"></script> 


# Mathematical Details

Suppose that we have $n$ estimates of a quantity of interest along with confidence (or credible) intervals for each. We begin by (optionally) applying a transformation $g$ to the estimates with inverse $g^{-1}$. This transformation is designed so that the sampling distribution for transformed estimates to be closer to normal than
raw estimates. Population Size estimates are log transformed. Proportion estimates
are logit transformed. Other estimates use the identity transformation.

If the ith raw estimate is $y'_i$ with lower and upper 95% interval ($l'_i$,$u'_i$), the transformed data is then $y_i=g(y'_i)$, $l_i=g(l'_i)$, and $u_i=g(u'_i)$. If the estimate follows the central limit theorem then the standard error is $\sigma_i=\frac{u_i-l_i}{2*1.96}$.

We construct a Bayesian hierarchical model using the transformed data where the data distribution is

$$y_i \sim N(\eta_i,\frac{\sigma_i}{c_i}),$$

where $\eta_i$ is the expected value of the estimate and $c_i$ is the design confidence. The true population value is denoted by $\theta$, so the bias of the ith study is $\eta_i - \theta$. we model the biases as

$$ \eta_i \sim N(\theta, \tau).$$

The prior for the population quantity is truncated normal

$$\theta \sim N(\mu_0, \sigma_0, \textrm{lower}, \textrm{upper}).$$

The prior for $\tau$ is half-cauchy in order to put most of the probability mass near 0 (i.e. no study bias not accounted for by the design confidences) with heavy tails, which allows for large amounts of bias if the data calls for it.

$$ \tau \sim \textrm{half_cauchy}( \alpha ).$$

$\alpha$ is chosen so that it is much smaller than the sample variability of the $y$s. By default we choose $\alpha=0.01s(y)$, where $s(y)$ is the observed sample standard deviation of the $y_i$s. The $0.01$ is dubbed the $\tau$ multiplier.

The posterior for $\theta$ has no closed form solution, but is easily sampled from using [Stan](https://mc-stan.org/). This results in $k$ sampled values from the posterior $\theta^{(1)},...,\theta^{(k)}$, which are then transformed back into the original scale of the population quantity $g^{-1}(\theta^{(1)}),...,g^{-1}(\theta^{(k)})$ and displayed to the user.
