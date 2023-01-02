<script type="text/x-mathjax-config">
    MathJax.Hub.Config({
      tex2jax: {
        skipTags: ['script', 'noscript', 'style', 'textarea', 'pre'],
        inlineMath: [['$','$']]
      }
    });
  </script>
  <script src="https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML" type="text/javascript"></script> 

# Two Source Capture Recapture

We will use the study [Population Size, HIV, and Behavior Among MSM in Luanda,
Angola: Challenges and Findings in the First Ever HIV and
Syphilis Biological and Behavioral Survey](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4733005/pdf/nihms753095.pdff), where they reported the results of a two source capture recapture with unique objects. They reported $n_1=792$ people were given the unique object, $n_2=353$ participated in the second capture, with $n_{12}=45$ recaptures.

Plugging these number into the [confidence interval calculator](https://www.quantitativeskills.com/sisa/statistics/crc.htm) results in an interval of $(4566.095,7859.505)$.


# Proportion Confidence Interval Example

We will start with the study [High levels of used syringe use and unsafe sex among people who inject drugs in Kumasi, Ghana: an urgent call for a comprehensive harm reduction approach](https://harmreductionjournal.biomedcentral.com/articles/10.1186/s12954-021-00510-7), which reports a 3% HIV prevalence rate with a sample size of 211. We will use a design effect of 1.5, and so $n=211/1.5=141$. This leads to a positive count of

$$n_+ = .03 * 141 = 4$$

Plugging $n_+=4$ and $n=141$ this into the [calculator](https://epitools.ausvet.com.au/ciproportion) results in a Clopper-Pearson interval of $(0.0078, 0.0710)$.
