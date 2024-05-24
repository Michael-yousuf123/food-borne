#### SAMPLE SIZE CALCULATION FOR ESTIMATING SENSITIVITY AND SPECIFICITY OF A DIAGNOSTIC TEST ####

### Based on Buderer NMF. Statistical Methodology: I. Incorporating the Prevalence of Disease into the Sample Size Calculation for Sensitivity and Specificity. Academic Emergency Medicine. 1996;3: 895–900. doi:10.1111/j.1553-2712.1996.tb03538.x

## Function to calculate sample size
# inputs: se = sensitivity, sp = specificity, p = prevalence, w = width of the confidence interval

s_size <- function(se, sp, p, w) {
  a_c = 1.96^2 * se * (1 - se) / w^2
  n1 = ceiling(a_c / p)
  b_d = 1.96^2 * (1 - sp) * sp / w^2
  n2 = ceiling(b_d / (1 - p))
  n = max(n1, n2)
  return(c(n1, n2, n))
}

## Example as in Buderer Appendix A
# inputs: se = sensitivity, sp = specificity, p = prevalence, w = width of the confidence interval
input <- list(se = 0.9, sp = 0.85, p = 0.2, w = 0.1)
# output: n1 = sample size for sensitivity, n2 = sample size for specificity, n = maximum sample size
output <- s_size(input$se, input$sp, input$p, input$w)
print(data.frame(
  se = input$se,
  sp = input$sp,
  p = input$p,
  w = input$w,
  n1 = output[1],
  n2 = output[2],
  n = output[3]
))

## Some realistic values for a PCR test
input <- list(se = 0.95, sp = 0.95, p = 0.1, w = 0.10)
output <- s_size(input$se, input$sp, input$p, input$w)
print(data.frame(
  se = input$se,
  sp = input$sp,
  p = input$p,
  w = input$w,
  n1 = output[1],
  n2 = output[2],
  n = output[3]
))

## Some realistic? values for shotgun test
input <- list(se = 0.90, sp = 0.99, p = 0.1, w = 0.10)
output <- s_size(input$se, input$sp, input$p, input$w)
print(data.frame(
  se = input$se,
  sp = input$sp,
  p = input$p,
  w = input$w,
  n1 = output[1],
  n2 = output[2],
  n = output[3]
))

## Reduce power to 80%
## Some realistic values for shotgun test
input <- list(se = 0.90, sp = 0.99, p = 0.2, w = 0.10)
output <- s_size(input$se, input$sp, input$p, input$w)
print(data.frame(
  se = input$se,
  sp = input$sp,
  p = input$p,
  w = input$w,
  n1 = output[1],
  n2 = output[2],
  n = output[3]
))
