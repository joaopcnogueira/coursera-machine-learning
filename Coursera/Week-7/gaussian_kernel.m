function f = gaussian_kernel(x1, x2)
  sigma2 = 1;
  f = exp(-((x1-x2)'*(x1-x2))/(2*sigma2));
end