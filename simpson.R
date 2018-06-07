simpson <- function(a, b, dx, n, func)
{
	xk <- seq(from = a, to = b, by = dx)
	v <- rep(1, n + 1)
	v[2:n] <- ifelse(2:n %% 2 == 0, 4, 2)
	c <- func(xk) ##log(4 + 3 * cos(xk))
	d <- c * v
	return(sum(d) * (dx / 3))
}
