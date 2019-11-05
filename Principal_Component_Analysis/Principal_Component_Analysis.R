data("USArrests")
states=row.names(USArrests) 
states
names(USArrests)


pr.out=prcomp(USArrests, scale=TRUE)
names(pr.out)

pr.out$center
pr.out$scale

pr.out$rotation
dim(pr.out$x)

# We can plot the first two principal components as follows:
biplot(pr.out, scale=0)
# The scale=0 argument to biplot() ensures that the arrows are scaled to represent the loadings;
# other values for scale give slightly different biplots with different interpretations.

# The prcomp() function also outputs the standard deviation of each principal component. 
#For instance, on the USArrests data set, we can access these standard deviations as follows:
pr.out$sdev

# The variance explained by each principal component is obtained by squaring these:
pr.var = pr.out$sdev^2
pr.var

# To compute the propotion of variance explained by each principal component, we simply
# devide the variance explained by each principal component by the total variance explained
# by all four principal components.
pve = pr.var/sum(pr.var)
pve





