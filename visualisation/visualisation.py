import numpy as np
from matplotlib import pyplot as plt

coeffs1 = [1.0,1.0,-4.666667,-9.555555,-0.75925994,-14.006171,-7.251543,-78.331795,-68.54959,-252.07207] #coefficients for perturbation series of eigenvalue that splits from 1
coeffs1.reverse()
p1= np.poly1d(coeffs1)
coeffs2 = [4.0,3.0,-0.61904764,5.6371894,-4.7553186,0.5315783,-10.987507,28.141384,-19.366415,38.909607]#coefficients for perturbation series of eigenvalue that splits from 4
coeffs2.reverse()
p2 = np.poly1d(coeffs2)
coeffs3 = [-3.0,1.0,5.285714,3.9183674,5.514577,13.474595,18.239044,50.190426,87.915985,213.16255]#coefficients for perturbation series of eigenvalue that splits from -3
coeffs3.reverse()
p3 = np.poly1d(coeffs3)
T0 = np.array([[1.0,0.0,0.0],[0.0,4.0,0.0],[0.0,0.0,-3.0]])
T1 = np.array([[1.0,-1.0,3.0],[-5.0,3.0,2.0],[-4.0,-8.0,1.0]])

def T(x):
    return T0+x*T1

fig, ax= plt.subplots(figsize=(8,6))
X = np.linspace(0,0.5,100)
test = np.array([np.sort(np.linalg.eigvals(T(x))) for x in X]) #numerically calculated

colors = ["red","orange","green"]
for i in range(3):
    labelstr = "Re $\lambda_{}(x)$".format(i)
    labelstr2 = "Im $\lambda_{}(x)$".format(i)
    ax.plot(X,np.real(test[:,i]),zorder=2,color =colors[i],lw=2, label = labelstr)
    ax.plot(X,np.imag(test[:,i]),zorder=2,color =colors[i], ls="--",lw=2, label= labelstr2)

ax.plot(X,p1(X),color="black",zorder=3, ls="--", label="perturbation")
ax.plot(X,p2(X),color="black",zorder=3, ls="--")
ax.plot(X,p3(X),color="black",zorder=3, ls="--")
ax.set_xlabel("$x$")
ax.set_yticks([-3,-2,-1,0,1,2,3,4,5,6])
plt.legend()
ax.set_xlim(0,0.5)
plt.grid(True)
plt.savefig("evals.png",dpi=150)

