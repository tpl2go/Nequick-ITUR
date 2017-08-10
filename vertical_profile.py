import NeQuick_ITUR
import numpy as np
import matplotlib.pyplot as plt

hs = np.arange(100,1000)

Ne = np.empty(np.shape(hs))

for i in range(len(hs)):
    # argument signature: nequick(h,alat,along,mth,flx,ut)
    Ne[i] = NeQuick_ITUR.nequick(hs[i], 40, 0, 4, 100, 12)

plt.plot(hs, Ne)
plt.show()
