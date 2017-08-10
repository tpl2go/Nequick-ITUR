""" Create a database of vertical profiles.
Database can be useful for comparing profiles of different models"""
import numpy as np
import os
import NeQuick_ITUR
import numpy as np


""" Input Parameters here """
mths = [4]
UTs = [12]
lats = [40]
lons = [100]
Azs = [64]

""" Processing Below """
for Az in Azs:
    for mth in mths:
        for UT in UTs:
            for lat in lats:
                for lon in lons:

                    hs = np.arange(100, 2000)
                    Ne = np.empty(np.shape(hs))
                    
                    for i in range(len(hs)):
                        # argument signature: nequick(h,alat,along,mth,flx,ut)
                        Ne[i] = NeQuick_ITUR.nequick(hs[i], lat, lon, mth, Az, UT)
                    # Naming convention:
                    # Azr / mth / time / position

                    path = os.path.join('profiles','Az:'+str(int(Az)), 'mth:'+str(mth), 'UT:' + str(UT))
                    if not os.path.exists(path):
                        os.makedirs(path)

                    out = np.transpose(np.vstack([hs,Ne]))
                    np.savetxt(os.path.join(path , str(lat)+ 'N' + str(lon)+'E'), out)
