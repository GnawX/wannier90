# read the Berry connection matrix AA
import numpy as np
from scipy.io import FortranFile

fil = 'si-AA.dat'

f = FortranFile(fil,'r')
nkpts, nbnds = f.read_ints()
aa = f.read_record(np.cdouble).reshape((nkpts,nbnds,nbnds,3),order='F')
