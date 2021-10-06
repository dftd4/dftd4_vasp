# This project is deprecated

You can find instructions to build Vasp with support for DFT-D4 in the main repository at [dftd4/dftd4](https://github.com/dftd4/dftd4).
Follow the instructions in the [README](https://github.com/dftd4/dftd4#building-vasp-with-support-for-d4) to build `dftd4` and make it available in Vasp.


## Setup *dftd4* for the *VASP* quantum chemistry code

[Get your copy](https://github.com/dftd4/dftd4) of the *dftd4*  code and install the code to obtain a shared library.
Tested using *ifort* (18.0.2).

```bash
FC=ifort CC=icc CXX=icpc meson setup build && ninja -C build
mkdir install
DESTDIR=/path/to/staging/area/install ninja -C build install
```

Extend your PATH by the build directory and be sure the shared library is available for linking purposes.

Modify your makefile.include

```bash
# Precompiler options
CPP_OPTIONS= -DHOST=\"LinuxIFC\"\
             -DMPI -DMPI_BLOCK=8000 \
             -Duse_collective \
             -DscaLAPACK \
             -DCACHE_SIZE=4000 \
             -Davoidalloc \
             -Duse_bse_te \
             -Dvasp6 \
             -Dtbdyn \
             -DWITH_DFTD4
# dftd4 path
DFTD4_PATH =/path/to/staging/area/install/usr/local
DFTD4      =-ldftd4 -L${DFTD4_PATH}/lib64/

LLIBS      = ...all libs...  $(DFTD4)
INCS       = ...all modules... -I${DFTD4_PATH}/include
```

Within *src/.objects* add "subdftd4.o\" below "subdftd3.o\".

Add the *dftd4* module within *src/vdwforcefield.F* by incorporating it with "USE vdwD4".

Within the "vdw_forces_main" subroutine, add a new IVDW case for *dftd4* just below the one of *dftd3*, which has "CASE(12)".

```fortran
! D4-ATM(EEQ) implemented by Eike Caldeweyher
CASE(13)
  CALL vdw_forces_D4(IO,LATT_CUR,DYN,T_INFO,TSIF,TIFOR,TOTEN,ELEM,IVDW)
``` 

Within the "vdw_read" subroutine, add the *dftd4* case (IVDW=13).
After processing the *INCAR* file this can easily be added:

```fortran
IF (IVDW==1 .OR. IVDW==2 .OR. IVDW==3 .OR. IVDW==4 .OR. (IVDW>=10 .AND. IVDW<=13) .OR.  &
&   IVDW==101 .OR. (IVDW>=20 .AND. IVDW<=22) .OR. IVDW==26 .OR.  IVDW==202 &
&   .OR. IVDW==212 .OR. IVDW==263 .OR. IVDW==612  ) THEN
  LVDW=.TRUE.
ELSE
  LVDW=.FALSE.
  CALL PROCESS_INCAR(LOPEN, IO%IU0, IO%IU5, 'LVDW', LVDW, IERR, WRITEXMLINCAR)
  IF (LVDW) IVDW=1
ENDIF
``` 
This implementation is tested for *VASP 6.0.X* versions. For earlier *VASP* versions take care of the vtutor module,
which is used within the subdftd4.F file. vtutor is not available in those versions (comment out those parts).

Citation
--------

Always cite:

Eike Caldeweyher, Christoph Bannwarth and Stefan Grimme, *J. Chem. Phys.*, **2017**, 147, 034112.
DOI: [10.1063/1.4993215](https://doi.org/10.1063/1.4993215)

Eike Caldeweyher, Sebastian Ehlert, Andreas Hansen, Hagen Neugebauer, Sebastian Spicher, Christoph Bannwarth and Stefan Grimme, *J. Chem Phys*, **2019**, 150, 154122.
DOI: [10.1063/1.5090222](https://doi.org/10.1063/1.5090222)
chemrxiv: [10.26434/chemrxiv.7430216](https://doi.org/10.26434/chemrxiv.7430216.v2)

Eike Caldeweyher, Jan-Michael Mewes, Sebastian Ehlert and Stefan Grimme, *Phys. Chem. Chem. Phys.*, **2020**, 22, 8499-8512.
DOI: [10.1039/D0CP00502A](https://doi.org/10.1039/D0CP00502A)
chemrxiv: [10.26434/chemrxiv.10299428](https://doi.org/10.26434/chemrxiv.10299428.v1)

Bugs
----

please report all bugs with an example input and the used geometry to [me](mailto:eike@thch.uni-bonn.de)
or open an issue.
