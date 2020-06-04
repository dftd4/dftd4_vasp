# Setup *dftd4* for the *VASP* quantum chemistry code

Get your copy of the *dftd4*  code and install the code to obtain a shared library.
Tested using *ifort* (18.0.2).

```bash
FC=ifort CC=icc CXX=icpc meson setup build && ninja -C build
mkdir install
DESTDIR=/path/to/staging/area/install ninja -C build install
```

Extend your PATH by the build directory and be sure the share library is available for linking purposes.

Modify your makefile.include

```bash
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
IF (IVDW==1 .OR. IVDW==2 .OR. IVDW==3 .OR. IVDW==4 .OR. (IVDW>=10 .AND. **IVDW<=13**) .OR.  &
&   IVDW==101 .OR. (IVDW>=20 .AND. IVDW<=22) .OR. IVDW==26 .OR.  IVDW==202 &
&   .OR. IVDW==212 .OR. IVDW==263 .OR. IVDW==612  ) THEN
  LVDW=.TRUE.
ELSE
  LVDW=.FALSE.
  CALL PROCESS_INCAR(LOPEN, IO%IU0, IO%IU5, 'LVDW', LVDW, IERR, WRITEXMLINCAR)
  IF (LVDW) IVDW=1
ENDIF
``` 
