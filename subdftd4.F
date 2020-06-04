      module vdwd4
      use prec
      use iso_fortran_env      
      use base
      use poscar
      use lattice
      use constant
      use vaspxml
      use main_mpi
      use mpimy
      use mgrid

      contains

#ifdef WITH_DFTD4
      subroutine vdw_forces_D4(io,latt_cur,dyn,t_info,tsif,tifor,toten, &
      &                        elem,ivdw)
!>
!>    Used modules
!>
      use prec
      use base
      use lattice
      use poscar
      use constant
      use tutor, only: vtutor
!>    DFT-D4 modules
      use mctc_environment
      use class_param
      use class_molecule
      use class_set
      use class_results
      use dispersion_calculator
      implicit none
!>
!>    Specific types
!>
      type(in_struct)  :: io
      type(dynamics)   :: dyn
      type(type_info)  :: t_info
      type(latt)       :: latt_cur      
!>    DFT-D4 error handler
      type(mctc_logger) :: env
!>    DFT-D4 molecule structure data
      type(molecule) :: mol
!>    Becke-Johnson damping parameter
      type(dftd_parameter) :: parameterDFTD4
!>    Set DFTD4 calculation setup
      type(dftd_options) :: optionsDFTD4
!>    DFT-D4 results container
      type(dftd_results) :: resultsDFTD4
!>    Variables for VASP - el. energy (toten), gradient (tifor), stress (tsif)
!>
      real(q)          :: toten
      real(q)          :: tsif(3,3)  
      real(q)          :: tifor(3,t_info%nions)
      character(len=2) :: elem(t_info%ntyp)
!>
!>    System specific information
!>    
      integer(int32) :: n
      logical(int32) :: pbc(3)
      integer(int32) :: at(t_info%nions)
      real(real64) :: charge
      real(real64) :: xyz(3,t_info%nions)
      real(real64) :: lat(3,3)
      real(real64) :: edisp
      real(real64) :: gdisp(3,t_info%nions)
      real(real64) :: latgrad(3,3)
      real(real64) :: sdisp(3,3)

!>    Dispersion correction version: dftd4 has only one valid version namely ivdw = 13
      integer :: ivdw 
!>    Counter variables
      integer :: i,j,k
!>    Treshold variables
      real(q) :: tresholdVDW
      real(q) :: tresholdCN

      character*10 :: method
      real(q)      :: hartreeToEV
      real(q)      :: hartreeToAngstroem
!>    First iteration?
      logical, save :: firstCycle = .true.

      hartreeToEV        = rytoev*2
      hartreeToAngstroem = autoa
      n                  = t_info%nions
      lat                = latt_cur%a/autoa

!>    Enable D4-ATM(EEQ) settings
      optionsDFTD4%lmbd        = 3          ! Axilrod-Teller-Muto term
      optionsDFTD4%refq        = 5          ! EEQ charge model
      optionsDFTD4%wf          = 6.0_q      ! Gaussian weighting factor
      optionsDFTD4%g_a         = 3.0_q      ! charge scaling factor height
      optionsDFTD4%g_c         = 2.0_q      ! charge scaling factor steepness
      optionsDFTD4%lmolpol     = .false.    ! skip molecular polarizability calculation  
      optionsDFTD4%lenergy     = .true.     ! calculate dispersion energy
      optionsDFTD4%lgradient   = .true.     ! calculate dispersion energy derivatives
      optionsDFTD4%lhessian    = .false.    ! skip hessian calculation
      optionsDFTD4%print_level = 2          ! dftd4 print level 

!>    Set up coordinates from lattice and ion position 
      xyz = matmul(lat,dyn%posion)
      
!>    Create array of ordinal numbers from element strings
      do i=1,n
        call getOrdinalNumber(elem(t_info%ityp(i)),at(i))
      enddo
!>    Extract BJ-damping parameter from functional name
!>    or read from INCAR file
      call getBeckeJohnsonParameter(parameterDFTD4, &
      &                      tresholdVDW,           &
      &                      tresholdCN,            &
      &                      io,method)
!>    Create molecule
      call mol%allocate(n,.false.)      
      mol%npbc = 3
      mol%pbc = .true.
      mol%at = at
      mol%xyz = xyz
      mol%lattice = lat      
      call mol%update
!>    Generate Wigner-Seitz cell
      call generate_wsc(mol, mol%wsc)
!>    Initialize output variables
      edisp  = 0.0_q 
      gdisp  = 0.0_q
      sdisp  = 0.0_q
!>    Perform DFT-D4 calculation from shared library
      call d4_calculation(io%iu6,env,optionsDFTD4,mol,&
      & parameterDFTD4, resultsDFTD4)
      if(.not.env%sane) then
         call env%write("Internal DFT-D4 error.")
         call vtutor%error("Internal DFT-D4 error.")
      endif
      if(allocated(resultsDFTD4%energy)) then
         edisp = resultsDFTD4%energy
      else
         call vtutor%error("DFT-D4 energy calculation was &
         &not calculated.")
      endif
      if(allocated(resultsDFTD4%gradient)) then
         gdisp = resultsDFTD4%gradient
      else
         call vtutor%error("DFT-D4 gradient calculation was &
         &not calculated.")
      endif
      if(allocated(resultsDFTD4%stress)) then
         sdisp = resultsDFTD4%stress * mol%volume
      else
         call vtutor%error("DFT-D4 stress calculation was &
         &not calculated.")
      endif
!>    Convert Hartree to eV or Angstroem and dump dispersion information
      toten = toten + edisp*hartreeToEV
      tifor = tifor - gdisp*hartreeToEV/hartreeToAngstroem
      tsif  = tsif  - sdisp*hartreeToEV
      call mol%deallocate
      call resultsDFTD4%deallocate
      end subroutine vdw_forces_D4
#else
!>    dummy routine which raises errors in case VASP is compiled w/o D4
      subroutine vdw_forces_D4(io,latt_cur,dyn,t_info,tsif,tifor,toten, &
      &                        elem,ivdw)
      use prec
      use base
      use lattice
      use poscar
      use constant
      use tutor, only: vtutor
      implicit none
      type(in_struct)  :: io
      type(dynamics)   :: dyn
      type(type_info)  :: t_info
      type(latt)       :: latt_cur
      real(q)          :: toten
      real(q)          :: tsif(3,3)  
      real(q)          :: tifor(3,t_info%nions)
      character(len=2) :: elem(t_info%ntyp)
      integer :: ivdw 
      call vtutor%error("VASP compiled w/o DFT-D4 support.")      
#endif
!>
!>    Extract ordinal number from element string
!>
      SUBROUTINE getOrdinalNumber(KEY1, NAT)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      CHARACTER*(*) KEY1
      CHARACTER*2 ELEMNT(94),E

      DATA ELEMNT/'h ','he',&
      & 'li','be','b ','c ','n ','o ','f ','ne',&
      & 'na','mg','al','si','p ','s ','cl','ar',&
      & 'k ','ca','sc','ti','v ','cr','mn','fe','co','ni','cu',&
      & 'zn','ga','ge','as','se','br','kr',&
      & 'rb','sr','y ','zr','nb','mo','tc','ru','rh','pd','ag',&
      & 'cd','in','sn','sb','te','i ','xe',&
      & 'cs','ba','la','ce','pr','nd','pm','sm','eu','gd','tb','dy',&
      & 'ho','er','tm','yb','lu','hf','ta','w ','re','os','ir','pt',&
      & 'au','hg','tl','pb','bi','po','at','rn',&
      & 'fr','ra','ac','th','pa','u ','np','pu'/

      nat=0
      e='  '
      k=1
      DO J=1,len(key1)
         if (k.gt.2)exit       
         N=ICHAR(key1(J:J))
         if(n.ge.ichar('A') .and. n.le.ichar('Z') )then
            e(k:k)=char(n+ICHAR('a')-ICHAR('A'))
            k=k+1
         endif
         if(n.ge.ichar('a') .and. n.le.ichar('z') )then
            e(k:k)=key1(j:j)
            k=k+1
         endif
      enddo

      DO I=1,94
         if(e.eq.elemnt(i))then
            NAT=I
            RETURN
         ENDIF
      ENDDO

      end SUBROUTINE getOrdinalNumber

!>
!>    Read Becke-Johnson damping parameters from INCAR file
!>
      subroutine getBeckeJohnsonParameter(dftd4Parameter,tresholdVDW,&
      & tresholdCN,io,method)
      use base
      use setexm
      use constant
      use class_param
      implicit none
    
      type(in_struct), intent(in) :: io
      type(dftd_parameter), intent(out) :: dftd4Parameter
      real(q), intent(out) :: tresholdVDW
      real(q), intent(out) :: tresholdCN
      character*10 :: method

!>    Local Variables
      LOGICAL :: LOPEN,LDUM
      INTEGER :: IDUM,IERR,N,i
      REAL(q) :: RDUM
      COMPLEX(q) :: CDUM
      CHARACTER  :: CHARAC

      tresholdVDW = 9000.0_q 
      tresholdCN  = 1600.0_q

!>    Reading from INCAR: LVDW= .FALSE. or .TRUE.
      lopen=.false.
      open(unit=io%iu5, file='INCAR', status='OLD')

!>    Select Becke-Johnson damping parameter from density functional approximation shortcut      
!>    DFT-D4(EEQ)-ATM/def2-QZVP fitted on NCIBLIND10, S22x5, S66x8
!>    Further information: J. Chem. Phys. 150, 154122 (2019); https://doi.org/10.1063/1.5090222         
      select case (lexch)
            case (4)
              method = 'b-p'
!>            Fitset: MD= 0.19316 MAD= 0.41912 RMSD= 0.60452
              dftd4Parameter = dftd_parameter ( & 
              &  s6=1.0000_q,     &
              &  s8=3.35497927_q, &
              &  a1=0.43645861_q, &
              &  a2=4.92406854_q )
            case (8) 
              method = 'pbe'
!>            Fitset: MD= -0.13857 MAD= 0.27919 RMSD= 0.47256
              dftd4Parameter = dftd_parameter ( & 
              &  s6=1.0000_q,     &
              &  s8=0.95948085_q, &
              &  a1=0.38574991_q, &
              &  a2=4.80688534_q )
            case (9) 
              method = 'rpbe'
!>            Fitset: MD= -0.03731 MAD= 0.19133 RMSD= 0.29091
              dftd4Parameter = dftd_parameter ( & 
              &  s6=1.0000_q,     &
              &  s8=1.31183787_q, &
              &  a1=0.46169493_q, &
              &  a2=3.15711757_q )
            case (40) 
              method = 'revpbe'
!>            Fitset: MD= -0.01326 MAD= 0.22598 RMSD= 0.36210
              dftd4Parameter = dftd_parameter ( & 
              &  s6=1.00000000_q,     &
              &  s8=1.74676530_q, &
              &  a1=0.53634900_q, &
              &  a2=3.07261485_q )
            case (14) 
              method = 'pbesol'
              !> Fixme: no dftd4 parameter for pbesol -> take pbe values          
!>            Fitset: MD= -0.13857 MAD= 0.27919 RMSD= 0.47256
              dftd4Parameter = dftd_parameter ( & 
              &  s6=1.00000000_q,     &
              &  s8=0.95948085_q, &
              &  a1=0.38574991_q, &
              &  a2=4.80688534_q )
            case (11,12) 
              method = 'b3-lyp'
!>            Fitset: MD= -0.05031 MAD= 0.18506 RMSD= 0.28010
              dftd4Parameter = dftd_parameter ( & 
              &  s6=1.00000000_q,     &
              &  s8=2.00246246_q, &
              &  a1=0.40276191_q, &
              &  a2=4.52778320_q )
            case default
              method=''
             IF (io%iu0 .gt. 0)  &
      &  write(io%iu0,*) &
      & 'No DFT-D parameters for this functional. They have to be', & 
      & 'assigned manually in INCAR.' 
      end select

      call rdatab(lopen,'INCAR',io%iu5,'VDW_A1','=','#',';','F', &
        &          idum,rdum,cdum,ldum,charac,n,1,ierr)
        if((ierr.eq.0).and.(rdum.gt.0.0_q).and.(rdum.lt.10.0_q))then
          dftd4Parameter%a1 = rdum
        else
          if(((ierr.ne.0).and.(ierr.ne.3)).or.((ierr.eq.0).and.(n.lt.1)))then
             if(io%iu0.ge.0)  &
             &  write(io%iu0,*) &
      & 'A1 parameter for BJ-Damping is not reasonable. Taking default.'
          endif
        endif
        
        call rdatab(lopen,'INCAR',io%iu5,'VDW_A2','=','#',';','F',& 
        &          idum,rdum,cdum,ldum,charac,n,1,ierr)
        if((ierr.eq.0).and.(rdum.gt.0.0_q).and.(rdum.lt.10.0_q))then
          dftd4Parameter%a2 = rdum
        else
          if(((ierr.ne.0).and.(ierr.ne.3)).or.((ierr.eq.0).and.(n.lt.1)))then
             if(io%iu0.ge.0) &
             &  write(io%iu0,*) &
      & 'A2 parameter for BJ-Damping is not reasonable. Taking default.'
          endif
       endif

        call rdatab(lopen,'INCAR',io%iu5,'VDW_S8','=','#',';','F',& 
        &          idum,rdum,cdum,ldum,charac,n,1,ierr)
        if((ierr.eq.0).and.(rdum.gt.0.0_q).and.(rdum.lt.10.0_q))then
          dftd4Parameter%s8 = rdum
        else
          if(((ierr.ne.0).and.(ierr.ne.3)).or.((ierr.eq.0).and.(n.lt.1)))then
             if(io%iu0.ge.0) &
             &  write(io%iu0,*) &
      & 'S8 parameter for BJ-Damping is not reasonable. Taking default.'
          endif
       endif

      close(io%iu5)
      end subroutine getBeckeJohnsonParameter
      end module vdwd4
