PROGRAM Siberian_jet_time_evolution
USE SUBS_SPLINES
USE SUBS
USE FLOW_INTEGRALS
IMPLICIT NONE

INTEGER :: LMAX_OBS, LMAX_SV, M_wave_max_MAX, M_wave_step, FILTER_SIGNAL, NTHETA_GRID, NTHETA_LSQ, IOS,L, M, MMAX_SV, M_wave_max, HARMONIC, NTHETA_VIS, NPHI_VIS, SINCOS, I
CHARACTER(300) :: FILENAME, JUNK, FILESTEM
REAL( KIND = 8) ::DELTA, lat_min1, lat_max1, lat_min2, lat_max2, VALUE(1:2), LSQ, SV_SQ, starttime, endtime, step, dt, PHI, THETA
CHARACTER(1) :: STRING_HARM_TYPE2
REAL( KIND = 8), ALLOCATABLE :: GAUSS(:), GAUSS_SV(:), SV_OPT(:,:), FLOW(:,:,:), FLOW_COEFFS(:), SV_OBS_VIS(:,:), GAUSS_SYN(:), KE_SPEC(:)
TYPE (HARMONIC_STRUCTURE), ALLOCATABLE, DIMENSION(:) :: HARMONICS
LOGICAL :: JUST_EVALUATE
REAL( KIND = 8), ALLOCATABLE :: GT(:,:), GTD(:,:), SPL(:),tknts(:), g(:,:), gta(:,:,:)
INTEGER :: lmax,nspl,n,np,nl,jord, k,nleft,j,der,dermax, ns1, I_THETA, I_PHI

! Fits a model of a broad jet under Siberia to the SV. The model satisfies
! quasi-z-invariance; incompressibility; large shear in phi direction.

! u_phi = (sqrt(1-rho^2) * exp(-(rho-ri)^2/delta^2)  - c )  * rho^(m+1)
! u_theta = [derived but analytic expression]
! u_r = 0

! Uses three grids:
! (a) large whole sphere to do the transforms
! (b) 2 x localised grids for least-squares fitting

! This code forms a grid of LSQ values over various max wave numbers and jet widths.

PRINT*, 'ENTER FILENAME FOR MAGNETIC FIELD SPLINE MODEL'
READ*, FILENAME

PRINT*,'Enter order of splines used in model: gufm is 4, CHAOS is 6'
READ*, jord

print*,'Enter start date, end date, step (to 1dp) e.g. 2005 2007 0.2 '
read(5,*) starttime, endtime, step

PRINT*, 'ENTER MAX DEGREE FOR MAGNETIC FIELD MODEL'
READ*, LMAX_OBS

PRINT*, 'ENTER MAX DEGREE FOR SV OBSERVATIONS'
READ*, LMAX_SV



PRINT*, 'ENTER DELTA'
READ*, DELTA

PRINT*, 'ENTER max wave number for flow'
READ*, M_wave_max

PRINT*, 'ENTER LATITUDE range (1) for least-squares fit'
READ*, lat_min1, lat_max1

PRINT*, 'ENTER LATITUDE range (2) for least-squares fit'
READ*, lat_min2, lat_max2

PRINT*, 'DO YOU WANT TO FILTER THE SYNTHETIC SV BEFORE LSQ FIT? (1 = YES, 0 = NO)'
READ*, FILTER_SIGNAL

! only the SV up to degree LMAX_OBS_SV is compared with the observed SV.

PRINT*, 'ENTER NUMBER OF THETA GRID PTS FOR: TRANSFORM, LSQ FIT (PER LATITUDE RANGE)'
READ*, NTHETA_GRID, NTHETA_LSQ

PRINT*, 'ENTER NUMBER THETA and PHI PTS FOR VISUALISATION'
READ*, NTHETA_VIS, NPHI_VIS

PRINT*, 'FILESTEM FOR GRIDDED-FLOW OUTPUT, OR N IF NONE'
READ*, FILESTEM

JUST_EVALUATE = .FALSE.

! Read in Gauss coefficients for field.
OPEN(1,file=filename, STATUS = 'OLD', IOSTAT = IOS)
IF( IOS .NE. 0) then
PRINT*, 'CAN''T OPEN FILE ', TRIM(FILENAME)
stop
endif
read(1,*)
read(1,*) LMAX,NSPL
n=lmax*(lmax+2)
np=n*nspl
nl=(lmax+1)*(lmax+2)/2
dermax = 1

ALLOCATE( gt(n,nspl),gtd(n,nspl),spl(nspl),tknts(nspl+jord),g(n,0:dermax),gta(n,nspl,0:dermax) )
! read in spline data:
read(1,*) (tknts(k),k=1,nspl+jord)
read(1,*) ((gt(k,ns1), k = 1,lmax*(lmax+2)), ns1 = 1,nspl)
CLOSE(1)

!     copy these coefficients to the more general array
!     for coefficients of field and derivatives

do j=1,nspl
do k=1,n
gta(k,j,0) = gt(k,j)
end do
end do

!-----
!     calculate secular variation coefficients and higher
!     derivatives (to degree dermax) if required at dt
!     first calculate the derivative of the spline coefficients
!     by differencing following de Boor, p116
!     obtain higher degrees by iteration
!     der gives the order of differentiation

do der=1,dermax
call derivcoeff(n,nspl,jord,der,tknts,gt,gtd)
do j=1,nspl
do k=1,n
gta(k,j,der) = gtd(k,j)
!           reuse gt for ease of iteration
gt(k,j) = gtd(k,j)
end do
end do
end do


ALLOCATE( GAUSS_SV(1:LMAX_SV*(LMAX_SV +2))  )
ALLOCATE( GAUSS(1:LMAX_OBS*(LMAX_OBS +2))  )

MMAX_SV = LMAX_SV

! define harmonics structure to be consistent with order of Gauss coefficients

ALLOCATE(HARMONICS(1: LMAX_SV * (LMAX_SV + 2) ) )
HARMONIC = 1
DO L = 1, LMAX_SV
DO M = 0, L
DO SINCOS = COSINE_HARMONIC, SINE_HARMONIC
!cos is 1, sin is 2.
IF( M .eq. 0 .AND. SINCOS .eq. 2) CYCLE
HARMONICS(HARMONIC)%M = M
HARMONICS(HARMONIC)%SINCOS = SINCOS
HARMONICS(HARMONIC)%L = L
HARMONIC= HARMONIC+1
ENDDO
ENDDO
ENDDO

OPEN(21, FILE = 'HARMONICS.DAT', FORM = 'FORMATTED', STATUS = 'REPLACE')
DO I=1, LMAX_SV * (LMAX_SV + 2)
IF(HARMONICS(I)%SINCOS .eq. COSINE_HARMONIC) WRITE(STRING_HARM_TYPE2,'(A)') 'C'
IF(HARMONICS(I)%SINCOS .eq. SINE_HARMONIC) WRITE(STRING_HARM_TYPE2,'(A)') 'S'

WRITE(21,'(i3,3x, i3, 3x, A1)')  &
HARMONICS(I)%L, HARMONICS(I)%M, STRING_HARM_TYPE2
ENDDO
CLOSE(21)

ALLOCATE( SV_OPT(1:NTHETA_VIS, 0:NPHI_VIS-1), FLOW(1:NTHETA_VIS, 0:NPHI_VIS-1,1:2), SV_OBS_VIS(1:NTHETA_VIS, 0:NPHI_VIS-1) )
ALLOCATE( GAUSS_SYN(1:LMAX_SV * (LMAX_SV+2)))


ALLOCATE( KE_SPEC(1:2*M_wave_max+1))
ALLOCATE( FLOW_COEFFS(1:2*M_wave_max+1))
OPEN(11, FILE = 'FLOW_RMS_WAVENUMBER.DAT')
WRITE(11,*) 'DATE  M=0    M=1   MINVAL(u_phi)'
!print*, 'Date  patch_lat   patch_long   long_speed'
dt = starttime - step
DO WHILE ( dt < ENDtime )
dt = dt + step

call interv(tknts,dt,nspl,nleft,jord)

! calculate the B-splines
do der = 0,dermax
! calculate the B-splines, reduced for the derivatives
call bspline(tknts,dt,nspl,jord-der,nleft,spl(nleft-jord+1+der))

do  k=1,n
g(k,der)=0.0
do j=1,jord-der
g(k,der) = g(k,der) + spl(j+nleft-jord+der)*gta(k,j+nleft-jord+der,der)
enddo
enddo
end do

GAUSS(1:LMAX_OBS*(LMAX_OBS+2)) = g(1:LMAX_OBS*(LMAX_OBS+2),0)
GAUSS_SV(1:LMAX_SV*(LMAX_SV+2)) = g(1:LMAX_SV*(LMAX_SV+2),1)

!PRINT*, GAUSS_SV
CALL FIND_OPT_FLOW( GAUSS, GAUSS_SV, NTHETA_LSQ, NTHETA_VIS, NPHI_VIS, NTHETA_GRID, M_wave_max, LMAX_OBS, LMAX_SV, MMAX_SV, LSQ, SV_SQ, &
               FLOW_COEFFS, SV_OPT, FLOW, KE_SPEC, SV_OBS_VIS, LAT_max1, lat_min1, LAT_MIN2, LAT_MAX2, FILTER_SIGNAL, HARMONICS, DELTA, GAUSS_SYN, JUST_EVALUATE)

PRINT*, dt, LSQ
IF( M_wave_max .eq. 1) WRITE(11,'(F10.3,X,F10.3,X,F10.3,F10.3)') dt, SQRT(KE_SPEC(1)), SQRT(KE_SPEC(2) + KE_SPEC(3)), MINVAL( FLOW(:,:,2) )
WRITE( FILENAME, '(A,I4)' ) 'GAUSS_SV_SYN.', NINT(dt)
OPEN(21,  FILE = FILENAME, STATUS = 'REPLACE')
WRITE(21,'(1F20.12)') GAUSS_SYN
CLOSE(21)

IF( .NOT. (FILESTEM(1:1) .EQ. 'N' .AND. LEN(TRIM(FILESTEM)) .EQ. 1)) THEN !WRITE GRIDDED OUTPUT TO DISK FOR PLOTTING
WRITE( FILENAME, '(A,A,F6.1)') TRIM(FILESTEM),'.', dt
OPEN( 16, FILE = FILENAME, STATUS = 'REPLACE')
DO I_PHI = 0, NPHI_VIS - 1
PHI = I_PHI * 2.0_LONG_REAL * Pi / REAL(NPHI_VIS, KIND = LONG_REAL)
DO I = 1, NTHETA_VIS
THETA = I * Pi / REAL( NTHETA_VIS+1, KIND = 8)
WRITE(16,'(2F8.2,2ES15.5)'), PHI*180.0/Pi ,90.0d0-THETA*180.0/Pi,  FLOW(I,I_PHI,1), FLOW(I,I_PHI,2)
ENDDO
ENDDO
CLOSE(16)
ENDIF


ENDDO
DEALLOCATE( FLOW_COEFFS, KE_SPEC )
CLOSE(11)

STOP
    



END PROGRAM Siberian_jet_time_evolution
