MODULE SUBS_SPLINES

CONTAINS

subroutine interv(tknts,dt,nspl,nleft,jord)

implicit real*8 (a-h,o-z)

dimension tknts(nspl+jord)

!  knots equally spaced (no stripping of repeated knots needed)
!  interval starts at tknts(jord) and ends at tknts(nspl+1)

!-----
!    calculate nleft:
!                  tknts(nleft) < tknts(nleft+1)
!                  tknts(nleft) <= dt <= tknts(nleft+1)
!

!  check we are in-range
if(dt.lt.tknts(jord).or.dt.gt.tknts(nspl+1)) return

do 200 n=jord+1,nspl+1
if(dt.le.tknts(n)) then
nleft=n-1
goto 210
endif
200   continue
210   continue
return
end

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

subroutine bspline(tknts,t,nspl,jorder,nleft,spl)

! calculate splines of order jorder

implicit real*8 (a-h,o-z)
dimension tknts(nspl+jorder)
dimension spl(jorder)

dimension deltal(jorder),deltar(jorder)

spl(1)=1.0

do 200 j=1,jorder-1

deltar(j) = tknts(nleft+j) - t
deltal(j) = t - tknts(nleft+1-j)
saved=0.0

do 100 i=1,j
term = spl(i)/(deltar(i)+deltal(j+1-i))
spl(i) = saved + deltar(i)*term
saved = deltal(j+1-i)*term
100   continue

spl(j+1) = saved

200   continue
end

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccc

subroutine derivcoeff(n,nspl,jordin,der,tknts,gt,gtd)

!     calculate first derivative by deriving a new set of
!     coefficients

implicit real*8 (a-h,o-z)
dimension tknts(nspl+jordin)
dimension gt(n,nspl),gtd(n,nspl)
dimension fact(nspl)

integer der

!     der defines the level of derivative sought, jordin
!     the order of the original spline

jord = jordin - der + 1

!     first calculate a vector for the time differentials
delt0 = tknts(nspl)-tknts(1)
do j=1,nspl
delt = ( tknts(j+jord-1) - tknts(j) )
if (abs(delt/delt0) .lt. 1.0e-6) then
!         Following deBoor p117 that anything multiplied by
!         nothing is nothing, getting rid of the singularity
fact(j) = 0.0
else
fact(j) = 1.0/delt
endif
end do
do k=1,n
!       Need a special case for j=1 (assume 0 bspline is 0)
gtd(k,1) = (jord-1)*gt(k,1)*fact(1)
do j=2,nspl
gtd(k,j) = (jord-1)*(gt(k,j)-gt(k,j-1))*fact(j)
end do
end do

return

end


END MODULE SUBS_SPLINES


