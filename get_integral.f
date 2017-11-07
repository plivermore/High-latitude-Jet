      MODULE FLOW_INTEGRALS
      CONTAINS
      FUNCTION get_integral(s,m,c,delta,ri)
      IMPLICIT NONE
      REAL(KIND = 8) :: s,delta,ri,c,cg, get_integral
      INTEGER :: m
      SELECT CASE (m)
      CASE(0)
      cg = 0.3D1 / 0.4D1 * erf((-dble(s) + ri) / delta) * sqrt(0.3141592
     #65358979323846264338328D1) * delta ** 3 * ri + erf((-dble(s) + ri)
     # / delta) * sqrt(0.314159265358979323846264338328D1) * delta * ri 
     #** 3 / 0.2D1 - 0.3D1 / 0.4D1 * ri * delta ** 3 * sqrt(0.3141592653
     #58979323846264338328D1) * erf(0.1D1 / delta * ri) - delta * ri ** 
     #3 * sqrt(0.314159265358979323846264338328D1) * erf(0.1D1 / delta *
     # ri) / 0.2D1 - delta * ri * sqrt(0.314159265358979323846264338328D
     #1) * erf((-dble(s) + ri) / delta) / 0.2D1 - c * dble(s ** 2) * sqr
     #t(dble(-s ** 2 + 1)) / 0.3D1 + c * sqrt(dble(-s ** 2 + 1)) / 0.3D1
     # + delta * ri * sqrt(0.314159265358979323846264338328D1) * erf(0.1
     #D1 / delta * ri) / 0.2D1 - c / 0.3D1 + exp(-(-dble(s) + ri) ** 2 /
     # delta ** 2) * delta ** 4 / 0.2D1 + exp(-(-dble(s) + ri) ** 2 / de
     #lta ** 2) * ri ** 2 * delta ** 2 / 0.2D1 + exp(-(-dble(s) + ri) **
     # 2 / delta ** 2) * delta ** 2 * ri * dble(s) / 0.2D1 + delta ** 2 
     #* dble(s ** 2) * exp(-(-dble(s) + ri) ** 2 / delta ** 2) / 0.2D1 -
     # delta ** 2 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) / 0.2D1 - de
     #lta ** 4 * exp(-0.1D1 / delta ** 2 * ri ** 2) / 0.2D1 - ri ** 2 * 
     #delta ** 2 * exp(-0.1D1 / delta ** 2 * ri ** 2) / 0.2D1 + delta **
     # 2 * exp(-0.1D1 / delta ** 2 * ri ** 2) / 0.2D1
      CASE(1)
      cg = 0.3D1 / 0.8D1 * sqrt(0.314159265358979323846264338328D1) * er
     #f((-dble(s) + ri) / delta) * delta ** 5 + 0.3D1 / 0.2D1 * sqrt(0.3
     #14159265358979323846264338328D1) * erf((-dble(s) + ri) / delta) * 
     #delta ** 3 * ri ** 2 + sqrt(0.314159265358979323846264338328D1) * 
     #erf((-dble(s) + ri) / delta) * delta * ri ** 4 / 0.2D1 - 0.3D1 / 0
     #.8D1 * delta ** 5 * sqrt(0.314159265358979323846264338328D1) * erf
     #(0.1D1 / delta * ri) - 0.3D1 / 0.2D1 * ri ** 2 * delta ** 3 * sqrt
     #(0.314159265358979323846264338328D1) * erf(0.1D1 / delta * ri) - d
     #elta * ri ** 4 * sqrt(0.314159265358979323846264338328D1) * erf(0.
     #1D1 / delta * ri) / 0.2D1 - delta ** 3 * sqrt(0.314159265358979323
     #846264338328D1) * erf((-dble(s) + ri) / delta) / 0.4D1 - delta * r
     #i ** 2 * sqrt(0.314159265358979323846264338328D1) * erf((-dble(s) 
     #+ ri) / delta) / 0.2D1 + delta ** 3 * sqrt(0.314159265358979323846
     #264338328D1) * erf(0.1D1 / delta * ri) / 0.4D1 + ri ** 2 * delta *
     # sqrt(0.314159265358979323846264338328D1) * erf(0.1D1 / delta * ri
     #) / 0.2D1 - c * dble(s ** 3) * sqrt(dble(-s ** 2 + 1)) / 0.4D1 + s
     #qrt(dble(-s ** 2 + 1)) * c * dble(s) / 0.8D1 - asin(dble(s)) * c /
     # 0.8D1 + 0.5D1 / 0.4D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) *
     # ri * delta ** 4 + 0.3D1 / 0.4D1 * exp(-(-dble(s) + ri) ** 2 / del
     #ta ** 2) * delta ** 4 * dble(s) + exp(-(-dble(s) + ri) ** 2 / delt
     #a ** 2) * ri ** 3 * delta ** 2 / 0.2D1 + exp(-(-dble(s) + ri) ** 2
     # / delta ** 2) * delta ** 2 * ri ** 2 * dble(s) / 0.2D1 + exp(-(-d
     #ble(s) + ri) ** 2 / delta ** 2) * delta ** 2 * ri * dble(s ** 2) /
     # 0.2D1 + delta ** 2 * dble(s ** 3) * exp(-(-dble(s) + ri) ** 2 / d
     #elta ** 2) / 0.2D1 - exp(-(-dble(s) + ri) ** 2 / delta ** 2) * ri 
     #* delta ** 2 / 0.2D1 - delta ** 2 * dble(s) * exp(-(-dble(s) + ri)
     # ** 2 / delta ** 2) / 0.2D1 - 0.5D1 / 0.4D1 * ri * delta ** 4 * ex
     #p(-0.1D1 / delta ** 2 * ri ** 2) - ri ** 3 * delta ** 2 * exp(-0.1
     #D1 / delta ** 2 * ri ** 2) / 0.2D1 + ri * delta ** 2 * exp(-0.1D1 
     #/ delta ** 2 * ri ** 2) / 0.2D1
      CASE(2)
      cg = -0.2D1 / 0.15D2 * c + 0.7D1 / 0.4D1 * exp(-(-s + ri) ** 2 / d
     #elta ** 2) * delta ** 4 * ri * s + exp(-(-s + ri) ** 2 / delta ** 
     #2) * delta ** 2 * ri ** 3 * s / 0.2D1 + exp(-(-s + ri) ** 2 / delt
     #a ** 2) * delta ** 2 * ri ** 2 * s ** 2 / 0.2D1 + exp(-(-s + ri) *
     #* 2 / delta ** 2) * delta ** 2 * ri * s ** 3 / 0.2D1 - c * s ** 4 
     #* sqrt(-s ** 2 + 0.1D1) / 0.5D1 + 0.9D1 / 0.4D1 * exp(-(-s + ri) *
     #* 2 / delta ** 2) * ri ** 2 * delta ** 4 + exp(-(-s + ri) ** 2 / d
     #elta ** 2) * ri ** 4 * delta ** 2 / 0.2D1 + exp(-(-s + ri) ** 2 / 
     #delta ** 2) * delta ** 2 * s ** 4 / 0.2D1 - exp(-(-s + ri) ** 2 / 
     #delta ** 2) * ri ** 2 * delta ** 2 / 0.2D1 + exp(-(-s + ri) ** 2 /
     # delta ** 2) * delta ** 4 * s ** 2 + exp(-(-s + ri) ** 2 / delta *
     #* 2) * delta ** 6 - delta ** 6 * exp(-0.1D1 / delta ** 2 * ri ** 2
     #) - delta * ri ** 5 * sqrt(0.314159265358979323846264338328D1) * e
     #rf(0.1D1 / delta * ri) / 0.2D1 - 0.5D1 / 0.2D1 * ri ** 3 * delta *
     #* 3 * sqrt(0.314159265358979323846264338328D1) * erf(0.1D1 / delta
     # * ri) - 0.15D2 / 0.8D1 * ri * delta ** 5 * sqrt(0.314159265358979
     #323846264338328D1) * erf(0.1D1 / delta * ri) - ri ** 4 * delta ** 
     #2 * exp(-0.1D1 / delta ** 2 * ri ** 2) / 0.2D1 - 0.9D1 / 0.4D1 * r
     #i ** 2 * delta ** 4 * exp(-0.1D1 / delta ** 2 * ri ** 2) - delta *
     #* 2 * s ** 2 * exp(-(-s + ri) ** 2 / delta ** 2) / 0.2D1 - exp(-(-
     #s + ri) ** 2 / delta ** 2) * delta ** 4 / 0.2D1 + 0.2D1 / 0.15D2 *
     # c * sqrt(-s ** 2 + 0.1D1) + c * s ** 2 * sqrt(-s ** 2 + 0.1D1) / 
     #0.15D2 + 0.15D2 / 0.8D1 * sqrt(0.314159265358979323846264338328D1)
     # * erf((-s + ri) / delta) * delta ** 5 * ri + 0.5D1 / 0.2D1 * sqrt
     #(0.314159265358979323846264338328D1) * erf((-s + ri) / delta) * de
     #lta ** 3 * ri ** 3 + sqrt(0.314159265358979323846264338328D1) * er
     #f((-s + ri) / delta) * delta * ri ** 5 / 0.2D1 - exp(-(-s + ri) **
     # 2 / delta ** 2) * delta ** 2 * ri * s / 0.2D1 - 0.3D1 / 0.4D1 * e
     #rf((-s + ri) / delta) * sqrt(0.314159265358979323846264338328D1) *
     # delta ** 3 * ri - erf((-s + ri) / delta) * sqrt(0.314159265358979
     #323846264338328D1) * delta * ri ** 3 / 0.2D1 + delta * ri ** 3 * s
     #qrt(0.314159265358979323846264338328D1) * erf(0.1D1 / delta * ri) 
     #/ 0.2D1 + 0.3D1 / 0.4D1 * ri * delta ** 3 * sqrt(0.314159265358979
     #323846264338328D1) * erf(0.1D1 / delta * ri) + delta ** 4 * exp(-0
     #.1D1 / delta ** 2 * ri ** 2) / 0.2D1 + ri ** 2 * delta ** 2 * exp(
     #-0.1D1 / delta ** 2 * ri ** 2) / 0.2D1
      CASE(3)
      cg = 0.3D1 / 0.8D1 * delta ** 5 * sqrt(0.3141592653589793238462643
     #38328D1) * erf(0.1D1 / delta * ri) + delta * ri ** 4 * sqrt(0.3141
     #59265358979323846264338328D1) * erf(0.1D1 / delta * ri) / 0.2D1 + 
     #0.3D1 / 0.2D1 * ri ** 2 * delta ** 3 * sqrt(0.31415926535897932384
     #6264338328D1) * erf(0.1D1 / delta * ri) + ri ** 3 * delta ** 2 * e
     #xp(-0.1D1 / delta ** 2 * ri ** 2) / 0.2D1 + 0.5D1 / 0.4D1 * ri * d
     #elta ** 4 * exp(-0.1D1 / delta ** 2 * ri ** 2) - 0.15D2 / 0.16D2 *
     # delta ** 7 * sqrt(0.314159265358979323846264338328D1) * erf(0.1D1
     # / delta * ri) - 0.7D1 / 0.2D1 * ri ** 3 * delta ** 4 * exp(-0.1D1
     # / delta ** 2 * ri ** 2) - ri ** 5 * delta ** 2 * exp(-0.1D1 / del
     #ta ** 2 * ri ** 2) / 0.2D1 - 0.33D2 / 0.8D1 * ri * delta ** 6 * ex
     #p(-0.1D1 / delta ** 2 * ri ** 2) - exp(-(-s + ri) ** 2 / delta ** 
     #2) * delta ** 2 * ri ** 2 * s / 0.2D1 - exp(-(-s + ri) ** 2 / delt
     #a ** 2) * delta ** 2 * ri * s ** 2 / 0.2D1 - 0.3D1 / 0.2D1 * sqrt(
     #0.314159265358979323846264338328D1) * erf((-s + ri) / delta) * del
     #ta ** 3 * ri ** 2 - sqrt(0.314159265358979323846264338328D1) * erf
     #((-s + ri) / delta) * delta * ri ** 4 / 0.2D1 - asin(s) * c / 0.16
     #D2 - delta ** 2 * s ** 3 * exp(-(-s + ri) ** 2 / delta ** 2) / 0.2
     #D1 - 0.5D1 / 0.4D1 * exp(-(-s + ri) ** 2 / delta ** 2) * ri * delt
     #a ** 4 - 0.3D1 / 0.4D1 * exp(-(-s + ri) ** 2 / delta ** 2) * delta
     # ** 4 * s - exp(-(-s + ri) ** 2 / delta ** 2) * ri ** 3 * delta **
     # 2 / 0.2D1 + c * s ** 3 * sqrt(-s ** 2 + 0.1D1) / 0.24D2 - 0.3D1 /
     # 0.8D1 * sqrt(0.314159265358979323846264338328D1) * erf((-s + ri) 
     #/ delta) * delta ** 5 + sqrt(-s ** 2 + 0.1D1) * c * s / 0.16D2 + e
     #xp(-(-s + ri) ** 2 / delta ** 2) * delta ** 2 * ri * s ** 4 / 0.2D
     #1 + 0.3D1 * exp(-(-s + ri) ** 2 / delta ** 2) * delta ** 4 * ri **
     # 2 * s + 0.9D1 / 0.4D1 * exp(-(-s + ri) ** 2 / delta ** 2) * delta
     # ** 4 * ri * s ** 2 + exp(-(-s + ri) ** 2 / delta ** 2) * delta **
     # 2 * ri ** 4 * s / 0.2D1 + exp(-(-s + ri) ** 2 / delta ** 2) * del
     #ta ** 2 * ri ** 3 * s ** 2 / 0.2D1 + exp(-(-s + ri) ** 2 / delta *
     #* 2) * delta ** 2 * ri ** 2 * s ** 3 / 0.2D1 + exp(-(-s + ri) ** 2
     # / delta ** 2) * ri ** 5 * delta ** 2 / 0.2D1 + 0.7D1 / 0.2D1 * ex
     #p(-(-s + ri) ** 2 / delta ** 2) * ri ** 3 * delta ** 4 + 0.5D1 / 0
     #.4D1 * exp(-(-s + ri) ** 2 / delta ** 2) * delta ** 4 * s ** 3 + 0
     #.33D2 / 0.8D1 * exp(-(-s + ri) ** 2 / delta ** 2) * ri * delta ** 
     #6 + 0.15D2 / 0.8D1 * exp(-(-s + ri) ** 2 / delta ** 2) * delta ** 
     #6 * s + delta ** 2 * s ** 5 * exp(-(-s + ri) ** 2 / delta ** 2) / 
     #0.2D1 - c * s ** 5 * sqrt(-s ** 2 + 0.1D1) / 0.6D1 + 0.45D2 / 0.8D
     #1 * sqrt(0.314159265358979323846264338328D1) * erf((-s + ri) / del
     #ta) * delta ** 5 * ri ** 2 + 0.15D2 / 0.4D1 * sqrt(0.3141592653589
     #79323846264338328D1) * erf((-s + ri) / delta) * delta ** 3 * ri **
     # 4 + sqrt(0.314159265358979323846264338328D1) * erf((-s + ri) / de
     #lta) * delta * ri ** 6 / 0.2D1 + 0.15D2 / 0.16D2 * sqrt(0.31415926
     #5358979323846264338328D1) * erf((-s + ri) / delta) * delta ** 7 - 
     #delta * ri ** 6 * sqrt(0.314159265358979323846264338328D1) * erf(0
     #.1D1 / delta * ri) / 0.2D1 - 0.15D2 / 0.4D1 * ri ** 4 * delta ** 3
     # * sqrt(0.314159265358979323846264338328D1) * erf(0.1D1 / delta * 
     #ri) - 0.45D2 / 0.8D1 * ri ** 2 * delta ** 5 * sqrt(0.3141592653589
     #79323846264338328D1) * erf(0.1D1 / delta * ri)
      CASE(4)
      cg = -0.8D1 / 0.105D3 * c - 0.3D1 * delta ** 8 * exp(-0.1D1 / delt
     #a ** 2 * ri ** 2) - exp(-(-s + ri) ** 2 / delta ** 2) * delta ** 2
     # * ri ** 2 * s ** 2 / 0.2D1 - exp(-(-s + ri) ** 2 / delta ** 2) * 
     #delta ** 2 * ri * s ** 3 / 0.2D1 + c * s ** 4 * sqrt(-s ** 2 + 0.1
     #D1) / 0.35D2 - exp(-(-s + ri) ** 2 / delta ** 2) * ri ** 4 * delta
     # ** 2 / 0.2D1 - exp(-(-s + ri) ** 2 / delta ** 2) * delta ** 2 * s
     # ** 4 / 0.2D1 - exp(-(-s + ri) ** 2 / delta ** 2) * delta ** 6 + d
     #elta ** 6 * exp(-0.1D1 / delta ** 2 * ri ** 2) + exp(-(-s + ri) **
     # 2 / delta ** 2) * delta ** 2 * ri ** 4 * s ** 2 / 0.2D1 + exp(-(-
     #s + ri) ** 2 / delta ** 2) * delta ** 2 * ri ** 3 * s ** 3 / 0.2D1
     # + exp(-(-s + ri) ** 2 / delta ** 2) * delta ** 2 * ri ** 2 * s **
     # 4 / 0.2D1 + exp(-(-s + ri) ** 2 / delta ** 2) * delta ** 2 * ri *
     # s ** 5 / 0.2D1 + 0.57D2 / 0.8D1 * exp(-(-s + ri) ** 2 / delta ** 
     #2) * delta ** 6 * ri * s + 0.9D1 / 0.2D1 * exp(-(-s + ri) ** 2 / d
     #elta ** 2) * delta ** 4 * ri ** 3 * s + 0.15D2 / 0.4D1 * exp(-(-s 
     #+ ri) ** 2 / delta ** 2) * delta ** 4 * ri ** 2 * s ** 2 + 0.11D2 
     #/ 0.4D1 * exp(-(-s + ri) ** 2 / delta ** 2) * delta ** 4 * ri * s 
     #** 3 + exp(-(-s + ri) ** 2 / delta ** 2) * delta ** 2 * ri ** 5 * 
     #s / 0.2D1 - 0.7D1 / 0.4D1 * exp(-(-s + ri) ** 2 / delta ** 2) * de
     #lta ** 4 * ri * s - exp(-(-s + ri) ** 2 / delta ** 2) * delta ** 2
     # * ri ** 3 * s / 0.2D1 + delta * ri ** 5 * sqrt(0.3141592653589793
     #23846264338328D1) * erf(0.1D1 / delta * ri) / 0.2D1 + 0.5D1 / 0.2D
     #1 * ri ** 3 * delta ** 3 * sqrt(0.314159265358979323846264338328D1
     #) * erf(0.1D1 / delta * ri) + 0.15D2 / 0.8D1 * ri * delta ** 5 * s
     #qrt(0.314159265358979323846264338328D1) * erf(0.1D1 / delta * ri) 
     #+ ri ** 4 * delta ** 2 * exp(-0.1D1 / delta ** 2 * ri ** 2) / 0.2D
     #1 + 0.9D1 / 0.4D1 * ri ** 2 * delta ** 4 * exp(-0.1D1 / delta ** 2
     # * ri ** 2) - exp(-(-s + ri) ** 2 / delta ** 2) * delta ** 4 * s *
     #* 2 + 0.5D1 * exp(-(-s + ri) ** 2 / delta ** 2) * ri ** 4 * delta 
     #** 4 + 0.3D1 * exp(-(-s + ri) ** 2 / delta ** 2) * delta ** 6 * s 
     #** 2 + 0.87D2 / 0.8D1 * exp(-(-s + ri) ** 2 / delta ** 2) * ri ** 
     #2 * delta ** 6 + exp(-(-s + ri) ** 2 / delta ** 2) * ri ** 6 * del
     #ta ** 2 / 0.2D1 - 0.9D1 / 0.4D1 * exp(-(-s + ri) ** 2 / delta ** 2
     #) * ri ** 2 * delta ** 4 + 0.3D1 / 0.2D1 * exp(-(-s + ri) ** 2 / d
     #elta ** 2) * delta ** 4 * s ** 4 + delta ** 2 * s ** 6 * exp(-(-s 
     #+ ri) ** 2 / delta ** 2) / 0.2D1 - ri ** 6 * delta ** 2 * exp(-0.1
     #D1 / delta ** 2 * ri ** 2) / 0.2D1 - 0.5D1 * ri ** 4 * delta ** 4 
     #* exp(-0.1D1 / delta ** 2 * ri ** 2) - 0.87D2 / 0.8D1 * ri ** 2 * 
     #delta ** 6 * exp(-0.1D1 / delta ** 2 * ri ** 2) + 0.8D1 / 0.105D3 
     #* c * sqrt(-s ** 2 + 0.1D1) + 0.4D1 / 0.105D3 * c * s ** 2 * sqrt(
     #-s ** 2 + 0.1D1) - c * s ** 6 * sqrt(-s ** 2 + 0.1D1) / 0.7D1 - 0.
     #15D2 / 0.8D1 * sqrt(0.314159265358979323846264338328D1) * erf((-s 
     #+ ri) / delta) * delta ** 5 * ri - sqrt(0.314159265358979323846264
     #338328D1) * erf((-s + ri) / delta) * delta * ri ** 5 / 0.2D1 + 0.1
     #05D3 / 0.16D2 * sqrt(0.314159265358979323846264338328D1) * erf((-s
     # + ri) / delta) * delta ** 7 * ri + 0.105D3 / 0.8D1 * sqrt(0.31415
     #9265358979323846264338328D1) * erf((-s + ri) / delta) * delta ** 5
     # * ri ** 3 + 0.21D2 / 0.4D1 * sqrt(0.31415926535897932384626433832
     #8D1) * erf((-s + ri) / delta) * delta ** 3 * ri ** 5 + sqrt(0.3141
     #59265358979323846264338328D1) * erf((-s + ri) / delta) * delta * r
     #i ** 7 / 0.2D1 - 0.5D1 / 0.2D1 * ri ** 3 * delta ** 3 * sqrt(0.314
     #159265358979323846264338328D1) * erf((-s + ri) / delta) + 0.3D1 * 
     #exp(-(-s + ri) ** 2 / delta ** 2) * delta ** 8 - delta * ri ** 7 *
     # sqrt(0.314159265358979323846264338328D1) * erf(0.1D1 / delta * ri
     #) / 0.2D1 - 0.21D2 / 0.4D1 * ri ** 5 * delta ** 3 * sqrt(0.3141592
     #65358979323846264338328D1) * erf(0.1D1 / delta * ri) - 0.105D3 / 0
     #.8D1 * ri ** 3 * delta ** 5 * sqrt(0.31415926535897932384626433832
     #8D1) * erf(0.1D1 / delta * ri) - 0.105D3 / 0.16D2 * ri * delta ** 
     #7 * sqrt(0.314159265358979323846264338328D1) * erf(0.1D1 / delta *
     # ri)
      CASE(5)
      cg = sqrt(dble(-s ** 2 + 1)) * c * dble(s ** 5) / 0.48D2 - c * dbl
     #e(s ** 7) * sqrt(dble(-s ** 2 + 1)) / 0.8D1 + 0.15D2 / 0.16D2 * de
     #lta ** 7 * sqrt(0.314159265358979323846264338328D1) * erf(0.1D1 / 
     #delta * ri) - 0.185D3 / 0.8D1 * ri ** 3 * delta ** 6 * exp(-0.1D1 
     #/ delta ** 2 * ri ** 2) - 0.279D3 / 0.16D2 * ri * delta ** 8 * exp
     #(-0.1D1 / delta ** 2 * ri ** 2) - ri ** 7 * delta ** 2 * exp(-0.1D
     #1 / delta ** 2 * ri ** 2) / 0.2D1 - 0.27D2 / 0.4D1 * ri ** 5 * del
     #ta ** 4 * exp(-0.1D1 / delta ** 2 * ri ** 2) - 0.105D3 / 0.32D2 * 
     #delta ** 9 * sqrt(0.314159265358979323846264338328D1) * erf(0.1D1 
     #/ delta * ri) + 0.7D1 / 0.2D1 * ri ** 3 * delta ** 4 * exp(-0.1D1 
     #/ delta ** 2 * ri ** 2) + ri ** 5 * delta ** 2 * exp(-0.1D1 / delt
     #a ** 2 * ri ** 2) / 0.2D1 + 0.33D2 / 0.8D1 * ri * delta ** 6 * exp
     #(-0.1D1 / delta ** 2 * ri ** 2) - delta * ri ** 8 * sqrt(0.3141592
     #65358979323846264338328D1) * erf(0.1D1 / delta * ri) / 0.2D1 - 0.7
     #D1 * ri ** 6 * delta ** 3 * sqrt(0.314159265358979323846264338328D
     #1) * erf(0.1D1 / delta * ri) - 0.105D3 / 0.4D1 * ri ** 4 * delta *
     #* 5 * sqrt(0.314159265358979323846264338328D1) * erf(0.1D1 / delta
     # * ri) - 0.105D3 / 0.4D1 * ri ** 2 * delta ** 7 * sqrt(0.314159265
     #358979323846264338328D1) * erf(0.1D1 / delta * ri) + exp(-(-dble(s
     #) + ri) ** 2 / delta ** 2) * delta ** 2 * ri ** 5 * dble(s ** 2) /
     # 0.2D1 + exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 2 * ri
     # ** 4 * dble(s ** 3) / 0.2D1 + exp(-(-dble(s) + ri) ** 2 / delta *
     #* 2) * delta ** 2 * ri ** 3 * dble(s ** 4) / 0.2D1 + exp(-(-dble(s
     #) + ri) ** 2 / delta ** 2) * delta ** 2 * ri ** 2 * dble(s ** 5) /
     # 0.2D1 + exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 2 * ri
     # * dble(s ** 6) / 0.2D1 - 0.3D1 * exp(-(-dble(s) + ri) ** 2 / delt
     #a ** 2) * delta ** 4 * ri ** 2 * dble(s) - 0.9D1 / 0.4D1 * exp(-(-
     #dble(s) + ri) ** 2 / delta ** 2) * delta ** 4 * ri * dble(s ** 2) 
     #- exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 2 * ri ** 4 *
     # dble(s) / 0.2D1 - exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta
     # ** 2 * ri ** 3 * dble(s ** 2) / 0.2D1 - exp(-(-dble(s) + ri) ** 2
     # / delta ** 2) * delta ** 2 * ri ** 2 * dble(s ** 3) / 0.2D1 + 0.1
     #41D3 / 0.8D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 
     #6 * ri ** 2 * dble(s) + 0.87D2 / 0.8D1 * exp(-(-dble(s) + ri) ** 2
     # / delta ** 2) * delta ** 6 * ri * dble(s ** 2) + 0.25D2 / 0.4D1 *
     # exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 4 * ri ** 4 * 
     #dble(s) + 0.11D2 / 0.2D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2)
     # * delta ** 4 * ri ** 3 * dble(s ** 2) + 0.9D1 / 0.2D1 * exp(-(-db
     #le(s) + ri) ** 2 / delta ** 2) * delta ** 4 * ri ** 2 * dble(s ** 
     #3) + 0.13D2 / 0.4D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * de
     #lta ** 4 * ri * dble(s ** 4) + exp(-(-dble(s) + ri) ** 2 / delta *
     #* 2) * delta ** 2 * ri ** 6 * dble(s) / 0.2D1 - 0.5D1 / 0.128D3 * 
     #asin(dble(s)) * c + 0.5D1 / 0.192D3 * c * dble(s ** 3) * sqrt(dble
     #(-s ** 2 + 1)) - 0.15D2 / 0.8D1 * exp(-(-dble(s) + ri) ** 2 / delt
     #a ** 2) * delta ** 6 * dble(s) - exp(-(-dble(s) + ri) ** 2 / delta
     # ** 2) * ri ** 5 * delta ** 2 / 0.2D1 - exp(-(-dble(s) + ri) ** 2 
     #/ delta ** 2) * delta ** 2 * dble(s ** 5) / 0.2D1 - 0.7D1 / 0.2D1 
     #* exp(-(-dble(s) + ri) ** 2 / delta ** 2) * ri ** 3 * delta ** 4 -
     # 0.33D2 / 0.8D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * ri * d
     #elta ** 6 + 0.7D1 / 0.4D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2
     #) * delta ** 4 * dble(s ** 5) + exp(-(-dble(s) + ri) ** 2 / delta 
     #** 2) * delta ** 2 * dble(s ** 7) / 0.2D1 + 0.185D3 / 0.8D1 * exp(
     #-(-dble(s) + ri) ** 2 / delta ** 2) * ri ** 3 * delta ** 6 + 0.279
     #D3 / 0.16D2 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * ri * delta
     # ** 8 + 0.35D2 / 0.8D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) *
     # delta ** 6 * dble(s ** 3) + exp(-(-dble(s) + ri) ** 2 / delta ** 
     #2) * ri ** 7 * delta ** 2 / 0.2D1 + 0.27D2 / 0.4D1 * exp(-(-dble(s
     #) + ri) ** 2 / delta ** 2) * ri ** 5 * delta ** 4 + 0.105D3 / 0.16
     #D2 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 8 * dble(s
     #) + 0.5D1 / 0.128D3 * sqrt(dble(-s ** 2 + 1)) * c * dble(s) - exp(
     #-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 2 * ri * dble(s ** 
     #4) / 0.2D1 - 0.5D1 / 0.4D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 
     #2) * delta ** 4 * dble(s ** 3) - 0.45D2 / 0.8D1 * sqrt(0.314159265
     #358979323846264338328D1) * erf((-dble(s) + ri) / delta) * delta **
     # 5 * ri ** 2 - 0.15D2 / 0.4D1 * sqrt(0.314159265358979323846264338
     #328D1) * erf((-dble(s) + ri) / delta) * delta ** 3 * ri ** 4 - sqr
     #t(0.314159265358979323846264338328D1) * erf((-dble(s) + ri) / delt
     #a) * delta * ri ** 6 / 0.2D1 - 0.15D2 / 0.16D2 * sqrt(0.3141592653
     #58979323846264338328D1) * erf((-dble(s) + ri) / delta) * delta ** 
     #7 + 0.105D3 / 0.4D1 * sqrt(0.314159265358979323846264338328D1) * e
     #rf((-dble(s) + ri) / delta) * delta ** 7 * ri ** 2 + 0.105D3 / 0.4
     #D1 * sqrt(0.314159265358979323846264338328D1) * erf((-dble(s) + ri
     #) / delta) * delta ** 5 * ri ** 4 + 0.7D1 * sqrt(0.314159265358979
     #323846264338328D1) * erf((-dble(s) + ri) / delta) * delta ** 3 * r
     #i ** 6 + sqrt(0.314159265358979323846264338328D1) * erf((-dble(s) 
     #+ ri) / delta) * delta * ri ** 8 / 0.2D1 + delta * ri ** 6 * sqrt(
     #0.314159265358979323846264338328D1) * erf(0.1D1 / delta * ri) / 0.
     #2D1 + 0.15D2 / 0.4D1 * ri ** 4 * delta ** 3 * sqrt(0.3141592653589
     #79323846264338328D1) * erf(0.1D1 / delta * ri) + 0.45D2 / 0.8D1 * 
     #ri ** 2 * delta ** 5 * sqrt(0.314159265358979323846264338328D1) * 
     #erf(0.1D1 / delta * ri) + 0.105D3 / 0.32D2 * sqrt(0.31415926535897
     #9323846264338328D1) * erf((-dble(s) + ri) / delta) * delta ** 9
      CASE(6)
      cg = -0.16D2 / 0.315D3 * c + 0.3D1 * delta ** 8 * exp(-0.1D1 / del
     #ta ** 2 * ri ** 2) + 0.2D1 / 0.105D3 * c * dble(s ** 4) * sqrt(dbl
     #e(-s ** 2 + 1)) + 0.15D2 / 0.2D1 * exp(-(-dble(s) + ri) ** 2 / del
     #ta ** 2) * delta ** 4 * ri ** 4 * dble(s ** 2) + 0.13D2 / 0.2D1 * 
     #exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 4 * ri ** 3 * d
     #ble(s ** 3) + 0.21D2 / 0.4D1 * exp(-(-dble(s) + ri) ** 2 / delta *
     #* 2) * delta ** 4 * ri ** 2 * dble(s ** 4) + 0.15D2 / 0.4D1 * exp(
     #-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 4 * ri * dble(s ** 
     #5) + exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 2 * ri ** 
     #7 * dble(s) / 0.2D1 + 0.285D3 / 0.8D1 * exp(-(-dble(s) + ri) ** 2 
     #/ delta ** 2) * delta ** 6 * ri ** 3 * dble(s) + 0.207D3 / 0.8D1 *
     # exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 6 * ri ** 2 * 
     #dble(s ** 2) + 0.123D3 / 0.8D1 * exp(-(-dble(s) + ri) ** 2 / delta
     # ** 2) * delta ** 6 * ri * dble(s ** 3) + 0.33D2 / 0.4D1 * exp(-(-
     #dble(s) + ri) ** 2 / delta ** 2) * delta ** 4 * ri ** 5 * dble(s) 
     #- 0.15D2 / 0.4D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta
     # ** 4 * ri ** 2 * dble(s ** 2) - exp(-(-dble(s) + ri) ** 2 / delta
     # ** 2) * delta ** 2 * ri ** 3 * dble(s ** 3) / 0.2D1 - exp(-(-dble
     #(s) + ri) ** 2 / delta ** 2) * delta ** 2 * ri ** 2 * dble(s ** 4)
     # / 0.2D1 + exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 2 * 
     #ri ** 6 * dble(s ** 2) / 0.2D1 + exp(-(-dble(s) + ri) ** 2 / delta
     # ** 2) * delta ** 2 * ri ** 5 * dble(s ** 3) / 0.2D1 + exp(-(-dble
     #(s) + ri) ** 2 / delta ** 2) * delta ** 2 * ri ** 4 * dble(s ** 4)
     # / 0.2D1 + exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 2 * 
     #ri ** 3 * dble(s ** 5) / 0.2D1 + exp(-(-dble(s) + ri) ** 2 / delta
     # ** 2) * delta ** 2 * ri ** 2 * dble(s ** 6) / 0.2D1 + exp(-(-dble
     #(s) + ri) ** 2 / delta ** 2) * delta ** 2 * ri * dble(s ** 7) / 0.
     #2D1 - 0.57D2 / 0.8D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * d
     #elta ** 6 * ri * dble(s) - 0.9D1 / 0.2D1 * exp(-(-dble(s) + ri) **
     # 2 / delta ** 2) * delta ** 4 * ri ** 3 * dble(s) - exp(-(-dble(s)
     # + ri) ** 2 / delta ** 2) * delta ** 2 * ri ** 4 * dble(s ** 2) / 
     #0.2D1 - 0.11D2 / 0.4D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) *
     # delta ** 4 * ri * dble(s ** 3) + 0.561D3 / 0.16D2 * exp(-(-dble(s
     #) + ri) ** 2 / delta ** 2) * delta ** 8 * ri * dble(s) - exp(-(-db
     #le(s) + ri) ** 2 / delta ** 2) * delta ** 2 * ri * dble(s ** 5) / 
     #0.2D1 - exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 2 * ri 
     #** 5 * dble(s) / 0.2D1 - 0.345D3 / 0.8D1 * ri ** 4 * delta ** 6 * 
     #exp(-0.1D1 / delta ** 2 * ri ** 2) - ri ** 8 * delta ** 2 * exp(-0
     #.1D1 / delta ** 2 * ri ** 2) / 0.2D1 - 0.35D2 / 0.4D1 * ri ** 6 * 
     #delta ** 4 * exp(-0.1D1 / delta ** 2 * ri ** 2) - 0.975D3 / 0.16D2
     # * ri ** 2 * delta ** 8 * exp(-0.1D1 / delta ** 2 * ri ** 2) + ri 
     #** 6 * delta ** 2 * exp(-0.1D1 / delta ** 2 * ri ** 2) / 0.2D1 + 0
     #.5D1 * ri ** 4 * delta ** 4 * exp(-0.1D1 / delta ** 2 * ri ** 2) +
     # 0.87D2 / 0.8D1 * ri ** 2 * delta ** 6 * exp(-0.1D1 / delta ** 2 *
     # ri ** 2) - 0.105D3 / 0.16D2 * sqrt(0.3141592653589793238462643383
     #28D1) * erf((-dble(s) + ri) / delta) * delta ** 7 * ri - 0.105D3 /
     # 0.8D1 * sqrt(0.314159265358979323846264338328D1) * erf((-dble(s) 
     #+ ri) / delta) * delta ** 5 * ri ** 3 - 0.21D2 / 0.4D1 * sqrt(0.31
     #4159265358979323846264338328D1) * erf((-dble(s) + ri) / delta) * d
     #elta ** 3 * ri ** 5 - sqrt(0.314159265358979323846264338328D1) * e
     #rf((-dble(s) + ri) / delta) * delta * ri ** 7 / 0.2D1 - 0.87D2 / 0
     #.8D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * ri ** 2 * delta *
     #* 6 + 0.6D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 6
     # * dble(s ** 4) + 0.2D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) 
     #* delta ** 4 * dble(s ** 6) + 0.345D3 / 0.8D1 * exp(-(-dble(s) + r
     #i) ** 2 / delta ** 2) * ri ** 4 * delta ** 6 + 0.12D2 * exp(-(-dbl
     #e(s) + ri) ** 2 / delta ** 2) * delta ** 8 * dble(s ** 2) + 0.35D2
     # / 0.4D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * ri ** 6 * del
     #ta ** 4 + 0.975D3 / 0.16D2 * exp(-(-dble(s) + ri) ** 2 / delta ** 
     #2) * ri ** 2 * delta ** 8 + exp(-(-dble(s) + ri) ** 2 / delta ** 2
     #) * ri ** 8 * delta ** 2 / 0.2D1 - exp(-(-dble(s) + ri) ** 2 / del
     #ta ** 2) * ri ** 6 * delta ** 2 / 0.2D1 - 0.5D1 * exp(-(-dble(s) +
     # ri) ** 2 / delta ** 2) * ri ** 4 * delta ** 4 - exp(-(-dble(s) + 
     #ri) ** 2 / delta ** 2) * delta ** 2 * dble(s ** 6) / 0.2D1 - 0.3D1
     # * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 6 * dble(s *
     #* 2) - 0.3D1 / 0.2D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * d
     #elta ** 4 * dble(s ** 4) + exp(-(-dble(s) + ri) ** 2 / delta ** 2)
     # * delta ** 2 * dble(s ** 8) / 0.2D1 + delta * ri ** 7 * sqrt(0.31
     #4159265358979323846264338328D1) * erf(0.1D1 / delta * ri) / 0.2D1 
     #+ 0.21D2 / 0.4D1 * ri ** 5 * delta ** 3 * sqrt(0.31415926535897932
     #3846264338328D1) * erf(0.1D1 / delta * ri) + 0.105D3 / 0.8D1 * ri 
     #** 3 * delta ** 5 * sqrt(0.314159265358979323846264338328D1) * erf
     #(0.1D1 / delta * ri) + 0.105D3 / 0.16D2 * ri * delta ** 7 * sqrt(0
     #.314159265358979323846264338328D1) * erf(0.1D1 / delta * ri) + 0.9
     #45D3 / 0.32D2 * sqrt(0.314159265358979323846264338328D1) * erf((-d
     #ble(s) + ri) / delta) * delta ** 9 * ri + 0.315D3 / 0.4D1 * sqrt(0
     #.314159265358979323846264338328D1) * erf((-dble(s) + ri) / delta) 
     #* delta ** 7 * ri ** 3 + 0.189D3 / 0.4D1 * sqrt(0.3141592653589793
     #23846264338328D1) * erf((-dble(s) + ri) / delta) * delta ** 5 * ri
     # ** 5 + 0.9D1 * sqrt(0.314159265358979323846264338328D1) * erf((-d
     #ble(s) + ri) / delta) * delta ** 3 * ri ** 7 + sqrt(0.314159265358
     #979323846264338328D1) * erf((-dble(s) + ri) / delta) * delta * ri 
     #** 9 / 0.2D1 - 0.3D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * d
     #elta ** 8 + 0.12D2 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * del
     #ta ** 10 + 0.16D2 / 0.315D3 * c * sqrt(dble(-s ** 2 + 1)) - c * db
     #le(s ** 8) * sqrt(dble(-s ** 2 + 1)) / 0.9D1 + 0.8D1 / 0.315D3 * s
     #qrt(dble(-s ** 2 + 1)) * c * dble(s ** 2) + sqrt(dble(-s ** 2 + 1)
     #) * c * dble(s ** 6) / 0.63D2 - 0.12D2 * delta ** 10 * exp(-0.1D1 
     #/ delta ** 2 * ri ** 2) - 0.945D3 / 0.32D2 * ri * delta ** 9 * sqr
     #t(0.314159265358979323846264338328D1) * erf(0.1D1 / delta * ri) - 
     #delta * ri ** 9 * sqrt(0.314159265358979323846264338328D1) * erf(0
     #.1D1 / delta * ri) / 0.2D1 - 0.9D1 * ri ** 7 * delta ** 3 * sqrt(0
     #.314159265358979323846264338328D1) * erf(0.1D1 / delta * ri) - 0.1
     #89D3 / 0.4D1 * ri ** 5 * delta ** 5 * sqrt(0.314159265358979323846
     #264338328D1) * erf(0.1D1 / delta * ri) - 0.315D3 / 0.4D1 * ri ** 3
     # * delta ** 7 * sqrt(0.314159265358979323846264338328D1) * erf(0.1
     #D1 / delta * ri)
      CASE(7)
      cg = -0.2895D4 / 0.32D2 * ri * delta ** 10 * exp(-0.1D1 / delta **
     # 2 * ri ** 2) - ri ** 9 * delta ** 2 * exp(-0.1D1 / delta ** 2 * r
     #i ** 2) / 0.2D1 - 0.11D2 * ri ** 7 * delta ** 4 * exp(-0.1D1 / del
     #ta ** 2 * ri ** 2) - 0.147D3 / 0.2D1 * ri ** 5 * delta ** 6 * exp(
     #-0.1D1 / delta ** 2 * ri ** 2) - 0.165D3 * ri ** 3 * delta ** 8 * 
     #exp(-0.1D1 / delta ** 2 * ri ** 2) + 0.7D1 / 0.480D3 * sqrt(dble(-
     #s ** 2 + 1)) * c * dble(s ** 5) + c * dble(s ** 7) * sqrt(dble(-s 
     #** 2 + 1)) / 0.80D2 - 0.7D1 / 0.256D3 * asin(dble(s)) * c - 0.945D
     #3 / 0.64D2 * delta ** 11 * sqrt(0.314159265358979323846264338328D1
     #) * erf(0.1D1 / delta * ri) + 0.185D3 / 0.8D1 * ri ** 3 * delta **
     # 6 * exp(-0.1D1 / delta ** 2 * ri ** 2) + 0.279D3 / 0.16D2 * ri * 
     #delta ** 8 * exp(-0.1D1 / delta ** 2 * ri ** 2) + ri ** 7 * delta 
     #** 2 * exp(-0.1D1 / delta ** 2 * ri ** 2) / 0.2D1 + 0.27D2 / 0.4D1
     # * ri ** 5 * delta ** 4 * exp(-0.1D1 / delta ** 2 * ri ** 2) + 0.1
     #05D3 / 0.32D2 * delta ** 9 * sqrt(0.314159265358979323846264338328
     #D1) * erf(0.1D1 / delta * ri) + 0.4725D4 / 0.32D2 * erf((-dble(s) 
     #+ ri) / delta) * sqrt(0.314159265358979323846264338328D1) * delta 
     #** 9 * ri ** 2 + 0.1575D4 / 0.8D1 * erf((-dble(s) + ri) / delta) *
     # sqrt(0.314159265358979323846264338328D1) * delta ** 7 * ri ** 4 +
     # 0.315D3 / 0.4D1 * erf((-dble(s) + ri) / delta) * sqrt(0.314159265
     #358979323846264338328D1) * delta ** 5 * ri ** 6 + 0.45D2 / 0.4D1 *
     # erf((-dble(s) + ri) / delta) * sqrt(0.314159265358979323846264338
     #328D1) * delta ** 3 * ri ** 8 + erf((-dble(s) + ri) / delta) * sqr
     #t(0.314159265358979323846264338328D1) * delta * ri ** 10 / 0.2D1 -
     # 0.105D3 / 0.4D1 * ri ** 2 * delta ** 7 * sqrt(0.31415926535897932
     #3846264338328D1) * erf((-dble(s) + ri) / delta) - 0.105D3 / 0.4D1 
     #* ri ** 4 * delta ** 5 * sqrt(0.314159265358979323846264338328D1) 
     #* erf((-dble(s) + ri) / delta) - delta * ri ** 8 * sqrt(0.31415926
     #5358979323846264338328D1) * erf((-dble(s) + ri) / delta) / 0.2D1 +
     # delta ** 2 * dble(s ** 9) * exp(-(-dble(s) + ri) ** 2 / delta ** 
     #2) / 0.2D1 - delta ** 2 * dble(s ** 7) * exp(-(-dble(s) + ri) ** 2
     # / delta ** 2) / 0.2D1 + delta * ri ** 8 * sqrt(0.3141592653589793
     #23846264338328D1) * erf(0.1D1 / delta * ri) / 0.2D1 + 0.7D1 * ri *
     #* 6 * delta ** 3 * sqrt(0.314159265358979323846264338328D1) * erf(
     #0.1D1 / delta * ri) + 0.105D3 / 0.4D1 * ri ** 4 * delta ** 5 * sqr
     #t(0.314159265358979323846264338328D1) * erf(0.1D1 / delta * ri) + 
     #0.105D3 / 0.4D1 * ri ** 2 * delta ** 7 * sqrt(0.314159265358979323
     #846264338328D1) * erf(0.1D1 / delta * ri) - 0.13D2 / 0.4D1 * exp(-
     #(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 4 * ri * dble(s ** 4
     #) - exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 2 * ri ** 6
     # * dble(s) / 0.2D1 + 0.945D3 / 0.64D2 * erf((-dble(s) + ri) / delt
     #a) * sqrt(0.314159265358979323846264338328D1) * delta ** 11 - 0.10
     #5D3 / 0.32D2 * delta ** 9 * sqrt(0.314159265358979323846264338328D
     #1) * erf((-dble(s) + ri) / delta) + 0.7D1 / 0.256D3 * sqrt(dble(-s
     # ** 2 + 1)) * c * dble(s) + 0.7D1 / 0.384D3 * c * dble(s ** 3) * s
     #qrt(dble(-s ** 2 + 1)) - 0.279D3 / 0.16D2 * exp(-(-dble(s) + ri) *
     #* 2 / delta ** 2) * ri * delta ** 8 - 0.27D2 / 0.4D1 * exp(-(-dble
     #(s) + ri) ** 2 / delta ** 2) * ri ** 5 * delta ** 4 - 0.105D3 / 0.
     #16D2 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 8 * dble
     #(s) - c * dble(s ** 9) * sqrt(dble(-s ** 2 + 1)) / 0.10D2 - 0.141D
     #3 / 0.8D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 6 *
     # ri ** 2 * dble(s) - 0.87D2 / 0.8D1 * exp(-(-dble(s) + ri) ** 2 / 
     #delta ** 2) * delta ** 6 * ri * dble(s ** 2) - 0.45D2 / 0.4D1 * ri
     # ** 8 * delta ** 3 * sqrt(0.314159265358979323846264338328D1) * er
     #f(0.1D1 / delta * ri) - 0.315D3 / 0.4D1 * ri ** 6 * delta ** 5 * s
     #qrt(0.314159265358979323846264338328D1) * erf(0.1D1 / delta * ri) 
     #- 0.1575D4 / 0.8D1 * ri ** 4 * delta ** 7 * sqrt(0.314159265358979
     #323846264338328D1) * erf(0.1D1 / delta * ri) - 0.4725D4 / 0.32D2 *
     # ri ** 2 * delta ** 9 * sqrt(0.314159265358979323846264338328D1) *
     # erf(0.1D1 / delta * ri) - delta * ri ** 10 * sqrt(0.3141592653589
     #79323846264338328D1) * erf(0.1D1 / delta * ri) / 0.2D1 - exp(-(-db
     #le(s) + ri) ** 2 / delta ** 2) * delta ** 2 * ri ** 5 * dble(s ** 
     #2) / 0.2D1 - exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 2 
     #* ri ** 4 * dble(s ** 3) / 0.2D1 - exp(-(-dble(s) + ri) ** 2 / del
     #ta ** 2) * delta ** 2 * ri ** 3 * dble(s ** 4) / 0.2D1 - exp(-(-db
     #le(s) + ri) ** 2 / delta ** 2) * delta ** 2 * ri * dble(s ** 6) / 
     #0.2D1 + 0.6D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta **
     # 4 * ri ** 2 * dble(s ** 5) + 0.17D2 / 0.4D1 * exp(-(-dble(s) + ri
     #) ** 2 / delta ** 2) * delta ** 4 * ri * dble(s ** 6) + exp(-(-dbl
     #e(s) + ri) ** 2 / delta ** 2) * delta ** 2 * ri ** 8 * dble(s) / 0
     #.2D1 + exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 2 * ri *
     #* 7 * dble(s ** 2) / 0.2D1 + exp(-(-dble(s) + ri) ** 2 / delta ** 
     #2) * delta ** 2 * ri ** 6 * dble(s ** 3) / 0.2D1 + 0.975D3 / 0.16D
     #2 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 8 * ri * db
     #le(s ** 2) + 0.255D3 / 0.4D1 * exp(-(-dble(s) + ri) ** 2 / delta *
     #* 2) * delta ** 6 * ri ** 4 * dble(s) + 0.405D3 / 0.8D1 * exp(-(-d
     #ble(s) + ri) ** 2 / delta ** 2) * delta ** 6 * ri ** 3 * dble(s **
     # 2) + 0.285D3 / 0.8D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * 
     #delta ** 6 * ri ** 2 * dble(s ** 3) + 0.165D3 / 0.8D1 * exp(-(-dbl
     #e(s) + ri) ** 2 / delta ** 2) * delta ** 6 * ri * dble(s ** 4) + 0
     #.21D2 / 0.2D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta **
     # 4 * ri ** 6 * dble(s) + 0.39D2 / 0.4D1 * exp(-(-dble(s) + ri) ** 
     #2 / delta ** 2) * delta ** 4 * ri ** 5 * dble(s ** 2) + 0.35D2 / 0
     #.4D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 4 * ri *
     #* 4 * dble(s ** 3) + 0.15D2 / 0.2D1 * exp(-(-dble(s) + ri) ** 2 / 
     #delta ** 2) * delta ** 4 * ri ** 3 * dble(s ** 4) + exp(-(-dble(s)
     # + ri) ** 2 / delta ** 2) * delta ** 2 * ri ** 5 * dble(s ** 4) / 
     #0.2D1 + exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 2 * ri 
     #** 4 * dble(s ** 5) / 0.2D1 + exp(-(-dble(s) + ri) ** 2 / delta **
     # 2) * delta ** 2 * ri ** 3 * dble(s ** 6) / 0.2D1 + exp(-(-dble(s)
     # + ri) ** 2 / delta ** 2) * delta ** 2 * ri ** 2 * dble(s ** 7) / 
     #0.2D1 - 0.11D2 / 0.2D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) *
     # delta ** 4 * ri ** 3 * dble(s ** 2) - 0.9D1 / 0.2D1 * exp(-(-dble
     #(s) + ri) ** 2 / delta ** 2) * delta ** 4 * ri ** 2 * dble(s ** 3)
     # - exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 2 * ri ** 2 
     #* dble(s ** 5) / 0.2D1 + 0.915D3 / 0.8D1 * exp(-(-dble(s) + ri) **
     # 2 / delta ** 2) * delta ** 8 * ri ** 2 * dble(s) + exp(-(-dble(s)
     # + ri) ** 2 / delta ** 2) * delta ** 2 * ri * dble(s ** 8) / 0.2D1
     # - 0.25D2 / 0.4D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delt
     #a ** 4 * ri ** 4 * dble(s) + exp(-(-dble(s) + ri) ** 2 / delta ** 
     #2) * ri ** 9 * delta ** 2 / 0.2D1 - 0.185D3 / 0.8D1 * exp(-(-dble(
     #s) + ri) ** 2 / delta ** 2) * ri ** 3 * delta ** 6 - 0.7D1 * sqrt(
     #0.314159265358979323846264338328D1) * erf((-dble(s) + ri) / delta)
     # * delta ** 3 * ri ** 6 - exp(-(-dble(s) + ri) ** 2 / delta ** 2) 
     #* ri ** 7 * delta ** 2 / 0.2D1 - 0.35D2 / 0.8D1 * exp(-(-dble(s) +
     # ri) ** 2 / delta ** 2) * delta ** 6 * dble(s ** 3) + 0.9D1 / 0.4D
     #1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 4 * dble(s 
     #** 7) + 0.315D3 / 0.16D2 * exp(-(-dble(s) + ri) ** 2 / delta ** 2)
     # * delta ** 8 * dble(s ** 3) + 0.63D2 / 0.8D1 * exp(-(-dble(s) + r
     #i) ** 2 / delta ** 2) * delta ** 6 * dble(s ** 5) + 0.945D3 / 0.32
     #D2 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 10 * dble(
     #s) + 0.165D3 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * ri ** 3 *
     # delta ** 8 - 0.7D1 / 0.4D1 * exp(-(-dble(s) + ri) ** 2 / delta **
     # 2) * delta ** 4 * dble(s ** 5) + 0.11D2 * exp(-(-dble(s) + ri) **
     # 2 / delta ** 2) * ri ** 7 * delta ** 4 + 0.147D3 / 0.2D1 * exp(-(
     #-dble(s) + ri) ** 2 / delta ** 2) * ri ** 5 * delta ** 6 + 0.2895D
     #4 / 0.32D2 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * ri * delta 
     #** 10
      CASE(8)
      cg = -0.128D3 / 0.3465D4 * c + 0.13D2 * exp(-(-s + ri) ** 2 / delt
     #a ** 2) * delta ** 4 * ri ** 7 * s + 0.49D2 / 0.4D1 * exp(-(-s + r
     #i) ** 2 / delta ** 2) * delta ** 4 * ri ** 6 * s ** 2 + 0.45D2 / 0
     #.4D1 * exp(-(-s + ri) ** 2 / delta ** 2) * delta ** 4 * ri ** 5 * 
     #s ** 3 + 0.10D2 * exp(-(-s + ri) ** 2 / delta ** 2) * delta ** 4 *
     # ri ** 4 * s ** 4 + 0.17D2 / 0.2D1 * exp(-(-s + ri) ** 2 / delta *
     #* 2) * delta ** 4 * ri ** 3 * s ** 5 + 0.27D2 / 0.4D1 * exp(-(-s +
     # ri) ** 2 / delta ** 2) * delta ** 4 * ri ** 2 * s ** 6 + 0.19D2 /
     # 0.4D1 * exp(-(-s + ri) ** 2 / delta ** 2) * delta ** 4 * ri * s *
     #* 7 + exp(-(-s + ri) ** 2 / delta ** 2) * delta ** 2 * ri ** 9 * s
     # / 0.2D1 + exp(-(-s + ri) ** 2 / delta ** 2) * delta ** 2 * ri ** 
     #8 * s ** 2 / 0.2D1 + exp(-(-s + ri) ** 2 / delta ** 2) * delta ** 
     #2 * ri ** 7 * s ** 3 / 0.2D1 + exp(-(-s + ri) ** 2 / delta ** 2) *
     # delta ** 2 * ri ** 6 * s ** 4 / 0.2D1 + exp(-(-s + ri) ** 2 / del
     #ta ** 2) * delta ** 2 * ri ** 5 * s ** 5 / 0.2D1 + exp(-(-s + ri) 
     #** 2 / delta ** 2) * delta ** 2 * ri ** 4 * s ** 6 / 0.2D1 + exp(-
     #(-s + ri) ** 2 / delta ** 2) * delta ** 2 * ri ** 3 * s ** 7 / 0.2
     #D1 + exp(-(-s + ri) ** 2 / delta ** 2) * delta ** 2 * ri ** 2 * s 
     #** 8 / 0.2D1 + exp(-(-s + ri) ** 2 / delta ** 2) * delta ** 2 * ri
     # * s ** 9 / 0.2D1 - 0.561D3 / 0.16D2 * exp(-(-s + ri) ** 2 / delta
     # ** 2) * delta ** 8 * ri * s - 0.285D3 / 0.8D1 * exp(-(-s + ri) **
     # 2 / delta ** 2) * delta ** 6 * ri ** 3 * s - 0.207D3 / 0.8D1 * ex
     #p(-(-s + ri) ** 2 / delta ** 2) * delta ** 6 * ri ** 2 * s ** 2 - 
     #0.123D3 / 0.8D1 * exp(-(-s + ri) ** 2 / delta ** 2) * delta ** 6 *
     # ri * s ** 3 - 0.33D2 / 0.4D1 * exp(-(-s + ri) ** 2 / delta ** 2) 
     #* delta ** 4 * ri ** 5 * s - delta * ri ** 11 * sqrt(0.31415926535
     #8979323846264338328D1) * erf(0.1D1 / delta * ri) / 0.2D1 - 0.55D2 
     #/ 0.4D1 * ri ** 9 * delta ** 3 * sqrt(0.31415926535897932384626433
     #8328D1) * erf(0.1D1 / delta * ri) - 0.495D3 / 0.4D1 * ri ** 7 * de
     #lta ** 5 * sqrt(0.314159265358979323846264338328D1) * erf(0.1D1 / 
     #delta * ri) - 0.15D2 / 0.2D1 * exp(-(-s + ri) ** 2 / delta ** 2) *
     # delta ** 4 * ri ** 4 * s ** 2 - 0.21D2 / 0.4D1 * exp(-(-s + ri) *
     #* 2 / delta ** 2) * delta ** 4 * ri ** 2 * s ** 4 - exp(-(-s + ri)
     # ** 2 / delta ** 2) * delta ** 2 * ri ** 7 * s / 0.2D1 - exp(-(-s 
     #+ ri) ** 2 / delta ** 2) * delta ** 2 * ri ** 6 * s ** 2 / 0.2D1 -
     # exp(-(-s + ri) ** 2 / delta ** 2) * delta ** 2 * ri ** 5 * s ** 3
     # / 0.2D1 - exp(-(-s + ri) ** 2 / delta ** 2) * delta ** 2 * ri ** 
     #4 * s ** 4 / 0.2D1 + 0.128D3 / 0.3465D4 * c * sqrt(-s ** 2 + 0.1D1
     #) + 0.345D3 / 0.8D1 * ri ** 4 * delta ** 6 * exp(-0.1D1 / delta **
     # 2 * ri ** 2) + ri ** 8 * delta ** 2 * exp(-0.1D1 / delta ** 2 * r
     #i ** 2) / 0.2D1 + 0.35D2 / 0.4D1 * ri ** 6 * delta ** 4 * exp(-0.1
     #D1 / delta ** 2 * ri ** 2) + 0.975D3 / 0.16D2 * ri ** 2 * delta **
     # 8 * exp(-0.1D1 / delta ** 2 * ri ** 2) - c * s ** 10 * sqrt(-s **
     # 2 + 0.1D1) / 0.11D2 + delta ** 2 * s ** 10 * exp(-(-s + ri) ** 2 
     #/ delta ** 2) / 0.2D1 - 0.60D2 * delta ** 12 * exp(-0.1D1 / delta 
     #** 2 * ri ** 2) - 0.345D3 / 0.8D1 * exp(-(-s + ri) ** 2 / delta **
     # 2) * ri ** 4 * delta ** 6 - exp(-(-s + ri) ** 2 / delta ** 2) * d
     #elta ** 2 * s ** 8 / 0.2D1 - 0.3465D4 / 0.8D1 * ri ** 5 * delta **
     # 7 * sqrt(0.314159265358979323846264338328D1) * erf(0.1D1 / delta 
     #* ri) - 0.17325D5 / 0.32D2 * ri ** 3 * delta ** 9 * sqrt(0.3141592
     #65358979323846264338328D1) * erf(0.1D1 / delta * ri) - 0.10395D5 /
     # 0.64D2 * ri * delta ** 11 * sqrt(0.314159265358979323846264338328
     #D1) * erf(0.1D1 / delta * ri) - 0.315D3 / 0.4D1 * sqrt(0.314159265
     #358979323846264338328D1) * erf((-s + ri) / delta) * delta ** 7 * r
     #i ** 3 - 0.9D1 * sqrt(0.314159265358979323846264338328D1) * erf((-
     #s + ri) / delta) * delta ** 3 * ri ** 7 - sqrt(0.31415926535897932
     #3846264338328D1) * erf((-s + ri) / delta) * delta * ri ** 9 / 0.2D
     #1 - ri ** 10 * delta ** 2 * exp(-0.1D1 / delta ** 2 * ri ** 2) / 0
     #.2D1 - 0.27D2 / 0.2D1 * ri ** 8 * delta ** 4 * exp(-0.1D1 / delta 
     #** 2 * ri ** 2) - 0.469D3 / 0.4D1 * ri ** 6 * delta ** 6 * exp(-0.
     #1D1 / delta ** 2 * ri ** 2) - 0.3045D4 / 0.8D1 * ri ** 4 * delta *
     #* 8 * exp(-0.1D1 / delta ** 2 * ri ** 2) - 0.12645D5 / 0.32D2 * ri
     # ** 2 * delta ** 10 * exp(-0.1D1 / delta ** 2 * ri ** 2) + 0.213D3
     # / 0.8D1 * exp(-(-s + ri) ** 2 / delta ** 2) * delta ** 6 * ri * s
     # ** 5 + 0.64D2 / 0.3465D4 * sqrt(-s ** 2 + 0.1D1) * c * s ** 2 - 0
     #.12D2 * exp(-(-s + ri) ** 2 / delta ** 2) * delta ** 10 + 0.60D2 *
     # exp(-(-s + ri) ** 2 / delta ** 2) * delta ** 12 + 0.16D2 / 0.1155
     #D4 * sqrt(-s ** 2 + 0.1D1) * c * s ** 4 + sqrt(-s ** 2 + 0.1D1) * 
     #c * s ** 8 / 0.99D2 + 0.8D1 / 0.693D3 * sqrt(-s ** 2 + 0.1D1) * c 
     #* s ** 6 + 0.12D2 * delta ** 10 * exp(-0.1D1 / delta ** 2 * ri ** 
     #2) + 0.945D3 / 0.32D2 * ri * delta ** 9 * sqrt(0.31415926535897932
     #3846264338328D1) * erf(0.1D1 / delta * ri) + delta * ri ** 9 * sqr
     #t(0.314159265358979323846264338328D1) * erf(0.1D1 / delta * ri) / 
     #0.2D1 + 0.9D1 * ri ** 7 * delta ** 3 * sqrt(0.31415926535897932384
     #6264338328D1) * erf(0.1D1 / delta * ri) + 0.189D3 / 0.4D1 * ri ** 
     #5 * delta ** 5 * sqrt(0.314159265358979323846264338328D1) * erf(0.
     #1D1 / delta * ri) + 0.315D3 / 0.4D1 * ri ** 3 * delta ** 7 * sqrt(
     #0.314159265358979323846264338328D1) * erf(0.1D1 / delta * ri) + 0.
     #10395D5 / 0.64D2 * sqrt(0.314159265358979323846264338328D1) * erf(
     #(-s + ri) / delta) * delta ** 11 * ri + 0.17325D5 / 0.32D2 * sqrt(
     #0.314159265358979323846264338328D1) * erf((-s + ri) / delta) * del
     #ta ** 9 * ri ** 3 + 0.3465D4 / 0.8D1 * sqrt(0.31415926535897932384
     #6264338328D1) * erf((-s + ri) / delta) * delta ** 7 * ri ** 5 + 0.
     #495D3 / 0.4D1 * sqrt(0.314159265358979323846264338328D1) * erf((-s
     # + ri) / delta) * delta ** 5 * ri ** 7 + 0.55D2 / 0.4D1 * sqrt(0.3
     #14159265358979323846264338328D1) * erf((-s + ri) / delta) * delta 
     #** 3 * ri ** 9 + sqrt(0.314159265358979323846264338328D1) * erf((-
     #s + ri) / delta) * delta * ri ** 11 / 0.2D1 - 0.945D3 / 0.32D2 * r
     #i * delta ** 9 * sqrt(0.314159265358979323846264338328D1) * erf((-
     #s + ri) / delta) - 0.189D3 / 0.4D1 * ri ** 5 * delta ** 5 * sqrt(0
     #.314159265358979323846264338328D1) * erf((-s + ri) / delta) - 0.13
     #D2 / 0.2D1 * exp(-(-s + ri) ** 2 / delta ** 2) * delta ** 4 * ri *
     #* 3 * s ** 3 - 0.15D2 / 0.4D1 * exp(-(-s + ri) ** 2 / delta ** 2) 
     #* delta ** 4 * ri * s ** 5 - exp(-(-s + ri) ** 2 / delta ** 2) * d
     #elta ** 2 * ri ** 3 * s ** 5 / 0.2D1 - exp(-(-s + ri) ** 2 / delta
     # ** 2) * delta ** 2 * ri ** 2 * s ** 6 / 0.2D1 - exp(-(-s + ri) **
     # 2 / delta ** 2) * delta ** 2 * ri * s ** 7 / 0.2D1 + 0.6555D4 / 0
     #.32D2 * exp(-(-s + ri) ** 2 / delta ** 2) * delta ** 10 * ri * s +
     # 0.585D3 / 0.2D1 * exp(-(-s + ri) ** 2 / delta ** 2) * delta ** 8 
     #* ri ** 3 * s + 0.3045D4 / 0.16D2 * exp(-(-s + ri) ** 2 / delta **
     # 2) * delta ** 8 * ri ** 2 * s ** 2 + 0.1545D4 / 0.16D2 * exp(-(-s
     # + ri) ** 2 / delta ** 2) * delta ** 8 * ri * s ** 3 + 0.105D3 * e
     #xp(-(-s + ri) ** 2 / delta ** 2) * delta ** 6 * ri ** 5 * s + 0.70
     #5D3 / 0.8D1 * exp(-(-s + ri) ** 2 / delta ** 2) * delta ** 6 * ri 
     #** 4 * s ** 2 + 0.545D3 / 0.8D1 * exp(-(-s + ri) ** 2 / delta ** 2
     #) * delta ** 6 * ri ** 3 * s ** 3 + 0.375D3 / 0.8D1 * exp(-(-s + r
     #i) ** 2 / delta ** 2) * delta ** 6 * ri ** 2 * s ** 4 + 0.30D2 * e
     #xp(-(-s + ri) ** 2 / delta ** 2) * delta ** 8 * s ** 4 + 0.10D2 * 
     #exp(-(-s + ri) ** 2 / delta ** 2) * delta ** 6 * s ** 6 + 0.12645D
     #5 / 0.32D2 * exp(-(-s + ri) ** 2 / delta ** 2) * ri ** 2 * delta *
     #* 10 + 0.60D2 * exp(-(-s + ri) ** 2 / delta ** 2) * delta ** 10 * 
     #s ** 2 + 0.27D2 / 0.2D1 * exp(-(-s + ri) ** 2 / delta ** 2) * ri *
     #* 8 * delta ** 4 + 0.469D3 / 0.4D1 * exp(-(-s + ri) ** 2 / delta *
     #* 2) * ri ** 6 * delta ** 6 + 0.3045D4 / 0.8D1 * exp(-(-s + ri) **
     # 2 / delta ** 2) * ri ** 4 * delta ** 8 - 0.12D2 * exp(-(-s + ri) 
     #** 2 / delta ** 2) * delta ** 8 * s ** 2 - 0.6D1 * exp(-(-s + ri) 
     #** 2 / delta ** 2) * delta ** 6 * s ** 4 - 0.2D1 * exp(-(-s + ri) 
     #** 2 / delta ** 2) * delta ** 4 * s ** 6 - exp(-(-s + ri) ** 2 / d
     #elta ** 2) * ri ** 8 * delta ** 2 / 0.2D1 - 0.35D2 / 0.4D1 * exp(-
     #(-s + ri) ** 2 / delta ** 2) * ri ** 6 * delta ** 4 - 0.975D3 / 0.
     #16D2 * exp(-(-s + ri) ** 2 / delta ** 2) * ri ** 2 * delta ** 8 + 
     #exp(-(-s + ri) ** 2 / delta ** 2) * ri ** 10 * delta ** 2 / 0.2D1 
     #+ 0.5D1 / 0.2D1 * exp(-(-s + ri) ** 2 / delta ** 2) * delta ** 4 *
     # s ** 8
      CASE(9)
      cg = 0.2895D4 / 0.32D2 * ri * delta ** 10 * exp(-0.1D1 / delta ** 
     #2 * ri ** 2) + ri ** 9 * delta ** 2 * exp(-0.1D1 / delta ** 2 * ri
     # ** 2) / 0.2D1 + 0.11D2 * ri ** 7 * delta ** 4 * exp(-0.1D1 / delt
     #a ** 2 * ri ** 2) + 0.147D3 / 0.2D1 * ri ** 5 * delta ** 6 * exp(-
     #0.1D1 / delta ** 2 * ri ** 2) + 0.165D3 * ri ** 3 * delta ** 8 * e
     #xp(-0.1D1 / delta ** 2 * ri ** 2) + 0.3D1 / 0.320D3 * c * dble(s *
     #* 7) * sqrt(dble(-s ** 2 + 1)) + 0.945D3 / 0.64D2 * delta ** 11 * 
     #sqrt(0.314159265358979323846264338328D1) * erf(0.1D1 / delta * ri)
     # - 0.2895D4 / 0.32D2 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * r
     #i * delta ** 10 + 0.711D3 / 0.4D1 * exp(-(-dble(s) + ri) ** 2 / de
     #lta ** 2) * ri ** 7 * delta ** 6 + 0.6279D4 / 0.8D1 * exp(-(-dble(
     #s) + ri) ** 2 / delta ** 2) * ri ** 5 * delta ** 8 + 0.267D3 / 0.8
     #D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 6 * ri * d
     #ble(s ** 6) + 0.63D2 / 0.4D1 * exp(-(-dble(s) + ri) ** 2 / delta *
     #* 2) * delta ** 4 * ri ** 8 * dble(s) + 0.15D2 * exp(-(-dble(s) + 
     #ri) ** 2 / delta ** 2) * delta ** 4 * ri ** 7 * dble(s ** 2) + 0.1
     #4D2 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 4 * ri **
     # 6 * dble(s ** 3) + 0.51D2 / 0.4D1 * exp(-(-dble(s) + ri) ** 2 / d
     #elta ** 2) * delta ** 4 * ri ** 5 * dble(s ** 4) + 0.45D2 / 0.4D1 
     #* exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 4 * ri ** 4 *
     # dble(s ** 5) + 0.19D2 / 0.2D1 * exp(-(-dble(s) + ri) ** 2 / delta
     # ** 2) * delta ** 4 * ri ** 3 * dble(s ** 6) + 0.15D2 / 0.2D1 * ex
     #p(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 4 * ri ** 2 * dbl
     #e(s ** 7) + 0.21D2 / 0.4D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 
     #2) * delta ** 4 * ri * dble(s ** 8) + exp(-(-dble(s) + ri) ** 2 / 
     #delta ** 2) * delta ** 2 * ri ** 10 * dble(s) / 0.2D1 + 0.5145D4 /
     # 0.8D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 8 * ri
     # ** 4 * dble(s) + 0.1875D4 / 0.4D1 * exp(-(-dble(s) + ri) ** 2 / d
     #elta ** 2) * delta ** 8 * ri ** 3 * dble(s ** 2) + 0.585D3 / 0.2D1
     # * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 8 * ri ** 2 
     #* dble(s ** 3) + 0.2295D4 / 0.16D2 * exp(-(-dble(s) + ri) ** 2 / d
     #elta ** 2) * delta ** 8 * ri * dble(s ** 4) + 0.651D3 / 0.4D1 * ex
     #p(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 6 * ri ** 6 * dbl
     #e(s) + 0.567D3 / 0.4D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) *
     # delta ** 6 * ri ** 5 * dble(s ** 2) + 0.465D3 / 0.4D1 * exp(-(-db
     #le(s) + ri) ** 2 / delta ** 2) * delta ** 6 * ri ** 4 * dble(s ** 
     #3) + 0.705D3 / 0.8D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * d
     #elta ** 6 * ri ** 3 * dble(s ** 4) + 0.477D3 / 0.8D1 * exp(-(-dble
     #(s) + ri) ** 2 / delta ** 2) * delta ** 6 * ri ** 2 * dble(s ** 5)
     # - 0.975D3 / 0.16D2 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * de
     #lta ** 8 * ri * dble(s ** 2) - 0.405D3 / 0.8D1 * exp(-(-dble(s) + 
     #ri) ** 2 / delta ** 2) * delta ** 6 * ri ** 3 * dble(s ** 2) - 0.2
     #85D3 / 0.8D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 
     #6 * ri ** 2 * dble(s ** 3) - 0.165D3 / 0.8D1 * exp(-(-dble(s) + ri
     #) ** 2 / delta ** 2) * delta ** 6 * ri * dble(s ** 4) - 0.21D2 / 0
     #.2D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 4 * ri *
     #* 6 * dble(s) - 0.39D2 / 0.4D1 * exp(-(-dble(s) + ri) ** 2 / delta
     # ** 2) * delta ** 4 * ri ** 5 * dble(s ** 2) - 0.35D2 / 0.4D1 * ex
     #p(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 4 * ri ** 4 * dbl
     #e(s ** 3) + 0.26685D5 / 0.32D2 * exp(-(-dble(s) + ri) ** 2 / delta
     # ** 2) * delta ** 10 * ri ** 2 * dble(s) + 0.12645D5 / 0.32D2 * ex
     #p(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 10 * ri * dble(s 
     #** 2) - exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 2 * ri 
     #** 3 * dble(s ** 6) / 0.2D1 - exp(-(-dble(s) + ri) ** 2 / delta **
     # 2) * delta ** 2 * ri ** 2 * dble(s ** 7) / 0.2D1 - exp(-(-dble(s)
     # + ri) ** 2 / delta ** 2) * delta ** 2 * ri * dble(s ** 8) / 0.2D1
     # + exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 2 * ri * dbl
     #e(s ** 10) / 0.2D1 - 0.915D3 / 0.8D1 * exp(-(-dble(s) + ri) ** 2 /
     # delta ** 2) * delta ** 8 * ri ** 2 * dble(s) - 0.17D2 / 0.4D1 * e
     #xp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 4 * ri * dble(s 
     #** 6) - exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 2 * ri 
     #** 6 * dble(s ** 3) / 0.2D1 - 0.255D3 / 0.4D1 * exp(-(-dble(s) + r
     #i) ** 2 / delta ** 2) * delta ** 6 * ri ** 4 * dble(s) - 0.35685D5
     # / 0.64D2 * ri * delta ** 12 * exp(-0.1D1 / delta ** 2 * ri ** 2) 
     #- 0.41685D5 / 0.32D2 * ri ** 3 * delta ** 10 * exp(-0.1D1 / delta 
     #** 2 * ri ** 2) - 0.6279D4 / 0.8D1 * ri ** 5 * delta ** 8 * exp(-0
     #.1D1 / delta ** 2 * ri ** 2) - ri ** 11 * delta ** 2 * exp(-0.1D1 
     #/ delta ** 2 * ri ** 2) / 0.2D1 - 0.65D2 / 0.4D1 * ri ** 9 * delta
     # ** 4 * exp(-0.1D1 / delta ** 2 * ri ** 2) - 0.711D3 / 0.4D1 * ri 
     #** 7 * delta ** 6 * exp(-0.1D1 / delta ** 2 * ri ** 2) - 0.10395D5
     # / 0.128D3 * delta ** 13 * sqrt(0.314159265358979323846264338328D1
     #) * erf(0.1D1 / delta * ri) - 0.31185D5 / 0.32D2 * ri ** 2 * delta
     # ** 11 * sqrt(0.314159265358979323846264338328D1) * erf(0.1D1 / de
     #lta * ri) - 0.51975D5 / 0.32D2 * ri ** 4 * delta ** 9 * sqrt(0.314
     #159265358979323846264338328D1) * erf(0.1D1 / delta * ri) - delta *
     # ri ** 12 * sqrt(0.314159265358979323846264338328D1) * erf(0.1D1 /
     # delta * ri) / 0.2D1 - 0.33D2 / 0.2D1 * ri ** 10 * delta ** 3 * sq
     #rt(0.314159265358979323846264338328D1) * erf(0.1D1 / delta * ri) -
     # 0.1485D4 / 0.8D1 * ri ** 8 * delta ** 5 * sqrt(0.3141592653589793
     #23846264338328D1) * erf(0.1D1 / delta * ri) - 0.3465D4 / 0.4D1 * r
     #i ** 6 * delta ** 7 * sqrt(0.314159265358979323846264338328D1) * e
     #rf(0.1D1 / delta * ri) + 0.7D1 / 0.640D3 * sqrt(dble(-s ** 2 + 1))
     # * c * dble(s ** 5) + 0.45D2 / 0.4D1 * ri ** 8 * delta ** 3 * sqrt
     #(0.314159265358979323846264338328D1) * erf(0.1D1 / delta * ri) + 0
     #.315D3 / 0.4D1 * ri ** 6 * delta ** 5 * sqrt(0.3141592653589793238
     #46264338328D1) * erf(0.1D1 / delta * ri) + 0.1575D4 / 0.8D1 * ri *
     #* 4 * delta ** 7 * sqrt(0.314159265358979323846264338328D1) * erf(
     #0.1D1 / delta * ri) + 0.4725D4 / 0.32D2 * ri ** 2 * delta ** 9 * s
     #qrt(0.314159265358979323846264338328D1) * erf(0.1D1 / delta * ri) 
     #+ delta * ri ** 10 * sqrt(0.314159265358979323846264338328D1) * er
     #f(0.1D1 / delta * ri) / 0.2D1 - exp(-(-dble(s) + ri) ** 2 / delta 
     #** 2) * delta ** 2 * ri ** 7 * dble(s ** 2) / 0.2D1 + 0.21D2 / 0.1
     #024D4 * sqrt(dble(-s ** 2 + 1)) * c * dble(s) + 0.10395D5 / 0.128D
     #3 * sqrt(0.314159265358979323846264338328D1) * erf((-dble(s) + ri)
     # / delta) * delta ** 13 - 0.945D3 / 0.64D2 * delta ** 11 * sqrt(0.
     #314159265358979323846264338328D1) * erf((-dble(s) + ri) / delta) +
     # delta ** 2 * dble(s ** 11) * exp(-(-dble(s) + ri) ** 2 / delta **
     # 2) / 0.2D1 - delta ** 2 * dble(s ** 9) * exp(-(-dble(s) + ri) ** 
     #2 / delta ** 2) / 0.2D1 - 0.9D1 / 0.4D1 * exp(-(-dble(s) + ri) ** 
     #2 / delta ** 2) * delta ** 4 * dble(s ** 7) + 0.65D2 / 0.4D1 * exp
     #(-(-dble(s) + ri) ** 2 / delta ** 2) * ri ** 9 * delta ** 4 - 0.63
     #D2 / 0.8D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 6 
     #* dble(s ** 5) - 0.315D3 / 0.16D2 * exp(-(-dble(s) + ri) ** 2 / de
     #lta ** 2) * delta ** 8 * dble(s ** 3) - 0.945D3 / 0.32D2 * exp(-(-
     #dble(s) + ri) ** 2 / delta ** 2) * delta ** 10 * dble(s) + 0.99D2 
     #/ 0.8D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 6 * d
     #ble(s ** 7) + 0.11D2 / 0.4D1 * exp(-(-dble(s) + ri) ** 2 / delta *
     #* 2) * delta ** 4 * dble(s ** 9) + 0.3465D4 / 0.32D2 * exp(-(-dble
     #(s) + ri) ** 2 / delta ** 2) * delta ** 10 * dble(s ** 3) + 0.693D
     #3 / 0.16D2 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 8 
     #* dble(s ** 5) + 0.41685D5 / 0.32D2 * exp(-(-dble(s) + ri) ** 2 / 
     #delta ** 2) * ri ** 3 * delta ** 10 + exp(-(-dble(s) + ri) ** 2 / 
     #delta ** 2) * ri ** 11 * delta ** 2 / 0.2D1 + 0.10395D5 / 0.64D2 *
     # exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 12 * dble(s) -
     # 0.147D3 / 0.2D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * ri **
     # 5 * delta ** 6 - 0.165D3 * exp(-(-dble(s) + ri) ** 2 / delta ** 2
     #) * ri ** 3 * delta ** 8 - exp(-(-dble(s) + ri) ** 2 / delta ** 2)
     # * ri ** 9 * delta ** 2 / 0.2D1 - 0.11D2 * exp(-(-dble(s) + ri) **
     # 2 / delta ** 2) * ri ** 7 * delta ** 4 + 0.35685D5 / 0.64D2 * exp
     #(-(-dble(s) + ri) ** 2 / delta ** 2) * ri * delta ** 12 - c * dble
     #(s ** 11) * sqrt(dble(-s ** 2 + 1)) / 0.12D2 + sqrt(dble(-s ** 2 +
     # 1)) * c * dble(s ** 9) / 0.120D3 + 0.7D1 / 0.512D3 * sqrt(dble(-s
     # ** 2 + 1)) * c * dble(s ** 3) - 0.15D2 / 0.2D1 * exp(-(-dble(s) +
     # ri) ** 2 / delta ** 2) * delta ** 4 * ri ** 3 * dble(s ** 4) - ex
     #p(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 2 * ri ** 5 * dbl
     #e(s ** 4) / 0.2D1 - exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delt
     #a ** 2 * ri ** 4 * dble(s ** 5) / 0.2D1 - 0.6D1 * exp(-(-dble(s) +
     # ri) ** 2 / delta ** 2) * delta ** 4 * ri ** 2 * dble(s ** 5) - ex
     #p(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 2 * ri ** 8 * dbl
     #e(s) / 0.2D1 + exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 
     #2 * ri ** 9 * dble(s ** 2) / 0.2D1 + exp(-(-dble(s) + ri) ** 2 / d
     #elta ** 2) * delta ** 2 * ri ** 8 * dble(s ** 3) / 0.2D1 + exp(-(-
     #dble(s) + ri) ** 2 / delta ** 2) * delta ** 2 * ri ** 7 * dble(s *
     #* 4) / 0.2D1 + exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 
     #2 * ri ** 6 * dble(s ** 5) / 0.2D1 + exp(-(-dble(s) + ri) ** 2 / d
     #elta ** 2) * delta ** 2 * ri ** 5 * dble(s ** 6) / 0.2D1 + exp(-(-
     #dble(s) + ri) ** 2 / delta ** 2) * delta ** 2 * ri ** 4 * dble(s *
     #* 7) / 0.2D1 + exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 
     #2 * ri ** 3 * dble(s ** 8) / 0.2D1 + exp(-(-dble(s) + ri) ** 2 / d
     #elta ** 2) * delta ** 2 * ri ** 2 * dble(s ** 9) / 0.2D1 - 0.21D2 
     #/ 0.1024D4 * asin(dble(s)) * c + 0.31185D5 / 0.32D2 * sqrt(0.31415
     #9265358979323846264338328D1) * erf((-dble(s) + ri) / delta) * delt
     #a ** 11 * ri ** 2 + 0.51975D5 / 0.32D2 * sqrt(0.314159265358979323
     #846264338328D1) * erf((-dble(s) + ri) / delta) * delta ** 9 * ri *
     #* 4 + 0.3465D4 / 0.4D1 * sqrt(0.314159265358979323846264338328D1) 
     #* erf((-dble(s) + ri) / delta) * delta ** 7 * ri ** 6 + 0.1485D4 /
     # 0.8D1 * sqrt(0.314159265358979323846264338328D1) * erf((-dble(s) 
     #+ ri) / delta) * delta ** 5 * ri ** 8 + 0.33D2 / 0.2D1 * sqrt(0.31
     #4159265358979323846264338328D1) * erf((-dble(s) + ri) / delta) * d
     #elta ** 3 * ri ** 10 + sqrt(0.314159265358979323846264338328D1) * 
     #erf((-dble(s) + ri) / delta) * delta * ri ** 12 / 0.2D1 - 0.4725D4
     # / 0.32D2 * ri ** 2 * delta ** 9 * sqrt(0.314159265358979323846264
     #338328D1) * erf((-dble(s) + ri) / delta) - 0.1575D4 / 0.8D1 * ri *
     #* 4 * delta ** 7 * sqrt(0.314159265358979323846264338328D1) * erf(
     #(-dble(s) + ri) / delta) - 0.315D3 / 0.4D1 * ri ** 6 * delta ** 5 
     #* sqrt(0.314159265358979323846264338328D1) * erf((-dble(s) + ri) /
     # delta) - 0.45D2 / 0.4D1 * ri ** 8 * delta ** 3 * sqrt(0.314159265
     #358979323846264338328D1) * erf((-dble(s) + ri) / delta) - delta * 
     #ri ** 10 * sqrt(0.314159265358979323846264338328D1) * erf((-dble(s
     #) + ri) / delta) / 0.2D1
      CASE(10)
      cg = -0.256D3 / 0.9009D4 * c + 0.256D3 / 0.9009D4 * c * sqrt(dble(
     #-s ** 2 + 1)) + delta * ri ** 11 * sqrt(0.314159265358979323846264
     #338328D1) * erf(0.1D1 / delta * ri) / 0.2D1 + 0.55D2 / 0.4D1 * ri 
     #** 9 * delta ** 3 * sqrt(0.314159265358979323846264338328D1) * erf
     #(0.1D1 / delta * ri) + 0.495D3 / 0.4D1 * ri ** 7 * delta ** 5 * sq
     #rt(0.314159265358979323846264338328D1) * erf(0.1D1 / delta * ri) -
     # 0.114765D6 / 0.32D2 * ri ** 4 * delta ** 10 * exp(-0.1D1 / delta 
     #** 2 * ri ** 2) - 0.1035D4 / 0.4D1 * ri ** 8 * delta ** 6 * exp(-0
     #.1D1 / delta ** 2 * ri ** 2) - ri ** 12 * delta ** 2 * exp(-0.1D1 
     #/ delta ** 2 * ri ** 2) / 0.2D1 - 0.77D2 / 0.4D1 * ri ** 10 * delt
     #a ** 4 * exp(-0.1D1 / delta ** 2 * ri ** 2) - 0.187425D6 / 0.64D2 
     #* ri ** 2 * delta ** 12 * exp(-0.1D1 / delta ** 2 * ri ** 2) - 0.1
     #1907D5 / 0.8D1 * ri ** 6 * delta ** 8 * exp(-0.1D1 / delta ** 2 * 
     #ri ** 2) + 0.80D2 / 0.9009D4 * sqrt(dble(-s ** 2 + 1)) * c * dble(
     #s ** 6) + 0.60D2 * delta ** 12 * exp(-0.1D1 / delta ** 2 * ri ** 2
     #) + 0.32D2 / 0.3003D4 * sqrt(dble(-s ** 2 + 1)) * c * dble(s ** 4)
     # + 0.3465D4 / 0.8D1 * ri ** 5 * delta ** 7 * sqrt(0.31415926535897
     #9323846264338328D1) * erf(0.1D1 / delta * ri) + 0.17325D5 / 0.32D2
     # * ri ** 3 * delta ** 9 * sqrt(0.314159265358979323846264338328D1)
     # * erf(0.1D1 / delta * ri) + 0.10395D5 / 0.64D2 * ri * delta ** 11
     # * sqrt(0.314159265358979323846264338328D1) * erf(0.1D1 / delta * 
     #ri) + ri ** 10 * delta ** 2 * exp(-0.1D1 / delta ** 2 * ri ** 2) /
     # 0.2D1 + 0.27D2 / 0.2D1 * ri ** 8 * delta ** 4 * exp(-0.1D1 / delt
     #a ** 2 * ri ** 2) + 0.469D3 / 0.4D1 * ri ** 6 * delta ** 6 * exp(-
     #0.1D1 / delta ** 2 * ri ** 2) + 0.3045D4 / 0.8D1 * ri ** 4 * delta
     # ** 8 * exp(-0.1D1 / delta ** 2 * ri ** 2) + 0.12645D5 / 0.32D2 * 
     #ri ** 2 * delta ** 10 * exp(-0.1D1 / delta ** 2 * ri ** 2) - 0.360
     #D3 * delta ** 14 * exp(-0.1D1 / delta ** 2 * ri ** 2) - 0.60D2 * e
     #xp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 12 + 0.360D3 * e
     #xp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 14 + sqrt(dble(-
     #s ** 2 + 1)) * c * dble(s ** 10) / 0.143D3 + 0.10D2 / 0.1287D4 * s
     #qrt(dble(-s ** 2 + 1)) * c * dble(s ** 8) + 0.128D3 / 0.9009D4 * s
     #qrt(dble(-s ** 2 + 1)) * c * dble(s ** 2) + 0.135135D6 / 0.128D3 *
     # erf((-dble(s) + ri) / delta) * sqrt(0.314159265358979323846264338
     #328D1) * delta ** 13 * ri + 0.135135D6 / 0.32D2 * erf((-dble(s) + 
     #ri) / delta) * sqrt(0.314159265358979323846264338328D1) * delta **
     # 11 * ri ** 3 + 0.135135D6 / 0.32D2 * erf((-dble(s) + ri) / delta)
     # * sqrt(0.314159265358979323846264338328D1) * delta ** 9 * ri ** 5
     # + 0.6435D4 / 0.4D1 * erf((-dble(s) + ri) / delta) * sqrt(0.314159
     #265358979323846264338328D1) * delta ** 7 * ri ** 7 + 0.2145D4 / 0.
     #8D1 * erf((-dble(s) + ri) / delta) * sqrt(0.3141592653589793238462
     #64338328D1) * delta ** 5 * ri ** 9 + 0.39D2 / 0.2D1 * erf((-dble(s
     #) + ri) / delta) * sqrt(0.314159265358979323846264338328D1) * delt
     #a ** 3 * ri ** 11 + erf((-dble(s) + ri) / delta) * sqrt(0.31415926
     #5358979323846264338328D1) * delta * ri ** 13 / 0.2D1 - 0.10395D5 /
     # 0.64D2 * ri * delta ** 11 * sqrt(0.314159265358979323846264338328
     #D1) * erf((-dble(s) + ri) / delta) - 0.17325D5 / 0.32D2 * ri ** 3 
     #* delta ** 9 * sqrt(0.314159265358979323846264338328D1) * erf((-db
     #le(s) + ri) / delta) - 0.3465D4 / 0.8D1 * ri ** 5 * delta ** 7 * s
     #qrt(0.314159265358979323846264338328D1) * erf((-dble(s) + ri) / de
     #lta) - c * dble(s ** 12) * sqrt(dble(-s ** 2 + 1)) / 0.13D2 - 0.49
     #5D3 / 0.4D1 * ri ** 7 * delta ** 5 * sqrt(0.3141592653589793238462
     #64338328D1) * erf((-dble(s) + ri) / delta) - 0.55D2 / 0.4D1 * ri *
     #* 9 * delta ** 3 * sqrt(0.314159265358979323846264338328D1) * erf(
     #(-dble(s) + ri) / delta) - delta * ri ** 11 * sqrt(0.3141592653589
     #79323846264338328D1) * erf((-dble(s) + ri) / delta) / 0.2D1 - 0.27
     #D2 / 0.2D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * ri ** 8 * d
     #elta ** 4 + exp(-(-dble(s) + ri) ** 2 / delta ** 2) * ri ** 12 * d
     #elta ** 2 / 0.2D1 - 0.60D2 * exp(-(-dble(s) + ri) ** 2 / delta ** 
     #2) * delta ** 10 * dble(s ** 2) - 0.30D2 * exp(-(-dble(s) + ri) **
     # 2 / delta ** 2) * delta ** 8 * dble(s ** 4) - 0.10D2 * exp(-(-dbl
     #e(s) + ri) ** 2 / delta ** 2) * delta ** 6 * dble(s ** 6) - 0.5D1 
     #/ 0.2D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 4 * d
     #ble(s ** 8) + 0.187425D6 / 0.64D2 * exp(-(-dble(s) + ri) ** 2 / de
     #lta ** 2) * ri ** 2 * delta ** 12 + 0.114765D6 / 0.32D2 * exp(-(-d
     #ble(s) + ri) ** 2 / delta ** 2) * ri ** 4 * delta ** 10 + 0.11907D
     #5 / 0.8D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * ri ** 6 * de
     #lta ** 8 + 0.3D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta
     # ** 4 * dble(s ** 10) + 0.180D3 * exp(-(-dble(s) + ri) ** 2 / delt
     #a ** 2) * delta ** 10 * dble(s ** 4) + 0.60D2 * exp(-(-dble(s) + r
     #i) ** 2 / delta ** 2) * delta ** 8 * dble(s ** 6) + 0.15D2 * exp(-
     #(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 6 * dble(s ** 8) - 0
     #.12645D5 / 0.32D2 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * ri *
     #* 2 * delta ** 10 + 0.360D3 * exp(-(-dble(s) + ri) ** 2 / delta **
     # 2) * delta ** 12 * dble(s ** 2) - exp(-(-dble(s) + ri) ** 2 / del
     #ta ** 2) * ri ** 10 * delta ** 2 / 0.2D1 - 0.469D3 / 0.4D1 * exp(-
     #(-dble(s) + ri) ** 2 / delta ** 2) * ri ** 6 * delta ** 6 - 0.3045
     #D4 / 0.8D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * ri ** 4 * d
     #elta ** 8 + 0.77D2 / 0.4D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 
     #2) * ri ** 10 * delta ** 4 + 0.1035D4 / 0.4D1 * exp(-(-dble(s) + r
     #i) ** 2 / delta ** 2) * ri ** 8 * delta ** 6 + delta ** 2 * dble(s
     # ** 12) * exp(-(-dble(s) + ri) ** 2 / delta ** 2) / 0.2D1 - delta 
     #** 2 * dble(s ** 10) * exp(-(-dble(s) + ri) ** 2 / delta ** 2) / 0
     #.2D1 - 0.135135D6 / 0.32D2 * ri ** 5 * delta ** 9 * sqrt(0.3141592
     #65358979323846264338328D1) * erf(0.1D1 / delta * ri) - 0.135135D6 
     #/ 0.32D2 * ri ** 3 * delta ** 11 * sqrt(0.314159265358979323846264
     #338328D1) * erf(0.1D1 / delta * ri) - delta * ri ** 13 * sqrt(0.31
     #4159265358979323846264338328D1) * erf(0.1D1 / delta * ri) / 0.2D1 
     #- 0.39D2 / 0.2D1 * ri ** 11 * delta ** 3 * sqrt(0.3141592653589793
     #23846264338328D1) * erf(0.1D1 / delta * ri) - 0.2145D4 / 0.8D1 * r
     #i ** 9 * delta ** 5 * sqrt(0.314159265358979323846264338328D1) * e
     #rf(0.1D1 / delta * ri) - 0.6435D4 / 0.4D1 * ri ** 7 * delta ** 7 *
     # sqrt(0.314159265358979323846264338328D1) * erf(0.1D1 / delta * ri
     #) - 0.135135D6 / 0.128D3 * ri * delta ** 13 * sqrt(0.3141592653589
     #79323846264338328D1) * erf(0.1D1 / delta * ri) - exp(-(-dble(s) + 
     #ri) ** 2 / delta ** 2) * delta ** 2 * ri ** 4 * dble(s ** 6) / 0.2
     #D1 - exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 2 * ri ** 
     #3 * dble(s ** 7) / 0.2D1 - exp(-(-dble(s) + ri) ** 2 / delta ** 2)
     # * delta ** 2 * ri ** 2 * dble(s ** 8) / 0.2D1 - 0.545D3 / 0.8D1 *
     # exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 6 * ri ** 3 * 
     #dble(s ** 3) - 0.375D3 / 0.8D1 * exp(-(-dble(s) + ri) ** 2 / delta
     # ** 2) * delta ** 6 * ri ** 2 * dble(s ** 4) - 0.213D3 / 0.8D1 * e
     #xp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 6 * ri * dble(s 
     #** 5) - 0.13D2 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta *
     #* 4 * ri ** 7 * dble(s) - 0.49D2 / 0.4D1 * exp(-(-dble(s) + ri) **
     # 2 / delta ** 2) * delta ** 4 * ri ** 6 * dble(s ** 2) - 0.10D2 * 
     #exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 4 * ri ** 4 * d
     #ble(s ** 4) - 0.17D2 / 0.2D1 * exp(-(-dble(s) + ri) ** 2 / delta *
     #* 2) * delta ** 4 * ri ** 3 * dble(s ** 5) - 0.27D2 / 0.4D1 * exp(
     #-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 4 * ri ** 2 * dble(
     #s ** 6) - 0.3045D4 / 0.16D2 * exp(-(-dble(s) + ri) ** 2 / delta **
     # 2) * delta ** 8 * ri ** 2 * dble(s ** 2) - 0.1545D4 / 0.16D2 * ex
     #p(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 8 * ri * dble(s *
     #* 3) - 0.105D3 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta *
     #* 6 * ri ** 5 * dble(s) - 0.705D3 / 0.8D1 * exp(-(-dble(s) + ri) *
     #* 2 / delta ** 2) * delta ** 6 * ri ** 4 * dble(s ** 2) + 0.75D2 /
     # 0.4D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 4 * ri
     # ** 9 * dble(s) + 0.18D2 * exp(-(-dble(s) + ri) ** 2 / delta ** 2)
     # * delta ** 4 * ri ** 8 * dble(s ** 2) - exp(-(-dble(s) + ri) ** 2
     # / delta ** 2) * delta ** 2 * ri * dble(s ** 9) / 0.2D1 + 0.89055D
     #5 / 0.64D2 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 12
     # * ri * dble(s) + 0.82845D5 / 0.32D2 * exp(-(-dble(s) + ri) ** 2 /
     # delta ** 2) * delta ** 10 * ri ** 3 * dble(s) + 0.49185D5 / 0.32D
     #2 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 10 * ri ** 
     #2 * dble(s ** 2) + 0.22005D5 / 0.32D2 * exp(-(-dble(s) + ri) ** 2 
     #/ delta ** 2) * delta ** 10 * ri * dble(s ** 3) + 0.10185D5 / 0.8D
     #1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 8 * ri ** 5
     # * dble(s) + 0.1995D4 / 0.2D1 * exp(-(-dble(s) + ri) ** 2 / delta 
     #** 2) * delta ** 8 * ri ** 4 * dble(s ** 2) + 0.2805D4 / 0.4D1 * e
     #xp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 8 * ri ** 3 * db
     #le(s ** 3) + 0.6795D4 / 0.16D2 * exp(-(-dble(s) + ri) ** 2 / delta
     # ** 2) * delta ** 8 * ri ** 2 * dble(s ** 4) + 0.3249D4 / 0.16D2 *
     # exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 8 * ri * dble(
     #s ** 5) - 0.19D2 / 0.4D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2)
     # * delta ** 4 * ri * dble(s ** 7) - exp(-(-dble(s) + ri) ** 2 / de
     #lta ** 2) * delta ** 2 * ri ** 9 * dble(s) / 0.2D1 - exp(-(-dble(s
     #) + ri) ** 2 / delta ** 2) * delta ** 2 * ri ** 8 * dble(s ** 2) /
     # 0.2D1 - exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 2 * ri
     # ** 7 * dble(s ** 3) / 0.2D1 - exp(-(-dble(s) + ri) ** 2 / delta *
     #* 2) * delta ** 2 * ri ** 6 * dble(s ** 4) / 0.2D1 - exp(-(-dble(s
     #) + ri) ** 2 / delta ** 2) * delta ** 2 * ri ** 5 * dble(s ** 5) /
     # 0.2D1 + exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 2 * ri
     # ** 10 * dble(s ** 2) / 0.2D1 - 0.6555D4 / 0.32D2 * exp(-(-dble(s)
     # + ri) ** 2 / delta ** 2) * delta ** 10 * ri * dble(s) - 0.585D3 /
     # 0.2D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 8 * ri
     # ** 3 * dble(s) + exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta 
     #** 2 * ri ** 9 * dble(s ** 3) / 0.2D1 + exp(-(-dble(s) + ri) ** 2 
     #/ delta ** 2) * delta ** 2 * ri ** 8 * dble(s ** 4) / 0.2D1 + exp(
     #-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 2 * ri ** 7 * dble(
     #s ** 5) / 0.2D1 + exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta 
     #** 2 * ri ** 6 * dble(s ** 6) / 0.2D1 + exp(-(-dble(s) + ri) ** 2 
     #/ delta ** 2) * delta ** 2 * ri ** 5 * dble(s ** 7) / 0.2D1 + exp(
     #-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 2 * ri ** 4 * dble(
     #s ** 8) / 0.2D1 + exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta 
     #** 2 * ri ** 3 * dble(s ** 9) / 0.2D1 + exp(-(-dble(s) + ri) ** 2 
     #/ delta ** 2) * delta ** 2 * ri ** 2 * dble(s ** 10) / 0.2D1 + exp
     #(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 2 * ri * dble(s **
     # 11) / 0.2D1 + 0.963D3 / 0.4D1 * exp(-(-dble(s) + ri) ** 2 / delta
     # ** 2) * delta ** 6 * ri ** 7 * dble(s) + 0.861D3 / 0.4D1 * exp(-(
     #-dble(s) + ri) ** 2 / delta ** 2) * delta ** 6 * ri ** 6 * dble(s 
     #** 2) + 0.735D3 / 0.4D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) 
     #* delta ** 6 * ri ** 5 * dble(s ** 3) + 0.1185D4 / 0.8D1 * exp(-(-
     #dble(s) + ri) ** 2 / delta ** 2) * delta ** 6 * ri ** 4 * dble(s *
     #* 4) + 0.885D3 / 0.8D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) *
     # delta ** 6 * ri ** 3 * dble(s ** 5) + 0.591D3 / 0.8D1 * exp(-(-db
     #le(s) + ri) ** 2 / delta ** 2) * delta ** 6 * ri ** 2 * dble(s ** 
     #6) + 0.327D3 / 0.8D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * d
     #elta ** 6 * ri * dble(s ** 7) - 0.45D2 / 0.4D1 * exp(-(-dble(s) + 
     #ri) ** 2 / delta ** 2) * delta ** 4 * ri ** 5 * dble(s ** 3) + 0.1
     #7D2 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 4 * ri **
     # 7 * dble(s ** 3) + 0.63D2 / 0.4D1 * exp(-(-dble(s) + ri) ** 2 / d
     #elta ** 2) * delta ** 4 * ri ** 6 * dble(s ** 4) + 0.57D2 / 0.4D1 
     #* exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 4 * ri ** 5 *
     # dble(s ** 5) + 0.25D2 / 0.2D1 * exp(-(-dble(s) + ri) ** 2 / delta
     # ** 2) * delta ** 4 * ri ** 4 * dble(s ** 6) + 0.21D2 / 0.2D1 * ex
     #p(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 4 * ri ** 3 * dbl
     #e(s ** 7) + 0.33D2 / 0.4D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 
     #2) * delta ** 4 * ri ** 2 * dble(s ** 8) + 0.23D2 / 0.4D1 * exp(-(
     #-dble(s) + ri) ** 2 / delta ** 2) * delta ** 4 * ri * dble(s ** 9)
     # + exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 2 * ri ** 11
     # * dble(s) / 0.2D1
      CASE(11)
      cg = 0.11D2 / 0.1680D4 * sqrt(dble(-s ** 2 + 1)) * c * dble(s ** 9
     #) - 0.278019D6 / 0.32D2 * ri ** 5 * delta ** 10 * exp(-0.1D1 / del
     #ta ** 2 * ri ** 2) - ri ** 13 * delta ** 2 * exp(-0.1D1 / delta **
     # 2 * ri ** 2) / 0.2D1 - 0.45D2 / 0.2D1 * ri ** 11 * delta ** 4 * e
     #xp(-0.1D1 / delta ** 2 * ri ** 2) - 0.2915D4 / 0.8D1 * ri ** 9 * d
     #elta ** 6 * exp(-0.1D1 / delta ** 2 * ri ** 2) - 0.10575D5 / 0.4D1
     # * ri ** 7 * delta ** 8 * exp(-0.1D1 / delta ** 2 * ri ** 2) - 0.3
     #64665D6 / 0.32D2 * ri ** 3 * delta ** 12 * exp(-0.1D1 / delta ** 2
     # * ri ** 2) - 0.509985D6 / 0.128D3 * ri * delta ** 14 * exp(-0.1D1
     # / delta ** 2 * ri ** 2) + sqrt(dble(-s ** 2 + 1)) * c * dble(s **
     # 11) / 0.168D3 + 0.35685D5 / 0.64D2 * ri * delta ** 12 * exp(-0.1D
     #1 / delta ** 2 * ri ** 2) + 0.41685D5 / 0.32D2 * ri ** 3 * delta *
     #* 10 * exp(-0.1D1 / delta ** 2 * ri ** 2) + 0.6279D4 / 0.8D1 * ri 
     #** 5 * delta ** 8 * exp(-0.1D1 / delta ** 2 * ri ** 2) + ri ** 11 
     #* delta ** 2 * exp(-0.1D1 / delta ** 2 * ri ** 2) / 0.2D1 + 0.65D2
     # / 0.4D1 * ri ** 9 * delta ** 4 * exp(-0.1D1 / delta ** 2 * ri ** 
     #2) + 0.711D3 / 0.4D1 * ri ** 7 * delta ** 6 * exp(-0.1D1 / delta *
     #* 2 * ri ** 2) + 0.10395D5 / 0.128D3 * delta ** 13 * sqrt(0.314159
     #265358979323846264338328D1) * erf(0.1D1 / delta * ri) + 0.31185D5 
     #/ 0.32D2 * ri ** 2 * delta ** 11 * sqrt(0.314159265358979323846264
     #338328D1) * erf(0.1D1 / delta * ri) + 0.51975D5 / 0.32D2 * ri ** 4
     # * delta ** 9 * sqrt(0.314159265358979323846264338328D1) * erf(0.1
     #D1 / delta * ri) + delta * ri ** 12 * sqrt(0.314159265358979323846
     #264338328D1) * erf(0.1D1 / delta * ri) / 0.2D1 + 0.33D2 / 0.2D1 * 
     #ri ** 10 * delta ** 3 * sqrt(0.314159265358979323846264338328D1) *
     # erf(0.1D1 / delta * ri) + 0.1485D4 / 0.8D1 * ri ** 8 * delta ** 5
     # * sqrt(0.314159265358979323846264338328D1) * erf(0.1D1 / delta * 
     #ri) + 0.3465D4 / 0.4D1 * ri ** 6 * delta ** 7 * sqrt(0.31415926535
     #8979323846264338328D1) * erf(0.1D1 / delta * ri) + 0.945945D6 / 0.
     #128D3 * sqrt(0.314159265358979323846264338328D1) * erf((-dble(s) +
     # ri) / delta) * delta ** 13 * ri ** 2 + 0.945945D6 / 0.64D2 * sqrt
     #(0.314159265358979323846264338328D1) * erf((-dble(s) + ri) / delta
     #) * delta ** 11 * ri ** 4 + 0.315315D6 / 0.32D2 * sqrt(0.314159265
     #358979323846264338328D1) * erf((-dble(s) + ri) / delta) * delta **
     # 9 * ri ** 6 + 0.45045D5 / 0.16D2 * sqrt(0.31415926535897932384626
     #4338328D1) * erf((-dble(s) + ri) / delta) * delta ** 7 * ri ** 8 +
     # 0.3003D4 / 0.8D1 * sqrt(0.314159265358979323846264338328D1) * erf
     #((-dble(s) + ri) / delta) * delta ** 5 * ri ** 10 + 0.91D2 / 0.4D1
     # * sqrt(0.314159265358979323846264338328D1) * erf((-dble(s) + ri) 
     #/ delta) * delta ** 3 * ri ** 12 + sqrt(0.314159265358979323846264
     #338328D1) * erf((-dble(s) + ri) / delta) * delta * ri ** 14 / 0.2D
     #1 - 0.31185D5 / 0.32D2 * ri ** 2 * delta ** 11 * sqrt(0.3141592653
     #58979323846264338328D1) * erf((-dble(s) + ri) / delta) - 0.51975D5
     # / 0.32D2 * ri ** 4 * delta ** 9 * sqrt(0.314159265358979323846264
     #338328D1) * erf((-dble(s) + ri) / delta) - 0.3465D4 / 0.4D1 * ri *
     #* 6 * delta ** 7 * sqrt(0.314159265358979323846264338328D1) * erf(
     #(-dble(s) + ri) / delta) - 0.1485D4 / 0.8D1 * ri ** 8 * delta ** 5
     # * sqrt(0.314159265358979323846264338328D1) * erf((-dble(s) + ri) 
     #/ delta) - 0.33D2 / 0.2D1 * ri ** 10 * delta ** 3 * sqrt(0.3141592
     #65358979323846264338328D1) * erf((-dble(s) + ri) / delta) - delta 
     #* ri ** 12 * sqrt(0.314159265358979323846264338328D1) * erf((-dble
     #(s) + ri) / delta) / 0.2D1 + exp(-(-dble(s) + ri) ** 2 / delta ** 
     #2) * delta ** 2 * ri ** 6 * dble(s ** 7) / 0.2D1 + exp(-(-dble(s) 
     #+ ri) ** 2 / delta ** 2) * delta ** 2 * ri ** 5 * dble(s ** 8) / 0
     #.2D1 + 0.19D2 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta **
     # 4 * ri ** 7 * dble(s ** 4) + 0.35D2 / 0.2D1 * exp(-(-dble(s) + ri
     #) ** 2 / delta ** 2) * delta ** 4 * ri ** 6 * dble(s ** 5) + 0.63D
     #2 / 0.4D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 4 *
     # ri ** 5 * dble(s ** 6) + 0.55D2 / 0.4D1 * exp(-(-dble(s) + ri) **
     # 2 / delta ** 2) * delta ** 4 * ri ** 4 * dble(s ** 7) + 0.23D2 / 
     #0.2D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 4 * ri 
     #** 3 * dble(s ** 8) + 0.9D1 * exp(-(-dble(s) + ri) ** 2 / delta **
     # 2) * delta ** 4 * ri ** 2 * dble(s ** 9) + 0.1099D4 / 0.4D1 * exp
     #(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 6 * ri ** 6 * dble
     #(s ** 3) + 0.231D3 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * del
     #ta ** 6 * ri ** 5 * dble(s ** 4) + 0.735D3 / 0.4D1 * exp(-(-dble(s
     #) + ri) ** 2 / delta ** 2) * delta ** 6 * ri ** 4 * dble(s ** 5) +
     # 0.1085D4 / 0.8D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delt
     #a ** 6 * ri ** 3 * dble(s ** 6) + 0.717D3 / 0.8D1 * exp(-(-dble(s)
     # + ri) ** 2 / delta ** 2) * delta ** 6 * ri ** 2 * dble(s ** 7) + 
     #0.393D3 / 0.8D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta 
     #** 6 * ri * dble(s ** 8) + 0.22D2 * exp(-(-dble(s) + ri) ** 2 / de
     #lta ** 2) * delta ** 4 * ri ** 10 * dble(s) + 0.85D2 / 0.4D1 * exp
     #(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 4 * ri ** 9 * dble
     #(s ** 2) + 0.81D2 / 0.4D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2
     #) * delta ** 4 * ri ** 8 * dble(s ** 3) + 0.82845D5 / 0.32D2 * exp
     #(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 10 * ri ** 2 * dbl
     #e(s ** 3) + 0.35595D5 / 0.32D2 * exp(-(-dble(s) + ri) ** 2 / delta
     # ** 2) * delta ** 10 * ri * dble(s ** 4) + delta ** 2 * dble(s ** 
     #13) * exp(-(-dble(s) + ri) ** 2 / delta ** 2) / 0.2D1 + 0.2331D4 *
     # exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 8 * ri ** 6 * 
     #dble(s) + 0.15351D5 / 0.8D1 * exp(-(-dble(s) + ri) ** 2 / delta **
     # 2) * delta ** 8 * ri ** 5 * dble(s ** 2) + 0.11655D5 / 0.8D1 * ex
     #p(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 8 * ri ** 4 * dbl
     #e(s ** 3) + 0.1995D4 / 0.2D1 * exp(-(-dble(s) + ri) ** 2 / delta *
     #* 2) * delta ** 8 * ri ** 3 * dble(s ** 4) + 0.4725D4 / 0.8D1 * ex
     #p(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 8 * ri ** 2 * dbl
     #e(s ** 5) + 0.4431D4 / 0.16D2 * exp(-(-dble(s) + ri) ** 2 / delta 
     #** 2) * delta ** 8 * ri * dble(s ** 6) + 0.2745D4 / 0.8D1 * exp(-(
     #-dble(s) + ri) ** 2 / delta ** 2) * delta ** 6 * ri ** 8 * dble(s)
     # + 0.1251D4 / 0.4D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * de
     #lta ** 6 * ri ** 7 * dble(s ** 2) - exp(-(-dble(s) + ri) ** 2 / de
     #lta ** 2) * delta ** 2 * ri ** 6 * dble(s ** 5) / 0.2D1 - exp(-(-d
     #ble(s) + ri) ** 2 / delta ** 2) * delta ** 2 * ri ** 5 * dble(s **
     # 6) / 0.2D1 - exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 2
     # * ri ** 4 * dble(s ** 7) / 0.2D1 - exp(-(-dble(s) + ri) ** 2 / de
     #lta ** 2) * delta ** 2 * ri ** 3 * dble(s ** 8) / 0.2D1 - exp(-(-d
     #ble(s) + ri) ** 2 / delta ** 2) * delta ** 2 * ri ** 2 * dble(s **
     # 9) / 0.2D1 - exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 2
     # * ri * dble(s ** 10) / 0.2D1 + 0.54495D5 / 0.8D1 * exp(-(-dble(s)
     # + ri) ** 2 / delta ** 2) * delta ** 12 * ri ** 2 * dble(s) + 0.18
     #7425D6 / 0.64D2 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta 
     #** 12 * ri * dble(s ** 2) + 0.216615D6 / 0.32D2 * exp(-(-dble(s) +
     # ri) ** 2 / delta ** 2) * delta ** 10 * ri ** 4 * dble(s) + 0.1466
     #85D6 / 0.32D2 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta **
     # 10 * ri ** 3 * dble(s ** 2) - 0.41685D5 / 0.32D2 * exp(-(-dble(s)
     # + ri) ** 2 / delta ** 2) * ri ** 3 * delta ** 10 - delta ** 2 * d
     #ble(s ** 11) * exp(-(-dble(s) + ri) ** 2 / delta ** 2) / 0.2D1 - 0
     #.3465D4 / 0.32D2 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta
     # ** 10 * dble(s ** 3) - 0.693D3 / 0.16D2 * exp(-(-dble(s) + ri) **
     # 2 / delta ** 2) * delta ** 8 * dble(s ** 5) - exp(-(-dble(s) + ri
     #) ** 2 / delta ** 2) * ri ** 11 * delta ** 2 / 0.2D1 + 0.135135D6 
     #/ 0.128D3 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 14 
     #* dble(s) - 0.10395D5 / 0.64D2 * exp(-(-dble(s) + ri) ** 2 / delta
     # ** 2) * delta ** 12 * dble(s) - 0.711D3 / 0.4D1 * exp(-(-dble(s) 
     #+ ri) ** 2 / delta ** 2) * ri ** 7 * delta ** 6 + 0.143D3 / 0.8D1 
     #* exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 6 * dble(s **
     # 9) + 0.13D2 / 0.4D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * d
     #elta ** 4 * dble(s ** 11) + 0.10575D5 / 0.4D1 * exp(-(-dble(s) + r
     #i) ** 2 / delta ** 2) * ri ** 7 * delta ** 8 + 0.45045D5 / 0.64D2 
     #* exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 12 * dble(s *
     #* 3) + 0.9009D4 / 0.32D2 * exp(-(-dble(s) + ri) ** 2 / delta ** 2)
     # * delta ** 10 * dble(s ** 5) + 0.1287D4 / 0.16D2 * exp(-(-dble(s)
     # + ri) ** 2 / delta ** 2) * delta ** 8 * dble(s ** 7) - 0.99D2 / 0
     #.8D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 6 * dble
     #(s ** 7) - 0.11D2 / 0.4D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2
     #) * delta ** 4 * dble(s ** 9) + 0.509985D6 / 0.128D3 * exp(-(-dble
     #(s) + ri) ** 2 / delta ** 2) * ri * delta ** 14 + 0.278019D6 / 0.3
     #2D2 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * ri ** 5 * delta **
     # 10 - 0.35685D5 / 0.64D2 * exp(-(-dble(s) + ri) ** 2 / delta ** 2)
     # * ri * delta ** 12 + exp(-(-dble(s) + ri) ** 2 / delta ** 2) * ri
     # ** 13 * delta ** 2 / 0.2D1 + 0.364665D6 / 0.32D2 * exp(-(-dble(s)
     # + ri) ** 2 / delta ** 2) * ri ** 3 * delta ** 12 + 0.45D2 / 0.2D1
     # * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * ri ** 11 * delta ** 4
     # + 0.2915D4 / 0.8D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * ri
     # ** 9 * delta ** 6 + 0.33D2 / 0.4480D4 * sqrt(dble(-s ** 2 + 1)) *
     # c * dble(s ** 7) - 0.65D2 / 0.4D1 * exp(-(-dble(s) + ri) ** 2 / d
     #elta ** 2) * ri ** 9 * delta ** 4 - 0.6279D4 / 0.8D1 * exp(-(-dble
     #(s) + ri) ** 2 / delta ** 2) * ri ** 5 * delta ** 8 - 0.135135D6 /
     # 0.256D3 * delta ** 15 * sqrt(0.314159265358979323846264338328D1) 
     #* erf(0.1D1 / delta * ri) - 0.2295D4 / 0.16D2 * exp(-(-dble(s) + r
     #i) ** 2 / delta ** 2) * delta ** 8 * ri * dble(s ** 4) - 0.651D3 /
     # 0.4D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 6 * ri
     # ** 6 * dble(s) - 0.567D3 / 0.4D1 * exp(-(-dble(s) + ri) ** 2 / de
     #lta ** 2) * delta ** 6 * ri ** 5 * dble(s ** 2) - 0.465D3 / 0.4D1 
     #* exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 6 * ri ** 4 *
     # dble(s ** 3) - 0.705D3 / 0.8D1 * exp(-(-dble(s) + ri) ** 2 / delt
     #a ** 2) * delta ** 6 * ri ** 3 * dble(s ** 4) - 0.477D3 / 0.8D1 * 
     #exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 6 * ri ** 2 * d
     #ble(s ** 5) - 0.267D3 / 0.8D1 * exp(-(-dble(s) + ri) ** 2 / delta 
     #** 2) * delta ** 6 * ri * dble(s ** 6) - exp(-(-dble(s) + ri) ** 2
     # / delta ** 2) * delta ** 2 * ri ** 8 * dble(s ** 3) / 0.2D1 - exp
     #(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 2 * ri ** 7 * dble
     #(s ** 4) / 0.2D1 - 0.15D2 * exp(-(-dble(s) + ri) ** 2 / delta ** 2
     #) * delta ** 4 * ri ** 7 * dble(s ** 2) - 0.14D2 * exp(-(-dble(s) 
     #+ ri) ** 2 / delta ** 2) * delta ** 4 * ri ** 6 * dble(s ** 3) - 0
     #.51D2 / 0.4D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta **
     # 4 * ri ** 5 * dble(s ** 4) - 0.45D2 / 0.4D1 * exp(-(-dble(s) + ri
     #) ** 2 / delta ** 2) * delta ** 4 * ri ** 4 * dble(s ** 5) - 0.19D
     #2 / 0.2D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 4 *
     # ri ** 3 * dble(s ** 6) - 0.15D2 / 0.2D1 * exp(-(-dble(s) + ri) **
     # 2 / delta ** 2) * delta ** 4 * ri ** 2 * dble(s ** 7) - 0.585D3 /
     # 0.2D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 8 * ri
     # ** 2 * dble(s ** 3) + exp(-(-dble(s) + ri) ** 2 / delta ** 2) * d
     #elta ** 2 * ri ** 4 * dble(s ** 9) / 0.2D1 + exp(-(-dble(s) + ri) 
     #** 2 / delta ** 2) * delta ** 2 * ri ** 3 * dble(s ** 10) / 0.2D1 
     #+ exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 2 * ri ** 2 *
     # dble(s ** 11) / 0.2D1 + exp(-(-dble(s) + ri) ** 2 / delta ** 2) *
     # delta ** 2 * ri * dble(s ** 12) / 0.2D1 - 0.26685D5 / 0.32D2 * ex
     #p(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 10 * ri ** 2 * db
     #le(s) - 0.12645D5 / 0.32D2 * exp(-(-dble(s) + ri) ** 2 / delta ** 
     #2) * delta ** 10 * ri * dble(s ** 2) - 0.5145D4 / 0.8D1 * exp(-(-d
     #ble(s) + ri) ** 2 / delta ** 2) * delta ** 8 * ri ** 4 * dble(s) -
     # 0.1875D4 / 0.4D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delt
     #a ** 8 * ri ** 3 * dble(s ** 2) - 0.63D2 / 0.4D1 * exp(-(-dble(s) 
     #+ ri) ** 2 / delta ** 2) * delta ** 4 * ri ** 8 * dble(s) + 0.25D2
     # / 0.4D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 4 * 
     #ri * dble(s ** 10) + exp(-(-dble(s) + ri) ** 2 / delta ** 2) * del
     #ta ** 2 * ri ** 12 * dble(s) / 0.2D1 + exp(-(-dble(s) + ri) ** 2 /
     # delta ** 2) * delta ** 2 * ri ** 11 * dble(s ** 2) / 0.2D1 - 0.33
     #D2 / 0.2048D4 * asin(dble(s)) * c - 0.21D2 / 0.4D1 * exp(-(-dble(s
     #) + ri) ** 2 / delta ** 2) * delta ** 4 * ri * dble(s ** 8) - exp(
     #-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 2 * ri ** 10 * dble
     #(s) / 0.2D1 - exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 2
     # * ri ** 9 * dble(s ** 2) / 0.2D1 + exp(-(-dble(s) + ri) ** 2 / de
     #lta ** 2) * delta ** 2 * ri ** 10 * dble(s ** 3) / 0.2D1 + exp(-(-
     #dble(s) + ri) ** 2 / delta ** 2) * delta ** 2 * ri ** 9 * dble(s *
     #* 4) / 0.2D1 + exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 
     #2 * ri ** 8 * dble(s ** 5) / 0.2D1 + exp(-(-dble(s) + ri) ** 2 / d
     #elta ** 2) * delta ** 2 * ri ** 7 * dble(s ** 6) / 0.2D1 + 0.13513
     #5D6 / 0.256D3 * sqrt(0.314159265358979323846264338328D1) * erf((-d
     #ble(s) + ri) / delta) * delta ** 15 - 0.10395D5 / 0.128D3 * delta 
     #** 13 * sqrt(0.314159265358979323846264338328D1) * erf((-dble(s) +
     # ri) / delta) + 0.33D2 / 0.2048D4 * sqrt(dble(-s ** 2 + 1)) * c * 
     #dble(s) - delta * ri ** 14 * sqrt(0.314159265358979323846264338328
     #D1) * erf(0.1D1 / delta * ri) / 0.2D1 - 0.91D2 / 0.4D1 * ri ** 12 
     #* delta ** 3 * sqrt(0.314159265358979323846264338328D1) * erf(0.1D
     #1 / delta * ri) - 0.3003D4 / 0.8D1 * ri ** 10 * delta ** 5 * sqrt(
     #0.314159265358979323846264338328D1) * erf(0.1D1 / delta * ri) - 0.
     #45045D5 / 0.16D2 * ri ** 8 * delta ** 7 * sqrt(0.31415926535897932
     #3846264338328D1) * erf(0.1D1 / delta * ri) - 0.315315D6 / 0.32D2 *
     # ri ** 6 * delta ** 9 * sqrt(0.314159265358979323846264338328D1) *
     # erf(0.1D1 / delta * ri) - 0.945945D6 / 0.64D2 * ri ** 4 * delta *
     #* 11 * sqrt(0.314159265358979323846264338328D1) * erf(0.1D1 / delt
     #a * ri) - 0.945945D6 / 0.128D3 * ri ** 2 * delta ** 13 * sqrt(0.31
     #4159265358979323846264338328D1) * erf(0.1D1 / delta * ri) - c * db
     #le(s ** 13) * sqrt(dble(-s ** 2 + 1)) / 0.14D2 + 0.11D2 / 0.1024D4
     # * sqrt(dble(-s ** 2 + 1)) * c * dble(s ** 3) + 0.11D2 / 0.1280D4 
     #* sqrt(dble(-s ** 2 + 1)) * c * dble(s ** 5)
      CASE(12)
      cg = -0.1024D4 / 0.45045D5 * c + 0.114765D6 / 0.32D2 * ri ** 4 * d
     #elta ** 10 * exp(-0.1D1 / delta ** 2 * ri ** 2) + 0.1035D4 / 0.4D1
     # * ri ** 8 * delta ** 6 * exp(-0.1D1 / delta ** 2 * ri ** 2) + ri 
     #** 12 * delta ** 2 * exp(-0.1D1 / delta ** 2 * ri ** 2) / 0.2D1 + 
     #0.77D2 / 0.4D1 * ri ** 10 * delta ** 4 * exp(-0.1D1 / delta ** 2 *
     # ri ** 2) + 0.187425D6 / 0.64D2 * ri ** 2 * delta ** 12 * exp(-0.1
     #D1 / delta ** 2 * ri ** 2) + 0.11907D5 / 0.8D1 * ri ** 6 * delta *
     #* 8 * exp(-0.1D1 / delta ** 2 * ri ** 2) - c * dble(s ** 14) * sqr
     #t(dble(-s ** 2 + 1)) / 0.15D2 + sqrt(dble(-s ** 2 + 1)) * c * dble
     #(s ** 12) / 0.195D3 - 0.2520D4 * delta ** 16 * exp(-0.1D1 / delta 
     #** 2 * ri ** 2) + 0.876015D6 / 0.64D2 * exp(-(-dble(s) + ri) ** 2 
     #/ delta ** 2) * delta ** 12 * ri ** 2 * dble(s ** 2) + 0.353115D6 
     #/ 0.64D2 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 12 *
     # ri * dble(s ** 3) - 0.17D2 * exp(-(-dble(s) + ri) ** 2 / delta **
     # 2) * delta ** 4 * ri ** 7 * dble(s ** 3) - 0.63D2 / 0.4D1 * exp(-
     #(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 4 * ri ** 6 * dble(s
     # ** 4) - 0.57D2 / 0.4D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) 
     #* delta ** 4 * ri ** 5 * dble(s ** 5) - 0.25D2 / 0.2D1 * exp(-(-db
     #le(s) + ri) ** 2 / delta ** 2) * delta ** 4 * ri ** 4 * dble(s ** 
     #6) - 0.21D2 / 0.2D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * de
     #lta ** 4 * ri ** 3 * dble(s ** 7) + 0.501795D6 / 0.32D2 * exp(-(-d
     #ble(s) + ri) ** 2 / delta ** 2) * delta ** 10 * ri ** 5 * dble(s) 
     #+ 0.370125D6 / 0.32D2 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * 
     #delta ** 10 * ri ** 4 * dble(s ** 2) + 0.239925D6 / 0.32D2 * exp(-
     #(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 10 * ri ** 3 * dble(
     #s ** 3) + 0.130725D6 / 0.32D2 * exp(-(-dble(s) + ri) ** 2 / delta 
     #** 2) * delta ** 10 * ri ** 2 * dble(s ** 4) + 0.54495D5 / 0.32D2 
     #* exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 10 * ri * dbl
     #e(s ** 5) + 0.16065D5 / 0.4D1 * exp(-(-dble(s) + ri) ** 2 / delta 
     #** 2) * delta ** 8 * ri ** 7 * dble(s) + 0.27405D5 / 0.8D1 * exp(-
     #(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 8 * ri ** 6 * dble(s
     # ** 2) + 0.21945D5 / 0.8D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 
     #2) * delta ** 8 * ri ** 5 * dble(s ** 3) + 0.16275D5 / 0.8D1 * exp
     #(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 8 * ri ** 4 * dble
     #(s ** 4) + 0.1365D4 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * de
     #lta ** 8 * ri ** 3 * dble(s ** 5) + 0.12705D5 / 0.16D2 * exp(-(-db
     #le(s) + ri) ** 2 / delta ** 2) * delta ** 8 * ri ** 2 * dble(s ** 
     #6) + 0.5865D4 / 0.16D2 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) *
     # delta ** 8 * ri * dble(s ** 7) + 0.1024D4 / 0.45045D5 * c * sqrt(
     #dble(-s ** 2 + 1)) - ri ** 14 * delta ** 2 * exp(-0.1D1 / delta **
     # 2 * ri ** 2) / 0.2D1 - 0.26D2 * ri ** 12 * delta ** 4 * exp(-0.1D
     #1 / delta ** 2 * ri ** 2) - 0.3993D4 / 0.8D1 * ri ** 10 * delta **
     # 6 * exp(-0.1D1 / delta ** 2 * ri ** 2) - 0.4455D4 * ri ** 8 * del
     #ta ** 8 * exp(-0.1D1 / delta ** 2 * ri ** 2) - 0.611415D6 / 0.32D2
     # * ri ** 6 * delta ** 10 * exp(-0.1D1 / delta ** 2 * ri ** 2) - 0.
     #292005D6 / 0.8D1 * ri ** 4 * delta ** 12 * exp(-0.1D1 / delta ** 2
     # * ri ** 2) - 0.3133935D7 / 0.128D3 * ri ** 2 * delta ** 14 * exp(
     #-0.1D1 / delta ** 2 * ri ** 2) - 0.15D2 * exp(-(-dble(s) + ri) ** 
     #2 / delta ** 2) * delta ** 6 * dble(s ** 8) - 0.3D1 * exp(-(-dble(
     #s) + ri) ** 2 / delta ** 2) * delta ** 4 * dble(s ** 10) - 0.18742
     #5D6 / 0.64D2 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * ri ** 2 *
     # delta ** 12 + 0.2520D4 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) 
     #* delta ** 14 * dble(s ** 2) + 0.1260D4 * exp(-(-dble(s) + ri) ** 
     #2 / delta ** 2) * delta ** 12 * dble(s ** 4) - 0.77D2 / 0.4D1 * ex
     #p(-(-dble(s) + ri) ** 2 / delta ** 2) * ri ** 10 * delta ** 4 + 0.
     #420D3 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 10 * db
     #le(s ** 6) + 0.105D3 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * d
     #elta ** 8 * dble(s ** 8) + 0.21D2 * exp(-(-dble(s) + ri) ** 2 / de
     #lta ** 2) * delta ** 6 * dble(s ** 10) + 0.4455D4 * exp(-(-dble(s)
     # + ri) ** 2 / delta ** 2) * ri ** 8 * delta ** 8 + 0.611415D6 / 0.
     #32D2 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * ri ** 6 * delta *
     #* 10 + 0.292005D6 / 0.8D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2
     #) * ri ** 4 * delta ** 12 + 0.26D2 * exp(-(-dble(s) + ri) ** 2 / d
     #elta ** 2) * ri ** 12 * delta ** 4 + 0.3993D4 / 0.8D1 * exp(-(-dbl
     #e(s) + ri) ** 2 / delta ** 2) * ri ** 10 * delta ** 6 + 0.360D3 * 
     #delta ** 14 * exp(-0.1D1 / delta ** 2 * ri ** 2) + 0.4D1 / 0.715D3
     # * sqrt(dble(-s ** 2 + 1)) * c * dble(s ** 10) + 0.8D1 / 0.1287D4 
     #* sqrt(dble(-s ** 2 + 1)) * c * dble(s ** 8) + 0.64D2 / 0.9009D4 *
     # sqrt(dble(-s ** 2 + 1)) * c * dble(s ** 6) + 0.128D3 / 0.15015D5 
     #* sqrt(dble(-s ** 2 + 1)) * c * dble(s ** 4) + 0.512D3 / 0.45045D5
     # * sqrt(dble(-s ** 2 + 1)) * c * dble(s ** 2) - delta * ri ** 15 *
     # sqrt(0.314159265358979323846264338328D1) * erf(0.1D1 / delta * ri
     #) / 0.2D1 - 0.105D3 / 0.4D1 * ri ** 13 * delta ** 3 * sqrt(0.31415
     #9265358979323846264338328D1) * erf(0.1D1 / delta * ri) - 0.4095D4 
     #/ 0.8D1 * ri ** 11 * delta ** 5 * sqrt(0.3141592653589793238462643
     #38328D1) * erf(0.1D1 / delta * ri) - 0.75075D5 / 0.16D2 * ri ** 9 
     #* delta ** 7 * sqrt(0.314159265358979323846264338328D1) * erf(0.1D
     #1 / delta * ri) - 0.2027025D7 / 0.256D3 * ri * delta ** 15 * sqrt(
     #0.314159265358979323846264338328D1) * erf(0.1D1 / delta * ri) - 0.
     #4729725D7 / 0.128D3 * ri ** 3 * delta ** 13 * sqrt(0.3141592653589
     #79323846264338328D1) * erf(0.1D1 / delta * ri) - 0.2837835D7 / 0.6
     #4D2 * ri ** 5 * delta ** 11 * sqrt(0.31415926535897932384626433832
     #8D1) * erf(0.1D1 / delta * ri) - 0.675675D6 / 0.32D2 * ri ** 7 * d
     #elta ** 9 * sqrt(0.314159265358979323846264338328D1) * erf(0.1D1 /
     # delta * ri) + exp(-(-dble(s) + ri) ** 2 / delta ** 2) * ri ** 14 
     #* delta ** 2 / 0.2D1 + 0.3133935D7 / 0.128D3 * exp(-(-dble(s) + ri
     #) ** 2 / delta ** 2) * ri ** 2 * delta ** 14 - 0.1035D4 / 0.4D1 * 
     #exp(-(-dble(s) + ri) ** 2 / delta ** 2) * ri ** 8 * delta ** 6 - e
     #xp(-(-dble(s) + ri) ** 2 / delta ** 2) * ri ** 12 * delta ** 2 / 0
     #.2D1 + 0.7D1 / 0.2D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * d
     #elta ** 4 * dble(s ** 12) - 0.360D3 * exp(-(-dble(s) + ri) ** 2 / 
     #delta ** 2) * delta ** 12 * dble(s ** 2) - 0.180D3 * exp(-(-dble(s
     #) + ri) ** 2 / delta ** 2) * delta ** 10 * dble(s ** 4) - 0.114765
     #D6 / 0.32D2 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * ri ** 4 * 
     #delta ** 10 - 0.11907D5 / 0.8D1 * exp(-(-dble(s) + ri) ** 2 / delt
     #a ** 2) * ri ** 6 * delta ** 8 - 0.360D3 * exp(-(-dble(s) + ri) **
     # 2 / delta ** 2) * delta ** 14 + 0.2520D4 * exp(-(-dble(s) + ri) *
     #* 2 / delta ** 2) * delta ** 16 + delta ** 2 * dble(s ** 14) * exp
     #(-(-dble(s) + ri) ** 2 / delta ** 2) / 0.2D1 - delta ** 2 * dble(s
     # ** 12) * exp(-(-dble(s) + ri) ** 2 / delta ** 2) / 0.2D1 - 0.60D2
     # * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 8 * dble(s *
     #* 6) + 0.135135D6 / 0.32D2 * ri ** 5 * delta ** 9 * sqrt(0.3141592
     #65358979323846264338328D1) * erf(0.1D1 / delta * ri) + 0.135135D6 
     #/ 0.32D2 * ri ** 3 * delta ** 11 * sqrt(0.314159265358979323846264
     #338328D1) * erf(0.1D1 / delta * ri) + delta * ri ** 13 * sqrt(0.31
     #4159265358979323846264338328D1) * erf(0.1D1 / delta * ri) / 0.2D1 
     #+ 0.39D2 / 0.2D1 * ri ** 11 * delta ** 3 * sqrt(0.3141592653589793
     #23846264338328D1) * erf(0.1D1 / delta * ri) + 0.2145D4 / 0.8D1 * r
     #i ** 9 * delta ** 5 * sqrt(0.314159265358979323846264338328D1) * e
     #rf(0.1D1 / delta * ri) + 0.6435D4 / 0.4D1 * ri ** 7 * delta ** 7 *
     # sqrt(0.314159265358979323846264338328D1) * erf(0.1D1 / delta * ri
     #) + 0.135135D6 / 0.128D3 * ri * delta ** 13 * sqrt(0.3141592653589
     #79323846264338328D1) * erf(0.1D1 / delta * ri) + 0.2027025D7 / 0.2
     #56D3 * sqrt(0.314159265358979323846264338328D1) * erf((-dble(s) + 
     #ri) / delta) * delta ** 15 * ri + 0.4729725D7 / 0.128D3 * sqrt(0.3
     #14159265358979323846264338328D1) * erf((-dble(s) + ri) / delta) * 
     #delta ** 13 * ri ** 3 + 0.2837835D7 / 0.64D2 * sqrt(0.314159265358
     #979323846264338328D1) * erf((-dble(s) + ri) / delta) * delta ** 11
     # * ri ** 5 + 0.675675D6 / 0.32D2 * sqrt(0.314159265358979323846264
     #338328D1) * erf((-dble(s) + ri) / delta) * delta ** 9 * ri ** 7 + 
     #0.75075D5 / 0.16D2 * sqrt(0.314159265358979323846264338328D1) * er
     #f((-dble(s) + ri) / delta) * delta ** 7 * ri ** 9 + 0.4095D4 / 0.8
     #D1 * sqrt(0.314159265358979323846264338328D1) * erf((-dble(s) + ri
     #) / delta) * delta ** 5 * ri ** 11 + 0.105D3 / 0.4D1 * sqrt(0.3141
     #59265358979323846264338328D1) * erf((-dble(s) + ri) / delta) * del
     #ta ** 3 * ri ** 13 + sqrt(0.314159265358979323846264338328D1) * er
     #f((-dble(s) + ri) / delta) * delta * ri ** 15 / 0.2D1 - 0.135135D6
     # / 0.128D3 * ri * delta ** 13 * sqrt(0.314159265358979323846264338
     #328D1) * erf((-dble(s) + ri) / delta) - 0.135135D6 / 0.32D2 * ri *
     #* 3 * delta ** 11 * sqrt(0.314159265358979323846264338328D1) * erf
     #((-dble(s) + ri) / delta) - 0.135135D6 / 0.32D2 * ri ** 5 * delta 
     #** 9 * sqrt(0.314159265358979323846264338328D1) * erf((-dble(s) + 
     #ri) / delta) - 0.6435D4 / 0.4D1 * ri ** 7 * delta ** 7 * sqrt(0.31
     #4159265358979323846264338328D1) * erf((-dble(s) + ri) / delta) - 0
     #.2145D4 / 0.8D1 * ri ** 9 * delta ** 5 * sqrt(0.314159265358979323
     #846264338328D1) * erf((-dble(s) + ri) / delta) - 0.39D2 / 0.2D1 * 
     #ri ** 11 * delta ** 3 * sqrt(0.314159265358979323846264338328D1) *
     # erf((-dble(s) + ri) / delta) - delta * ri ** 13 * sqrt(0.31415926
     #5358979323846264338328D1) * erf((-dble(s) + ri) / delta) / 0.2D1 +
     # exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 2 * ri ** 4 * 
     #dble(s ** 10) / 0.2D1 + exp(-(-dble(s) + ri) ** 2 / delta ** 2) * 
     #delta ** 2 * ri ** 3 * dble(s ** 11) / 0.2D1 - 0.3249D4 / 0.16D2 *
     # exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 8 * ri * dble(
     #s ** 5) - 0.963D3 / 0.4D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2
     #) * delta ** 6 * ri ** 7 * dble(s) - 0.861D3 / 0.4D1 * exp(-(-dble
     #(s) + ri) ** 2 / delta ** 2) * delta ** 6 * ri ** 6 * dble(s ** 2)
     # - 0.735D3 / 0.4D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * del
     #ta ** 6 * ri ** 5 * dble(s ** 3) - 0.1185D4 / 0.8D1 * exp(-(-dble(
     #s) + ri) ** 2 / delta ** 2) * delta ** 6 * ri ** 4 * dble(s ** 4) 
     #- 0.885D3 / 0.8D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delt
     #a ** 6 * ri ** 3 * dble(s ** 5) - 0.591D3 / 0.8D1 * exp(-(-dble(s)
     # + ri) ** 2 / delta ** 2) * delta ** 6 * ri ** 2 * dble(s ** 6) - 
     #0.327D3 / 0.8D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta 
     #** 6 * ri * dble(s ** 7) + 0.3795D4 / 0.8D1 * exp(-(-dble(s) + ri)
     # ** 2 / delta ** 2) * delta ** 6 * ri ** 9 * dble(s) + 0.1755D4 / 
     #0.4D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 6 * ri 
     #** 8 * dble(s ** 2) + 0.1575D4 / 0.4D1 * exp(-(-dble(s) + ri) ** 2
     # / delta ** 2) * delta ** 6 * ri ** 7 * dble(s ** 3) + 0.1365D4 / 
     #0.4D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 6 * ri 
     #** 6 * dble(s ** 4) + 0.567D3 / 0.2D1 * exp(-(-dble(s) + ri) ** 2 
     #/ delta ** 2) * delta ** 6 * ri ** 5 * dble(s ** 5) + 0.69D2 / 0.4
     #D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 4 * ri ** 
     #5 * dble(s ** 7) + 0.15D2 * exp(-(-dble(s) + ri) ** 2 / delta ** 2
     #) * delta ** 4 * ri ** 4 * dble(s ** 8) + 0.25D2 / 0.2D1 * exp(-(-
     #dble(s) + ri) ** 2 / delta ** 2) * delta ** 4 * ri ** 3 * dble(s *
     #* 9) + 0.39D2 / 0.4D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * 
     #delta ** 4 * ri ** 2 * dble(s ** 10) + 0.27D2 / 0.4D1 * exp(-(-dbl
     #e(s) + ri) ** 2 / delta ** 2) * delta ** 4 * ri * dble(s ** 11) + 
     #exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 2 * ri ** 13 * 
     #dble(s) / 0.2D1 + exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta 
     #** 2 * ri ** 12 * dble(s ** 2) / 0.2D1 + exp(-(-dble(s) + ri) ** 2
     # / delta ** 2) * delta ** 2 * ri ** 11 * dble(s ** 3) / 0.2D1 + ex
     #p(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 2 * ri ** 10 * db
     #le(s ** 4) / 0.2D1 + exp(-(-dble(s) + ri) ** 2 / delta ** 2) * del
     #ta ** 2 * ri ** 9 * dble(s ** 5) / 0.2D1 + exp(-(-dble(s) + ri) **
     # 2 / delta ** 2) * delta ** 2 * ri ** 8 * dble(s ** 6) / 0.2D1 + e
     #xp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 2 * ri ** 7 * db
     #le(s ** 7) / 0.2D1 + exp(-(-dble(s) + ri) ** 2 / delta ** 2) * del
     #ta ** 2 * ri ** 6 * dble(s ** 8) / 0.2D1 + exp(-(-dble(s) + ri) **
     # 2 / delta ** 2) * delta ** 2 * ri ** 5 * dble(s ** 9) / 0.2D1 - 0
     #.75D2 / 0.4D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta **
     # 4 * ri ** 9 * dble(s) + exp(-(-dble(s) + ri) ** 2 / delta ** 2) *
     # delta ** 2 * ri ** 2 * dble(s ** 12) / 0.2D1 + exp(-(-dble(s) + r
     #i) ** 2 / delta ** 2) * delta ** 2 * ri * dble(s ** 13) / 0.2D1 - 
     #0.89055D5 / 0.64D2 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * del
     #ta ** 12 * ri * dble(s) - 0.82845D5 / 0.32D2 * exp(-(-dble(s) + ri
     #) ** 2 / delta ** 2) * delta ** 10 * ri ** 3 * dble(s) - 0.49185D5
     # / 0.32D2 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 10 
     #* ri ** 2 * dble(s ** 2) - 0.22005D5 / 0.32D2 * exp(-(-dble(s) + r
     #i) ** 2 / delta ** 2) * delta ** 10 * ri * dble(s ** 3) - 0.10185D
     #5 / 0.8D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 8 *
     # ri ** 5 * dble(s) - 0.1995D4 / 0.2D1 * exp(-(-dble(s) + ri) ** 2 
     #/ delta ** 2) * delta ** 8 * ri ** 4 * dble(s ** 2) - 0.2805D4 / 0
     #.4D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 8 * ri *
     #* 3 * dble(s ** 3) - 0.6795D4 / 0.16D2 * exp(-(-dble(s) + ri) ** 2
     # / delta ** 2) * delta ** 8 * ri ** 2 * dble(s ** 4) - exp(-(-dble
     #(s) + ri) ** 2 / delta ** 2) * delta ** 2 * ri ** 5 * dble(s ** 7)
     # / 0.2D1 - exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 2 * 
     #ri ** 4 * dble(s ** 8) / 0.2D1 - exp(-(-dble(s) + ri) ** 2 / delta
     # ** 2) * delta ** 2 * ri ** 3 * dble(s ** 9) / 0.2D1 - exp(-(-dble
     #(s) + ri) ** 2 / delta ** 2) * delta ** 2 * ri ** 2 * dble(s ** 10
     #) / 0.2D1 - exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 2 *
     # ri * dble(s ** 11) / 0.2D1 + 0.1785D4 / 0.8D1 * exp(-(-dble(s) + 
     #ri) ** 2 / delta ** 2) * delta ** 6 * ri ** 4 * dble(s ** 6) + 0.1
     #305D4 / 0.8D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta **
     # 6 * ri ** 3 * dble(s ** 7) + 0.855D3 / 0.8D1 * exp(-(-dble(s) + r
     #i) ** 2 / delta ** 2) * delta ** 6 * ri ** 2 * dble(s ** 8) + 0.46
     #5D3 / 0.8D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 6
     # * ri * dble(s ** 9) + 0.51D2 / 0.2D1 * exp(-(-dble(s) + ri) ** 2 
     #/ delta ** 2) * delta ** 4 * ri ** 11 * dble(s) + 0.99D2 / 0.4D1 *
     # exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 4 * ri ** 10 *
     # dble(s ** 2) + 0.95D2 / 0.4D1 * exp(-(-dble(s) + ri) ** 2 / delta
     # ** 2) * delta ** 4 * ri ** 9 * dble(s ** 3) + 0.45D2 / 0.2D1 * ex
     #p(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 4 * ri ** 8 * dbl
     #e(s ** 4) + 0.21D2 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * del
     #ta ** 4 * ri ** 7 * dble(s ** 5) + 0.77D2 / 0.4D1 * exp(-(-dble(s)
     # + ri) ** 2 / delta ** 2) * delta ** 4 * ri ** 6 * dble(s ** 6) - 
     #0.33D2 / 0.4D1 * exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta *
     #* 4 * ri ** 2 * dble(s ** 8) - 0.23D2 / 0.4D1 * exp(-(-dble(s) + r
     #i) ** 2 / delta ** 2) * delta ** 4 * ri * dble(s ** 9) - exp(-(-db
     #le(s) + ri) ** 2 / delta ** 2) * delta ** 2 * ri ** 11 * dble(s) /
     # 0.2D1 - exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 2 * ri
     # ** 10 * dble(s ** 2) / 0.2D1 - exp(-(-dble(s) + ri) ** 2 / delta 
     #** 2) * delta ** 2 * ri ** 9 * dble(s ** 3) / 0.2D1 - exp(-(-dble(
     #s) + ri) ** 2 / delta ** 2) * delta ** 2 * ri ** 8 * dble(s ** 4) 
     #/ 0.2D1 - exp(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 2 * r
     #i ** 7 * dble(s ** 5) / 0.2D1 - exp(-(-dble(s) + ri) ** 2 / delta 
     #** 2) * delta ** 2 * ri ** 6 * dble(s ** 6) / 0.2D1 - 0.18D2 * exp
     #(-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 4 * ri ** 8 * dble
     #(s ** 2) + 0.1381905D7 / 0.128D3 * exp(-(-dble(s) + ri) ** 2 / del
     #ta ** 2) * delta ** 14 * ri * dble(s) + 0.797895D6 / 0.32D2 * exp(
     #-(-dble(s) + ri) ** 2 / delta ** 2) * delta ** 12 * ri ** 3 * dble
     #(s)
      CASE DEFAULT
      PRINT*, 'FLOW NOT DEFINED FOR WAVENUMBER', m; STOP
      END SELECT
      get_integral = cg
      END FUNCTION GET_INTEGRAL
      END MODULE FLOW_INTEGRALS
