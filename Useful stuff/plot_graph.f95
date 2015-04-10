PROGRAM VIS01
winapp
library 'c:\TFG\simple.dll','c:\TFG\salflibc.dll'
INTEGER NX,NY
PARAMETER(NX=36,NY=40)
REAL Z(NX,NY)
!$$$$$$ OPEN(10,FILE= vis01.dat ,STATUS= OLD )
!$$$$$$ READ(10,*)Z
!$$$$$$ 
!$$$$$$ CLOSE(10)

do i=1,NX
	do j=1,NY
    	Z(i,j)=max(i,j)
    enddo
enddo

CALL VSNEW

CALL VSRG(Z,NX,NY)

CALL VSOUT


CALL ENDPLT

END