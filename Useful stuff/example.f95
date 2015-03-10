MODULE spwin ! Contains definition of values to pass to DDDATA
INTEGER,PARAMETER::SIMPLE_WINDOWS_WINDOW=1
INTEGER,PARAMETER::SIMPLE_WINDOWS_PRINTER=2
INTEGER,PARAMETER::SIMPLE_WINDOWS_DIB=4
INTEGER,PARAMETER::SIMPLE_WINDOWS_METAFILE=8
INTEGER,PARAMETER::SIMPLE_PERCENT=0
INTEGER,PARAMETER::SIMPLE_PIXELS=1
INTEGER,PARAMETER::SIMPLE_MM=2
INTEGER,PARAMETER::SIMPLE_WINDOWS_END_LIST=0
INTEGER,PARAMETER::SIMPLE_WINDOWS_SET_WINDOWNAME=6
INTEGER,PARAMETER::SIMPLE_WINDOWS_SET_HDC_DIM=7

TYPE SIMPLE_WINDOWS_DATA_STRUCT_T
  INTEGER :: iOpc ! Corresponds to C++ long int
  INTEGER :: iPtr ! Address of OpCode specific data
END TYPE SIMPLE_WINDOWS_DATA_STRUCT_T

TYPE SIMPLE_WINDOWS_HDC_DATA_T
  INTEGER :: hDC ! device context handle
  INTEGER :: iWidth, iHeight ! plotting area
END TYPE SIMPLE_WINDOWS_HDC_DATA_T

TYPE SIMPLE_WINDOWS_SIZE_T
  INTEGER :: iUnits ! SIMPLE_PERCENT, ..._PIXELS, ..._MM
  INTEGER :: iWidth, iHeight, iDepth
END TYPE SIMPLE_WINDOWS_SIZE_T

TYPE SIMPLE_WINDOWS_ORIGIN_T
  INTEGER :: iUnits ! SIMPLE_PERCENT, ..._PIXELS, ..._MM
  INTEGER :: iX, iY
END TYPE SIMPLE_WINDOWS_ORIGIN_T

TYPE HDC_DIM_TYPE
  INTEGER*4 iBitmapDC, iWidth, iHeight
END TYPE HDC_DIM_TYPE

TYPE(HDC_DIM_TYPE)::HDC_DIM

! Useful definition of data structure to hold BitmapDC to pass to SIMPLEPLOT

CONTAINS

SUBROUTINE SP_SupplyBitmap ! Pass Bitmap to SIMPLEPLOT
! Set up a Static (SAVEd) array of OpCodes. Each OpCode consists of an
! OpCode ID followed by an optional address of a data structure.
! The parameter block address is passed to SIMPLEPLOT by SUBROUTINE DDDATA.
!
! In this example:
! SIMPLE_WINDOWS_SET_HDC_DIM specifies that the next integer is the address
! of a data structure consisting of a Bitmap DC followed by its dimensions
! SIMPLE_WINDOWS_END_LIST is the mandatory end of list item which does not
! require the address of a structure
  TYPE(SIMPLE_WINDOWS_DATA_STRUCT_T),DIMENSION(2)::OpCodes
  SAVE OpCodes
  OpCodes(1)%iOpc = SIMPLE_WINDOWS_SET_HDC_DIM ! hDC + dimensions
  OpCodes(1)%iPtr = LOC(HDC_DIM) ! See definition of SPWIN above
  OpCodes(2)%iOpc = SIMPLE_WINDOWS_END_LIST! End of list
  CALL DDDATA(LOC(OpCodes))! Notify SIMPLEPLOT
  CALL DEVNAM('WINDOW') ! Select Window
  CALL OWNNEW(.TRUE.) ! Inhibit 'Continue' button
END SUBROUTINE SP_SupplyBitmap

!This subroutine is needed if the content of the graph window changes during the program run
SUBROUTINE UpdateWin
  CALL OUTBUF ! Flush buffers
  CALL WINDOW_UPDATE@(HDC_DIM%iBitmapDC) ! Copy Bitmap to window
END SUBROUTINE UpdateWin

END MODULE spwin

WINAPP
PROGRAM Simpleplot_plots
USE mswin
USE spwin
LIBRARY 'c:\program files (x86)\silverfrost\ftn95\simple.dll'
IMPLICIT NONE
REAL,DIMENSION(100) ::xarr,yarr
INTEGER :: narr,ltype,xscale,yscale,nstep=1,mtype=5
INTEGER :: ctype,i,ans
CHARACTER(8) :: xlabel="x",ylabel="y",ptype
REAL :: minx,maxx,miny,maxy

narr=0
DO i=1,100
  xarr(i)=2*i
  yarr(i)=i*i
  narr=narr+1
END DO

HDC_DIM%iWidth = 600
HDC_DIM%iHeight = 800
HDC_DIM%iBitmapDC = GET_Bitmap_DC@(HDC_DIM%iWidth, HDC_DIM%iHeight)
CALL SP_SupplyBitmap ! Pass Bitmap to SIMPLEPLOT
CALL INITSP
CALL PGFULL(.true.)
!scale factor for text in graph
CALL TEXTMG(1.5)
!characters set
CALL CHSET(0)
!direction of ticks: in, out
CALL AXSBTK('XC','I')
CALL AXSBTK('YC','O')
!line thickness
CALL THCKMG('L',2.0)
!location of labels in the axes
CALL AXLBJS('**', 'Centre')
minx=minval(xarr)
maxx=maxval(xarr)
miny=minval(yarr)
maxy=maxval(yarr)
!define the graph scales: max and min values of x and y and the type of scales: 1-linear, 2-log
xscale=1;yscale=1
CALL SCALES(minx,maxx,xscale,miny,maxy,yscale)
!pen color 1...for axes
CALL PEN(4)
!cross point of x and y
CALL AXCRSS('YC',minx)
! labels of axes
CALL AXES7(xlabel, ylabel)
!pen color for data
CALL PEN(2)
!full line or dashed?
!ltype=0, full line,ltype=1, dashed
ltype=0
ptype="scatter"
ptype="curve"
IF(ptype=='curve') THEN
 !cvtype=1 -> smooth
 !cvtype=3 -> straight lines
 ctype=3
 CALL CVTYPE(ctype)
 CALL BRKNCV(real(xarr,1),real(yarr,1),narr,ltype)
ELSE !scatter
 CALL MKSET(1) !set of markers 1,2,3......
 CALL MARKCV(real(xarr,1),real(yarr,1),narr,mtype,nstep)
ENDIF
ans=winio@('%ca[A Simple Plot with Simpleplot]%bg[grey]&')
ans=winio@('%dw&',HDC_DIM%iBitmapDC) ! Pass bitmapDC to ClearWin
ans=winio@('%ff%nl%cn%6bt[Close]')
END PROGRAM Simpleplot_plots