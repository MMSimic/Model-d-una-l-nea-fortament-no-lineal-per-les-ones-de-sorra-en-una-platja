!Simple3.for
      WINAPP
      LIBRARY 'c:\program files (x86)\silverfrost\ftn95\simple.dll'
      INTEGER winio@,i,StartCB
      EXTERNAL StartCB
      i=winio@('%sy[no_border]%ca[User drawn]&')
      i=winio@('%pl[user_drawn]&',400,300)
      i=winio@('%sc',StartCB)
      END
!The start-up callback
      INTEGER FUNCTION StartCB()
      INCLUDE 'CLEARWIN.INS'
      INTEGER N,i
      PARAMETER(N=101)
      REAL x(N),y(N)
!Create the data.
      DO i=1,N
        x(i)=i-1
        y(i)=x(i)**2
      ENDDO
!Call SIMPLEPLOT routines to plot the graph.
      CALL INITSP
      CALL PAGE(12.0,10.0)
      CALL CHSET(-11)
      CALL NEWPAG
      CALL TITLE7('Higher','Centre','Sample')
      CALL SCALES(0.0,10.0,1,0.0,100.0,1)
      CALL AXGRID('*Cartesian',1,-1)
      CALL NEWPIC
      CALL AXIS7('XCartesian','x-axis')
      CALL AXIS7('YCartesian','y-axis')
      CALL BRKNCV(x,y,N,-1)
      CALL SIMPLEPLOT_REDRAW@
      StartCB=1
      END