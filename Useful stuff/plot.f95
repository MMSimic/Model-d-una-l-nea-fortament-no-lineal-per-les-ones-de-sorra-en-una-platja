
	winapp
   		integer winio@,i,StartCB
    	external StartCB
        i=winio@('%sy[no_border]%ca[Titulo]&')
        i=winio@('%pl[user_drawn]&',400,300)
        i=winio@('%sc',StartCB)
    end
	integer function StartCB()
    integer N,i
    parameter(N=101)
    real x(N),y(N)
    do i=1,N
      x(i)=i-1
      y(i)=x(i)**2
    enddo
    

    call initsp
    call page(12.0,10.0)
    call chset(-11)
    call newpag
    call title7('Higher','Centre','Sample')
    call scales(0.0,10,0,1.0,0.0,100.0,1.0)
    call axgrid('*Cartesian',1,-1)
    call newpic
    call axis7('XCastesian','x-axis')
    call axis7('YCartesian','y-axis')
    call BRKNCV(x,y,N,-1)
    StartCB=1
    end

