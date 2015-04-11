!-------------------------------------------------------------------------------------------------------
!---                                         Modules                                                 ---
!-------------------------------------------------------------------------------------------------------

MODULE definitions
    !Define variables and parameters
!$$$$$$     character(len=10):: name
    character*7 shore_cases(2)
    data shore_cases /'Case 1','Case 2'/
    integer :: nT,n,ny,Lx,Ly,T_current,show_init,lock_plot,lock_data
    real*8 :: deltaT,deltaX,deltaY,pi,fill,slider_current,K_par,ro_s,ro_w,p,brindex,mu,T,theta0,H0,A,d50,Hb_local,theta_b_local,d
    real*8 :: x_gr,h_gr,w_gr
    real*8,dimension(:,:),allocatable :: ys_omni,Q,Hb,theta_b,H_vis
    real*8,dimension(:,:,:),allocatable :: H,theta
    real*8,dimension(:),allocatable :: x,ys,y,H_local,ygroin
    parameter(pi=3.1415927)
    parameter(g=9.80665)
ENDMODULE definitions

!-------------------------------------------------------------------------------------------------------
!---                                       Main program                                              ---
!-------------------------------------------------------------------------------------------------------

PROGRAM sedtrans
    winapp
    library 'c:\TFG\simple.dll','c:\TFG\salflibc.dll'
    use definitions
    implicit none
    include <windows.ins>
    integer :: i,j,k,m,setup_plot,update_plot,update_chart,sure_quit,plot_window,page1,page2,lock_unlock,propagate,Gt
    external setup_plot,update_plot,update_chart,sure_quit,lock_unlock,propagate,resize_array,reset_shore,add_groin

    !Provisional size of beach
    Lx=10000
    Ly=10000

    !Define accuracy of net
!$$$$$$     print *, 'Number of ''x'' divisions?'
!$$$$$$     read *, n
    n=50
    ny=50
    allocate(x(n+1))
    allocate(ys(n+1))
    allocate(ygroin(n+1))
    allocate(H_vis(n+1,ny+1))
    allocate(y(ny+1))
    allocate(H_local(ny+1))
!$$$$$$     print *, 'Number of time steps?'
!$$$$$$     read *, T
    nT=3000
    allocate(H(n+1,ny+1,nT+1))
    allocate(theta(n+1,ny+1,nT+1))
    allocate(Hb(n+1,nT+1))
    allocate(theta_b(n+1,nT+1))
    allocate(ys_omni(nT+1,n+1))
    allocate(Q(nT+1,n+1))

    !Calculate deltas
    deltaX=Lx/n
    deltaY=Ly/ny
    deltaT=365*24*3600/nT	!Length of experiment is 1 yr. (measured in seconds)
    
    do j=1,ny+1
        y(j)=(j-1)*deltaY
    enddo
    !Set initial conditions
    d50=0.2
    A=0.09
    T=14.0
    theta0=15.0
    H0=3.0
    K_par=0.7
    ro_s=2.65
    ro_w=1.0
    p=0.4
    brindex=0.5
    mu=(K_par*ro_w*sqrt(g/brindex))/(16*(ro_s-ro_w)*(1-p))
    ygroin(:)=0
    x_gr=0
    h_gr=0
    w_gr=0
    d=200
    do i=1,n+1
        x(i)=(i-1)*deltaX
!$$$$$$         ys(i)=sin((i-1)*(pi/(37.1/8)))*10+200
        ys(i)=d
        ys_omni(1,i)=ys(i)
        m=propagate(ys(i))
            Hb(i,1)=Hb_local
            theta_b(i,1)=theta_b_local
            H(i,:,1)=H_local(:)
    enddo
    Gt=mu*((sum(Hb)/size(Hb))**2.5)/(A*(Ly**0.6667))
!$$$$$$     print*,(deltaX**2)/(2*G)
!$$$$$$     print*,deltaT
    lock_plot=0
    lock_data=1
    T_current=0
    fill=T_current/real(nT)


    !Create the window
    !Sheet pages setup
    i=winio@('%sh&',page1)
        i=winio@('%ca[Parameters]&')
        i=winio@('%1.1ob[named_l][Accuracy]&')
            i=winio@('%1.3ob[no_border]&')
                i=winio@('nT:  %cb&')
                i=winio@('nx:  %cb&')
                i=winio@('ny:  %cb&')
        i=winio@('%cb&')
        i=winio@('%ff%nl%1.1ob[named_l][Data]&')
            i=winio@('%2.1ob[no_border]&')
                i=winio@('%`bg[inactivecaption]%1.2ob&')
                    i=winio@('%1.5ob[no_border]&')
                        i=winio@('%eq[K]    %co[check_on_focus_loss,right_justify]&',23L,31L)
                            i=winio@('%dy%`bg[white]%~^5rf%cb&',0.25D0,K_par,lock_data,update_chart)
                        i=winio@('%eq[{rho}{sub S}]%co[check_on_focus_loss,right_justify]&',36L,33L)
                            i=winio@('%dy%`bg[white]%~^5rf%cb&',0.5D0,ro_s,lock_data,update_chart)
                        i=winio@('%eq[{rho}{sub W}]%co[check_on_focus_loss,right_justify]&',36L,33L)
                            i=winio@('%dy%`bg[white]%~^5rf%cb&',0.5D0,ro_w,lock_data,update_chart)
                        i=winio@('%eq[p]    %co[check_on_focus_loss,right_justify]&',23L,33L)
                            i=winio@('%dy%`bg[white]%~^5rf%cb&',0.5D0,p,lock_data,update_chart)
                        i=winio@('%eq[{gamma}{sub b}]%co[check_on_focus_loss,right_justify]&',36L,33L)
                            i=winio@('%dy%`bg[white]%~^5rf%cb%cb&',0.5D0,brindex,lock_data,update_chart)
                    i=winio@('%eq[{mu}]    %co[check_on_focus_loss,right_justify]&',22L,29L)
                        i=winio@('%dy%`bg[white]%`~5rf%cb%cb&',0.5D0,mu,lock_data)
                i=winio@('%`bg[inactivecaption]%1.2ob&')
                        i=winio@('%eq[d{sub 50}]%co[check_on_focus_loss,right_justify]&',35L,33L)
                            i=winio@('%dy%`bg[white]%~^5rf%cb&',0.25D0,d50,lock_data,update_chart)
                        i=winio@('%eq[A]      %co[check_on_focus_loss,right_justify]&',17L,25L)
                            i=winio@('%dy%`bg[white]%`~5rf%cb&',0.25D0,A,lock_data)
                i=winio@('%ff%nl%`bg[inactivecaption]%1.1ob&')
                        i=winio@('%eq[T]      %co[check_on_focus_loss,right_justify]&',17L,25L)
                            i=winio@('%dy%`bg[white]%~5rf%cb&',0.25D0,T,lock_data)
                i=winio@('%ff%nl%`bg[inactivecaption]%1.1ob&')
                    i=winio@('%1.2ob[no_border]&')
                        i=winio@('%eq[{theta}{sub 0}]  %co[check_on_focus_loss,right_justify]&',30L,33L)
                            i=winio@('%dy%`bg[white]%~5rf%cb&',0.25D0,theta0,lock_data)
                        i=winio@('%nl%eq[H{sub 0}]%co[check_on_focus_loss,right_justify]&',35L,32L)
                            i=winio@('%dy%`bg[white]%~5rf%cb%cb%cb&',0.25D0,H0,lock_data)
        i=winio@('%cb&')
        i=winio@('%ff%nl%cn%`bg[scrollbar]%1.1ob&')
            i=winio@('Initial shore at:  %co[check_on_focus_loss,right_justify]%`bg[white]%~5rf (m)&',d,lock_data)
            i=winio@('%ff%nl%cn%~^bb[Reset Shoreline]&',lock_data,reset_shore)
        i=winio@('%cb')
    i=winio@('%sh&',page2)
        i=winio@('%ca[Structures]&')
        i=winio@('%1.2ob[no_border]&')
            i=winio@('%ff%nl%cn%`bg[scrollbar]%1.1ob&')
                i=winio@('Insert groin at:  %co[check_on_focus_loss,right_justify]%`bg[white]%~5rf (m)&',x_gr,lock_data)
                i=winio@('%ff%nlHeight:  %co[check_on_focus_loss,right_justify]%`bg[white]%~5rf (m)&',h_gr,lock_data)
                i=winio@('%ff%nlWidth:  %co[check_on_focus_loss,right_justify]%`bg[white]%~5rf (m)&',w_gr,lock_data)
                i=winio@('%ff%nl%cn%~^bb[Insert]%cb&',lock_data,add_groin)
            i=winio@('%cb&')
            i=winio@('%nlList%cb')
    !H plot sub-window
    !i=winio@('%ww[topmost,no_caption,no_frame]%ca[Wave Height]&')
        !i=winio@('%1.1ob%1.2ob[no_border]%pl[x_axis=Y,y_axis=H,colour=blue,x_array]%cb%cn%bt[Close]%cb%cb',500,400,ny+1,y,H(1,:,1))
    !ys plot sub-window
    i=winio@('%ww[no_caption,no_frame]%`lw&',plot_window)
        i=winio@('%ap&',65L,0L)
        i=winio@('%`bg[window]%1.1ob[thin_panelled]%`rb[Show initial line.]%cb%ff&',show_init)
        i=winio@('%pl[n_graphs=2,colour=red,colour=black,y_min=0,y_max=700,x_array]&',600,450,n+1,x,ys,ygroin)
        i=winio@('%ff%nl%cn%`bg[window]%3.1ob[thin_panelled]&')
            i=winio@('%dy%`bg[white]%30br[percentage]%cb&',0.7D0,fill,RGB@(0,128,128))
            i=winio@('    %dy%`bg[white]%1.1ob[scored]%co[no_data_border,right_justify]&',0.4D0)
                i=winio@('%dd%il%~^rd%cb%cb&',nT/200,0,nT,T_current,lock_plot,update_plot)
            i=winio@(' %dy%`bg[white]%~^bt[Edit Data]%cb',0.25D0,lock_plot,lock_unlock)
    !Main frame window
    i=winio@('%mi[MainIcon]%ww[no_maxbox]%ca[Shoreline Plot]%bg[grey]&')
        i=winio@('%2.1ob[no_border]%`bg[white]%1.1ob[scored]%ch%cb%cb&',plot_window)
            i=winio@('%1.2ob[no_border]%2ps%cb&',page1,page2)
            i=winio@('%nl%cn%fn[Times New Roman]%ts%`bg[inactiveborder]%~^bt[Calculate]%sf%cb%cb&',1.4D0,lock_data,setup_plot)
        i=winio@('%cc',sure_quit)
        
!$$$$$$         i=winio@('%pl[colour=red,x_array]',600,400,ny+1,y,H(1,:,1))

!$$$$$$     do i=1,n+1
!$$$$$$         H_vis(i,:)=H(i,:,1)
!$$$$$$     enddo
!$$$$$$     CALL VSNEW
!$$$$$$ !$$$$$$     CALL VSRG(H_vis,n+1,ny+1)
!$$$$$$     CALL sfeqx(0,deltaX)
!$$$$$$     CALL sfeqy(0,deltaY)
!$$$$$$     CALL rgsurf(H_vis,n+1,ny+1)
!$$$$$$     CALL VSOUT
!$$$$$$     CALL title7('T','C','Surface')
!$$$$$$     CALL ENDPLT
       
    !i=winio@('%ff%nl%cn%^tt[Evaluar]&',update_plot)

ENDPROGRAM sedtrans

!-------------------------------------------------------------------------------------------------------
!---                                       Subroutines                                               ---
!-------------------------------------------------------------------------------------------------------

!---------------------------------------------------------------------
!---                       Calculate Shore                         ---
!---------------------------------------------------------------------

SUBROUTINE shore_calc
    use definitions
    implicit none
    include <windows.ins>
    real*8 :: progress
    integer :: i,j,k,m,ctrl,propagate
    external propagate
    progress=0.0
    i=winio@('%ww[no_caption,topmost]&')
    i=winio@('Processing... %2nl&')
    i=winio@('%`bg[white]%20br[percentage]&',progress,RGB@(0,128,128))
    i=winio@('%lw',ctrl)
    
    do i=1,n+1
        m=propagate(ys_omni(1,i))
            Hb(i,1)=Hb_local
            theta_b(i,1)=theta_b_local
            H(i,:,1)=H_local(:)
    enddo
    do k=1,nT
     !!Calculate Q (Sediment flux)
      !Initial point (i=1)
      if (ygroin(1)-ys_omni(k,1)>=0) then !groin
        Q(k+1,1)=0 
      else !Periodic
        Q(k+1,1)=mu*(Hb(1,k)**(5/2))*sin(2*((theta_b(1,k)*pi/180)-atan((ys_omni(k,2)-ys_omni(k,n))/(2*deltaX)))) 
      endif
      !Intermediate points
      do i=2,n
        if (ygroin(i)-ys_omni(k,i)>=0) then !Groin
          Q(k+1,i)=0 
        else !Periodic
          if (ygroin(i+1)>0) then !Right before a groin (don't use its 'ys' value!!)
            Q(k+1,i)=mu*(Hb(i,k)**(5/2))*sin(2*((theta_b(i,k)*pi/180)-atan((ys_omni(k,i)-ys_omni(k,i-1))/(deltaX)))) 
          elseif (ygroin(i-1)>0) then !Right after a groin (don't use its 'ys' value!!)
            Q(k+1,i)=mu*(Hb(i,k)**(5/2))*sin(2*((theta_b(i,k)*pi/180)-atan((ys_omni(k,i+1)-ys_omni(k,i))/(deltaX)))) 
          else
            Q(k+1,i)=mu*(Hb(i,k)**(5/2))*sin(2*((theta_b(i,k)*pi/180)-atan((ys_omni(k,i+1)-ys_omni(k,i-1))/(2*deltaX))))
          endif
        endif
      enddo
      !Final point (i=n+1)
      if (ygroin(n+1)-ys_omni(k,n+1)>=0) then !Groin
        Q(k+1,n+1)=0 
      else !Periodic
        Q(k+1,n+1)=mu*(Hb(n+1,k)**(5/2))*sin(2*((theta_b(n+1,k)*pi/180)-atan((ys_omni(k,2)-ys_omni(k,n))/(2*deltaX))))
      endif
      
     !!Calculate ys (Shore-line)
      !Initial point (i=1)
      if (ygroin(i)-ys_omni(k,i)>0) then !Groin
        ys_omni(k+1,1)=ys_omni(k,1)-(deltaT/(2*deltaX*10))*(Q(k+1,2)-Q(k+1,n)) 
        m=propagate(ys_omni(k+1,1)) !This function calculates Hb and theta_b for the given 'i' position.
            Hb(1,k+1)=Hb_local
            theta_b(1,k+1)=theta_b_local
            H(1,:,k+1)=H_local(:)
      else
        if (ygroin(i)>0) then !Sand has reached the tip of the groin.
          ys_omni(k+1,i)=ygroin(i)
          m=propagate(ys_omni(k+1,i))
              Hb(i,k+1)=Hb_local
              theta_b(i,k+1)=theta_b_local
              H(i,:,k+1)=H_local(:)
        else !No groin (periodic)
          ys_omni(k+1,1)=ys_omni(k,1)-(deltaT/(2*deltaX*10))*(Q(k+1,2)-Q(k+1,n))
          m=propagate(ys_omni(k+1,1))
              Hb(1,k+1)=Hb_local
              theta_b(1,k+1)=theta_b_local
              H(1,:,k+1)=H_local(:)
        endif
      endif
      !Intermediate points
      do i=2,n
        if (ygroin(i)-ys_omni(k,i)>0) then
          ys_omni(k+1,i)=ys_omni(k,i)-(deltaT/(deltaX*10))*(Q(k+1,i+1)-Q(k+1,i)) !Groin
          m=propagate(ys_omni(k+1,i))
              Hb(i,k+1)=Hb_local
              theta_b(i,k+1)=theta_b_local
              H(i,:,k+1)=H_local(:)
        else
          if (ygroin(i)>0) then !Sand has reached the tip of the groin.
            ys_omni(k+1,i)=ygroin(i)
            m=propagate(ys_omni(k+1,i))
                Hb(i,k+1)=Hb_local
                theta_b(i,k+1)=theta_b_local
                H(i,:,k+1)=H_local(:)
          else !No groin
            ys_omni(k+1,i)=ys_omni(k,i)-(deltaT/(2*deltaX*10))*(Q(k+1,i+1)-Q(k+1,i-1))
            m=propagate(ys_omni(k+1,i))
                Hb(i,k+1)=Hb_local
                theta_b(i,k+1)=theta_b_local
                H(i,:,k+1)=H_local(:)
          endif
        endif
      enddo
      !Final point (i=n+1)
      if (ygroin(i)-ys_omni(k,i)>0) then !Groin
        ys_omni(k+1,n+1)=ys_omni(k,n+1)-(deltaT/(deltaX*10))*(Q(k+1,n+1)-Q(k+1,n)) 
        m=propagate(ys_omni(k+1,n+1))
            Hb(n+1,k+1)=Hb_local
            theta_b(n+1,k+1)=theta_b_local
            H(n+1,:,k+1)=H_local(:)
      else
        if (ygroin(i)>0) then !Sand has reached the tip of the groin.
          ys_omni(k+1,n+1)=ygroin(n+1)
          m=propagate(ys_omni(k+1,n+1))
              Hb(n+1,k+1)=Hb_local
              theta_b(n+1,k+1)=theta_b_local
              H(n+1,:,k+1)=H_local(:)
        else !No groin (periodic)
          ys_omni(k+1,n+1)=ys_omni(k+1,1)
          m=propagate(ys_omni(k+1,n+1))
              Hb(n+1,k+1)=Hb_local
              theta_b(n+1,k+1)=theta_b_local
              H(n+1,:,k+1)=H_local(:)
        endif
      endif
        
        progress=progress+1.0/(nT+1) !Display progress.
        CALL window_update@(progress) 
    enddo

    ctrl=1 !This is so the progress-bar window closes.
    CALL window_update@(ctrl)
ENDSUBROUTINE shore_calc

!-------------------------------------------------------------------------------------------------------
!---                                    External functions                                           ---
!-------------------------------------------------------------------------------------------------------

! Resize an array    
FUNCTION resize_array(array,oldsize,newsize)
!$$$$$$     integer :: oldsize,newsize
!$$$$$$     real*8,dimension(oldsize),allocatable :: array
!$$$$$$     real*8,dimension(:),allocatable :: tmp_arr
!$$$$$$     
!$$$$$$     allocate(tmp_arr(newsize))
!$$$$$$     tmp_arr(1:size(array))=array
!$$$$$$     deallocate(array)
!$$$$$$     allocate(array(size(tmp_arr)))
!$$$$$$     array=tmp_arr
ENDFUNCTION resize_array

! Convert a string to a numeric value
FUNCTION strtonum(str)
    integer strtonum
    character str
    read (str,'(I10)') strtonum
ENDFUNCTION strtonum

! Convert a value to a formatted string
FUNCTION numtostr(num)
    integer num
    character(len=10) numtostr
    write (numtostr,'(I10)') num
ENDFUNCTION numtostr

FUNCTION propagate(y_value)
    use definitions
    implicit none
    include <windows.ins>
    integer :: propagate,j,iters
    real*8 :: y_value,L0,Ks,Kr
    real*8,dimension(ny+1) :: D_local,L1,L2,L3

    L0=(g*(T**2))/(2*pi)
    L1(:)=0.75*L0
    
    !Calculate D(depth) along the y axis:
    do j=1,ny+1
        if ( y(j)-y_value>0 ) then
            D_local(j)=A*((y(j)-y_value)**(0.6667))
        else
            D_local(j)=0
        endif
    enddo
    !Estimate L(wave length) along the y axis:
    do iters=1,10
        do j=1,ny+1
            L2(j)=L0*tanh((2*pi*D_local(j))/L1(j))
            L3(j)=(2*L2(j)+L1(j))/3
            L1(j)=L3(j)
        enddo
    enddo
    !Calculate H(wave height) along the y axis:
    !First iteration to allow initial comparison
    j=ny+1
    if (D_local(j)/L1(j)>=0.5) then !deep waters
        Ks=L0/(2*T)
    elseif (D_local(j)/L1(j)<=0.05) then !shallow waters
        Ks=sqrt(g*D_local(j))
    else !intermediate waters
        Ks=sqrt((L0/(2*T))/((L1(j)/(2*T))*(1+(4*pi*D_local(j)/L1(j))/(sinh(4*pi*D_local(j)/L1(j))))))
    endif
    Kr=sqrt(cos(theta0*pi/180)/cos(asin((L1(j)/L0)*sin(theta0*pi/180))))
    H_local(j)=H0*Ks*Kr
    !Iterate while checking for breaking conditions
    do while (j>1.and.D_local(j)>0.and.H_local(j)/D_local(j)<=brindex)
        j=j-1
        if (D_local(j)/L1(j)>=0.5) then !deep waters
            Ks=L0/(2*T)
        elseif (D_local(j)/L1(j)<=0.05) then !shallow waters
            Ks=sqrt(g*D_local(j))
        else !intermediate waters
            Ks=sqrt((L0/(2*T))/((L1(j)/(2*T))*(1+(4*pi*D_local(j)/L1(j))/(sinh(4*pi*D_local(j)/L1(j))))))
        endif
        Kr=sqrt(cos(theta0*pi/180)/cos(asin((L1(j)/L0)*sin(theta0*pi/180))))
        H_local(j)=H0*Ks*Kr
    enddo
    !Breaking conditions achieved:
    Hb_local=H_local(j)
    theta_b_local=(180/pi)*asin((L1(j)/L0)*sin(theta0*pi/180))
    !From now on, H decreases until meeting the shoreline:
    do while (j>1)
        j=j-1
        H_local(j)=brindex*D_local(j)
    enddo
    
    propagate=1
ENDFUNCTION propagate

FUNCTION update_chart
    use definitions
    implicit none
    include <windows.ins>
    integer :: update_chart
    mu=(K_par*ro_w*sqrt(g/brindex))/(16*(ro_s-ro_w)*(1-p))

    if (d50<=0.4) then
        A=0.41*(d50**(0.94))
    elseif (d50>0.4 .and. d50<=10) then
        A=0.23*(d50**(0.32))
    elseif (d50>10 .and. d50<=40) then
        A=0.23*(d50**(0.28))
    else
        A=0.46*(d50**(0.11))
    endif

    update_chart=1
ENDFUNCTION update_chart

FUNCTION setup_plot
    use definitions
    implicit none
    include <windows.ins>
    integer :: i,setup_plot,update_plot,lock_unlock
    external update_plot,lock_unlock

    ys_omni(1,:)=ys_omni(T_current+1,:) !Reset timer
    T_current=0
    CALL shore_calc

    i=update_plot()
    i=lock_unlock()
    
    setup_plot=1
ENDFUNCTION setup_plot

FUNCTION update_plot
    use definitions
    implicit none
    include <windows.ins>
    integer :: update_plot
    
    !Update the filling bar
    fill=T_current/real(nT)
    CALL window_update@(fill) 
    CALL window_update@(T_current)   

    !Update the plot
    ys=ys_omni(T_current+1,:)
    CALL simpleplot_redraw@

    update_plot=1
ENDFUNCTION update_plot

FUNCTION reset_shore
    use definitions
    implicit none
    include <windows.ins>
    integer :: reset_shore

    ys=d
    ys_omni(1,:)=d
    CALL simpleplot_redraw@

    reset_shore=1
ENDFUNCTION reset_shore

FUNCTION add_groin !Revisar
    use definitions
    implicit none
    include <windows.ins>
    integer :: add_groin,i,j
    if (nint(x_gr/deltaX)+1==(n+1).or.nint(x_gr/deltaX)+1==1) then
        ygroin(1)=h_gr
        ygroin(n+1)=h_gr
        do i=1,nint((w_gr/2)/deltaX)+1
            ygroin(i)=h_gr
        enddo
        do i=nint((n+1)-((w_gr/2)/deltaX)),n+1
            ygroin(i)=h_gr
        enddo
    elseif (nint((x_gr+(w_gr/2))/deltaX)+1>=(n+1).or.nint((x_gr-(w_gr/2))/deltaX)+1<=1) then
        j=winio@('%1SI!The groin specified exceeds beach dimensions.%nl%nl%cn%`6bt[Ok]%ca[Warning]')
    else
        do i=nint((x_gr-(w_gr/2))/deltaX)+1,nint((x_gr+(w_gr/2))/deltaX)+1
            ygroin(i)=h_gr
        enddo
    endif
    x_gr=0.0
    h_gr=0.0
    w_gr=0.0
    CALL simpleplot_redraw@

    add_groin=1
ENDFUNCTION add_groin

FUNCTION lock_unlock
    use definitions
    implicit none
    include <windows.ins>
    integer :: lock_unlock

    if (lock_plot==1) then
        lock_plot=0
        lock_data=1
    else
        lock_plot=1
        lock_data=0
    endif

    CALL window_update@(lock_plot) 
    CALL window_update@(lock_data) 

    lock_unlock=1
ENDFUNCTION lock_unlock

FUNCTION sure_quit
    implicit none
    integer :: i,sure_quit
    !The parameter 'i' returns 1 if 'Yes' is pressed, 2 if 'No' is pressed and zero if the window is closed.
    i=winio@('%1SI!Are you sure you want to quit?%nl%nl%cn%`6bt[Yes]          %6bt[No]%ca[Warning]')
    sure_quit=abs(i-1)
ENDFUNCTION sure_quit

!-------------------------------------------------------------------------------------------------------
!---                                        Resources                                                ---
!-------------------------------------------------------------------------------------------------------

RESOURCES
	MainIcon ICON c:\TFG\Media\icono2.ico