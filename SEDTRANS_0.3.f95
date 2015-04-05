!-------------------------------------------------------------------------------------------------------
!---                                         Modules                                                 ---
!-------------------------------------------------------------------------------------------------------

MODULE definitions
    !Define variables and parameters
!$$$$$$     character(len=10):: name
    character*7 shore_cases(2)
    data shore_cases /'Case 1','Case 2'/
    integer :: nT,n,ny,Lx,Ly,T_current,selected_case,show_init,lock_plot,lock_data
    real*8 :: deltaT,deltaX,deltaY,pi,fill,slider_current,K_par,ro_s,ro_w,p,brindex,mu,T,theta0,H0,A,Hb_local,theta_b_local
    real*8,dimension(:,:),allocatable :: ys_omni,Q,Hb,theta_b
    real*8,dimension(:,:,:),allocatable :: H,theta
    real*8,dimension(:),allocatable :: x,ys,y,H_local
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
    integer :: i,j,k,setup_plot,update_plot,update_chart,sure_quit,plot_window,page1,page2,lock_unlock
    external setup_plot,update_plot,update_chart,sure_quit,lock_unlock

    !Provisional size of beach
    Lx=3000
    Ly=3000

    !Define accuracy of net
!$$$$$$     print *, 'Number of ''x'' divisions?'
!$$$$$$     read *, n
    n=50
    ny=50
    allocate(x(n+1))
    allocate(ys(n+1))
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
    do i=1,n+1
        x(i)=(i-1)*deltaX
!$$$$$$         ys(i)=sin((i-1)*(pi/(37.1/8)))*10+200
        ys(i)=200
        ys_omni(1,i)=ys(i)
    enddo
    theta0=15.0
    T=14.0
    H0=3.0
    K_par=0.7
    ro_s=2.65
    ro_w=1.0
    p=0.4
    brindex=0.5
    mu=0.2
    selected_case=1
    lock_plot=0
    lock_data=1
    T_current=0
    fill=T_current/real(nT)

    A=0.1

    !Create the window
    !Sheet pages setup
    i=winio@('%sh&',page1)
        i=winio@('%ca[Parameters]&')
        i=winio@('%1.1ob[named_l][Accuracy]&')
            i=winio@('%1.5ob[no_border]&')
                i=winio@('nT:  %cb&')
                i=winio@('Lx:  %cb&')
                i=winio@('Ly:  %cb&')
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
                i=winio@('%`bg[inactivecaption]%1.1ob&')
                        i=winio@('%eq[A]      %co[check_on_focus_loss,right_justify]&',17L,25L)
                            i=winio@('%dy%`bg[white]%~5rf%cb&',0.25D0,A,lock_data)
                i=winio@('%ff%nl%`bg[inactivecaption]%1.1ob&')
                        i=winio@('%eq[T]      %co[check_on_focus_loss,right_justify]&',17L,25L)
                            i=winio@('%dy%`bg[white]%~5rf%cb&',0.25D0,T,lock_data)
                i=winio@('%ff%nl%`bg[inactivecaption]%1.1ob&')
                    i=winio@('%1.2ob[no_border]&')
                        i=winio@('%eq[{theta}{sub 0}]  %co[check_on_focus_loss,right_justify]&',30L,33L)
                            i=winio@('%dy%`bg[white]%~5rf%cb&',0.25D0,theta0,lock_data)
                        i=winio@('%nl%eq[H{sub 0}]%co[check_on_focus_loss,right_justify]&',35L,32L)
                            i=winio@('%dy%`bg[white]%~5rf%cb%cb%cb&',0.25D0,H0,lock_data)
        i=winio@('%cb')
    i=winio@('%sh&',page2)
        i=winio@('%ca[Structures]&')
        i=winio@('%ff%nl%cn%`bg[window]%1.1ob[thin_panelled]ASDF%cb')
    !Plot sub-window
    i=winio@('%ww[no_caption,no_frame]%`lw&',plot_window)
        i=winio@('%ap&',65L,0L)
        i=winio@('%`bg[window]%1.1ob[thin_panelled]%`rb[Show initial line.]%cb%ff&',show_init)
        i=winio@('%pl[colour=red,y_min=0,y_max=700,x_array]&',600,400,n,x,ys)
        i=winio@('%ff%nl%cn%`bg[window]%3.1ob[thin_panelled]&')
            i=winio@('%dy%`bg[white]%30br[percentage]%cb&',0.7D0,fill,RGB@(0,128,128))
            i=winio@('    %dy%`bg[white]%1.1ob[scored]%co[no_data_border,right_justify]&',0.4D0)
                i=winio@('%dd%il%~^rd%cb%cb&',nT/200,0,nT,T_current,lock_plot,update_plot)
            i=winio@(' %dy%`bg[white]%~^bt[Edit Data]%cb',0.25D0,lock_plot,lock_unlock)
    !Main frame window
    i=winio@('%ww[no_maxbox]%ca[Shoreline Plot]%bg[grey]&')
        i=winio@('%2.1ob[no_border]%`bg[white]%1.1ob[scored]%ch%cb%cb&',plot_window)
            i=winio@('%1.2ob[no_border]%2ps%cb&',page1,page2)
            i=winio@('%nl%cn%fn[Times New Roman]%ts%`bg[inactiveborder]%~^bt[Calculate]%sf%cb%cb&',1.4D0,lock_data,setup_plot)
        i=winio@('%cc',sure_quit)
        
        !i=winio@('%pl[colour=red,x_array]',600,400,ny,y,-D(1,:))
       
    !i=winio@('%ff%nl%cn%^tt[Evaluar]&',update_plot)

ENDPROGRAM sedtrans

!-------------------------------------------------------------------------------------------------------
!---                                       Subroutines                                               ---
!-------------------------------------------------------------------------------------------------------

!---------------------------------------------------------------------
!---                          Utilities                            ---
!---------------------------------------------------------------------
    
SUBROUTINE resize_array
!$$$$$$     INTEGER,DIMENSION(:),ALLOCATABLE :: tmp_arr
!$$$$$$     
!$$$$$$     ALLOCATE(tmp_arr(2*SIZE(array)))
!$$$$$$     tmp_arr(1:SIZE(array))=array
!$$$$$$     DEALLOCATE(array)
!$$$$$$     ALLOCATE(array(size(tmp_arr)))
!$$$$$$     array=tmp_arr
ENDSUBROUTINE resize_array

!---------------------------------------------------------------------
!---                       Calculate Shore                         ---
!---------------------------------------------------------------------
    
!Case 1: Groins at x=o and x=Lx (cases will be removed altogether in future versions)
SUBROUTINE shore_case_1
    use definitions
    implicit none
    include <windows.ins>
    real*8 :: progress
    integer :: i,k,m,ctrl,propagate
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
        Q(k+1,1)=0
        do i=2,n
            Q(k+1,i)=mu*(Hb(i,k)**(5/2))*sin(2*((theta_b(i,k)*pi/180)-atan((ys_omni(k,i+1)-ys_omni(k,i-1))/(2*deltaX))))
        enddo
        Q(k+1,n+1)=0
        ys_omni(k+1,1)=ys_omni(k,1)-(deltaT/(deltaX*10))*(Q(k+1,2)-Q(k+1,1))
        m=propagate(ys_omni(k+1,1))
            Hb(1,k+1)=Hb_local
            theta_b(1,k+1)=theta_b_local
            H(1,:,k+1)=H_local(:)
        do i=2,n
            ys_omni(k+1,i)=ys_omni(k,i)-(deltaT/(2*deltaX*10))*(Q(k+1,i+1)-Q(k+1,i-1))
            m=propagate(ys_omni(k+1,i))
                Hb(i,k+1)=Hb_local
                theta_b(i,k+1)=theta_b_local
                H(i,:,k+1)=H_local(:)
        enddo
        ys_omni(k+1,n+1)=ys_omni(k,n+1)-(deltaT/(deltaX*10))*(Q(k+1,n+1)-Q(k+1,n))
        m=propagate(ys_omni(k+1,n+1))
            Hb(n+1,k+1)=Hb_local
            theta_b(n+1,k+1)=theta_b_local
            H(n+1,:,k+1)=H_local(:)
        
        progress=progress+1.0/(nT+1) !Display progress.
        CALL window_update@(progress) 
    enddo
    
    ctrl=1 !This is so the progress-bar window closes.
    CALL window_update@(ctrl) 
ENDSUBROUTINE shore_case_1

!Case 2: Periodic beach (cases will be removed altogether in future versions)
SUBROUTINE shore_case_2
    use definitions
    implicit none
    include <windows.ins>
    real*8 :: progress
    integer :: i,k,ctrl
    progress=0.0D0
    i=winio@('%ww[no_caption,topmost]&')
    i=winio@('Processing... %2nl&')
    i=winio@('%`bg[white]%20br[percentage]&',progress,RGB@(0,128,128))
    i=winio@('%lw',ctrl)
    do k=1,nT
        Q(k+1,1)=mu*(1.2**(5/2))*sin(2*((theta0*pi/180)-atan((ys_omni(k,2)-ys_omni(k,1))/(deltaX))))
        do i=2,n
            Q(k+1,i)=mu*(1.2**(5/2))*sin(2*((theta0*pi/180)-atan((ys_omni(k,i+1)-ys_omni(k,i-1))/(2*deltaX))))
        enddo
        Q(k+1,n+1)=mu*(1.2**(5/2))*sin(2*((theta0*pi/180)-atan((ys_omni(k,n+1)-ys_omni(k,n))/(deltaX))))
        ys_omni(k+1,1)=ys_omni(k,1)-(deltaT/(2*deltaX*10))*(Q(k+1,2)-Q(k+1,n))
        do i=2,n
            ys_omni(k+1,i)=ys_omni(k,i)-(deltaT/(2*deltaX*10))*(Q(k+1,i+1)-Q(k+1,i-1))
        enddo
        ys_omni(k+1,n+1)=ys_omni(k,n+1)-(deltaT/(2*deltaX*10))*(Q(k+1,2)-Q(k+1,n))
        progress=progress+1.0/(nT+1)
        CALL window_update@(progress) 
    enddo
    ctrl=1
    CALL window_update@(ctrl) 
ENDSUBROUTINE shore_case_2

!-------------------------------------------------------------------------------------------------------
!---                                    External functions                                           ---
!-------------------------------------------------------------------------------------------------------

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
    do while (H_local(j)/D_local(j)<=brindex)
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
    
!$$$$$$     do j=ny+1,1,-1
!$$$$$$         if (D_local(j)/L1(j)<=0.05) then !shallow waters
!$$$$$$             Ks=sqrt(g*D_local(j))
!$$$$$$         elseif (D_local(j)/L1(j)>=0.5) then !deep waters
!$$$$$$             Ks=L0/(2*T)
!$$$$$$         else !intermediate waters
!$$$$$$             Ks=sqrt((L0/(2*T))/((L1(j)/(2*T))*(1+(4*pi*D_local(j)/L1(j))/(sinh(4*pi*D_local(j)/L1(j))))))
!$$$$$$         endif
!$$$$$$         Kr=sqrt(cos(theta0*pi/180)/cos(asin((L1(j)/L0)*sin(theta0*pi/180))))
!$$$$$$         H_local(j)=H0*Ks*Kr
!$$$$$$         !Take into account breaking conditions:
!$$$$$$         if (D_local(j)>0.and.H_local(j)/D_local(j)>=brindex) then
!$$$$$$             H_local(j)=brindex*D_local(j)
!$$$$$$         elseif (D_local(j)==0) then
!$$$$$$             H_local(j)=0
!$$$$$$         endif
!$$$$$$     enddo
!$$$$$$     Hb_local=maxval(H_local,ny+1)
    
    propagate=1
ENDFUNCTION propagate

FUNCTION update_chart
    use definitions
    implicit none
    include <windows.ins>
    integer :: update_chart
    mu=(K_par*ro_w*sqrt(g/brindex))/(16*(ro_s-ro_w)*(1-p))
    update_chart=1
ENDFUNCTION update_chart
    
FUNCTION setup_plot
    use definitions
    implicit none
    include <windows.ins>
    integer :: i,setup_plot,update_plot,lock_unlock
    external update_plot,lock_unlock,shore_case_1,shore_case_2

    ys_omni(1,:)=ys_omni(T_current+1,:) !Reset timer
    T_current=0
    if (selected_case == 1) then !Temporal, se mezclaran los dos en futuras versiones
        CALL shore_case_1
    elseif (selected_case == 2) then
        CALL shore_case_2
    endif
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