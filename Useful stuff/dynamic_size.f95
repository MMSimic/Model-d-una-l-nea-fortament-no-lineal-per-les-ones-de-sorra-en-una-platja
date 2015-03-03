PROGRAM dynamic_size
    INTEGER,DIMENSION(:),ALLOCATABLE :: array
    
    ALLOCATE(array(10))
    
    array=(/1,2,5,7,4,3,6,5,6,7/)
    
    WRITE(*,*)SIZE(array)
    CALL resize_array
    WRITE(*,*)size(array)
    
    CONTAINS
    
    SUBROUTINE resize_array
        INTEGER,DIMENSION(:),ALLOCATABLE :: tmp_arr
        
        ALLOCATE(tmp_arr(2*SIZE(array)))
        tmp_arr(1:SIZE(array))=array
        DEALLOCATE(array)
        ALLOCATE(array(size(tmp_arr)))
    	array=tmp_arr
    
    ENDSUBROUTINE resize_array

ENDPROGRAM dynamic_size