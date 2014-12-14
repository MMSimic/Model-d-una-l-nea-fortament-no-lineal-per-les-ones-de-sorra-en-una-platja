program convert
   !This example shows the use of integer  and character variables
   implicit none
   integer   ::  pounds,pence,total
   character(len=10):: name
   print *,'What is your name?'
   read *,name
   print *, 'Hi ',TRIM(name),'! Enter number of pounds and  pence'
   read *, pounds,pence
   total =100 * pounds + pence
   print *,'the total money in pence is ',total
end program convert