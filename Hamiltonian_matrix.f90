program main
 implicit none

 integer :: i, j !indexes
 real*8, allocatable :: H(:,:), S(:,:) !matrix to diagonalize and eigenvalues
 integer :: n  !dimension of the matrix 
 real*8, allocatable :: v(:), vt(:)

 write(*,*) 'matrix dimension'
 read(*,*) n

 allocate(H(n,n))
 allocate(S(n,n))
 allocate(v(n))
 allocate(vt(n))

!create hermitian matrix
  do i= 1, n
    do j=1,n
       H(i,j)= i*j
    enddo
 enddo 

!print the matrix elements to the screen
! do i= 1,n
! write(*,*) H(i,:)
! enddo 

!overlap matrix with only positive elements
do i=1,n
  v(i)=i*2
enddo

do i=1,n
   vt(i)=(v(i))
enddo


do i=1,n
  do j=1,n
  S(i,j)= v(i)*vt(i)
  enddo
enddo

do i=1,n
 write(*,*) S(i,:)
enddo


!print the matrix elements to the screen
! do i=1,n
 !  write(*,*) H(i,:)
! enddo


 deallocate(H)
 deallocate(S)
 deallocate(v)
 deallocate(vt)

 end program main 
