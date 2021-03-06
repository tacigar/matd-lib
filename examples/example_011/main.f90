program main
  use mpi
  use matd
  implicit none

  type(matd_real8_matrix) :: m ! 8バイト実数の行列

  integer :: map1(5) = (/1,3,6,7,10/)
  integer :: map2(4) = (/1,3,6,10/)
  integer, parameter :: dim1 = 10, dim2 = 11

  integer :: ierr, myrank, i, ibegin, iend
  real(matd_dp) :: buf(110)
  real(matd_dp), pointer :: ptr(:)

  double precision, allocatable :: X(:) ! GELLANの領域と考える
  allocate(X(10000)) ! メモリプールの確保

  call mpi_init(ierr)
  call mpi_comm_rank(mpi_comm_world, myrank, ierr)

  ibegin = 200 ! index=200からX領域を使い始める

  ! X領域を使ってイレギュラーブロックサイクリック分散行列を生成
  call matd_create_irreg_gellan( &
    m, dim1, dim2, map1, map2, mpi_comm_world, X, ibegin, iend, .true. &
  )
  call matd_fence(m)

  ! 各プロセスがX領域のどの部分を利用したかを示す。
  ! 例えばこの分散方法の場合ランク0のプロセスは
  ! 23個の8バイト実数の要素を持つ。
  ! したがって要素分としてはそのまま23インデックスが進む。
  ! またマップ情報分(4バイト整数x9) 9 / 2 + 1 = 5
  ! インデックスが進み、計28インデックスが進む。
  do i = 0, 5
    if (myrank == i) then
      print *, iend - ibegin
    endif
    call matd_fence(m)
  enddo

  if (myrank == 0) then
    do i = 1, 110
      buf(i) = dble(i)
    enddo
    call matd_put(m, 1, 10, 1, 11, buf)
  endif
  call matd_data(m, ptr)

  call matd_fence(m)

  do i = 0, 5
    if (myrank == i) then
      print *, "== Rank ==", i, ", SIZE=", size(ptr)
      print *, ptr
    endif
    call matd_fence(m)
  enddo

  call matd_destroy_gellan(m)
  call mpi_finalize(ierr)

end program main
