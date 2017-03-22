program main
  use mpi
  use matd
  implicit none

  type(matd_real8_matrix) :: m
  integer :: ierr, i, myrank

  integer, parameter :: SIZE = 20000
  real(matd_dp) :: buf(SIZE*SIZE)

  call mpi_init(ierr)
  call mpi_comm_rank(mpi_comm_world, myrank, ierr)

  call matd_create_reg(m, SIZE, SIZE, 500, 500, mpi_comm_world, .true.)

  call matd_fence(m)

  if (myrank == 0) then
    do i = 1, SIZE * SIZE
      buf(i) = dble(i)
    enddo
    call matd_put(m, 1, SIZE, 1, SIZE, buf)
  endif
  call matd_fence(m)

  call matd_destroy(m)
  call mpi_finalize(ierr)
end program main
