  module module_madwrf

    implicit none

    private
    public :: Init_madwrf_tracers

  contains

   subroutine Init_madwrf_tracers (tracer, moist, p_qc, p_qi, p_qs, p_tr_qc, p_tr_qi, p_tr_qs, &
       ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, its , ite , jts , jte , kts , kte)

     implicit none

     real, dimension (ims:ime, kms:kme, jms:jme, *), intent (inout) :: tracer
     real, dimension (ims:ime, kms:kme, jms:jme, *), intent (in) :: moist
     integer, intent (in) :: p_qc, p_qi, p_qs, p_tr_qc, p_tr_qi, p_tr_qs, ids, ide, jds, jde, &
         kds, kde, ims, ime, jms, jme, kms, kme, its , ite , jts , jte , kts , kte

     integer :: i, j, k


     do j = jts, min (jte, jde - 1)
       do k = kts, kte - 1
         do i = its, min (ite, ide - 1)
           tracer(i, k, j, p_tr_qc) = moist(i, k, j, p_qc)
           tracer(i, k, j, p_tr_qi) = moist(i, k, j, p_qi)
           tracer(i, k, j, p_tr_qs) = moist(i, k, j, p_qs)
         end do
       end do
     end do

   end subroutine Init_madwrf_tracers

  end module module_madwrf
