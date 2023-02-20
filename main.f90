!
! Diffie-Hellman�������v���O����
! �Q�l����:https://youtu.be/XOn3dt0y8iE
!
module dh
    implicit none
    type dh_class
        integer(8) p, g, a, b, x, y, at, bt, t
    end type

    interface operator(.nxor.)
        module procedure nxor
    end interface

    interface operator(.lsh.)
        module procedure l_shift
    end interface

    interface operator(.rsh.)
        module procedure r_shift
    end interface
contains
    ! Xorshift��������
    integer(8) function random(w)
        implicit none
        integer(8), intent(inout) :: w
        integer(8), save :: x = 123456789, y = 362436069, z = 521288629
        integer(8) t
        t = x .nxor. (x .lsh. 11_8)
        x = y; y = z; z = w
        w = (w .nxor. (w .rsh. 19_8)) .nxor. (t .nxor. (t .rsh. 8_8))
        random = w 
    end function random

    subroutine init(rc)
        implicit none
        logical(8), allocatable :: sieve(:)
        integer(8) :: MAX = 10000000
        integer(8) tmp, t, i, j
        type(dh_class), intent(inout) :: rc
        allocate(sieve(MAX))
        sieve(:) = .true.
        sieve(1) = .false.
        do i = 3, MAX, 2
            if (sieve(i)) then
                do j = i * 2, MAX, i
                    sieve(j) = .false.
                end do
            end if
        end do
        t = time()
        tmp = mod(random(t), MAX) + 1
        do
            if (sieve(tmp)) then
                rc%p = tmp
                exit
            end if
            tmp = tmp + 1
        end do
        tmp = mod(random(t), MAX / 100000) + 1
        do
            if (sieve(tmp)) then
                rc%g = tmp
                exit
            end if
            tmp = tmp + 1
        end do
        deallocate(sieve)

        ! �A���X�̔閧��
        do
            rc%a = mod(random(t), MAX) + 1
            if (1 < rc%a .and. rc%a < rc%p - 2) then
                exit
            end if
        end do
        ! �{�u�̔閧��
        do
            rc%b = mod(random(t), MAX) + 1
            if (1 < rc%b .and. rc%b < rc%p - 2) then
                exit
            end if
        end do
    end subroutine init

    integer(8) function nxor(a, b) result(c)
        implicit none
        integer(8), intent(in) :: a, b
        c = xor(a, b)
    end function nxor

    integer(8) function l_shift(a, b) result(c)
        implicit none
        integer(8), intent(in) :: a, b
        c = lshift(a, b)
    end function l_shift

    integer(8) function r_shift(a, b) result(c)
        implicit none
        integer(8), intent(in) :: a, b
        c = rshift(a, b)
    end function r_shift
end module dh

program main
    use dh
    implicit none
    type(dh_class) :: rc
    ! ������
    call init(rc)
    print '("\n�f��p, g               :", I0, ", ", I0)', rc%p, rc%g

    print '("\n�A���X�̃v���C�x�[�g�� :", I0)', rc%a
    print '("�{�u�̃v���C�x�[�g��   :", I0)', rc%b

    ! �������ꂽ�����擾
    rc%x = modPow(rc%g, rc%a, rc%p)
    rc%y = modPow(rc%g, rc%b, rc%p)

    ! ��������̔閧���̐���
    rc%at = modPow(rc%y, rc%a, rc%p)
    rc%bt = modPow(rc%x, rc%b, rc%p)
    print '("\n�A���X�̔閧��         :", I0)', rc%at
    print '("�{�u�̔閧��           :", I0)', rc%bt

    print '(A)', "\nPress Enter to exit."

    read *
contains
    ! �ׂ���]
    pure integer(8) function modPow(a, k, n)
        implicit none
        integer(8), intent(in) :: a, k, n
        integer(8) i, va, t
        t = mod(a, n)
        if (a .eq. 0 .or. n .eq. 0) then
            modPow = 0
        else if (k .eq. 0) then
            modPow = mod(1, n)
        end if
        va = 1
        do i = 0, k-1
            va = va * t
            if (va >= n) va = mod(va, n)
        end do
        modPow = va
    end function modPow
end program main